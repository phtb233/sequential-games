{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, RankNTypes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Chat.Data where

{- 
- The client communicates with the chat application via AJAX requests,
- which are handled by the routes defined here. 
-
-}

import Blaze.ByteString.Builder.Char.Utf8 (fromText, fromString, fromShow)
import Blaze.ByteString.Builder (fromByteString, Builder)
import Control.Concurrent.Chan
import Network.Wai.EventSource
import Data.Monoid                        ((<>))
import ClassyPrelude hiding ((<>), writeChan, dupChan, fromString, Builder)
import Yesod
import Data.String.Interpolate (i)
import Yesod.EventSource
import Data.Conduit (bracketP, yield)
import Control.Concurrent (threadDelay)
import Control.Monad.Logger (MonadLogger(monadLoggerLog), defaultLoc)
import Utils (logInfoHelper)
import qualified Network.Wai as W
import qualified Data.IORef as I
import Yesod.Core.Types (GHState(..), HandlerFor (HandlerFor), HandlerData (..), SubHandlerFor (SubHandlerFor))
import ClassyPrelude.Conduit (runResourceT)
import Control.Monad.Trans.Resource (withInternalState)

newtype Chat = Chat (Chan ServerEvent)

mkYesodSubData "Chat" [parseRoutes|
                      /send       SendR       POST
                      /recv       ReceiveR    GET
                      /uname      UsernameR   POST
                      /lobby      LobbyR      POST
                      /search     SearchR     POST
                      /stopSearch StopSearchR POST
                      /taketurn   TakeTurnR   POST
                      /close      CloseR      POST
                      /track      TrackR      POST
                      |]

-- This typeclass is used to interact with the main application,
-- particularly the game lobbies etc.
class (Yesod master, RenderMessage master FormMessage)
        => YesodChat master where
        getUserName           :: SubHandlerFor Chat master Text
        getCurrentGame        :: SubHandlerFor Chat master Text
        getUserId             :: SubHandlerFor Chat master Text
        isLoggedIn            :: SubHandlerFor Chat master Bool
        setUserName           :: Text -> SubHandlerFor Chat master ()
        setCurrentGame        :: Text -> SubHandlerFor Chat master ()
        addToLobby            :: SubHandlerFor Chat master ()
        testAddToLobby        :: Text -> Text -> Text -> SubHandlerFor Chat master ()
        removeFromLobby       :: SubHandlerFor Chat master ()
        testRemoveFromLobby   :: Text -> Text -> HandlerFor master ()
        searchForOpponent     :: SubHandlerFor Chat master (Maybe (Text,Text))
        testSearchForOpponent :: Text -> Text -> Text
                                 -> HandlerFor master (Maybe (Text,Text))
        getCurrentOpponent    :: Text -> Text
                                 -> HandlerFor master (Maybe (Text,Text))
        quitMatch             :: Text -> Text -> Text -> HandlerFor master ()
        getLobbyCount         :: Text -> HandlerFor master Int
        getMatchCount         :: Text -> HandlerFor master Int

-- Short-hand for the chat module's type.
-- type ChatHandler a = SubHandlerFor Chat master Html

-- This was defined for the sake of handlerToIO, which only takes master
-- site handlers (e.g. HandlerT site IO a)
-- type ChatHandler' a = forall master. YesodChat master => HandlerFor master a

-- The client sends messages to this route to be relayed to the specified
-- recipient.
postSendR :: (YesodChat master) => SubHandlerFor Chat master ()
postSendR = do
        from <- runInputPost $ ireq textField "from"
        to <-   runInputPost $ ireq textField "to"
        body <- runInputPost $ ireq textField "message"
        sendEvent to "message" $ return $
            fromText from <> fromText " : " <> fromText body

-- Sends a SourceEvent object to the client which they can use to listen
-- for messages.
getReceiveR :: (YesodChat master) => SubHandlerFor Chat master ()
getReceiveR = do
        Chat chan0 <- getSubYesod
        chan <- liftIO $ dupChan chan0
        sendWaiApplication $ eventSourceAppChan chan

-- The client is required to supply a username, which the server will use
-- when they send messages. The response body contains their unique id.
postUsernameR :: (YesodChat master) => SubHandlerFor Chat master Text
postUsernameR = do
        username <- runInputPost $ ireq textField "username"
        setUserName username
        pid <- getUserId
        monadLoggerLog defaultLoc "postUsernameR" LevelInfo ("Username = " ++ username)
        monadLoggerLog defaultLoc "postUsernameR" LevelInfo ("PID = " ++ pid)
        {-lift $ setCurrentGame game-}
        return pid

-- Add a player to the lobby. Those in the lobby can be matched with each
-- other to start a competitive match.
postLobbyR :: (YesodChat master) => SubHandlerFor Chat master ()
postLobbyR = do
        name <- runInputPost $ ireq textField "name"
        pid <- runInputPost $ ireq textField "id"
        game <- runInputPost $ ireq textField "game"
        liftIO $ putStrLn "LobbyR called"
        {-lift $ addToLobby-}
        -- Because reading from session on same computer causes problems
        testAddToLobby pid name game
        -- Send event updating the number of people in lobby.
        sendGameStatus game

-- Set up a match between a pair of players, and send each of them an SSE.
-- The SSE specifies whether they go first (player X) or not (player O).
-- If an opponent couldn't be found, do nothing.
postSearchR :: (YesodChat master) => SubHandlerFor Chat master ()
postSearchR = do
        liftIO $ putStrLn "SearchR called"
        pid <- runInputPost $ ireq textField "id"
        name <- runInputPost $ ireq textField "name"
        game <- runInputPost $ ireq textField "game"
        maybeOpp <- liftHandler $ testSearchForOpponent pid name game
        -- Get opponent id, send an sse to them and this player.
        case maybeOpp of
            Just (oppId,oppName) -> do
                sendEvent pid  "match" $ jsonBuilder [("player", "X")
                                                     ,("oppId", oppId)
                                                     ,("oppName", oppName)]
                sendEvent oppId "match" $ jsonBuilder [("player", "O")
                                                     ,("oppId", pid)
                                                     ,("oppName", name)]
            Nothing -> return ()
        sendGameStatus game

-- Get the move played from the client's JSON request body. Send
-- this information to their opponent.
postTakeTurnR :: (YesodChat master) => SubHandlerFor Chat master ()
postTakeTurnR = do
        pid   <- runInputPost $ ireq textField "id"
        move  <- runInputPost $ ireq textField "move"
        game  <- runInputPost $ ireq textField "game"
        maybeOppId <- liftHandler $ getCurrentOpponent pid game
        case maybeOppId of
            Just (oppId,_) -> sendEvent oppId "move" [fromText move]
            Nothing  -> sendEvent pid "error" $
                                [fromByteString "Server error: lobby mismatch"]

-- Close the current match between this player and their opponent.
-- First, find their match and remove it from the application.
-- Then send them both an sse to close their multiplayer game.
postCloseR :: (YesodChat master) => SubHandlerFor Chat master ()
postCloseR = do
        pid <- runInputPost $ ireq textField "id"
        game <- runInputPost $ ireq textField "game"
        -- Check if the opponent quit or disconnected.
        maybeOppId <- liftHandler $ getCurrentOpponent pid game
        logInfoHelper "postCloseR" "opponent: "
        logInfoHelper "postCloseR" [i|#{maybeOppId}|]
        -- If they quit, alert them both, otherwise alert connected player.
        case maybeOppId of
            Just (oppId, oppName) -> do
                liftHandler $ quitMatch pid oppId game
                sendEvent pid "close" [fromString "You quit the game"]
                sendEvent oppId "close" [fromText $
                                            oppName ++ " quit the game"]
                logInfoHelper "postCloseR" "Players have been sent SSE to close"
            _ -> sendEvent pid "close" [fromString "Your opponent left"]
        logInfoHelper "postCloseR" "Sending game status"
        sendGameStatus game

-- No opponents were found during the searching interval.
-- Stop the search and remove the player from the lobby.
postStopSearchR :: (YesodChat master) => SubHandlerFor Chat master ()
postStopSearchR = do
        pid <- runInputPost $ ireq textField "id"
        game <- runInputPost $ ireq textField "game"
        liftHandler $ testRemoveFromLobby pid game
        sendGameStatus game

-- Taken from Michael Snoyman, Google Groups:
-- URL  : https://groups.google.com/forum/#!topic/yesodweb/UFZaplhoTU0
-- Date : 01/06/2014

-- Accept an AJAX request, respond with infinite loop pinging the client.
-- When ping fails, user has disconnected. Remove them from lobby/matches.
postTrackR :: (YesodChat master) => SubHandlerFor Chat master TypedContent
postTrackR = do
        pid  <- runInputPost $ ireq textField "id"
        name <- runInputPost $ ireq textField "name"
        game <- runInputPost $ ireq textField "game"
        -- respond "text/plain" ("test" :: String)
        runInnerHandlerMaybe <- liftHandler handlerToIO
        runInnerHandlerInt <- liftHandler handlerToIO
        runInnerHandler <- liftHandler handlerToIO
        runOuterHandler <- liftHandler handlerToIO
        runSubHandler <- subHandlerToIO
        liftHandler $ repEventSource $ \_ -> bracketP
            (do -- Initial setup.
                runOuterHandler $ logInfoHelper "postTrackR" "received connection"
            )
            (\_ -> do  -- Cleanup. 
                runOuterHandler $ logInfoHelper "postTrackR" "connection terminated"
                maybeOpp <- runInnerHandlerMaybe $ getCurrentOpponent pid game
                -- Remove player from lobby.
                runInnerHandler $ testRemoveFromLobby pid game
                -- Remove match and notify other player of disconnection.
                case maybeOpp of
                  Just (oppId, _) -> do
                        runInnerHandler $ quitMatch pid oppId game
                        runInnerHandler $ sendEvent' oppId "close"
                                    [fromText $ name
                                        ++ " has disconnected"]
                  _ -> runInnerHandler $ logInfoHelper "postTrackR" "Couldn't find the opponent"
                lobbyCount <- runInnerHandlerInt $ getLobbyCount game
                matchCount <- runInnerHandlerInt $ getMatchCount game
                runSubHandler $
                        sendEvent "" "lobbyCount" [fromShow lobbyCount]
                runSubHandler $
                        sendEvent "" "matchCount" [fromShow matchCount]
            )
            $ \_ -> forever $ do -- Ping to check the connection.
                liftIO $ do
                    runInnerHandler $ logInfoHelper "postTrackR" "delaying"
                    threadDelay 5000000
                    runInnerHandler $ logInfoHelper "postTrackR" "pinging the client"
                yield $ ServerEvent Nothing Nothing [fromByteString "Hello"]
    -- There must be a better way. Had to rewrite sendEvent for 
    -- (HandlerT Chat IO a) monad, i.e not wrapping master site. This is
    -- because handlerToIO accepts this type, not the subsite wrapper.
    where sendEvent' :: (YesodChat master) => Text -> Text -> [Builder] -> HandlerFor master ()
          sendEvent' s t _ = do
            logInfoHelper "sendEvent" [i|#{s}, #{t} |]


-- Send an SSE to a particular client (every client receives this message,
-- but only one will be listening for it).
sendEvent :: YesodChat master => Text -> Text -> [Builder] -> SubHandlerFor Chat master ()
sendEvent toId suffix message = do
    Chat chan <- liftSubHandler getSubYesod
    liftIO $ writeChan chan $
        ServerEvent (Just (fromText $ toId ++ suffix)) Nothing message

-- Send the number of players in lobbies/matches.
sendGameStatus :: (YesodChat master) => Text -> SubHandlerFor Chat master ()
sendGameStatus game = do
        lobbyCount <- liftHandler $ getLobbyCount game
        matchCount <- liftHandler $ getMatchCount game
        sendEvent "" "lobbyCount" [fromShow lobbyCount]
        sendEvent "" "matchCount" [fromShow matchCount]

-- Turn a series of name-value pairs to a JSON object stored in
-- a [Builder].
jsonBuilder :: [(Text, Text)] -> [Builder]
jsonBuilder    [] = []
jsonBuilder pairs =
  let body  = map (\(k,v) -> fromString (show k)
                          <> fromString " : "
                          <> fromString (show v)) pairs
      leftb  = [fromText "{"]
      rightb = [fromText "}"]
      comma  = fromText ","
  in  leftb ++ intersperse comma body ++ rightb


-- Copied from https://hackage.haskell.org/package/yesod-core-1.6.24.3/docs/src/Yesod.Core.Handler.html#handlerToIO
subHandlerToIO :: MonadIO m => SubHandlerFor sub master (SubHandlerFor sub master a -> m a)
subHandlerToIO = 
    SubHandlerFor $ \oldHandlerData -> do
    -- Take just the bits we need from oldHandlerData.
    let newReq = oldReq { reqWaiRequest = newWaiReq }
          where
            oldReq    = handlerRequest oldHandlerData
            oldWaiReq = reqWaiRequest oldReq
            newWaiReq = oldWaiReq { W.requestBody = return mempty
                                  , W.requestBodyLength = W.KnownLength 0
                                  }
        oldEnv = handlerEnv oldHandlerData
    newState <- liftIO $ do
      oldState <- I.readIORef (handlerState oldHandlerData)
      return $ oldState { ghsRBC = Nothing
                        , ghsIdent = 1
                        , ghsCache = mempty
                        , ghsCacheBy = mempty
                        , ghsHeaders = mempty }

    -- xx From this point onwards, no references to oldHandlerData xx
    liftIO $ evaluate (newReq `seq` oldEnv `seq` newState `seq` ())

    -- Return HandlerFor running function.
    return $ \(SubHandlerFor f) ->
      liftIO $
      runResourceT $ withInternalState $ \resState -> do
        -- The state IORef needs to be created here, otherwise it
        -- will be shared by different invocations of this function.
        newStateIORef <- liftIO (I.newIORef newState)
        let newHandlerData =
              HandlerData
                { handlerRequest  = newReq
                , handlerEnv      = oldEnv
                , handlerState    = newStateIORef
                , handlerResource = resState
                }
        liftIO (f newHandlerData)

