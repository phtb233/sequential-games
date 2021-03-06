{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, RankNTypes, TemplateHaskell, TypeFamilies #-}
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

import Yesod.EventSource
import Data.Conduit (bracketP, yield)

data Chat = Chat (Chan ServerEvent)

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
        getUserName           :: HandlerT master IO Text
        getCurrentGame        :: HandlerT master IO Text
        getUserId             :: HandlerT master IO Text
        isLoggedIn            :: HandlerT master IO Bool
        setUserName           :: Text -> HandlerT master IO ()
        setCurrentGame        :: Text -> HandlerT master IO ()
        addToLobby            :: HandlerT master IO ()
        testAddToLobby        :: Text -> Text -> Text -> HandlerT master IO ()
        removeFromLobby       :: HandlerT master IO ()
        testRemoveFromLobby   :: Text -> Text -> HandlerT master IO ()
        searchForOpponent     :: HandlerT master IO (Maybe (Text,Text))
        testSearchForOpponent :: Text -> Text -> Text 
                                 -> HandlerT master IO (Maybe (Text,Text))
        getCurrentOpponent    :: Text -> Text
                                 -> HandlerT master IO (Maybe (Text,Text))
        quitMatch             :: Text -> Text -> Text -> HandlerT master IO ()
        getLobbyCount         :: Text -> HandlerT master IO Int
        getMatchCount         :: Text -> HandlerT master IO Int

-- Short-hand for the chat module's type.
type ChatHandler a = forall master. YesodChat master =>
                        HandlerT Chat (HandlerT master IO) a

-- This was defined for the sake of handlerToIO, which only takes master
-- site handlers (e.g. HandlerT site IO a)
type ChatHandler' a = HandlerT Chat IO a

-- The client sends messages to this route to be relayed to the specified
-- recipient.
postSendR :: ChatHandler ()
postSendR = do
        from <- lift $ runInputPost $ ireq textField "from"
        to <-   lift $ runInputPost $ ireq textField "to"
        body <- lift $ runInputPost $ ireq textField "message"
        sendEvent to "message" $ return $
            fromText from <> fromText " : " <> fromText body

-- Sends a SourceEvent object to the client which they can use to listen
-- for messages.
getReceiveR :: ChatHandler ()
getReceiveR = do
        Chat chan0 <- getYesod
        chan <- liftIO $ dupChan chan0
        sendWaiApplication $ eventSourceAppChan chan

-- The client is required to supply a username, which the server will use
-- when they send messages. The response body contains their unique id.
postUsernameR :: ChatHandler Text
postUsernameR = do
        username <- lift $ runInputPost $ ireq textField "username"
        lift $ setUserName username
        pid <- lift getUserId
        liftIO $ print username
        liftIO $ print pid
        {-lift $ setCurrentGame game-}
        return pid

-- Add a player to the lobby. Those in the lobby can be matched with each
-- other to start a competitive match.
postLobbyR :: ChatHandler ()
postLobbyR = do
        name <- lift $ runInputPost $ ireq textField "name"
        pid <- lift $ runInputPost $ ireq textField "id"
        game <- lift $ runInputPost $ ireq textField "game"
        liftIO $ putStrLn "LobbyR called"
        {-lift $ addToLobby-}
        -- Because reading from session on same computer causes problems
        lift $ testAddToLobby pid name game
        -- Send event updating the number of people in lobby.
        sendGameStatus game

-- Set up a match between a pair of players, and send each of them an SSE.
-- The SSE specifies whether they go first (player X) or not (player O).
-- If an opponent couldn't be found, do nothing.
postSearchR :: ChatHandler ()
postSearchR = do
        liftIO $ putStrLn "SearchR called"
        pid <- lift $ runInputPost $ ireq textField "id"
        name <- lift $ runInputPost $ ireq textField "name"
        game <- lift $ runInputPost $ ireq textField "game"
        maybeOpp <- lift $ testSearchForOpponent pid name game
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
postTakeTurnR :: ChatHandler ()
postTakeTurnR = do
        pid   <- lift $ runInputPost $ ireq textField "id"
        move  <- lift $ runInputPost $ ireq textField "move"
        game  <- lift $ runInputPost $ ireq textField "game"
        maybeOppId <- lift $ getCurrentOpponent pid game
        case maybeOppId of
            Just (oppId,_) -> sendEvent oppId "move" [fromText move]
            Nothing  -> sendEvent pid "error" $
                                [fromByteString "Server error: lobby mismatch"]

-- Close the current match between this player and their opponent.
-- First, find their match and remove it from the application.
-- Then send them both an sse to close their multiplayer game.
postCloseR :: ChatHandler ()
postCloseR = do
        pid <- lift $ runInputPost $ ireq textField "id"
        game <- lift $ runInputPost $ ireq textField "game"
        -- Check if the opponent quit or disconnected.
        maybeOppId <- lift $ getCurrentOpponent pid game
        liftIO $ putStrLn "opponent: "   
        liftIO $ print maybeOppId
        -- If they quit, alert them both, otherwise alert connected player.
        case maybeOppId of
            Just (oppId, oppName) -> do
                lift $ quitMatch pid oppId game
                sendEvent pid "close" [fromString "You quit the game"]
                sendEvent oppId "close" [fromText $ 
                                            oppName ++ " quit the game"]
                liftIO $ putStrLn "Players have been sent sse to close"
            _ -> sendEvent pid "close" [fromString "Your opponent left"]
        liftIO $ putStrLn "Sending game status"
        sendGameStatus game
 
-- No opponents were found during the searching interval.
-- Stop the search and remove the player from the lobby.
postStopSearchR :: ChatHandler ()
postStopSearchR = do
        pid <- lift $ runInputPost $ ireq textField "id"
        game <- lift $ runInputPost $ ireq textField "game"
        lift $ testRemoveFromLobby pid game
        sendGameStatus game

-- Taken from Michael Snoyman, Google Groups:
-- URL  : https://groups.google.com/forum/#!topic/yesodweb/UFZaplhoTU0
-- Date : 01/06/2014

-- Accept an AJAX request, respond with infinite loop pinging the client.
-- When ping fails, user has disconnected. Remove them from lobby/matches.
postTrackR :: ChatHandler TypedContent
postTrackR = do
        pid  <- lift $ runInputPost $ ireq textField "id"
        name <- lift $ runInputPost $ ireq textField "name"
        game <- lift $ runInputPost $ ireq textField "game"
        runInnerHandlerMaybe <- lift handlerToIO
        runInnerHandlerInt <- lift handlerToIO
        runInnerHandler <- lift handlerToIO
        runOuterHandler <- handlerToIO
        lift $ repEventSource $ \_ -> bracketP
            (do -- Initial setup.
                putStrLn "received connection"
            )
            (\_ -> do  -- Cleanup. 
                putStrLn $ "connection terminated"
                maybeOpp <- runInnerHandlerMaybe $ getCurrentOpponent pid game
                -- Remove player from lobby.
                runInnerHandler $ testRemoveFromLobby pid game
                -- Remove match and notify other player of disconnection.
                case maybeOpp of
                  Just (oppId, _) -> do
                        runInnerHandler $ quitMatch pid oppId game
                        runOuterHandler $ sendEvent' oppId "close"
                                    [fromText $ name
                                        ++ " has disconnected"]
                  _ -> putStrLn "Couldn't find the opponent"
                lobbyCount <- runInnerHandlerInt $ getLobbyCount game
                matchCount <- runInnerHandlerInt $ getMatchCount game
                runOuterHandler $ 
                        sendEvent' "" "lobbyCount" [fromShow lobbyCount]
                runOuterHandler $ 
                        sendEvent' "" "matchCount" [fromShow matchCount]
            )
            $ \_ -> forever $ do -- Ping to check the connection.
                liftIO $ do
                    putStrLn "delaying"
                    threadDelay 5000000
                    putStrLn "pinging the client"
                yield $ ServerEvent Nothing Nothing [fromByteString "Hello"]
    -- There must be a better way. Had to rewrite sendEvent for 
    -- (HandlerT Chat IO a) monad, i.e not wrapping master site. This is
    -- because handlerToIO accepts this type, not the subsite wrapper.
    where sendEvent' :: Text -> Text -> [Builder] -> ChatHandler' ()
          sendEvent' toId suffix message = do
              Chat chan <- getYesod
              liftIO $ writeChan chan $ 
                ServerEvent (Just (fromText $ toId ++ suffix)) Nothing message

-- Send an SSE to a particular client (every client receives this message,
-- but only one will be listening for it).
sendEvent :: Text -> Text -> [Builder] -> ChatHandler ()
sendEvent toId suffix message = do
    Chat chan <- getYesod
    liftIO $ writeChan chan $ ServerEvent (Just (fromText $ toId ++ suffix))
                                Nothing
                                message

-- Send the number of players in lobbies/matches.
sendGameStatus :: Text -> ChatHandler ()
sendGameStatus game = do
        lobbyCount <- lift $ getLobbyCount game
        matchCount <- lift $ getMatchCount game
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
  in  leftb ++ (intersperse comma body) ++ rightb

