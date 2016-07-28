{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, RankNTypes, TemplateHaskell, TypeFamilies #-}
module Chat.Data where

import Blaze.ByteString.Builder.Char.Utf8 (fromText, fromString)
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

type ChatHandler a = forall master. YesodChat master =>
                        HandlerT Chat (HandlerT master IO) a

type ChatHandler' a = HandlerT Chat IO a

postSendR :: ChatHandler ()
postSendR = do
        from <-   lift $ runInputPost $ ireq textField "from"
        to <-   lift $ runInputPost $ ireq textField "to"
        body <- lift $ runInputPost $ ireq textField "message"
        sendEvent to "message" $ return $
            fromText from <> fromText " : " <> fromText body

getReceiveR :: ChatHandler ()
getReceiveR = do
        Chat chan0 <- getYesod
        chan <- liftIO $ dupChan chan0
        -- sendWaiApplication blocks.
        -- sendWaiApplication short circuits (anything after it is not
        -- reached).
        {-key <- register $ liftIO $ putStrLn "disconnect"-}
        sendWaiApplication $ eventSourceAppChan chan

postUsernameR :: ChatHandler Text
postUsernameR = do
        username <- lift $ runInputPost $ ireq textField "username"
        game     <- lift $ runInputPost $ ireq textField "game"
        pid <- lift getUserId
        liftIO $ print username
        liftIO $ print pid
        lift $ setUserName username
        lift $ setCurrentGame game
        return pid

postLobbyR :: ChatHandler ()
postLobbyR = do
        name <- lift $ runInputPost $ ireq textField "name"
        pid <- lift $ runInputPost $ ireq textField "id"
        game <- lift $ runInputPost $ ireq textField "game"
        liftIO $ putStrLn "LobbyR called"
        {-lift $ addToLobby-}
        -- Because reading from session on same computer causes problems
        lift $ testAddToLobby pid name game

-- Set up a match between a pair of players, and send each of them an sse.
-- The sse specifies whether they go first (player X) or not (player O).
-- Otherwise, do nothing.
postSearchR :: ChatHandler ()
postSearchR = do
        liftIO $ putStrLn "SearchR called"
        pid <- lift $ runInputPost $ ireq textField "id"
        name <- lift $ runInputPost $ ireq textField "name"
        game <- lift $ runInputPost $ ireq textField "game"
        {-name <- lift getUserName-}
        {-maybeOpp <- lift searchForOpponent-}
        maybeOpp <- lift $ testSearchForOpponent pid name game
        -- Get opponent id, send a sse to them and this player.
        -- Dont use Ajax response, as that can only be sent to one player.
        case maybeOpp of
            Just (oppId,oppName) -> do
                sendEvent pid  "match" $ jsonBuilder [("player", "X")
                                                     ,("oppId", oppId)
                                                     ,("oppName", oppName)]
                sendEvent oppId "match" $ jsonBuilder [("player", "O")
                                                     ,("oppId", pid)
                                                     ,("oppName", name)]
            Nothing -> return ()

postTakeTurnR :: ChatHandler ()
postTakeTurnR = do
        {-pid   <- lift getUserId-}
        pid <- lift $ runInputPost $ ireq textField "id"
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
        maybeOppId <- lift $ getCurrentOpponent pid game
        liftIO $ putStrLn "opponent: "   
        liftIO $ print maybeOppId
        case maybeOppId of
            Just (oppId, oppName) -> do
                lift $ quitMatch pid oppId game
                sendEvent pid "close" [fromString "You quit the game"]
                sendEvent oppId "close" [fromText $ 
                                            oppName ++ " quit the game"]
                liftIO $ putStrLn "Players have been sent sse to close"
            _ -> sendEvent pid "close" [fromString "Your opponent left"]
 
postStopSearchR :: ChatHandler ()
postStopSearchR = do
        pid <- lift $ runInputPost $ ireq textField "id"
        game <- lift $ runInputPost $ ireq textField "game"
        lift $ testRemoveFromLobby pid game

-- Taken from Michael Snoyman, Google Groups:
-- https://groups.google.com/forum/#!topic/yesodweb/UFZaplhoTU0
-- Accept an AJAX request, respond with infinite loop pinging the client.
-- When ping fails, user has disconnected. Remove them from lobby/matches.
postTrackR :: ChatHandler TypedContent
postTrackR = do
        pid  <- lift $ runInputPost $ ireq textField "id"
        name <- lift $ runInputPost $ ireq textField "name"
        game <- lift $ runInputPost $ ireq textField "game"
        runInnerHandlerMaybe <- lift handlerToIO
        runInnerHandler <- lift handlerToIO
        runOuterHandler <- handlerToIO
        lift $ repEventSource $ \_ -> bracketP
            (do
                putStrLn "received connection"
            )
            (\_ -> do 
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
            )
            $ \_ -> forever $ do
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

sendEvent :: Text -> Text -> [Builder] -> ChatHandler ()
sendEvent toId suffix message = do
    Chat chan <- getYesod
    liftIO $ writeChan chan $ ServerEvent (Just (fromText $ toId ++ suffix))
                                Nothing
                                message

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

