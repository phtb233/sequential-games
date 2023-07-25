{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Foundation where

import Import.NoFoundation
import Data.Kind            (Type)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Control.Monad.Logger (LogSource)

-- Used only when in "auth-dummy-login" setting is enabled.

import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Chat.Data
import System.Random
import Data.Maybe (fromJust)
import Data.List (nubBy)
import Handler.Helper
import qualified Data.Map.Strict as Map
import Utils(logInfoHelper)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static 
    , appConnPool    :: ConnectionPool 
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , getChat        :: Chat
    , getTicTacToeLobby       :: MVar [(Text,Text)]
    , getTicTacToeMatches     :: MVar (Map Text (Text,Text))
    , getConnect3Lobby        :: MVar [(Text,Text)]
    , getConnect3Matches      :: MVar (Map Text (Text,Text))
    -- For reading Sudoku puzzles from a local text file.
    , getPuzzles :: [[Maybe Int]]
    }


-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: Type -> Type).
    (MonadUnliftIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addScript $ StaticR js_bootstrap_toolkit_js
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheetRemote $ 
                "https://fonts.googleapis.com/css?family=Lobster"
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")


    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

-- Main navigation links.
navbar :: Widget
navbar = [whamlet|
            <div #nav-bar>
                <div>
                    <span>
                        <a href=@{HomeR}>Home
                <div>
                    <span>
                        <a href=@{TicTacToeR}>Tic-Tac-Toe
                <div>
                    <span>
                        <a href=@{Connect3R}>Connect 3
                <div>
                    <span>
                        <a href=@{SudokuR}>Sudoku
                <div>
                    <span>
                        <a href=@{NQueensR}>N-Queens
        |]

-- Unfortunate workaround to make the puzzle fit smaller screens.
adaptiveText :: Widget
adaptiveText = toWidget [lucius|
    @media (max-width:767px){ /*xs*/ 
        .player-info { top: 8%; }
        body { font-size: smaller; }
    }
    @media (min-width:768px){ /*sm*/ 
        .player-info { top: 8%; }
        body { font-size: small; }
    }
    @media (min-width:992px){ /*md*/ 
        .player-info { top: 20%; }
        body {font-size: large;}
    }
    @media (min-width:1200px){ /*lg*/ 
        .player-info { top: 20%; }
        body {font-size: large;}
    }
|]

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

instance YesodChat App where
        -- These functions query the user's session data for their
        -- details.
        getUserName :: SubHandlerFor Chat App Text
        getUserName = do
            name <- lookupSession "player_name" 
            return $ fromJust name
        getUserId = do
            pid <- lookupSession "player_id"
            return $ fromJust pid
        getCurrentGame = do
            game <- lookupSession "player_game"
            return $ fromJust game
        isLoggedIn = do
            musrname <- lookupSession "player_name" 
            return $ isJust musrname
        setUserName newName = do
            changeId
            setSession "player_name" $ newName
        -- Not a good idea to store which game player is playing on
        -- serverside. Better to send from client. (unused)
        setCurrentGame newGame = setSession "player_game" $ newGame
        addToLobby = do
            pid   <- getUserId
            name  <- getUserName
            game <- getCurrentGame
            testAddToLobby pid name game
        -- All the test functions avoid querying the session data for the
        -- client's details. This was done while testing on a single
        -- machine to allow multiplayer in different tabs.
        testAddToLobby pid name game  = do
            let getLobby = whichLobby game
            lobby <- getLobby <$> getYesod
            ls <- liftIO $ takeMVar lobby
            {-liftIO $ putStrLn "takeMVar on Lobby"-}
            let newLobby = nubBy func $ (pid, name) : ls
                func (a,_) (b,_) = a == b
            {-liftIO $ putStrLn "putMVar on Lobby"-}
            liftIO $ putMVar lobby newLobby
            logInfoHelper "YesodChat Instance" "Current lobby:"
            logInfoHelper "YesodChat Instance" newLobby
        searchForOpponent = do
            pid <- getUserId
            name <- getUserName
            game <- getCurrentGame
            liftHandler $ testSearchForOpponent pid name game
        testSearchForOpponent pid name game = do
            let getLobby = whichLobby game
            lobby <- getLobby <$> getYesod
            ls    <- liftIO $ readMVar lobby
            -- Look for other players in the lobby.
            let maybeOpp = find ((/= pid) . fst) ls
            case maybeOpp of
                Nothing    -> return Nothing
                Just (oppId,oppName) -> do
                    -- Remove this player and their opponent from lobby.
                    ls' <- liftIO $ takeMVar lobby
                    let newLobby = 
                         filter (\(qid,_) -> qid /= pid && qid /= oppId) ls'
                    liftIO $ putMVar lobby newLobby
                    logInfoHelper "YesodChat Instance" $ "The new lobby after removal of "
                                        ++ pid ++ " and " ++ oppId ++ " : "
                    logInfoHelper "YesodChat Instance" newLobby
                    -- Add them to matches map.
                    void $ addMatch pid name oppId oppName game
                    return $ Just (oppId,oppName)
        getCurrentOpponent pid game = do
            let getMatches = whichMatches game
            mvmatches <- getMatches <$> getYesod
            matches <- liftIO $ readMVar mvmatches
            logInfoHelper "YesodChat Instance" matches
            return $ Map.lookup pid matches
        quitMatch = removeMatch
        removeFromLobby = do
            pid <- getUserId
            game <- getCurrentGame 
            liftHandler $ testRemoveFromLobby pid game
        testRemoveFromLobby pid game = do
            let getLobby = whichLobby game
            lobby <- getLobby <$> getYesod
            ls <- liftIO $ takeMVar lobby
            let newLobby = filter (\(qid,_) -> qid /= pid) ls
            liftIO $ putMVar lobby newLobby
            logInfoHelper "YesodChat Instance" "Current lobby:"
            logInfoHelper "YesodChat Instance" newLobby
        getLobbyCount game = do
            let getLobby = whichLobby game
            lobby <- getLobby <$> getYesod
            ls <- liftIO $ readMVar lobby
            return $ length ls
        getMatchCount game = do
            let getMatches = whichMatches game
            matches <- getMatches <$> getYesod
            ms <- liftIO $ readMVar matches
            return $ length ms `div` 2

-- Generate an ID number, and assign it to the user's session.
changeId :: MonadHandler m => m ()
changeId = do
        num <- liftIO $ show <$> (randomIO :: IO Int)
        setSession "player_id" $ pack num

-- Could've been shorter by using session data, but this was avoided for
-- the sake of testing.
addMatch :: Text -> Text -> Text -> Text -> Text -> HandlerFor App ()
addMatch pid name oppId oppName game = do
        let getMatches = whichMatches game
        matches <- getMatches <$> getYesod
        ms <- liftIO $ takeMVar matches
        let newMs =  Map.insert pid (oppId, oppName) ms
            newMs' = Map.insert oppId (pid, name) newMs
        liftIO $ putMVar matches newMs'
        logInfoHelper "addMatch" "Current matches:"
        logInfoHelper "addMatch" newMs'        

removeMatch :: Text -> Text -> Text -> HandlerFor App ()
removeMatch pid oppId game = do
        let getMatches = whichMatches game
        matches <- getMatches <$> getYesod
        ms <- liftIO $ takeMVar matches
        let newMs =  Map.delete pid   ms
            newMs' = Map.delete oppId newMs
        liftIO $ putMVar matches newMs'
        logInfoHelper "removeMatch" "Current matches:"
        logInfoHelper "removeMatch" newMs'        

-- Takes a string of the game's name and returns the appropriate lobby.
whichLobby :: Text -> (App -> MVar [(Text,Text)])
whichLobby game = case game of
                      "tictactoe" -> getTicTacToeLobby
                      "connect3"  -> getConnect3Lobby
                      _ -> error "Unspecified game"

whichMatches :: Text -> (App -> MVar (Map Text (Text,Text)))
whichMatches game = case game of
                      "tictactoe" -> getTicTacToeMatches
                      "connect3"  -> getConnect3Matches
                      _ -> error "Unspecified game"

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
