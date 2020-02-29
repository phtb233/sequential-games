-- NOTE: This code was generated, and not written by the project's author.

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Chat.Data
import Yesod.Auth.Dummy
import System.Random
import Data.Maybe (fromJust)
import Data.List (nubBy)
import Handler.Helper
import qualified Data.Map.Strict as Map

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

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

instance Yesod App where
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    yesodMiddleware = defaultYesodMiddleware

    defaultLayout widget = do
        master <- getYesod

        pc <- widgetToPageContent $ do
            addScript $ StaticR js_bootstrap_toolkit_js
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheetRemote $ 
                "https://fonts.googleapis.com/css?family=Lobster"
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    authRoute _ = Just $ AuthR LoginR

    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

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
        genFileName lbs = "autogen-" ++ base64md5 lbs

    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

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

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = Text

    loginDest _ = HomeR
    logoutDest _ = HomeR
    redirectToReferer _ = True

    authPlugins _ = [authDummy]
    getAuthId = return . Just . credsIdent
    maybeAuthId = lookupSession "_ID"

    authHttpManager = getHttpManager

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- The implementations of the functions made accessible to the chat module.
instance YesodChat App where
        -- These functions query the user's session data for their
        -- details.
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
            liftIO $ putStrLn "Current lobby:"
            liftIO $ print newLobby
        searchForOpponent = do
            pid <- getUserId
            name <- getUserName
            game <- getCurrentGame
            testSearchForOpponent pid name game
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
                         filter (\(id,_) -> id /= pid && id /= oppId) ls'
                    liftIO $ putMVar lobby newLobby
                    liftIO $ putStrLn $ "The new lobby after removal of "
                                        ++ pid ++ " and " ++ oppId ++ " : "
                    liftIO $ print newLobby
                    -- Add them to matches map.
                    addMatch pid name oppId oppName game
                    return $ Just (oppId,oppName)
        getCurrentOpponent pid game = do
            let getMatches = whichMatches game
            mvmatches <- getMatches <$> getYesod
            matches <- liftIO $ readMVar mvmatches
            liftIO $ print matches
            return $ Map.lookup pid matches
        quitMatch = removeMatch
        removeFromLobby = do
            pid <- getUserId
            game <- getCurrentGame 
            testRemoveFromLobby pid game
        testRemoveFromLobby pid game = do
            let getLobby = whichLobby game
            lobby <- getLobby <$> getYesod
            ls <- liftIO $ takeMVar lobby
            let newLobby = filter (\(id,_) -> id /= pid) ls
            liftIO $ putMVar lobby newLobby
            liftIO $ putStrLn "Current lobby:"
            liftIO $ print newLobby
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
changeId :: Handler ()
changeId = do
        num <- liftIO $ show <$> (randomIO :: IO Int)
        setSession "player_id" $ pack num

-- Could've been shorter by using session data, but this was avoided for
-- the sake of testing.
addMatch :: Text -> Text -> Text -> Text -> Text -> Handler ()
addMatch pid name oppId oppName game = do
        let getMatches = whichMatches game
        matches <- getMatches <$> getYesod
        ms <- liftIO $ takeMVar matches
        let newMs =  Map.insert pid (oppId, oppName) ms
            newMs' = Map.insert oppId (pid, name) newMs
        liftIO $ putMVar matches newMs'
        liftIO $ putStrLn "Current matches:"
        liftIO $ print newMs'        

removeMatch :: Text -> Text -> Text -> Handler ()
removeMatch pid oppId game = do
        let getMatches = whichMatches game
        matches <- getMatches <$> getYesod
        ms <- liftIO $ takeMVar matches
        let newMs =  Map.delete pid   ms
            newMs' = Map.delete oppId newMs
        liftIO $ putMVar matches newMs'
        liftIO $ putStrLn "Current matches:"
        liftIO $ print newMs'        

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

