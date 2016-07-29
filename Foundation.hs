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
-- ghc-mod: readCreateProcess: cabal "configure" "--with-ghc=ghc" "--flags" 
-- "library-only" (exit 1): failed
import Chat.Data
import Yesod.Auth.Dummy
import System.Random
import Data.Maybe (fromJust)
import Data.List (nubBy)
import Handler.Helper
import qualified Data.Map.Strict as Map

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , getChat        :: Chat
    , getTicTacToeLobby       :: MVar [(Text,Text)]
    , getTicTacToeMatches     :: MVar (Map Text (Text,Text))
    , getConnect3Lobby        :: MVar [(Text,Text)]
    , getConnect3Matches      :: MVar (Map Text (Text,Text))
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
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout widget = do
        master <- getYesod
        {-mmsg <- getMessage-}

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

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
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
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

navbar :: Widget
navbar = [whamlet|
            <div #nav-bar>
                <div>
                    <span>
                        <a href=@{TicTacToeR}>Tic-Tac-Toe
                <div>
                    <span>
                        <a href=@{Connect3R}>Connect 3
                <div>
                    <span>
                        <a href=@{SudokuR}>Sudoku
        |]

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    {-type AuthId App = UserId-}
    type AuthId App = Text

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    {-
     -authenticate creds = runDB $ do
     -    x <- getBy $ UniqueUser $ credsIdent creds
     -    case x of
     -        Just (Entity uid _) -> return $ Authenticated uid
     -        Nothing -> Authenticated <$> insert User
     -            { userIdent = credsIdent creds
     -            , userPassword = Nothing
     -            }
     -}

    -- You can add other plugins like Google Email, email or OAuth here
    {-authPlugins _ = [authOpenId Claimed []]-}
    authPlugins _ = [authDummy]
    getAuthId = return . Just . credsIdent
    maybeAuthId = lookupSession "_ID"

    authHttpManager = getHttpManager

{-instance YesodAuthPersist App-}

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
--
-- ADDED
instance YesodChat App where
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
        setUserName newName = setSession "player_name" $ newName
        setCurrentGame newGame = setSession "player_game" $ newGame
        addToLobby = do
            pid   <- getUserId
            name  <- getUserName
            game <- getCurrentGame
            testAddToLobby pid name game
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
            {-pid <- getUserId-}
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
            {-liftIO $ putStrLn "takeMVar on Lobby"-}
            let newLobby = filter (\(id,_) -> id /= pid) ls
            {-liftIO $ putStrLn "putMVar on Lobby"-}
            liftIO $ putMVar lobby newLobby
            liftIO $ putStrLn "Current lobby:"
            liftIO $ print newLobby

changeId :: Handler ()
changeId = do
        num <- liftIO $ show <$> (randomIO :: IO Int)
        setSession "player_id" $ pack num

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

