module Foundation
    ( Happiage (..)
    , Route (..)
    , HappiageMessage (..)
    , resourcesHappiage
    , Handler
    , Widget
    , Form
    , maybeAuthId
    , maybeUserId
    , getUsersMap
    , requireAuth
    , module Settings
    , module Yesod.Auth
    , module Model
    ) where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.Mail
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Logger (Logger, logMsg, formatLogText)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import qualified Database.Persist.Store
import Settings.StaticFiles
import Database.Persist.MongoDB
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)
import Text.Shakespeare.Text(stext) -- This is for authEmail. Delete later
import Text.Hamlet (shamlet)
import Data.Maybe (isNothing, isJust)
import Control.Monad (join)
import qualified Smtp.SmtpClient as SMTP
import Text.Blaze.Renderer.Utf8 (renderHtml)
import qualified Data.Text.Lazy.Encoding
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Text as DT
import qualified HappiageAuthMessage as HAM
import Yesod.Auth.Mail
import Codec.Binary.UTF8.String as UTF8S

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data Happiage = Happiage
    { settings :: AppConfig DefaultEnv Extra
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    }

-- Set up i18n messages. See the message folder.
mkMessage "Happiage" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype HappiageRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route Happiage = HappiageRoute
-- * Creates the value resourcesHappiage which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- Happiage. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the HappiageRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "Happiage" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm Happiage Happiage (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Happiage where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        return . Just $ clientSessionBackend key 120

    -- アップロードできるサイズ上限
    maximumContentLength _ _ = 100 * 1024 * 1024

    defaultLayout widget = do
        maid <- maybeAuthId
        master <- getYesod
        mmsg <- getMessage
        subroute <- getCurrentRoute
        toMaster <- getRouteToMaster
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            -- $(widgetFile "normalize")
            -- addScript $ StaticR js_jquery_1_7_1_min_js
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    messageLogger y loc level msg =
      formatLogText (getLogger y) loc level msg >>= logMsg (getLogger y)

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- for Authorization isAuthorized :: Route a -> Bool -> GHandler s a AuthResult
    isAuthorized AdminR _ = isAdmin
    isAuthorized AdminuserR _ = isAdmin
    isAuthorized AdminimportR _ = isAdmin
    isAuthorized (AuthR LoginR) _ = return Authorized
--    isAuthorized (AuthR CheckR) _ = return Authorized
--    isAuthorized FaviconR _ = return Authorized
    isAuthorized (AuthR loginR) _ = return Authorized
--    isAuthorized (AuthR (PluginR "mail" ["login"]) ) _ = return Authorized
--    isAuthorized (AuthR (PluginR "mail" ["verify"]) ) _ = return Authorized
--    isAuthorized (AuthR (PluginR "mail" ["password"]) ) _ = return Authorized
    isAuthorized (AuthR (PluginR "mail" ["reset"]) ) _ = return Authorized
    isAuthorized (AuthR (PluginR "mail" ["emailregister"]) ) _ = isAdmin
    isAuthorized (AuthR _) _ = return $ Unauthorized "Untouchable Resource."
    isAuthorized _ _ = isAuthenticated

isAuthenticated = do
    maid <- maybeAuthId
    if isNothing maid
      then return AuthenticationRequired
      else return Authorized

isAdmin = do
    authEntity <- requireAuth
    let auth = (\(Entity authid auth) -> auth) authEntity
    return $ case userAuthEmail auth of
        "seizans@gmail.com" -> Authorized
        "sasaki0603shoko@gmail.com" -> Authorized
        _ -> Unauthorized "You must be admin."

-- How to run database actions.
instance YesodPersist Happiage where
    type YesodPersistBackend Happiage = Action
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

instance YesodAuth Happiage where
    type AuthId Happiage = UserAuthId

    -- Where to send a user after successful login
    loginDest _ = WelcomeR
    -- Where to send a user after logout
    logoutDest _ = WelcomeR

    getAuthId creds = runDB $ do
      x <- insertBy $ UserAuth (credsIdent creds) Nothing Nothing False
      return $ Just $
        case x of
          Left (Entity userid _) -> userid -- newly added user
          Right userid -> userid -- existing user

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authMail]

    renderAuthMessage _ _ = HAM.happiageMessage
    authHttpManager = error "Email doesn't need an HTTP manager"

instance YesodAuthMail Happiage where
    type AuthEmailId Happiage = UserAuthId
    addUnverified email verkey =
      runDB $ insert $ UserAuth email Nothing (Just verkey) False
    sendAdminEmail email _ verurl = do
        authEntity <- requireAuth
        let emailTo = (\(Entity authid auth) -> userAuthEmail auth) authEntity
        liftIO $ SMTP.send (DT.unpack emailTo) "subject" (UTF8S.encodeString $ DT.unpack $ email `DT.append` "\n" `DT.append` verurl `DT.append` "\nend.")
    sendVerifyEmail email _ verurl = 
        liftIO $ SMTP.send (DT.unpack email) "subject"
          (UTF8S.encodeString $ DT.unpack $ "以下のURLでパスワードの再設定を行なってください.\n" `DT.append` verurl `DT.append` "\n\nFrom Happiage.")
    sendInviteEmail = sendVerifyEmail
    sendRegisterEmail = sendVerifyEmail
    getVerifyKey = runDB . fmap (join . fmap userAuthVerkey) . get
    setVerifyKey uid key = runDB $ update uid [UserAuthVerkey =. Just key]
    verifyAccount uid = runDB $ do
      mu <- get uid
      case mu of
        Nothing -> return Nothing
        Just u -> do
          update uid [UserAuthVerified =. True]
          return $ Just uid
    getPassword = runDB . fmap (join . fmap userAuthPassword) . get
    setPassword uid pass = runDB $ update uid [UserAuthPassword =. Just pass]
    getEmailCreds email = runDB $ do
      mu <- getBy $ UniqueUserAuth email
      case mu of
        Nothing -> return Nothing
        Just (Entity uid u) -> return $ Just EmailCreds
          { emailCredsId = uid
          , emailCredsAuthId = Just uid
          , emailCredsStatus = isJust $ userAuthPassword u
          , emailCredsVerkey = userAuthVerkey u
          }
    getEmail = runDB . fmap (fmap userAuthEmail) . get

--user_authからuserを引いてくる
maybeUserId :: Maybe UserAuthId -> Handler (Maybe UserId)
maybeUserId maid = do
    mUserEntity <- case maid of
        Just authId -> runDB $ selectFirst [UserAuthid ==. authId] []
        _ -> return Nothing
    return $ fmap entityKey mUserEntity

-- auth_id から user を引く
maybeUser :: UserAuthId -> Handler (Maybe User)
maybeUser authid = do
    mUserEntity <- runDB $ selectFirst [UserAuthid ==. authid] []
    return $ fmap (\(Entity _ user) -> user) mUserEntity

--全ユーザを引いてくる
getUsersMap :: Handler (Map.Map UserId User)
getUsersMap = do
    userEntities <- runDB $ selectList [UserDeleted ==. False] []
    return $ Map.fromList $ map (\(Entity pid val) -> (pid, val)) userEntities

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage Happiage FormMessage where
    renderMessage _ _ = defaultFormMessage
