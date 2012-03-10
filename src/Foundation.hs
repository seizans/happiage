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
    , requireAuth
    , module Settings
    , module Yesod.Auth
    , module Model
    ) where

import Prelude
import Yesod
import Yesod.Static
import Settings.StaticFiles
import Yesod.Auth
import Yesod.Auth.Email
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Logger (Logger, logMsg, formatLogText)
import Network.HTTP.Conduit (Manager)
#ifdef DEVELOPMENT
import Yesod.Logger (logLazyText)
#endif
import qualified Settings
import qualified Data.ByteString.Lazy as L
import qualified Database.Persist.Store
import Database.Persist.GenericSql
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)
import Text.Shakespeare.Text(stext) -- This is for authEmail. Delete later
import Text.Hamlet (shamlet)
import Data.Maybe (isJust)
import Control.Monad (join)
import Network.Mail.Mime
import Text.Blaze.Renderer.Utf8 (renderHtml)
import qualified Data.Text.Lazy.Encoding
#ifndef DEVELOPMENT
import Network.Mail.Mime (sendmail)
#endif

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

    -- Place the session key file in the config folder
    encryptKey _ = fmap Just $ getKey "config/client_session_key.aes"


    defaultLayout widget = do
        maid <- maybeAuthId
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
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

    -- Enable Javascript async loading
    yepnopeJs _ = Just $ Right $ StaticR js_modernizr_js

-- How to run database actions.
instance YesodPersist Happiage where
    type YesodPersistBackend Happiage = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

instance YesodAuth Happiage where
    type AuthId Happiage = UserAuthId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
      x <- insertBy $ UserAuth (credsIdent creds) Nothing Nothing False
      return $ Just $
        case x of
          Left (Entity userid _) -> userid -- newly added user
          Right userid -> userid -- existing user
{-    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ User (credsIdent creds) Nothing
-}

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authEmail]

    authHttpManager = error "Email doesn't need an HTTP manager"

instance YesodAuthEmail Happiage where
    type AuthEmailId Happiage = UserAuthId
    addUnverified email verkey =
      runDB $ insert $ UserAuth email Nothing (Just verkey) False
    sendVerifyEmail email _ verurl =
      liftIO $ renderSendMail (emptyMail $ Address Nothing "noreply")
        { mailTo = [Address Nothing email]
        , mailHeaders = [("Subject", "Verify your email address")]
        , mailParts = [[textPart, htmlPart]]
        }
      where
        textPart = Part
          { partType = "text/plain charset=utf-8"
          , partEncoding = None
          , partFilename = Nothing
          , partContent = Data.Text.Lazy.Encoding.encodeUtf8 [stext|
Please confirm your email address by clicking on the link below.

\#{verurl}

Thank you
|]
          , partHeaders = []
          }
        htmlPart = Part
          { partType = "text/html; charset=utf-8"
          , partEncoding = None
          , partFilename = Nothing
          , partContent = renderHtml [shamlet|
<p>Please confirm your email address by clicking on the link below.
<p>
    <a href=#{verurl}>#{verurl}
<p>Thank you
|]
          , partHeaders = []
          }
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


-- Sends off your mail. Requires sendmail in production!
deliver :: Happiage -> L.ByteString -> IO ()
#ifdef DEVELOPMENT
deliver y = logLazyText (getLogger y) . Data.Text.Lazy.Encoding.decodeUtf8
#else
deliver _ = sendmail
#endif

--user_authからuserを引いてくる
maybeUserId :: Maybe UserAuthId -> Handler (Maybe UserId)
maybeUserId maid = do
  mUserEntity <- case maid of
    Just authId -> 
      runDB $ do
        mUserEntity <- selectFirst [UserAuthid ==. authId] []
        return $ mUserEntity
    _ -> return Nothing
  return $ fmap entityKey mUserEntity

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage Happiage FormMessage where
    renderMessage _ _ = defaultFormMessage
