module Handler.Root where

import Import
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Debug.Trace --printfデバッグ用
traceD a b = trace ("TRACE DEBUG:" ++ show a) b
printD a = putStrLn ("TRACE DEBUG:" ++ show a)

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

--トップページ
getRootR :: Handler RepHtml
getRootR = do
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "happiage homepage"
        $(widgetFile "homepage")

--開催案内ページ
getGuideR :: Handler RepHtml
getGuideR = do
    defaultLayout $ do
        h2id <- lift newIdent
        $(widgetFile "guide")

--参加登録ページ
getRegisterR :: Handler RepHtml
getRegisterR = do
  ((_, widget), enctype) <- runFormPost registerForm
  defaultLayout $ do
    h2id <- lift newIdent
    $(widgetFile "register")

postRegisterR :: Handler RepHtml
postRegisterR = do
  ((res, widget), enctype) <- runFormPost registerForm
  mUserRegisterInfo <- case res of
    FormSuccess userRegisterInfo -> return $ Just userRegisterInfo
    _ -> return Nothing
  maid <- maybeAuthId
  --ログイン時、postデータがあった場合に、書き込み
  case (mUserRegisterInfo, maid) of
    (Just userRegisterInfo, Just authid) ->
      runDB $ do --TODO:runDBのエラーチェック
        _ <- insert $ User {
          userAuthid = authid, 
          userNickname = urNickname userRegisterInfo, 
          userFirstname = urFirstname userRegisterInfo, 
          userFamilyname = urFamilyname userRegisterInfo, 
          userKanafirst = urKanafirst userRegisterInfo, 
          userKanafamily = urKanafamily userRegisterInfo, 
          userSex = Male, 
          userAttend = Suspense, 
          userInvitedby = Nothing, 
          userDeleted = False
        }
        return ()
    _ -> return ()
  defaultLayout $ case (mUserRegisterInfo, maid) of
    (Just userRegisterInfo, Just authid) -> do
      h2id <- lift newIdent
      $(widgetFile "registerpost")
    _ -> do
      h2id <- lift newIdent
      $(widgetFile "register")

--アルバムページ
getAlbumR :: Handler RepHtml
getAlbumR = do
    photoEntities <- runDB $ do
      selectList [] []
    let photos = map (\(Entity _ p) -> picturePath p) photoEntities
    defaultLayout $ do
        h2id <- lift newIdent
        $(widgetFile "album")

--新郎新婦紹介ページ
getIntroR :: Handler RepHtml
getIntroR = do
    defaultLayout $ do
        h2id <- lift newIdent
        $(widgetFile "intro")

--幹事紹介ページ
getOrganizerR :: Handler RepHtml
getOrganizerR = do
    defaultLayout $ do
        h2id <- lift newIdent
        $(widgetFile "organizer")

--参加登録用フォーム
registerForm :: Html -> MForm Happiage Happiage (FormResult UserRegisterInfo, Widget )
registerForm = renderDivs $ 
  UserRegisterInfo
    <$> areq textField "ニックネーム" Nothing 
    <*> areq textField "名前" Nothing
    <*> areq textField "苗字" Nothing
    <*> areq textField "名前（かな）" Nothing
    <*> areq textField "苗字（かな）" Nothing

--これmodelからもっと華麗に作れないものか
data UserRegisterInfo = UserRegisterInfo {
       urNickname :: Text
      , urFirstname :: Text
      , urFamilyname :: Text
      , urKanafirst :: Text
      , urKanafamily :: Text
    }
    deriving Show

-- ここから写真アップロード用フォーム
data Photo = Photo
    { photoName :: Text
    , photoFile :: FileInfo
    }
    deriving Show

fileuploadForm :: Html -> MForm Happiage Happiage (FormResult Photo, Widget )
fileuploadForm = renderDivs $ Photo <$> areq textField "Name" Nothing <*> fileAFormReq "File"

--ファイルアップロードサンプルページ<GET>
getFileuploadR :: Handler RepHtml
getFileuploadR = do
  ((_, widget), enctype) <- runFormPost fileuploadForm
  defaultLayout $ do
    $(widgetFile "fileupload")

--ファイルアップロードサンプルページ<POST>
postFileuploadR :: Handler RepHtml
postFileuploadR = do
  ((res, widget), enctype) <- runFormPost fileuploadForm
  mPhoto <- case res of
    FormSuccess photo -> do
      if T.isPrefixOf "image/" $ fileContentType $ photoFile photo then
        do
          liftIO $ printD $ fileName $ photoFile photo
          liftIO $ printD $ fileContentType $ photoFile photo
          liftIO $ L.writeFile ((++) "static/photo/" $  T.unpack $ fileName $ photoFile photo) (fileContent $ photoFile photo)
          return $ Just photo
        else return Nothing
    _ -> return Nothing
  --photoあった場合にDB書き込み
  maybe (return ())
    (\photo -> runDB $ do
        let fname = fileName $ photoFile photo
        _ <- insert $ Picture {pictureTitle=fname, picturePath="static/photo/" `T.append` fname, pictureDeleted=False}
        return ()
    ) mPhoto
  defaultLayout $ case mPhoto of
    Just photo  -> do
      $(widgetFile "fileuploadPost")
    Nothing -> do
      $(widgetFile "fileupload")
