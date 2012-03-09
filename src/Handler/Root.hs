module Handler.Root where

import Import
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Debug.Trace --printfデバッグ用
traceD :: Show a => a -> b -> b
traceD a b = trace ("TRACE DEBUG(D):" ++ show a) b
printD :: Show a => a -> IO ()
printD a = putStrLn ("TRACE DEBUG(P):" ++ show a)

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

--アルバムページ
getAlbumR :: Handler RepHtml
getAlbumR = do
    photoEntities <- runDB $ do
      selectList [PictureDeleted ==. False] []
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
  maid <- maybeAuthId
  muid <- maybeUserId maid
  case maid of
    Nothing -> --not logged in
      defaultLayout [whamlet|<h2>写真をアップロードするためには、ログイン(していなければ参加登録も)をしてください|]
    Just authId -> 
      case muid of 
        Nothing -> --not registered
          defaultLayout [whamlet|<h2>写真をアップロードするためには、参加登録をしてください|]
        Just userId -> do
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
          liftIO $ L.writeFile ((++) "static/photo/" $  T.unpack $ fileName $ photoFile photo) (fileContent $ photoFile photo)
          return $ Just photo
        else return Nothing
    _ -> return Nothing
  maid <- maybeAuthId
  muid <- maybeUserId maid
  --photoあった場合にDB書き込み
  case (mPhoto, muid) of
    (Just photo, Just uid) -> runDB $ do
        let fname = fileName $ photoFile photo
        _ <- insert $ Picture {pictureUser = uid, pictureTitle=fname,
           picturePath="static/photo/" `T.append` fname, pictureDeleted=False}
        return ()
    _ -> return ()
  defaultLayout $ case mPhoto of
    Just photo -> do
      $(widgetFile "fileuploadPost")
    Nothing -> do
      $(widgetFile "fileupload")

