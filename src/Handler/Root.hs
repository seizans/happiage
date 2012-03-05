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
    defaultLayout $ do
        h2id <- lift newIdent
        $(widgetFile "register")

--アルバムページ
getAlbumR :: Handler RepHtml
getAlbumR = do
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
--defaultLayout $ case res of
  mPhoto <- case res of
    FormSuccess photo -> do
      case T.isPrefixOf "image/" $ fileContentType $ photoFile photo of
        True -> do
          liftIO $ printD $ fileName $ photoFile photo
          liftIO $ printD $ fileContentType $ photoFile photo
          liftIO $ L.writeFile ((++) "static/photo/" $  init $ tail $ show $ fileName $ photoFile photo) (fileContent $ photoFile photo)
          return $ Just photo
        _ -> return Nothing
    _ -> return Nothing
  --DB書き込み
  maybe (return ())
    (\photo -> runDB $ do
        let fname = fileName $ photoFile photo
        _ <- insert $ Picture {pictureTitle=fname, picturePath=fname, pictureDeleted=False}
        return ()
    ) mPhoto
  defaultLayout $ case mPhoto of
    Just photo  -> do
      $(widgetFile "fileuploadPost")
    Nothing -> do
      $(widgetFile "fileupload")
