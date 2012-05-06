module Handler.Root where

import Import
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Codec.Archive.Zip
import Debug.Trace --printfデバッグ用
import Handler.Album

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
        $(widgetFile "invitation")

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

--zipファイルを解凍して、バイナリを返す
--TODO:ファイルの正しさのチェック
photosFromZip :: L.ByteString -> [Photo]
photosFromZip contents =
  let archive = toArchive contents
      entries = filter isImageFile $ zEntries archive in
  map photoFromEntry entries
  where 
    isImageFile filename = --拡張子で判断
      or $ map ((flip T.isSuffixOf) (T.pack $ eRelativePath $ filename)) [".jpg",".jpeg",".png",".gif"]
    photoFromEntry entry = 
      Photo fname (FileInfo fname fname (fromEntry entry))
      where fname = last $ T.splitOn "/" $ (T.pack $ eRelativePath entry)

--画像ファイルを作成
writePhoto :: Photo -> IO ()
writePhoto photo = L.writeFile ((++) "static/photo/" $  T.unpack $ fileName $ photoFile photo) (fileContent $ photoFile photo)

--ファイルアップロードサンプルページ<POST>
postFileuploadR :: Handler RepHtml
postFileuploadR = do
  ((res, widget), enctype) <- runFormPost fileuploadForm
  photos <- case res of
    FormSuccess photo -> do
      let cType = fileContentType $ photoFile photo
      if T.isPrefixOf "image/" cType then do --画像のとき
        liftIO $ writePhoto photo
        return $ [photo]
        else if cType == "application/zip" then do --Zipのとき (複数画像をまとめてアップロード)
          let filedata = fileContent $ photoFile photo
              photos = photosFromZip filedata
          liftIO $ mapM_ writePhoto photos
          return photos
          else return [] --その他
    _ -> return []
  maid <- maybeAuthId
  muid <- maybeUserId maid
  --photoあった場合にDB書き込み
  case (photos, muid) of
    (ps@(_:_), Just uid) -> runDB $ do --注.photosが一個以上になるように←の記法になっている
        let fnames = map (fileName . photoFile) photos
        mapM_ (\f->insert $ Picture {pictureUser = uid, pictureTitle=f,
           picturePath="static/photo/" `T.append` f, pictureDeleted=False} ) fnames
        return ()
    _ -> return ()
  case photos of
   [] -> defaultLayout 
      $(widgetFile "fileupload")
   _ -> getAlbumPageMainR True 0 "アップロードしました"
 
-- TODO: move to other file.
getAdminR :: Handler RepHtml
getAdminR = do
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "happiage homepage"
        $(widgetFile "homepage")
