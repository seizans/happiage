module Handler.Album where

import Import
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Codec.Archive.Zip

getAlbumR :: Handler RepHtml
getAlbumR = getAlbumPageMainR True 1

--アルバムページ
getAlbumPageR :: Int -> Handler RepHtml
getAlbumPageR pageNumber = getAlbumPageMainR False pageNumber

postAlbumPageR :: Int -> Handler RepHtml
postAlbumPageR _ = postAlbumR

getAlbumPageMainR :: Bool -> Int -> Handler RepHtml
getAlbumPageMainR isTop pageNumber = do
    if pageNumber <= 0
      then redirect WelcomeR
      else do
        mmsg <- lookupGetParam "msg"
        maid <- maybeAuthId
        muid <- maybeUserId maid
        usersMap <- getUsersMap
        let picPerPage = 3
            (prevPage, nextPage) = (pageNumber - 1, pageNumber + 1)
            startNum = picPerPage * (pageNumber - 1) + 1
            endNum   = picPerPage * pageNumber
        photoEntities <- runDB $ selectList [PictureDeleted ==. False] [Desc PictureId, OffsetBy $ (startNum - 1), LimitTo picPerPage]
        numOfPic <- runDB $ count [PictureDeleted ==. False]
        let photos = map (\(Entity _ p) -> p) photoEntities
            photoUsers = map (\photo->(photo, usersMap ! (pictureUser photo))) photos
            maxpage = (numOfPic + picPerPage - 1) `div` picPerPage
            pagenums = [1..maxpage]
        (widget, enctype) <- generateFormPost fileuploadForm
        defaultLayout $ do
            $(widgetFile "album")

-- ここから写真アップロード用フォーム
data Photo = Photo
    { photoName :: Text
    , photoFile :: FileInfo
    }
    deriving Show

fileuploadForm :: Html -> MForm Happiage Happiage (FormResult Photo, Widget )
fileuploadForm = renderDivs $ Photo <$> areq textField "Name" Nothing <*> fileAFormReq "File"

--zipファイルを解凍して、バイナリを返す
--TODO:ファイルの正しさのチェック
photosFromZip :: L.ByteString -> [Photo]
photosFromZip contents =
  let archive = toArchive contents
      entries = filter isImageFile $ zEntries archive in
  map photoFromEntry entries
  where 
    isImageFile filename = --拡張子で判断
      or $ map ((flip T.isSuffixOf) (T.toLower $ T.pack $ eRelativePath $ filename)) [".jpg",".jpeg",".png",".gif"]
    photoFromEntry entry = 
      Photo fname (FileInfo fname fname (fromEntry entry))
      where fname = last $ T.splitOn "/" $ (T.pack $ eRelativePath entry)

--画像ファイルを作成
writePhoto :: Photo -> IO ()
writePhoto photo = L.writeFile ((++) "static/photo/" $  T.unpack $ fileName $ photoFile photo) (fileContent $ photoFile photo)

postAlbumR :: Handler RepHtml
postAlbumR = do
    ((result, widget), enctype) <- runFormPost fileuploadForm
    photos <- case result of
      FormSuccess photo -> do
        let cType = fileContentType $ photoFile photo
        liftIO $ print cType
        if T.isPrefixOf "image/" cType
          then do --画像のとき
            liftIO $ writePhoto photo
            return $ [photo]
          else
            if cType == "application/zip"
              then do --Zipのとき (複数画像をまとめてアップロード)
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
            mapM_ (\f -> insertUnique $ Picture {pictureUser = uid, pictureTitle=f,
               picturePath="static/photo/" `mappend` f, pictureDeleted=False} ) fnames
            return ()
        _ -> return ()
    case photos of
        [] -> redirect WelcomeR
        _ -> redirect (AlbumR, [("msg", "アップロードしました.")])
{- 上を書きなおそうとして途中のコード片
    maid <- maybeAuthId
    muid <- maybeUserId maid
    if muid == Nothing
      then
        return() -- ここは即エラー
      else
        return()
    let uid = fromJust muid
    ((result, widget), enctype) <- runFormPost fileuploadForm
    case result of
        FormSuccess photo -> do
            let contType = fileContentType $ photoFile photo
            if contType == "application/zip"
              then do
                let filedata = fileContent $ photoFile photo
                    photos = photosFromZip filedata
                liftIO $ mapM_ writePhoto photos
                defaultLayout $(widgetFile "album")
              else do
                    if T.isPrefixOf "image/" contType
                      then do
                        liftIO $ writePhoto photo
                        let fname = fileName $ photoFile photo
                        _ <- runDB $ insert $ Picture {pictureUser = uid, pictureTitle = fname , picturePath = "static/photo/" `mappend` fname, pictureDeleted = False}
                        getAlbumPageMainR True 0 "アップロードしました"
                      else
                        defaultLayout $(widgetFile "album")
        _ -> defaultLayout $(widgetFile "album")
-}
