module Handler.Album where

import Import
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Codec.Archive.Zip

getAlbumR :: Handler RepHtml
getAlbumR = getAlbumPageMainR True 0 ""

--アルバムページ
getAlbumPageR :: Int -> Handler RepHtml
getAlbumPageR pageNumber = getAlbumPageMainR False pageNumber ""

postAlbumPageR :: Int -> Handler RepHtml
postAlbumPageR _ = postAlbumR

getAlbumPageMainR :: Bool -> Int -> String -> Handler RepHtml
getAlbumPageMainR isTop pageNumber message = do
  maid <- maybeAuthId
  muid <- maybeUserId maid
  usersMap <- getUsersMap
  let resultsPerPage = 5
  let (prevPage, nextPage) = (pageNumber-1, pageNumber+1)
  let startNum = pageNumber*resultsPerPage + 1
  let endNum   = (pageNumber+1)*resultsPerPage
  photoEntities <- runDB $ do
    selectList [PictureDeleted ==. False]
                [Desc PictureId, OffsetBy $ (startNum-1)
                ,LimitTo resultsPerPage]
  let photos = map (\(Entity _ p) -> p) photoEntities
  let photoUsers = map (\photo->(photo, usersMap ! (pictureUser photo))) photos
  ((_, widget), enctype) <- runFormPost fileuploadForm
  defaultLayout $ do
    h2id <- lift newIdent
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
      or $ map ((flip T.isSuffixOf) (T.pack $ eRelativePath $ filename)) [".jpg",".jpeg",".png",".gif"]
    photoFromEntry entry = 
      Photo fname (FileInfo fname fname (fromEntry entry))
      where fname = last $ T.splitOn "/" $ (T.pack $ eRelativePath entry)

--画像ファイルを作成
writePhoto :: Photo -> IO ()
writePhoto photo = L.writeFile ((++) "static/photo/" $  T.unpack $ fileName $ photoFile photo) (fileContent $ photoFile photo)

--ファイルアップロードサンプルページ<POST>
postAlbumR :: Handler RepHtml
postAlbumR = do
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
        $(widgetFile "homepage")
    _ -> getAlbumPageMainR True 0 "アップロードしました"
