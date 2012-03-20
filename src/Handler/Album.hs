module Handler.Album where

import Import
import qualified Data.Map as Map
import Data.Map ((!))


getAlbumR :: Handler RepHtml
getAlbumR = getAlbumPageMainR True 0

--アルバムページ
getAlbumPageR :: Int -> Handler RepHtml
getAlbumPageR pageNumber = getAlbumPageMainR False pageNumber

getAlbumPageMainR :: Bool -> Int -> Handler RepHtml
getAlbumPageMainR isTop pageNumber = do
  maid <- maybeAuthId
  muid <- maybeUserId maid
  let resultsPerPage = 5
  let (prevPage, nextPage) = (pageNumber-1, pageNumber+1)
  photoEntities <- runDB $ do
    selectList [PictureDeleted ==. False]
                [OffsetBy $ pageNumber * resultsPerPage
                ,LimitTo resultsPerPage]
  let photos = map (\(Entity _ p) -> picturePath p) photoEntities
  defaultLayout $ do
    h2id <- lift newIdent
    $(widgetFile "album")
