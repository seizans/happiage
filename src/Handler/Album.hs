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
  usersMap <- getUsersMap
  let resultsPerPage = 5
  let (prevPage, nextPage) = (pageNumber-1, pageNumber+1)
  let startNum = pageNumber*resultsPerPage + 1
  let endNum   = (pageNumber+1)*resultsPerPage
  photoEntities <- runDB $ do
    selectList [PictureDeleted ==. False]
                [OffsetBy $ (startNum-1)
                ,LimitTo resultsPerPage]
  let photos = map (\(Entity _ p) -> p) photoEntities
  let photoUsers = map (\photo->(photo, usersMap ! (pictureUser photo))) photos
  defaultLayout $ do
    h2id <- lift newIdent
    $(widgetFile "album")
