module Handler.Album where

import Import
import qualified Data.Map as Map
import Data.Map ((!))

--アルバムページ
getAlbumR :: Handler RepHtml
getAlbumR = do
  maid <- maybeAuthId
  muid <- maybeUserId maid
  photoEntities <- runDB $ do
    selectList [PictureDeleted ==. False] []
  let photos = map (\(Entity _ p) -> picturePath p) photoEntities
  defaultLayout $ do
    h2id <- lift newIdent
    $(widgetFile "album")


