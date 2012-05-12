module Handler.Profile where

import Import
import qualified Data.Map as Map
import Data.Map ((!))

getMessages :: Handler [(MessageId, Message)]
getMessages = do
  messageEntities <- runDB $ do
    selectList [MessageDeleted ==. False] []
  return $ map (\(Entity pid val) -> (pid, val)) messageEntities

-- メッセージ投稿者、メッセージ本文のリストにする
composeMessageLine :: (Map.Map UserId User) -> [(MessageId, Message)] -> [(Text, Text)]
composeMessageLine usersMap [] = []
composeMessageLine usersMap !messages@((mid, m):ms) =
  if Map.member (messageUser m) usersMap then
    (messageUserName, messageText) : composeMessageLine usersMap ms
  else 
    composeMessageLine usersMap ms
  where messageText = messageBody m
        messageUserName = userName $ usersMap ! (messageUser m)

--新郎新婦紹介ページ
getProfileR :: Handler RepHtml
getProfileR = do
  usersMap <- getUsersMap
  messages <- getMessages
  let messageLines = composeMessageLine usersMap messages
  defaultLayout $ do
    h2id <- lift newIdent
    $(widgetFile "profile")
