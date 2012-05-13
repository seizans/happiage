module Handler.Invitation where

import Import
import Data.Map (Map, (!))

-- InvitationPage(開催案内ページ)
getInvitationR :: Handler RepHtml
getInvitationR = do
    msgs <- getMessages
    usersMap <- getUsersMap
    let messages = map (mkUsrAndMsg usersMap) msgs
    defaultLayout $
        $(widgetFile "invitation")

getMessages :: Handler [(MessageId, Message)]
getMessages = do
    messageEntities <- runDB $ selectList [MessageDeleted ==. False] []
    return $ map (\(Entity ident val) -> (ident, val)) messageEntities

mkUsrAndMsg :: Map UserId User -> (MessageId, Message) -> (Text, Text)
mkUsrAndMsg usersMap (_, message) = (messageBody message, userName user)
  where
    user = usersMap ! messageUser message
