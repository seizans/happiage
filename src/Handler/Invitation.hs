module Handler.Invitation where

import Import

-- 開催案内ページ
getInvitationR :: Handler RepHtml
getInvitationR = do
    defaultLayout $ do
        h2id <- lift newIdent
        $(widgetFile "invitation")
