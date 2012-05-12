module Handler.Root where

import Import

--トップページ
getRootR :: Handler RepHtml
getRootR = do
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "happiage homepage"
        $(widgetFile "homepage")

--開催案内ページ
getInvitationR :: Handler RepHtml
getInvitationR = do
    defaultLayout $ do
        h2id <- lift newIdent
        $(widgetFile "invitation")

-- TODO: move to other file.
getAdminR :: Handler RepHtml
getAdminR = do
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "happiage homepage"
        $(widgetFile "homepage")
