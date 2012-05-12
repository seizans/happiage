module Handler.Welcome where

import Import

-- WelcomePage(ごあいさつ)
getRootR :: Handler RepHtml
getRootR = do
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "happiage"
        $(widgetFile "welcome")

-- TODO: move to other file.
getAdminR :: Handler RepHtml
getAdminR = do
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "happiage"
        $(widgetFile "welcome")
