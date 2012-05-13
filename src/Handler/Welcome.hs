module Handler.Welcome where

import Import

-- WelcomePage(ごあいさつ)
getWelcomeR :: Handler RepHtml
getWelcomeR = do
    defaultLayout $ do
        setTitle "happiage"
        $(widgetFile "welcome")

-- TODO: move to other file.
getAdminR :: Handler RepHtml
getAdminR = do
    defaultLayout $ do
        setTitle "happiage"
        $(widgetFile "welcome")
