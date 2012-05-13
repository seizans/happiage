module Handler.Profile where

import Import

-- ProfilePage(新郎新婦紹介ページ)
getProfileR :: Handler RepHtml
getProfileR = do
    usersMap <- getUsersMap
    defaultLayout $
        $(widgetFile "profile")
