module Handler.Profile where

import Import

-- ProfilePage(新郎新婦紹介ページ)
getProfileR :: Handler RepHtml
getProfileR = do
    profileEntities <- runDB $ selectList [] [Asc ProfileOrder]
    let profiles = map (\(Entity _ model) -> model) profileEntities
    defaultLayout $
        $(widgetFile "profile")
