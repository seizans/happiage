module Handler.Admin where

import Import

-- アップロード用フォーム
data Email = Email
    {
      emailFile :: FileInfo
    }
    deriving Show

emailForm :: Html -> MForm Happiage Happiage (FormResult Email, Widget )
emailForm = renderDivs $ Email <$> fileAFormReq "File"

--ユーザー管理ページ
getAdminuserR :: Handler RepHtml
getAdminuserR = do
    users <- runDB $ selectList [] [Asc UserAuthid]
    userAuths <- runDB $ selectList [] [Asc UserAuthId]
    let userAs = zipJoin users userAuths []
    let notRegisterUsers = filter (\x -> notElem (entityKey $ x) $ map (\y -> userAuthid $ entityVal $ y) users) userAuths
    defaultLayout $ do
      h2id <- lift newIdent
      $(widgetFile "adminuser")
      where
        zipJoin [] _ acc = acc
        zipJoin _ [] acc = acc
        zipJoin (x:xs) ys acc = zipJoin xs ys $ (x, (filter (\y -> (userAuthid $ entityVal $ x) == (entityKey $ y)) ys) !! 0) : acc
 
