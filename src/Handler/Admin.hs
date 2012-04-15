module Handler.Admin where

import Import

import qualified Data.Text as DT
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List.Split
-- アップロード用フォーム
data Email = Email
    {
      emailFile :: FileInfo
    }
    deriving Show

emailForm :: Html -> MForm Happiage Happiage (FormResult Email, Widget )
emailForm = renderDivs $ Email <$> fileAFormReq "File"

--インポートページ
getAdminimportR :: Handler RepHtml
getAdminimportR = do
    ((_, widget), enctype) <- runFormPost emailForm
    defaultLayout $ do
      $(widgetFile "adminimport")

--インポート受け取り
postAdminimportR :: Handler RepHtml
postAdminimportR = do
    ((res, widget), enctype) <- runFormPost emailForm
    defaultLayout $ case res of
      FormSuccess email -> do
        case DT.unpack $ fileContentType $ emailFile email of
          "text/csv" -> do
            let mails = splitOn "\n" $ unpack $ fileContent $ emailFile email
            $(widgetFile "adminimportPost")
          _ -> do
            $(widgetFile "adminimport")
      _ -> do
        $(widgetFile "adminimport")

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

