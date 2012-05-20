module Handler.Admin where

import Import

import qualified Data.Text as DT
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List.Split
import Data.Maybe (fromMaybe)
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
    ((result, formWidget), formEnctype) <- runFormGet sortForm
    let submission = case result of
          FormSuccess res -> Just res
          _ -> Nothing
        sort = maybe "UserAuthid" fst submission
        options = case sort of
          "UserAuthid" -> [Asc UserAuthid]
          _ -> [Desc UserAuthid]
    msort <- lookupGetParam "sort"
    let sort_past = fromMaybe "UserAuthid" msort
        options_past = case sort of
          "UserAuthid" -> [Asc UserAuthid]
          _ -> [Desc UserAuthid]
    users <- runDB $ selectList [] options
    userAuths <- runDB $ selectList [] [Asc UserAuthId]
    let userAs = zipJoin users userAuths []
    let notRegisterUsers = filter (\x -> notElem (entityKey $ x) $ map (\y -> userAuthid $ entityVal $ y) users) userAuths
    defaultLayout $ 
        $(widgetFile "adminuser")
  where
    zipJoin [] _ acc = acc
    zipJoin _ [] acc = acc
    zipJoin (x:xs) ys acc = zipJoin xs ys $ (x, (filter (\y -> (userAuthid $ entityVal $ x) == (entityKey $ y)) ys) !! 0) : acc

sortFieldList :: [(Text, Text)]
sortFieldList = zip ["A", "B"] ["kana", "sex"]
ascFieldList :: [(Text, Text)]
ascFieldList = zip ["C", "D"] ["Asc", "Desc"]

sortForm :: Form (Text, Text)
sortForm = renderDivs $ (,)
    <$> areq (selectFieldList sortFieldList) "ソート順" Nothing
    <*> areq (selectFieldList ascFieldList) "AscOrDesc" Nothing
