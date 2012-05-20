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
    defaultLayout $
        $(widgetFile "adminimport")

--インポート受け取り
postAdminimportR :: Handler RepHtml
postAdminimportR = do
    ((res, widget), enctype) <- runFormPost emailForm
    defaultLayout $ case res of
      FormSuccess email ->
        case DT.unpack $ fileContentType $ emailFile email of
          "text/csv" -> do
            let mails = splitOn "\n" $ unpack $ fileContent $ emailFile email
            $(widgetFile "adminimportPost")
          _ -> $(widgetFile "adminimport")
      _ -> $(widgetFile "adminimport")

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
            "UserKananame" -> [Asc UserKananame]
            "UserSex" -> [Asc UserSex]
            _ -> [Desc UserAuthid]
    users <- runDB $ selectList [] options
    userAuths <- runDB $ selectList [] [Asc UserAuthId]
    let userAs = zipJoin users userAuths []
        notRegisterUsers = filter (\x -> notElem (entityKey x) $ map (userAuthid . entityVal) users) userAuths
    defaultLayout $ 
        $(widgetFile "adminuser")
  where
    zipJoin [] _ acc = acc
    zipJoin _ [] acc = acc
    zipJoin (x:xs) ys acc = zipJoin xs ys $ (x, head (filter (\y -> userAuthid (entityVal x) == entityKey y) ys) ) : acc

sortFieldList :: [(Text, Text)]
sortFieldList = zip ["A", "B"] ["UserKananame", "UserSex"]
ascFieldList :: [(Text, Text)]
ascFieldList = zip ["C", "D"] ["Asc", "Desc"]

sortForm :: Form (Text, Text)
sortForm = renderDivs $ (,)
    <$> areq (selectFieldList sortFieldList) "ソート順" Nothing
    <*> areq (selectFieldList ascFieldList) "AscOrDesc" Nothing
