module Handler.Register where

import Import
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

-- <注>この関数使ってないです
-- 多くのページは、非ログイン時、ログインしているが参加登録していない時、参加登録済の３つに分かれる
-- それらをcase分岐で書くと冗長なので、専用の関数を用意した
branchR :: Handler RepHtml -> Handler RepHtml -> Handler RepHtml -> Handler RepHtml
branchR notLoginR notRegisterR mainR = do
  maid <- maybeAuthId
  muid <- maybeUserId maid
  case maid of
    Nothing -> --not logged in
      notLoginR
    Just authId -> 
      case muid of 
       Nothing ->
         mainR
       Just userId -> 
         notRegisterR

--参加登録ページ
getRegisterR :: Handler RepHtml
getRegisterR = do
  maid <- maybeAuthId
  muid <- maybeUserId maid
  case maid of
    Nothing -> --not logged in
      defaultLayout [whamlet|<h2>参加登録をするためには、ログインをしてください|]
    Just authId -> 
      case muid of 
        Just userId -> 
          defaultLayout [whamlet|<h2>すでに参加登録済です|]
        Nothing -> do --not registered 
          ((_, widget), enctype) <- runFormPost registerForm
          defaultLayout $ do
            h2id <- lift newIdent
            $(widgetFile "register")

postRegisterR :: Handler RepHtml
postRegisterR = do
  ((res, widget), enctype) <- runFormPost registerForm
  mUserRegisterInfo <- case res of
    FormSuccess userRegisterInfo -> return $ Just userRegisterInfo
    _ -> return Nothing
  maid <- maybeAuthId
  --ログイン時、postデータがあった場合に、書き込み
  case (mUserRegisterInfo, maid) of
    (Just userRegisterInfo, Just authid) ->
      runDB $ do --TODO:runDBのエラーチェック
        _ <- insert $ User {
          userAuthid = authid, 
          userNickname = urNickname userRegisterInfo, 
          userFirstname = urFirstname userRegisterInfo, 
          userFamilyname = urFamilyname userRegisterInfo, 
          userKanafirst = urKanafirst userRegisterInfo, 
          userKanafamily = urKanafamily userRegisterInfo, 
          userSex = urSex userRegisterInfo, 
          userAttend = Suspense, 
          userInvitedby = Nothing, 
          userDeleted = False
        }
        return ()
    _ -> return ()
  defaultLayout $ case (mUserRegisterInfo, maid) of
    (Just userRegisterInfo, Just authid) -> do
      h2id <- lift newIdent
      $(widgetFile "registerpost")
    _ -> do
      h2id <- lift newIdent
      $(widgetFile "register")

--参加登録用フォーム
registerForm :: Html -> MForm Happiage Happiage (FormResult UserRegisterInfo, Widget )
registerForm = renderDivs $ 
  UserRegisterInfo
    <$> areq textField "ニックネーム" Nothing 
    <*> areq textField "名前" Nothing
    <*> areq textField "苗字" Nothing
    <*> areq textField "名前（かな）" Nothing
    <*> areq textField "苗字（かな）" Nothing
    <*> areq (selectFieldList (zip (map T.pack ["男性","女性"]) [Male, Female])) "性別" Nothing

--これmodelからもっと華麗に作れないものか
data UserRegisterInfo = UserRegisterInfo {
       urNickname :: Text
      , urFirstname :: Text
      , urFamilyname :: Text
      , urKanafirst :: Text
      , urKanafamily :: Text
      , urSex :: Sex
    }
    deriving Show


