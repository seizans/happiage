module Handler.Entry where

import Import
import qualified Data.Text as T
import Data.Maybe (fromJust)

-- EntryPage (参加登録ページ)
getEntryR :: Handler RepHtml
getEntryR = do
    maid <- maybeAuthId
    muid <- maybeUserId maid
    case muid of 
        Just _ -> redirect EntryupdateR        
        Nothing -> do
            (widget, enctype) <- generateFormPost registerForm
            defaultLayout $ do
                let title = T.pack "参加登録"
                $(widgetFile "entry")

postEntryR :: Handler RepHtml
postEntryR = do
    ((res, widget), enctype) <- runFormPost registerForm
    mUserRegisterInfo <- case res of
        FormSuccess userRegisterInfo -> return $ Just userRegisterInfo
        _ -> return Nothing
    maid <- maybeAuthId
    --ログイン時、postデータがあった場合に、書き込み
    case (mUserRegisterInfo, maid) of
        (Just userRegisterInfo, Just authid) -> do
            _ <- runDB $ insertUnique $ User {
                    userAuthid = authid, 
                    userName = urName userRegisterInfo, 
                    userKananame = urKananame userRegisterInfo, 
                    userZipcode = urZipcode userRegisterInfo,
                    userAddress = urAddress userRegisterInfo,
                    userSex = urSex userRegisterInfo, 
                    userAttend = urAttend userRegisterInfo, 
                    userInvitedby = Nothing, 
                    userDeleted = False
                  }
            redirect WelcomeR
        _ ->
            defaultLayout $ do
                let title = T.pack "参加登録"
                $(widgetFile "entry")

-- 登録済みだった場合はこの変更用URLに来る
getEntryupdateR :: Handler RepHtml
getEntryupdateR = do 
    maid <- maybeAuthId
    muid <- maybeUserId maid
    let authId = fromJust maid
        userId = fromJust muid
    user <- runDB $ get404 userId
    messages <- runDB $ selectList [MessageUser ==. userId] []
    message <- case messages of
        [] -> return Nothing
        _ -> return $ Just $ (\(Entity id val) -> val) (messages !! 0)
    ((_, widget), enctype) <- runFormPost (updateForm (Just user) (message))
    defaultLayout $ do
        let title = T.pack "参加登録情報更新"
        $(widgetFile "entry")

-- 変更用URL
postEntryupdateR :: Handler RepHtml
postEntryupdateR = do
    ((res, widget), enctype) <- runFormPost $ updateForm Nothing Nothing
    mUserUpdateInfo <- case res of
        FormSuccess userUpdateInfo -> return $ Just userUpdateInfo
        _ -> return Nothing
    maid <- maybeAuthId
    muid <- maybeUserId maid
    let authid = fromJust maid
        uid = fromJust muid
    case mUserUpdateInfo of
      Nothing -> redirect WelcomeR
      Just userUpdateInfo -> do
        _ <- runDB $ replace uid User
            { userAuthid = authid
            , userName = urName userUpdateInfo
            , userKananame = urKananame userUpdateInfo
            , userZipcode = urZipcode userUpdateInfo
            , userAddress = urAddress userUpdateInfo
            , userSex = urSex userUpdateInfo
            , userAttend = urAttend userUpdateInfo
            , userInvitedby = Nothing
            , userDeleted = False
            }
        defaultLayout $ do
            let title = T.pack "参加登録"
            $(widgetFile "entry")

-- フォームの選択要素
genderFieldList :: [(Text, Sex)]
genderFieldList = (zip (map T.pack ["男性","女性"]) [Male, Female])

attendFieldList :: [(Text, Attend)]
attendFieldList = (zip (map T.pack ["欠席","出席"]) [Absent, Present])

-- 参加登録用フォーム
registerForm :: Html -> MForm Happiage Happiage (FormResult UserRegisterInfo, Widget)
registerForm = renderDivs $ UserRegisterInfo
    <$> areq textField "名前" Nothing
    <*> areq textField "名前かな" Nothing
    <*> areq textField "郵便番号" Nothing
    <*> areq textField "住所" Nothing
    <*> areq (selectFieldList genderFieldList) "性別" Nothing
    <*> areq (selectFieldList attendFieldList) "ご出席" (Just Present)
    <*> aopt  textField "メッセージ(任意)" Nothing

data UserRegisterInfo = UserRegisterInfo
      { urName :: Text
      , urKananame :: Text
      , urZipcode :: Text
      , urAddress :: Text
      , urSex :: Sex
      , urAttend :: Attend
      , urMessage :: Maybe Text
      }
    deriving Show

-- 参加登録更新用フォーム（一部項目のみ変更可能）
updateForm :: Maybe User -> Maybe Message -> Html -> MForm Happiage Happiage (FormResult UserRegisterInfo, Widget)
updateForm muser mmessage = renderDivs $ UserRegisterInfo
    <$> areq textField "名前" (fmap userName muser)
    <*> areq textField "名前かな" (fmap userKananame muser)
    <*> areq textField "郵便番号" (fmap userZipcode muser)
    <*> areq textField "住所" (fmap userAddress muser)
    <*> areq (selectFieldList genderFieldList) "性別" (fmap userSex muser)
    <*> areq (selectFieldList attendFieldList) "ご出席" (fmap userAttend muser)
    <*> aopt textField "メッセージ(任意)" (body mmessage)
  where
    body (Just m) = Just (Just $ messageBody m)
    body Nothing = Nothing
