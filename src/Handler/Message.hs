module Handler.Message where

import Import
import qualified Data.Text as T

--一行メッセージ書き込みページ
getMessageR :: Handler RepHtml
getMessageR = do
  maid <- maybeAuthId
  muid <- maybeUserId maid
  case maid of
    Nothing -> --not logged in
      defaultLayout [whamlet|<h2>メッセージを書き込むためには、ログインしてください|]
    Just authId -> 
      case muid of 
        Nothing ->  --not registered 
          defaultLayout [whamlet|<h2>メッセージを書き込むためには、先に参加登録してください|]
        Just userId -> do
          ((_, widget), enctype) <- runFormPost messageForm
          defaultLayout $ do
            h2id <- lift newIdent
            $(widgetFile "message")

postMessageR :: Handler RepHtml
postMessageR = do
  ((res, widget), enctype) <- runFormPost messageForm
  mMessageInfo <- case res of
    FormSuccess info -> return $ Just info
    _ -> return Nothing
  maid <- maybeAuthId
  muid <- maybeUserId maid
  case (mMessageInfo, muid) of
    (Just messageInfo, Just uid) ->
      runDB $ do --TODO:runDBのエラーチェック
        _ <- insertUnique $ Message {
          messageUser = uid, 
          messageBody = messageInfoBody messageInfo,
          messageDeleted = False
        }
        return ()
    _ -> return ()
  case (mMessageInfo, muid) of
    (Just messageInfo, Just uid) ->
      redirect RootR
    _ -> defaultLayout $ do
      h2id <- lift newIdent
      $(widgetFile "message")

--参加登録用フォーム
messageForm :: Html -> MForm Happiage Happiage (FormResult MessageInfo, Widget )
messageForm = renderDivs $ 
  MessageInfo
    <$> areq textField "メッセージ" Nothing 

data MessageInfo = MessageInfo {
       messageInfoBody :: Text
    }
    deriving Show


