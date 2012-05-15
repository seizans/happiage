{-# LANGUAGE OverloadedStrings #-}
module HappiageAuthMessage where

import Data.Monoid (mappend)
import Data.Text (Text)
import Yesod.Auth.Message (AuthMessage(..))

happiageMessage :: AuthMessage -> Text
happiageMessage NoOpenID = "No OpenID identifier found"
happiageMessage LoginOpenID = "OpenIDでログイン"
happiageMessage LoginGoogle = "Googleでログイン"
happiageMessage LoginYahoo = "Yahooでログイン"
happiageMessage Email = "メールアドレス"
happiageMessage Password = "パスワード"
happiageMessage Register = "登録"
happiageMessage RegisterLong = "Register a new account"
happiageMessage EnterEmail = "確認メールをお送りしますので、登録済みメールアドレスを入力してください."
    `mappend` "\n(未登録メールアドレスにはメールが送信されません)"
happiageMessage ConfirmationEmailSentTitle = "招待した人にメールを送信しました."
happiageMessage (ConfirmationEmailSent email) =
    "ログイン用のメールを後ほど、次のアドレスにお送り致します。<br>" `mappend` email
      `mappend` "<br><br>もし一日以内にメールが届かない場合はお手数ですが、<br>seizans@gmail.com<br>までご連絡いただけますと幸いです."
happiageMessage AddressVerified = "パスワードを設定し、ログインしてください."
happiageMessage InvalidKeyTitle = "Invalid verification key"
happiageMessage InvalidKey = "I'm sorry, but that was an invalid verification key."
happiageMessage InvalidEmailPass = "Eメールとパスワードのどちらかが違います"
happiageMessage BadSetPass = "You must be logged in to set a password"
happiageMessage SetPassTitle = "パスワード設定"
happiageMessage SetPass = "パスワードの設定"
happiageMessage NewPass = "新しいパスワード"
happiageMessage ConfirmPass = "確認"
happiageMessage PassMismatch = "2つのパスワードが同じでなかったので、再度入力してください."
happiageMessage PassUpdated = "パスワードが設定されました."
happiageMessage Facebook = "Login with Facebook"
happiageMessage LoginViaEmail = "ログイン"
happiageMessage InvalidLogin = "ログインしました."
happiageMessage NowLoggedIn = "ログインしました."
happiageMessage LoginTitle = "ログイン"
happiageMessage PleaseProvideUsername = "Please fill in your username"
happiageMessage PleaseProvidePassword = "Please fill in your password"
