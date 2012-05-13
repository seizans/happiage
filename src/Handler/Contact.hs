module Handler.Contact where

import Import
import Data.Maybe (fromJust)

-- ContactPage (お問い合わせページ)
getContactR :: Handler RepHtml
getContactR = do
    _ <- maybeAuthId
    (formWidget, formEnctype) <- generateFormPost contactForm
    let submission = Nothing
    defaultLayout $
        $(widgetFile "contact")

postContactR :: Handler RepHtml
postContactR = do
    maid <- maybeAuthId
    ((result, formWidget), formEnctype) <- runFormPost contactForm
    submission <- case result of
        FormSuccess (formContact, formName) -> do
            _ <- runDB $ insert Contact {contactAuthid = fromJust maid, contactContent = formContact, contactName = formName}
            return (Just (formContact, formName))
        _ -> return Nothing
    defaultLayout $
        $(widgetFile "contact")

contactForm :: Form (Text, Text)
contactForm = renderDivs $ (,)
    <$> areq textField "お問い合わせ内容：" Nothing
    <*> areq textField "お名前：" Nothing
