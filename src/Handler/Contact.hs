module Handler.Contact where

import Import

-- ContactPage (お問い合わせページ)
getContactR :: Handler RepHtml
getContactR = do
    (formWidget, formEnctype) <- generateFormPost contactForm
    let submission = Nothing
    defaultLayout $
        $(widgetFile "contact")

postContactR :: Handler RepHtml
postContactR = do
    ((result, formWidget), formEnctype) <- runFormPost contactForm
    let submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing
    defaultLayout $
        $(widgetFile "contact")

contactForm :: Form (Text, Text)
contactForm = renderDivs $ (,)
    <$> areq textField "お問い合わせ内容：" Nothing
    <*> areq textField "お名前：" Nothing
