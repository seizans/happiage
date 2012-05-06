module Handler.Root where

import Import
import Debug.Trace --printfデバッグ用

traceD :: Show a => a -> b -> b
traceD a b = trace ("TRACE DEBUG(D):" ++ show a) b
printD :: Show a => a -> IO ()
printD a = putStrLn ("TRACE DEBUG(P):" ++ show a)

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

--トップページ
getRootR :: Handler RepHtml
getRootR = do
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "happiage homepage"
        $(widgetFile "homepage")

--開催案内ページ
getGuideR :: Handler RepHtml
getGuideR = do
    defaultLayout $ do
        h2id <- lift newIdent
        $(widgetFile "invitation")

--幹事紹介ページ
getOrganizerR :: Handler RepHtml
getOrganizerR = do
    defaultLayout $ do
        h2id <- lift newIdent
        $(widgetFile "organizer")

-- TODO: move to other file.
getAdminR :: Handler RepHtml
getAdminR = do
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "happiage homepage"
        $(widgetFile "homepage")
