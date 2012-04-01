{-# LANGUAGE OverloadedStrings #-}
module Smtp.SmtpClient (send) where

import Prelude
-- import qualified Network.SMTP.ClientSession as CS
import qualified Network.SMTP.Client as C
import qualified Network.Socket as S
--import qualified Network.Socket.Internal as SI
import qualified System.Time as T
import qualified System.IO as IO
-- import qualified Data.Bits as BITS
import qualified Data.IORef as IOR
import qualified Smtp.SmtpProperties as PROP

send :: String -> String -> String -> IO ()
send mailTo subject mailBody = do
    now <- T.getClockTime
    nowCT <- T.toCalendarTime now
    -- Check Network.SMTP.Client.Message property. How do I write text/part, encoding etc.?
    let message = C.Message [
                C.From [C.NameAddr (Just PROP.fromName) PROP.fromAddr],
                --C.To   [C.NameAddr (Just "ToName") mailTo], -- TODO: fix "ToName"
                C.To   [C.NameAddr (Just "ToName") mailTo],
                C.Subject subject,
                C.Date nowCT
            ]
            mailBody
    addrs <- S.getAddrInfo Nothing (Just PROP.smtpHost) Nothing
    let S.SockAddrInet _ hostAddr = S.addrAddress (head addrs)
        sockAddr = S.SockAddrInet 587 hostAddr -- 587 is the relation port.
        --sockAddr = S.SockAddrInet (fromIntegral 25) hostAddr
    putStrLn $ "connecting to " ++ show sockAddr -- TODO:Logging
    sentRef <- IOR.newIORef []
    C.sendSMTP' (IO.hPutStrLn IO.stderr) (Just sentRef) PROP.myDomain
        sockAddr [message]
    statuses <- IOR.readIORef sentRef
    -- If no exception was caught, statuses is guaranteed to be
    -- the same length as the list of input messages, therefore head won't fail here.
    case head statuses of
        Nothing     -> putStrLn "Message successfully sent"
        Just status -> putStrLn $ "Message send failed with status "++show status
