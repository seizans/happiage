{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, FlexibleContexts, GADTs #-}

import Model
import Database.Persist.MongoDB

--main :: IO ()
main = do
    withMongoDBConn dbname hostname $ runMongoDBConn master $ do
        insert $ Profile
          { profileQuestion = "question1"
          , profileGroom = "groom1"
          , profileBride = "bride1"
          , profileOrder = 10
          }
        return ()
  where
    hostname = "localhost"
    dbname = "test"
