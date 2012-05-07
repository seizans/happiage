{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, FlexibleContexts, GADTs #-}
{-
import Data.Text (Text)
import Database.Persist.Quasi
import Database.Persist.MongoDB
import Database.Persist.TH
import Language.Haskell.TH.Syntax

share [mkPersist MkPersistSettings {mpsBackend = ConT '' Action}, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
-}

import Model
import Database.Persist.MongoDB

--main :: IO ()
main = withMongoDBConn dbname hostname $ runMongoDBConn master $ do
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
