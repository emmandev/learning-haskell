{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Schema where

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import Database.Persist (Entity(..), Entity)
import qualified Database.Persist.TH as PTH

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  Todo sql=todos
    description Text
    completed Bool
    deriving Show Read
|]

instance ToJSON Todo where 
  toJSON todo = object
    [
      "description" .= todoDescription todo,
      "completed" .= todoCompleted todo
    ]

instance FromJSON Todo where
  parseJSON = withObject "Todo" parseTodo

parseTodo :: Object -> Parser Todo
parseTodo object = do
  description <- object .: "description"
  completed <- object .: "completed"
  return Todo
    {
      todoDescription = description,
        todoCompleted = completed
    }
