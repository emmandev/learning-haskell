{-# LANGUAGE TemplateHaskell #-}
module Todo where

import Data.Aeson
import Data.Aeson.TH
import Servant

data Todo = Todo
  { id        :: Int
  , todo      :: String
  , completed :: Bool
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Todo)

