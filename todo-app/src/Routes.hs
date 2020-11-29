{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Routes where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Todo (Todo)

type Routes = "todos" :> Get '[JSON] [Todo]
