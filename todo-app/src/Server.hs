module Server where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Api
import Database (connectionString)
import Todo 

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve todoApi (todoServer connectionString)


