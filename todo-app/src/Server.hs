module Server where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Routes (Routes)
import Todo 

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy Routes
api = Proxy

server :: Server Routes
server = return todos

todos :: [Todo]
todos = [ Todo 1 "Walk the dog" False
        , Todo 2 "Clean the kitchen" False
        ]
