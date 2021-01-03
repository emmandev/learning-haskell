{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Todo where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import Data.Aeson
import Data.Aeson.TH
import Data.Int (Int64)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Postgresql (ConnectionString, Entity, Update)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Database
import Schema (Todo(Todo))

type TodoApi =
  "todos" :> ReqBody '[JSON] Todo :> Post '[JSON] Int64 :<|>
  "todos" :> Capture "id" Int64 :> Delete '[JSON] () :<|>
  "todos" :> Capture "id" Int64 :> ReqBody '[JSON] UpdateTodo :> Put '[JSON] () :<|>
  "todos" :> Capture "id" Int64 :> Get '[JSON] Todo :<|>
  "todos" :> Get '[JSON] [Todo]

todoApi :: Proxy TodoApi
todoApi = Proxy :: Proxy TodoApi

todoServer :: ConnectionString -> Server TodoApi
todoServer connString = 
  (createTodoHandler connString) :<|> 
  (deleteTodoHandler connString) :<|> 
  (updateTodoHandler connString) :<|> 
  (fetchTodoHandler connString) :<|>
  (fetchTodosHandler connString)

createTodoHandler :: ConnectionString -> Todo -> Handler Int64
createTodoHandler connString todo = liftIO $ createTodo connString todo

deleteTodoHandler :: ConnectionString -> Int64 -> Handler ()
deleteTodoHandler connString id = liftIO $ deleteTodo connString id

updateTodoHandler :: ConnectionString -> Int64 -> UpdateTodo -> Handler ()
updateTodoHandler connString id todo = liftIO $ updateTodo connString id updateTodo
  where
    updateTodo :: [Update Todo]
    updateTodo = Entity todo

fetchTodoHandler :: ConnectionString -> Int64 -> Handler Todo
fetchTodoHandler connString id = do
  todo <- liftIO $ fetchTodo connString id
  case todo of
    Just todo -> return todo
    Nothing -> Handler $ (throwE $ err401 { errBody = "Todo not found." })

fetchTodosHandler :: ConnectionString -> Handler [Entity Todo]
fetchTodosHandler connString = liftIO $ fetchTodos connString

data UpdateTodo = UpdateTodo
  {
    description :: Maybe Text,
    completed :: Maybe Bool
  } deriving (Show, Read)
