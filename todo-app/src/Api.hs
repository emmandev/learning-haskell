{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Api where

-- import Database.Persist.Postgresql
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

-- import Database (runAction)
import Todo (Todo)

type Routes = 
        -- GET /todos
        "todos" :> Get '[JSON] [Todo] 
        -- GET /todos/:id
  :<|>  "todos" :> Capture "id" Integer :> Get '[JSON] Todo
  --       -- DELETE /todos/:id
  -- :<|>  "todos" :> Capture "id" Integer :> Delete '[JSON] Int64
  --       -- POST /todos
  -- :<|>  "todos" :> ReqBody '[JSON] Todo :> Post '[JSON] Int64

handlers :: Server Routes
handlers = 
  todo :<|>
  todos
  where
    todo id = fetchTodo id
    todos = fetchTodos

fetchTodo :: Int -> Handler Todo
fetchTodo id = do
  Just id -> return Todo { todo = 'test', completed = False }
  Nothing -> throwError err404 { errBody JSON.encode "Todo not found." }

fetchTodos :: [Todo]
fetchTodos = [ Todo 1 "Walk the dog." False
  , Todo 2 "Clean the kitchen." False
  ]
-- fetchTodos = do
--   todos <- runAction $ selectList [] []
--   return $ map (\(Entity _ todo) -> todo) todos


