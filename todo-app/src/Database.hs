{-# LANGUAGE OverloadedStrings #-}

module Database where

import Control.Exception
import Control.Monad.Logger (runStdoutLoggingT, MonadLogger, LoggingT)
import Control.Monad.Reader (lift, runReaderT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Int (Int64)
import Database.Persist (get, insert, delete, update, selectList)
import Database.Persist.Postgresql (ConnectionString, Entity, withPostgresqlConn, runMigration, SqlPersistT, Update)
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import System.Environment (lookupEnv)
import qualified Data.ByteString.Char8 as BS

import Schema

data DatabaseException = InvalidConfig deriving (Show)

instance Exception DatabaseException

connectionString :: ConnectionString
connectionString = "host=127.0.0.1 port=5432 user=postgres dbname=postgres"

getConnectionString :: IO ConnectionString
getConnectionString = return connectionString

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connString action = 
  runStdoutLoggingT $ withPostgresqlConn connString $ \backend ->
    runReaderT action backend

migrateDB :: ConnectionString -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

createTodo :: ConnectionString -> Todo -> IO Int64
createTodo connString todo = fromSqlKey <$> runAction connString (insert todo)

deleteTodo :: ConnectionString -> Int64 -> IO ()
deleteTodo connString id = runAction connString (delete todoId)
  where
    todoId :: Key Todo
    todoId = toSqlKey id 

updateTodo :: ConnectionString -> Int64 -> [Update Todo] -> IO ()
updateTodo connString id todo = runAction connString (update todoId todo)
  where
    todoId :: Key Todo
    todoId = toSqlKey id 

fetchTodo :: ConnectionString -> Int64 -> IO (Maybe Todo)
fetchTodo connString id = runAction connString (get (toSqlKey id))

fetchTodos :: ConnectionString -> IO [Entity Todo]
fetchTodos connString = runAction connString (selectList [] [])
