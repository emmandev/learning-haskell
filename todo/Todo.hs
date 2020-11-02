data Todo = Todo {
  completed :: Bool,
  task :: String
} 

createTodo :: String -> Todo
createTodo task = 
  Todo
  {
    completed = completed',
    task = task'
  }
  where
    completed' = False
    task' = task

printTodo :: Todo -> IO ()
printTodo (Todo { completed = completed', task = task'}) = do
  putStrLn(symbol ++ " " ++ task')
  where
    symbol = "test"

printTodos :: [Todo] -> IO ()
printTodos list = do
  mapM_ printTodo list


controller :: (String, [Todo], String) -> IO ()
controller input = do
  let (command, list, todo) = input
  let (Just action) = lookup command commands
  printTodos (action todo list)

commands :: [(String, String -> [Todo] -> [Todo])]
commands = [("add", addTodo), ("delete", deleteTodo)]

addTodo :: String -> [Todo] -> [Todo]
addTodo todo list = createTodo todo : list

deleteTodo :: String -> [Todo] -> [Todo]
deleteTodo _ [] = []
deleteTodo index (todo:todos) = do
  let n = (read index :: Int) - 1
  if n <= 0
    then (todo:todos)
    else take n (todo:todos) ++ drop (n + 1) (todo:todos)

main = do 
  putStrLn "Commands:"
  putStrLn "add"
  putStrLn "delete"
  putStrLn "read"
  putStrLn "any other to quit"
  putStrLn "Enter a command"
  command <- getLine
  putStrLn "Enter todo"
  todo <- getLine
  controller (command, [Todo { completed = False, task = "a"}, Todo { completed = False, task = "b"}], todo)
