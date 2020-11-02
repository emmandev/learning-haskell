# CLI TODO app in Haskell

~~The goal in this first month is to learn the syntax and concepts of Haskell~~.

The goal for this first month is to learn the syntax and concepts of Haskell and demonstrate it with a simple CLI Todo app.

Another Todo app? Yes.

So what is a Todo app? 
Well, it's where we can have a list of tasks, which we can add or remove tasks. 
We should also be able to set the status of the tasks as completed or not. *Update: I wasn't able to add this.*

## Types
---

So first off, how should we represent a Todo?

Initially, we can just make a list of tasks. 
The task will be a type of `String`, which is just an alias for a list of `Char`.
Basically, `String` and `[Char]` are the same thing.

```haskell
todos :: [String]
todos = []
```

Here, we start off with an empty list.
The top part just means that we are giving a type definition for the binding `todos`.
Since Haskell lists are homogenous, this means we can't have a list that have items of different types.
Although, there is a way to make them heterogenous but I won't cover that here.

But now, how can we have any info if a task is completed or not?
We could use a list of tuples instead like so:

```haskell
todos :: [(Bool, String)]
todos = []
```

Tuples are like lists but they can contain different types and have a specified required length.
With this, for each item in the list, it will have a `Bool` and a `String`. 
The `Bool` will hold either `True` or `False`, which we can use for our completed status.
We can have something like:

```haskell
todos = [(False, "Walk the dog.")]
```

It's a bit unreadable as we dont have anything codewise to denote that `False` refers to the status being incomplete.
So this is something I've learned much more recent, creating our own types.
Much like in Typescript, and probably any other strongly typed languages, we can create our own types.
We do so by using the `data`, `type`, and `newType` keywords.
There are differences between these, but we'll just use the `data` keyword for now.

```haskell
data Todo = Todo Bool String
```

Notice `Todo` is capitalized and is singular.
It is capitalized because constructors are required to, the `Todo` after the `=` sign.
It is singular because it is just an item for our todo list.
This basically translates to "Create a type `Todo` which has a constructor `Todo` that takes in a `Bool` and a `String` as parameters".
But still, we aren't denoting what those parameters mean. Let's rewrite this type.

```haskell
data Todo = Todo {
  completed :: Bool,
  todo :: String,
}
```

Now that's a lot clearer!
Coming from OOP land, this looks really familiar with classes, although Haskell does have classes I haven't learned what they mean in FP land yet.
We can also add a `type` for our todos list, which is just an alias.

```haskell
type Todos = [Todo]
```

From what I understand, `newType` is identical to `data` that you can replace `newType` with `data` and it will still work the same.
But this won't work the other way around, as `newType` can only take in one constructor parameter and is a lot stricter.
Unlike `newType`, the `data` type is being wrapped, and unwrapped when evaluated, and is usually slower.


A great example I found on the web is from [Learn You A Haskell](http://learnyouahaskell.com/functors-applicative-functors-and-monoids):

> Now consider the following type:
>
> ```haskell
> data CoolBool = CoolBool { getCoolBool :: Bool }
> ```
> 
> It's your run-of-the-mill algebraic data type that was defined with the data keyword. It has one value constructor, which has one field whose type is Bool. Let's make a function that pattern matches on a CoolBool and returns the value "hello" regardless of whether the Bool inside the CoolBool was True or False:
>
> ```haskell
> helloMe :: CoolBool -> String  
> helloMe (CoolBool _) = "hello"
> ```
> 
> Instead of applying this function to a normal CoolBool, let's throw it a curveball and apply it to undefined!
> 
> ```
> ghci> helloMe undefined  
> "*** Exception: Prelude.undefined  
> ```
> 
> Yikes! An exception! Now why did this exception happen? Types defined with the data keyword can have multiple value constructors (even though CoolBool only has one). So in order to see if the value given to our function conforms to the (CoolBool _) pattern, Haskell has to evaluate the value just enough to see which value constructor was used when we made the value. And when we try to evaluate an undefined value, even a little, an exception is thrown.
>
> Instead of using the data keyword for CoolBool, let's try using newtype:
>
> ```haskell
> newtype CoolBool = CoolBool { getCoolBool :: Bool }
> ```
> 
> We don't have to change our helloMe function, because the pattern matching syntax is the same if you use newtype or data to define your type. Let's do the same thing here and apply helloMe to an undefined value:
>
> ```
> ghci> helloMe undefined  
> "hello"
> ```
> It worked!

This kinda made sense but I still yet to figure out when do I use `newType` over `data`.

For reference, Haskell has these common internal types:

`Int` or `Integer`, `Float`, `Double`, `Bool`, and `Char`.

Anyway, now that we have our type, let's move on.

## Functions
---

So now, how do we create an instance of our Todo type?

We can create a Todo by:

```haskell
Todo { completed: False, todo: "Walk the dog." }
```

But since this should be able to add todo, we need to take an input and making it as a Todo.
What I thought of is creating a function that will take in a string and give back a Todo, a generator.

```haskell
createTodo :: String -> Todo
createTodo todo = Todo False todo
```

So this function, `createTodo`, takes in a `String` and returns a `Todo`.
In this case, `todo` is given the parameter value and is passed to `Todo`.
We can also see from `Todo False todo` that the `completed` is set to `False`.
We can also write this as:

```haskell
createTodo :: String -> Todo
createTodo todo = Todo
  {
    completed = completed',
    todo = todo'
  }
  where
    completed' = False
    todo' = todo
```

Although this is more verbose than the previous, I think this is much readable especially when learning, it far clearer what the function is doing.
So inside the curly brackets, we can see that we are setting the members of Todo to `completed'` and `todo'` (read as "completed prime" and "todo prime" respectively) and then, we have a `where` to set their values.

In Haskell, functions are fairly simple to write, there is no need for parenthesis for the parameters.
So multiple parameters would look like:

```haskell
createTodo :: String -> Bool -> Todo
createTodo todo completed = Todo
  {
    completed = completed',
    todo = todo'
  }
  where
    completed' = completed
    todo' = todo
```

## Lists
---

Now let's handle a list of `Todo`s.
Let's create a function that can add a `Todo` to our list.
Since this is FP land, we cannot mutate an existing list, that is by adding a new todo.
So instead, we create a new list.
Also, I will try as much as possible to have pure functions, which I may or may not know if I did yet.
So, we also need to pass the current list to the function.

```haskell
addTodo :: String -> [Todo] -> [Todo]
```

Now we have our type definition, let's create the actual function

```haskell
addTodo :: String -> [Todo] -> [Todo]
addTodo todo list = list ++ [createTodo todo]
```

Lots of things are happening here.
The `[createTodo todo]` creates a Todo with the string `todo` and wraps it in brackets to create a list.
In the expression `list ++ [createTodo todo]`, `++` is a function that concatenates two lists. 
This is also called an infix function as it is placed in the middle of its parameters.
It can also be written as `++ list [createTodo todo]` but it makes a lot more sense to have the infix way.
We can also use GHCI to see the type definition for this function

```
ghci > :t (++)
(++) :: [a] -> [a] -> [a]
```

So this function takes in two lists of Type `a` and returns a list of Type `a`.
What this does is map through the first list, then add the second list at the end of the first list.
This sounds slow, and it is.

Another way of adding to a list is prepending, using the function `:`.
Unlike `++`, the `:` doesn't have to map over a list. Let's see the type definition.

```haskell
(:) :: a -> [a] -> [a]
```

So this takes in a type and a list of that type, then returns a list of that type.
We could rewrite our function like this:

```haskell
addTodo :: String -> [Todo] -> [Todo]
addTodo todo list = createTodo todo : list
```

And that is it for for our `addTodo` function.

For our delete todo function, it can take an `Int` parameter to know which index of the list to remove and the Todo list.
This will also return a new list.

```haskell
deleteTodo :: Int -> [Todo] -> [Todo]
```

With this method, we will have to map through our list and find the index to which the value we will remove from the list.

```haskell
deleteTodo :: Int -> [Todo] -> [Todo]
deleteTodo index (todo:todos) = do
  let n = index - 1
  if n < 0
    then (x:xs)
    else take n (x:xs) ++ drop (n + 1) (x:xs)
```

Here, we are destructuring the list of todos, with `todo` as the head of the list (`Todo`) and `todos` as the rest of the list (`[Todo]`).
We then create `n` which we decrement by `1` since lists are zero-based index.
And we have our first if-else statement, where if `n` is lesser than zero, we just return the list.
If not, we use `take` function to take `n` number of `Todo` in our list, then `drop` function to remove the first `n` number of `Todo` from the start of the list, and then concatenate the two results.

But you might have noticed, what if our list is empty?
It shouldn't take anything, but instead of adding another condition, let's overload our function `deleteTodo`.

```haskell
deleteTodo :: Int -> [Todo] -> [Todo]
deleteTodo _ [] = [] 
deleteTodo index (todo:todos) = do
  let n = index - 1
  if n <= 0
    then (x:xs)
    else take n (x:xs) ++ drop (n + 1) (x:xs)
```

Here, our parameter is `_` because we don't need to use it.
We just return the empty list.

In the actual code, I used a `String` for the parameter instead of an `Int`.
One reason is for me to be able to use List Comprehesion for another way of doing the delete function.
Here, we don't delete by the index, instead by the todo.

```haskell
deleteTodo :: String -> [Todo] -> [Todo]
deleteTodo _ [] = [] 
deleteTodo todo list = [item | item <- list, task item /= todo]
```

So here we have a `list`, where each element's value is placed in `item`.
What this List Comprehesion does is that it filters through the list using the predicate `task item /= todo`.
In the predicate, I extract the `task` from the `item` and have it checked against the parameter `todo`.
If the predicate returns `True`, then the item is included in the new list.

This is great and all, but it is harder to actually delete a todo by typing its content, so let's revert it back to deleting by index.
But this time, we keep the `String` parameter.

```haskell
deleteTodo :: String -> [Todo] -> [Todo]
deleteTodo _ [] = []
deleteTodo index (todo:todos) = do
  let n = (read index :: Int) - 1
  if n <= 0
    then (todo:todos)
    else take n (todo:todos) ++ drop (n + 1) (todo:todos)
```

There is only one difference from the original which is I had to typecast the `index` parameter into an `Int` type.
This is so I would be able to use math operations.

Lastly, I want to map our commands so that we don't have to call `addTodo` or `deleteTodo` manually.
We can do this by creating a dictionary called `commands`.
Interestingly, because our `addTodo` and `deleteTodo` have the same parameters and return types, we can do this easy.

```haskell
commands :: [(String, String -> [Todo] -> [Todo])]
commands = [("add", addTodo), ("delete", deleteTodo)]
```

Here, we are mapping `"add"` to the function `addTodo`, and `"delete"` to `deleteTodo`.
We then `lookup` that mapper from `commands`.

```haskell
lookup "add" commands
```

This will then return the function `addTodo`.
So we can do something like this:

```haskell
(lookup "add" commands) "Do the laundry." [Todo { completed = False, todo = "Walk the dog." }]
```

Which then returns:

```haskell
[Todo { completed = False, todo = "Walk the dog." }, Todo { completed = False, todo = "Do the laundry." }]
```

## Input and Output (IO)
---

Now since this is a CLI app, we have to get input and output from the command line.
For the first time, this is where I saw a side effect when creating a function.
Anyway, I'm sure this has Haskell all figured out but when I started using this, it was really tougher than it looked.

Simply put, to output something to the CLI, we use Haskell's `putStrLn`.

```haskell
putStrLn :: String -> IO ()
```

So this takes in a `String` and then outputs an `IO ()`.
The `IO` means an action that has a side effect, in this case, an output to the CLI.
The `()` means an empty tuple, this is the equivalent of null or nil in some languages.
So `IO ()` just means an action that returns null or nil.

So let's try creating a function that will print out the todo.
But since `putStrLn` takes in a `String`, we can't really print out the `Todo` data type, unless we derive some Typeclasses (more on that later).
We can, however, print out the `todo`.

```haskell
printTodo :: Todo -> IO ()
printTodo (Todo { completed = completed', todo = todo' }) = putStrLn todo'
```

Here, we destructure the todo parameter to get the task value.
We then assign that value to `task'` and print it out.
Pretty simple right?

We can also map through our todo list to print all of the todos.

```haskell
printTodos :: [Todo] -> IO ()
printTodos list = mapM_ printTodo list
```

Now, we can surely understand most of the code here, except for the new function `mapM_`.
This is from the `Control.Monad` module, where this will map through the list and perform an `IO` on each one.
Its brother `mapM` does the same thing, except it collects the result of each `IO`.
But since we are expecting an `()`, we can just use `mapM_`.
They are both from the `map` function, which does what its name implies.

Now that we can print a list of todos, let's accept some input for our `addTodo` and `deleteTodo`.
Haskell has a built in `main` function where our app will start and this is probably the best place to add our input for now.

```haskell
main = input <- getLine
```

Here, `getLine` is the `IO` action and the result of that action is returned to `input`.

```haskell
getLine :: IO String
```

This basically just takes an input and the program ends.
Let's try to print out what the input is.

```haskell
main = do
  input <- getLine
  putStrLn input
```

Okay now, that we have some input and output, we can now try to route whatever input as a command and some arguments.
We'll start by creating a function `controller`, I know it looks OOP-ish, but this will just route the command to our functions.
Remember our `commands` function?

```haskell
commands :: [(String, String -> [Todo] -> [Todo])]
commands = [("add", addTodo), ("delete", deleteTodo)]
```

Now we need to lookup the functions in our `controller` function.
So we take in a `String` for the command, the list of todos (`[Todo]`) we will base off of, and a `String` for the todo to be added or deleted.
Then we will print out the list of todos so we use an `IO` that will return `()`.

```haskell
controller :: String -> [Todo] -> String -> IO ()
controller command list todo = do
  let (Just action) = lookup command commands
  printTodos action todo list
```

Here we used a new `Just` keyword because `lookup` is a monadic function.
For now I had not yet took a much needed time to learn about monads.
From what I understand, it is like a black box where it will take an argument, apply a function to it and return it.
The `IO` is actually a monad.
For now let's it for what it is.
Monadic functions like `lookup` return a `Maybe` which in other languages mean a nullable.
We add `Just` to denote if its not null, which we can then use `action` since we are certain it is not null.

And this is why I said in the beginning that this is tougher than it looks.
I still have not yet wrapped my head around how monads work.
Hopefully, I will and update this code once I do.

Let's move on to adding this `controller` function to our `main` function.

```haskell
main = do 
  putStrLn "Commands:"
  putStrLn "add"
  putStrLn "delete"
  putStrLn "Enter a command"
  command <- getLine
  putStrLn "Enter todo"
  todo <- getLine
  controller command [] todo
```

Now we can compile our code and try it out.
If you did, you probably noticed something.
It's not recursive and so our code just takes in a command and a todo, applies the command to our empty list.
In that sense, our delete command doesn't really work, although you could hardcode the `[]` in the controller.
It seems that I will either have to dive in deeper to Monads, which I will, or add persistence, which I will probably do later on.
This will be the end for now.

## Typeclasses
---

But before ending, I want to talk about Typeclasses for a bit.
A Typeclass is like an interface for the Types.
For example, the reason why we cannot print out a Todo is because it's not a `String`.
To make it printable without it being a `String` is to make it dervice from the Typeclass `Show`.
Which makes a Type converted into a `String`.
Let's try changing our `Todo` data type to be printable.

```haskell
data Todo = Todo {
  completed :: Bool,
  task :: String
} deriving (Show)
```

With this change, we can now call `putStrLn` to a Todo.
Pretty neat huh?

There are couple other Typeclasses such as `Ord` which can sort stuff, `Eq` which then we can compare, and a whole bunch more.
We could add `Eq` to our Todo since it would make sense that we can compare each todo.
It can be added by separating each with commas.

```haskell
data Todo = Todo {
  completed :: Bool,
  task :: String
} deriving (Show, Eq)
```