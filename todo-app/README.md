# Introduction

This is my attempt to create a backend service in Haskell. Over the course of two months this will be continually updated as I will still be learning what possible packages I could use or find interesting and of course, the language itself.

In the end, this will give me a glimpse as to how viable this language could be for actual work, to answer "Why bother?".

# The Application

This will be continuation of the previous Haskell CLI app, but databases, server, and whatnots.

To run this application, install Stack.
For Unix:
`curl -sSL https://get.haskellstack.org/ | sh`

For Windows, there is a downloable installer in their site:
`https://docs.haskellstack.org/en/stable/README/`

Then `cd` into this current directory and build:
`stack build`

# "Package Manager"

Between Cabal and Stack, I find the latter far more convenient to use. 

The first problem I encountered getting into packages were the conflicts.
They both have some mechanism to avoid dependency conflicts but figuring out how to do that in Stack was somehow easier with the use of Stackage, which makes sure that packages are consistent and are not in conflict based on the snapshots it makes.
In `stack.yaml`, I provided `resolver` with a specific snapshot version and in `package.yaml`, I would just then add the packages under `dependencies` without the need for specifying the versions for each of them.
This works because all of the packages' version would resolve to whatever was in `stack.yaml`'s `resolver`.
If however, a package is not in Stackage, we could add it to `extra-deps` and add versions.
Contrast this to Cabal where I would have to add specific versions for each packages and maintain them myself.

Also, one good thing about Stack is that it also works with Cabal files.

# Packages

## Servant

[Servant](https://www.servant.dev/) is what I used as the web framework. It deals with creating the server and the APIs.

## Persistent

[Persistent](https://www.yesodweb.com/book/persistent) handles the database layer. It is database agnostic but has built in support for some databases, one of which is PostgreSQL, which is used for this app.

# Resources
 
- [Servant docs](https://docs.servant.dev/en/stable/tutorial/index.html)
- [parsonsmatt/servant-persistent](https://github.com/parsonsmatt/servant-persistent)
- [haskell-servant/example-servant-persistent](https://github.com/haskell-servant/example-servant-persistent)
- [jkachmar/servant-persistent-realworld](https://github.com/jkachmar/servant-persistent-realworld)

# Todo List (so meta)

- [x] Learn to use Stack or Cabal
    - [x] Adding dependencies
    - [x] What packages are stable and in active maintenance
- [x] Add a local server
- [ ] Add Endpoints
    - [x] Fetching a todo with an ID
    - [x] Fetching all todos
    - [x] Creating a todo
    - [x] Deleting a todo
    - [x] Updating a todo
- [x] Add a database
- [ ] Writeup

Extras:
- [ ] Experimenting with other stuff
    - [ ] GraphQL
    - [ ] MongoDB
    - [ ] ?

