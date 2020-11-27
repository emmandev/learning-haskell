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

# Todo List (so meta)

- [ ] Learn to use Stack or Cabal
    - [ ] Adding dependencies
    - [ ] What packages are stable and in active maintenance
- [ ] Add a local server
- [ ] Add Endpoints
    - [ ] Fetching a todo with an ID
    - [ ] Fetching all todos
    - [ ] Creating a todo
    - [ ] Deleting a todo
    - [ ] Updating a todo
- [ ] Add a database
- [ ] Writeup

Extras:
- [ ] Experimenting with other stuff
    - [ ] GraphQL
    - [ ] MongoDB
    - [ ] ?

