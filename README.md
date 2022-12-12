[![CI](https://github.com/nogibjj/python-template/actions/workflows/cicd.yml/badge.svg)](https://github.com/nogibjj/python-template/actions/workflows/cicd.yml)
## Template for Haskell projects 

## Lesson 2:  Using Cabal

* [Walk through this](https://www.haskell.org/ghcup/steps/#using-external-packages-in-ghci)


## Lesson:  Install and Getting Started

* [Haskell Downloads](https://www.haskell.org/downloads/)
* [Getting started first steps](https://www.haskell.org/ghcup/steps/)

* Step 1:  Where is it?  `which gch` and should show `/home/codespace/.local/bin/ghc`
* Step2:  Get the version `ghc --version` you should see: `The Glorious Glasgow Haskell Compilation System, version 9.2.4` or something similar
* Step 3:  Write a hello world in `hello.hs` with body of `main = putStrLn "Hello, Haskell!"`
* Step 4:  Compile:  `ghc hello.hs`
* Step 5:  Run:   `./hello `
* Step 6:  You can also just run it: `runghc hello.hs`

### Interactive Haskell

`ghci` starts the interactive mode:

```bash
@noahgift ➜ /workspaces/assimilate-haskell (main ✗) $ ghci
GHCi, version 9.2.4: https://www.haskell.org/ghc/  :? for help
ghci> 1+1
2
ghci> main = putStrLn "Hello, Haskell!"
ghci> main
Hello, Haskell!
ghci> 
```

You can see `putStrLn "This is interactive Haskell"` to echo

Load existing code:

`:load hello.hs`

Then run main

Can also import library:

`import Data.Bits`

Check expression

Quit:

### Warnings

`ghc -Wall hello.hs -fforce-recomp`

Example Output

```bash
hello.hs:2:1: warning: [-Wmissing-signatures]
    Top-level binding with no type signature: main :: IO ()
  |
2 | main = putStrLn "Hello, Haskell!"
  | ^^^^
Linking hello ...
```

Put this into `Makefile`:  `make compile-haskell-warnings`



### References



