[![CI](https://github.com/nogibjj/python-template/actions/workflows/cicd.yml/badge.svg)](https://github.com/nogibjj/python-template/actions/workflows/cicd.yml)
## Template for Haskell projects 

## Lesson 4:  Figure out formatting and linting

* What is popular for formatting?
* What is popular for linting?


## Lesson 3:  Build a working Marco Polo Command-Line Tools

* Use cabal and build out a working command-line tool that takes in Marco and returns Polo

This create a new cabal structure:

```bash
mkdir myfirstapp
cd myfirstapp
cabal init
```
After you do this, run `tree`
```bash
.
├── app
│   └── Main.hs
├── CHANGELOG.md
└── myfirstapp.cabal

1 directory, 3 files
```

* To run it:  `cabal run` or `make run`
* To add a package you tweak like this (you can check formatting by running `cargo format`)

```cabal
executable myfirstapp
    main-is:          Main.hs
    hs-source-dirs:   app
    default-language: Haskell2010
    build-depends:
        base ^>=4.16.4.0,
        haskell-say ^>=1.0.0.0
```

* To recompile and run with package you just do `cargo run`
```bash
 ________________________________________________________
 /                                                        \
| Hello, Haskell! You're using a function from another     |
| package!                                                 |
 \____       _____________________________________________/
      \    /
       \  /
        \/
  _____   _____
  \    \  \    \
   \    \  \    \
    \    \  \    \
     \    \  \    \  \-----------|
      \    \  \    \  \          |
       \    \  \    \  \---------|
       /    /  /     \
      /    /  /       \  \-------|
     /    /  /    ^    \  \      |
    /    /  /    / \    \  \ ----|
   /    /  /    /   \    \
  /____/  /____/     \____\
  ```

What about formatting?  Let's try to install: `make install` which 
installs `cabal install hlint stylish-haskell`


### Reference

* [Quickstart Cabal](https://cabal.readthedocs.io/en/stable/getting-started.html)
* [Haskell Code Formatter](https://hackage.haskell.org/package/haskell-formatter)
* [Search packages, i.e. hlint](https://hackage.haskell.org/package/hlint)

## Lesson 2:  Using Cabal

* [Walk through this](https://www.haskell.org/ghcup/steps/#using-external-packages-in-ghci)

`mkdir haskell-project`
`cd haskell-project`
`cabal init --interactive`
`make build` which is `cabal build`
`make run` which is `cabal run`

add more files, and then tweak `haskell-project.cabal` and then open up repl: `cabal repl`

```bash
@noahgift ➜ /workspaces/assimilate-haskell/haskell-project (main ✗) $ cabal repl
Resolving dependencies...
Build profile: -w ghc-9.2.5 -O1
In order, the following will be built (use -v for more details):
 - haskell-project-0.1.0.0 (lib) (configuration changed)
Configuring library for haskell-project-0.1.0.0..
Preprocessing library for haskell-project-0.1.0.0..
GHCi, version 9.2.5: https://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling MyLib            ( src/MyLib.hs, interpreted )
[2 of 2] Compiling OtherLib         ( src/OtherLib.hs, interpreted )
Ok, two modules loaded.
ghci> 
```



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

***Note:  To quit do Control-D ***

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



