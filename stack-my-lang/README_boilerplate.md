# PP Final Project: Stack boilerplate project

This folder contains a boilerplate project for Haskell & Stack, which includes:

- HSpec/QuickCheck
- ParSec
- Sprockell
- Example program that parses a number and generates a SPRIL program which calculates the fibonacci numbers up to the given number

## Prerequisites

Make sure you have installed:

- Stack (<https://docs.haskellstack.org/en/stable/README/>). Version needs to be 2.7 or higher.

To test if you have these tools set up properly, run the following command in the command line:

```
stack --version
```

If this command prints a sensible output (e.g. "version so and so"), this boilerplate project should work fine. Otherwise, you will have to install these tools either manually or using your operating system's package manager.

## Compiling

In a terminal, run:

```
stack build
```

This installs a local version of GHC and the libraries needed. The files in `src/` and `app/` are then compiled.

## Running

In a terminal, run:

```
stack run
```

By default, if no arguments are passed, this runs the first executable as specified in `package.yml`. So make sure if you make any changes that the first executable is still the main entry point in your compiler! If needed, your source files will first be compiled. You can also choose which executable from the project is executed if you pass the right flags. See `stack run --help` for more info.

## Tests

In a terminal, run:

```
stack test
```

This will run all test executables as defined in the "test" section of `package.yml`.

## What can you change about this project?

First, we encourage you to decide on a name for your language, and fill that in in `package.yaml`. We also encourage you to fill out the other info that seems relevant.

Second, there are several aspects about this project that you can change. The main principle is that your project should be easily usable by using the default stack commands. This means that:

- `stack run -- program.mylang` should run your compiler on an input program located at the path `program.mylang`
- `stack test` should run all your automated tests

Beyond this, you can decide how you want to structure your project. For example, you can choose a different folder hierarchy instead of the current `app`/`src`/`test`, as long as the names are sensible. You could choose to change your testing framework, for example to "tasty", which is another popular testing framework. Or, if you want to include changes to sprockell in your project, you can fork the sprockell repo and point your project to your sprockell fork.
