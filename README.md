# Endless

A Haskell library implementing fundamental functional combinators, recursion schemes, and algorithms using point-free programming, coproducts, and category-theoretic abstractions.

## Getting Started

Ensure you have [GHC](https://www.haskell.org/ghc/) and [Cabal](https://www.haskell.org/cabal/) installed via [ghcup](https://www.haskell.org/ghcup/).

### Compilation and Building

To configure the project and build the library:

```bash
cabal build
```

### Running Tests

To run the full test suite:

```bash
cabal test
```

### Interactive REPL (GHCi)

To load the library in the interactive REPL:

```bash
cabal repl
```

To load the test suite in the REPL:

```bash
cabal repl test:algorithms-in-haskell-test
```

### Generating Documentation

To generate Haddock HTML documentation for the codebase:

```bash
cabal haddock
```

To build and open the generated documentation in your default web browser:

```bash
cabal haddock --open
```

### Clean Build Artifacts

To remove build files and clean the project directory:

```bash
cabal clean
```
