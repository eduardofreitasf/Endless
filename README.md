# Endless

A Haskell library implementing fundamental functional combinators, recursion schemes, and
algorithms using point-free programming, coproducts, and category-theoretic abstractions.

## Design Philosophy

Endless is built around two interlocking ideas:

1. **Point-free programming** — computations are expressed by composing primitive operators
   (`/\`, `><`, `\/`, `-|-`, …) rather than by naming intermediate values. The `Algebra`
   module is the foundation; every other module builds on top of it.

2. **Recursion schemes** — every recursive datatype is equipped with a _base functor_ and
   all traversals are expressed uniformly as:
   - `cata` (catamorphism) — fold / consume a structure,
   - `ana` (anamorphism) — unfold / produce a structure,
   - `hylo` (hylomorphism) — unfold then immediately fold, fusing the two passes.

This style follows the tradition of _Algebra of Programming_ (Bird & de Moor) and makes
the structure of every algorithm explicit in its type.

## Project Structure

```
Endless/
├── lib/               # Library source modules
│   ├── Algebra.hs     # Core combinators: products, coproducts, conditionals, while
│   ├── Utils.hs       # Shared utilities (partition, …)
|   └── ...            # Other modules (Nat, List, Maybe, BTree, ...)
├── test/              # Test suite (Tasty + HUnit + QuickCheck)
├── fourmolu.yaml      # Code formatter configuration
├── Makefile           # Development task runner
└── endless.cabal      # Build configuration
```

## Getting Started

Ensure you have [GHC](https://www.haskell.org/ghc/) and [Cabal](https://www.haskell.org/cabal/)
installed via [ghcup](https://www.haskell.org/ghcup/).

All common tasks are available as `make` targets.

## Development Commands

### Build

```bash
make build
```

### Run Tests

```bash
make test
```

### Interactive REPL (GHCi)

```bash
make repl
```

To load the test suite in the REPL directly:

```bash
make repl-test
```

### Generate Documentation

Build Haddock HTML documentation:

```bash
make haddock
```

Build and open the documentation in your default browser:

```bash
make haddock-open
```

### Clean Build Artifacts

```bash
cabal clean
```

## Code Style

The project uses [fourmolu](https://github.com/fourmolu/fourmolu) for formatting. Configuration lives in `fourmolu.yaml`.

**Format all source files in-place:**

```bash
make format
```

**Check formatting without modifying files:**

```bash
make lint
```

Install `fourmolu` once with:

```bash
cabal install fourmolu
```
