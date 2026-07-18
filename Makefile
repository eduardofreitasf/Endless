SRCS = $(shell find lib test -name "*.hs")

.PHONY: format lint build test haddock haddock-open repl

format:
	fourmolu --mode inplace $(SRCS)

lint:
	fourmolu --mode check $(SRCS)

build:
	cabal build

test:
	cabal test

haddock:
	cabal haddock

haddock-open:
	cabal haddock --open

repl:
	cabal repl

repl-test:
	cabal repl test:endless-test
