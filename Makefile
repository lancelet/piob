.DEFAULT_GOAL := build
.PHONY: format

format:
	@echo "Formatting piob.cabal..."
	cabal-fmt --inplace piob.cabal

	@echo "Formatting Haskell source files..."
	find . -name '*.hs' -exec ormolu --mode inplace {} \;

build: format
	cabal build

test: build
	cabal test

clean:
	cabal clean