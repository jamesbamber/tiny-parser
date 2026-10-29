# tiny-parser

## Build

Run `$ cabal build` to build the project
## Documentation

Run `$ cabal haddock --open` to generate a reference for the API of the project.

## Fourmolu

To use the Fourmolu pre-commit hook, install the version listed in `.fourmolu-version` (currently `cabal install fourmolu-0.21.0.0`), then enable the hook with `git config core.hooksPath .githooks`.
