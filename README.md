# pandoc-diff

CLI tool that produces diffs and patches between two Pandoc documents.

**Important:** The tool is still under development (WIP).

## Prerequisites

- Install GHC and [cabal](https://www.haskell.org/cabal/), e.g. with [GHCup](https://www.haskell.org/ghcup/). Use the GHC version that `with-compiler` names in `cabal.project`, and a cabal version that supports it.
- Check out `pandoc-tree` next to this repository.

## How to Run

Navigate to project root and run `cabal build` to compile the project.

Run:

```
cabal run pandoc-diff -- --format json PANDOC_1_JSON PANDOC_2_JSON
```

**Note:** The JSON strings must be the (escaped) JSON string of the native Pandoc AST.

## Testing

Navigate to the project root folder and run `cabal test` to build and run the tests.

## Dependency Versions

The dependencies are solved from the state of the package index in `index-state` in `cabal.project`, and `cabal.project.freeze` pins their versions. Commit both with any change to the dependencies.

After adding or removing a dependency, run `cabal freeze`.

To upgrade the dependencies:

1. Run `cabal update`, which prints the state it updated the index to.
2. Set `index-state` in `cabal.project` to that time.
3. Delete `cabal.project.freeze` and run `cabal freeze`.
4. Build, run the tests, and commit `cabal.project` together with `cabal.project.freeze`.
