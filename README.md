# pandoc-diff

CLI tool that produces diffs and patches between two Pandoc documents.

**Important:** The tool is still under development (WIP).

## How to Run

Navigate to project root and run `stack build` to compile the project.

Run:

```
stack exec pandoc-diff -- --format json PANDOC_1_JSON PANDOC_2_JSON
```

**Note:** The JSON strings must be the (escaped) JSON string of the native Pandoc AST.
