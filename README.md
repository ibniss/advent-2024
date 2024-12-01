# Advent of Code 2024

My solutions to Advent of Code 2024, learning Ocaml.

See https://adventofcode.com/2024

## Setup

Install dependencies

```bash
opam switch create . 5.2.1

# might not be needed, should pick up switch from local folder
eval $(opam env)

# install dependencies
opam install . --deps-only
```

For LSP to work (tested in neovim):

```bash
opam install ocaml-lsp-server
```

# Running solutions

To run e.g. day 1:

```bash
dune exec ./bin/day1.exe
```

Small test inputs are checked into the repo. Real inputs should be placed in `./inputs`, e.g.

```
- inputs/
  - day1.txt
  - day1_prod.txt
```

`*_prod.txt` files are gitignored since AoC asks participants to not share puzzle inputs
