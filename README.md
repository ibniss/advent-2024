# Advent of Code 2024

My solutions to Advent of Code 2024, learning Ocaml.

See https://adventofcode.com/2024

Repo setup inspired by https://github.com/fangyi-zhou/advent-of-code-ocaml-starter

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

To run:

```bash
dune exec aoc DAY
```

Small test inputs are inlined into the day files. Real inputs should be placed in `./inputs`, e.g.

```
- inputs/
  - 1.txt
  - 2.txt
```

`*.txt` files are gitignored since AoC asks participants to not share puzzle inputs
