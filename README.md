# Advent of Code 2024

My solutions to Advent of Code 2024, learning Ocaml.

See https://adventofcode.com/2024

Repo setup inspired by https://github.com/fangyi-zhou/advent-of-code-ocaml-starter

## Setup

Use `dune-preview`

Install dependencies

```bash
dune pkg lock
```

# Running solutions

To run:

```bash
dune exec aoc -- DAY
```

E.g. only Day 3 Part 1:

```bash
dune exec aoc -- 3 -1
```

See help:

```bash
dune exec aoc -- -help
```

Small test inputs are inlined into the day files. Real inputs should be placed in `./inputs`, e.g.

```
- inputs/
  - 1.txt
  - 2.txt
```

`*.txt` files are gitignored since AoC asks participants to not share puzzle inputs
