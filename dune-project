(lang dune 3.16)

(name aoc)

(generate_opam_files true)

(authors "Krzysztof Bielikowicz")

(maintainers "Krzysztof Bielikowicz")

(license MIT)

(package
 (name aoc)
 (synopsis "Advent of Code 2024 solutions")
 (depends
   ocaml
   dune
   core
   core_unix
   fmt
   ppx_deriving))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
