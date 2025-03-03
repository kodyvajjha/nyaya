(lang dune 3.15)

(using menhir 3.0)

(name nyaya)

(generate_opam_files true)

(source
 (github kodyvajjha/nyaya))

(authors "Koundinya Vajjha")

(maintainers "Koundinya Vajjha")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name nyaya)
 (synopsis "An external type-checker for Lean 4.")
 (description "")
 (depends ocaml dune containers ppx_deriving)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
