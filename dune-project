(lang dune 3.14)

(name ntt)

(generate_opam_files true)

(source
 (github k-nrd/ntt-ml))

(authors "Gustavo Konrad")

(maintainers "Gustavo Konrad")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ntt)
 (synopsis
  "NTT is an ECS library with good performance and nice ergonomics.")
 (description "A longer description")
 (depends ocaml dune ppxlib containers)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
