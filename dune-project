(lang dune 3.14)

(name ocomps)

(generate_opam_files true)

(source
 (github k-nrd/ocomps))

(authors "Gustavo Konrad")

(maintainers "Gustavo Konrad")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ocomps)
 (synopsis
  "OComps is an ECS library with good performance and nice ergonomics.")
 (description "A longer description")
 (depends ocaml dune ppxlib containers)
 (tags
  (topics ecs "entity component system" "games" "entity" "component" "system")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
