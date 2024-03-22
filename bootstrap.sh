opam switch create ocomps 5.1.1
opam install . --deps-only
opam install ocaml-lsp-server
echo "eval $(opam env --switch=ocomps)" > .envrc
direnv allow
