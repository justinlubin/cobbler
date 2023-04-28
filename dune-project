(lang dune 3.0)

(name component_based_refactoring)

(generate_opam_files true)

(source
 (github justinlubin/component-based-refactoring))

(authors "Justin Lubin")

(maintainers "justinlubin@berkeley.edu")

(license BSD-3-Clause)

(package
 (name component_based_refactoring)
 (synopsis "Refactoring with component-based synthesis")
 (depends
  (ocaml
   (>= 4.14.0))
  dune
  core
  ppx_jane
  ppx_deriving))
