(lang dune 3.14)

(cram enable)

(name bfa)

(using dune_site 0.1)

(generate_opam_files)

(license BSD-3-Clause)

(authors
 "Sacha-Élie Ayoun <s.ayoun17@imperial.ac.uk>"
 "Opale Sjöstedt <opale.sjostedt23@imperial.ac.uk>")

(maintainers
 "Sacha-Élie Ayoun <s.ayoun17@imperial.ac.uk>"
 "Opale Sjöstedt <opale.sjostedt23@imperial.ac.uk>")

(source
 (github bugs-for-all/bfa))

(package
 (name bfa)
 (synopsis "Bugs for All!")
 (description "Bugs for All!")
 (depends
  (ocaml
   (>= 5.3.0))
  (tsort
   (>= 2.1.0))
  eio_main
  simple_smt
  iter
  (ppx_expect
   (>= 0.17.0))
  ppx_deriving
  fmt
  logs
  (odoc :with-doc)
  (dune
   (and
    :build
    (>= 3.14.0)))
  ppxlib
  (ppx_inline_test :with-test)
  (ocamlformat
   (and
    :with-dev-detup
    (= 0.27.0)))
  (ocaml-lsp-server :with-dev-detup)))

(package
 (name bfa-c)
 (synopsis "Bugs for All, for C.")
 (description "Bugs for All, for C.")
 (depends
  (ocaml
   (>= 5.3.0))
  (shexp :build)
  eio_main
  bfa
  cerberus-lib
  zarith
  ppx_deriving
  hashcons
  cmdliner
  fmt
  logs
  printbox-text
  (linol
   (>= 0.8))
  (linol-eio
   (>= 0.8))
  (odoc :with-doc)
  (alcotest :with-test)
  (dune
   (and
    :build
    (>= 3.14.0)))
  (ocamlformat
   (and
    :with-dev-detup
    (= 0.27.0)))
  (ocaml-lsp-server :with-dev-setup)))

(package
 (name bfa-vscode)
 (allow_empty)
 (synopsis "VSCode extension for BFA")
 (description "VSCode extension for BFA")
 (depends
  (ocaml
   (>= 5.3.0))
  (dune
   (and
    :build
    (>= 3.14.0)))
  (ocaml-lsp-server :with-dev-detup)
  (ocamlformat
   (and
    :with-dev-detup
    (= 0.26.2)))
  js_of_ocaml
  js_of_ocaml-ppx
  ppx_deriving
  promise_jsoo
  vscode
  vscode-interop
  vscode-node))

(package
 (name bfa-rust)
 (synopsis "Bugs for All, for Rust.")
 (description "Bugs for All, for Rust.")
 (sites
  (share kani_lib))
 (depends
  (ocaml
   (>= 5.3.0))
  (shexp :build)
  eio_main
  bfa
  dune-site
  zarith
  ppx_deriving
  hashcons
  cmdliner
  fmt
  logs
  printbox-text
  unionFind
  name_matcher_parser
  charon
  (linol
   (>= 0.8))
  (linol-eio
   (>= 0.8))
  (odoc :with-doc)
  (alcotest :with-test)
  (dune
   (and
    :build
    (>= 3.14.0)))
  (ocamlformat
   (and
    :with-dev-detup
    (= 0.27.0)))
  (ocaml-lsp-server :with-dev-setup)))
