(lang dune 3.14)

(cram enable)

(name soteria)

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
 (github soteria-tools/soteria))

(package
 (name soteria)
 (synopsis "Soteria is a toolkit for writing symbolic bug-finding tools")
 (description "Soteria is a toolkit for writing symbolic bug-finding tools")
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
  htmlit
  progress
  grace
  (odoc :with-doc)
  (dune
   (and
    :build
    (>= 3.14.0)))
  (alt-ergo-lib
   (>= 2.6.2))
  ppxlib
  (ppx_inline_test :with-test)
  (ocamlformat
   (and
    :with-dev-detup
    (= 0.27.0)))
  (ocaml-lsp-server :with-dev-detup)))

(package
 (name soteria-c)
 (synopsis "A C bug-finder based on Soteria")
 (description "A C bug-finder based on Soteria")
 (sites
  (share includes))
 (depends
  (ocaml
   (>= 5.3.0))
  (shexp :build)
  eio_main
  soteria
  cerberus-lib
  zarith
  ppx_deriving
  hashcons
  cmdliner
  fmt
  printbox-text
  (linol
   (>= 0.10))
  (linol-eio
   (>= 0.10))
  (odoc :with-doc)
  (alcotest :with-test)
  (ppx_blob
   (and
    :build
    (>= 0.9.0)))
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
 (name soteria-vscode)
 (allow_empty)
 (synopsis "VSCode extension for Soteria-C")
 (description "VSCode extension for Soteria-C")
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
    (= 0.27.0)))
  js_of_ocaml
  js_of_ocaml-ppx
  ppx_deriving
  promise_jsoo
  vscode
  vscode-interop
  vscode-node))

(package
 (name soteria-rust)
 (synopsis "Rust symbolic execution tool based on Soteria")
 (description "Rust symbolic execution tool based on Soteria")
 (sites
  (share plugins))
 (depends
  (ocaml
   (>= 5.3.0))
  (shexp :build)
  soteria
  dune-site
  zarith
  ppx_deriving
  hashcons
  cmdliner
  fmt
  printbox-text
  unionFind
  name_matcher_parser
  charon
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
