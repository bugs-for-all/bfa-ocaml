# Soteria

Soteria is a library for writing efficient symbolic interpreters directly in OCaml.
The core library is just a small toolbox that we use for writing a set of analyses, currently for C and Rust.

# Build instructions

To build the current repository, you first need to [install OCaml](https://ocaml.org/docs/installing-ocaml).
We then advise to create a local switch for development, and build the project:
```sh
opam switch create . --deps-only -y
dune build @all
```

If you want to run the build version of the code, you must also [install Z3](https://github.com/Z3Prover/z3).

You can make sure that everything is working properly by running the test suite:
```sh
dune test
```

# bfa-c (working name)

BFA-C is an automatic in-IDE bug finder for C programs. It is in heavy development.

It can currently be tested on standalone files by opening this project in VSCode.
To do so, open the `Run and Debug` tab of the sidebar, select the "Launch (Local)" configuration and click the "play" button.

# Rusteria (bfa-rust)

BFA-Rust is a Kani-like symbolic execution engine for Rust. It is in heavy development.

It can run standalone files, symbolically executing the `main` function, or any function with the attribute `#{kani::proof}`. To run it, you must first export the environment variable `KANI_LIB_PATH` to `path/to/bfa-ocaml/bfa_rust/kani_lib`. You must also have [Charon](https://github.com/AeneasVerif/charon) built and on your path -- to do so, clone Charon and run `make build-dev-charon-rust`. Once this is done, simply run:
```sh
bfa-rust exec-main <file>
```

To test Rusteria on the Kani test suite, clone [Kani](https://github.com/model-checking/kani) next to `bfa-ocaml`, and run `bfa_rust/scripts/kani.sh`. Run `bfa_rust/scripts/kani.sh --help` to see all available arguments.
