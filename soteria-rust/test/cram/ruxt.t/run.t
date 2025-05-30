Test RUXt in even example
  $ soteria-rust ruxt safe_even.rs --clean
  No type unsoundness found!

  $ soteria-rust ruxt unsafe_even.rs --clean
  Found type unsoundness!
  [1]
