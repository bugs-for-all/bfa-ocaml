Test memory leaks
  $ soteria-rust exec-main leak.rs --clean
  Error in 1 branch:
  - Memory leak
    Trace:
    • Triggering memory operation: ../cram/simple.t/leak.rs:1:0-3:1
  [1]

Test reading the max and min chars (used to crash Charon-ML)
  $ soteria-rust exec-main char_min_max.rs --clean
  Done. - Ran 1 branches
  PC: empty

Test casting between integer types
  $ soteria-rust exec-main int_casting.rs --clean
  Done. - Ran 1 branches
  PC: empty

Splitting and merging, via a union
  $ soteria-rust exec-main split_merges.rs --clean
  Done. - Ran 1 branches
  PC: empty
