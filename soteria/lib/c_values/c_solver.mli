module Z3_solver :
  Soteria_symex.Solver.Mutable_incremental with module Value = Typed

module Z3_incremental_solver :
  Soteria_symex.Solver.Mutable_incremental with module Value = Typed

module Alt_ergo_solver :
  Soteria_symex.Solver.Mutable_incremental with module Value = Typed
