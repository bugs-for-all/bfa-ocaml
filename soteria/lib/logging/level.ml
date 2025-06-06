type t = Trace | Smt | Debug | Info | Warn | App | Error

let to_string = function
  | Smt -> "SMT"
  | Trace -> "TRACE"
  | Debug -> "DEBUG"
  | Info -> "INFO"
  | Warn -> "WARN"
  | Error -> "ERROR"
  | App -> "APP"

let ( >= ) l r =
  match (l, r) with
  | _, _ when l == r -> true
  | Smt, _ -> false
  | _, Smt -> true
  | Trace, _ -> false
  | _, Trace -> true
  | Debug, _ -> false
  | _, Debug -> true
  | Info, _ -> false
  | _, Info -> true
  | Warn, _ -> false
  | _, Warn -> true
  | App, _ -> false
  | _, App -> true
  | Error, _ -> true

let geq = ( >= )
