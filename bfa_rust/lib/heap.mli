include
  Heap_intf.S
    with type 'a err = 'a * Call_trace.t
     and type serialized =
      (Typed.T.sloc Typed.t
      * Tree_block.serialized Rustsymex.Freeable.serialized)
      list

val serialize : t -> serialized
val pp_serialized : Format.formatter -> serialized -> unit

val iter_vars_serialized :
  serialized -> (Svalue.Var.t * [< Typed.T.cval ] Typed.ty -> unit) -> unit

val subst_serialized :
  (Svalue.Var.t -> Svalue.Var.t) -> serialized -> serialized
