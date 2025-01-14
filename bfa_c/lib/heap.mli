include
  Heap_intf.S
    with type 'a err = 'a * Cerb_location.t
     and type serialized =
      (Typed.T.sloc Typed.t * Tree_block.serialized Csymex.Freeable.serialized)
      list

val serialize : t -> serialized
val pp_serialized : Format.formatter -> serialized -> unit

val iter_vars_serialized :
  serialized -> (Svalue.Var.t * Svalue.ty -> unit) -> unit

val subst_serialized :
  (Svalue.Var.t -> Svalue.Var.t) -> serialized -> serialized

val consume : serialized -> t -> (t, 'err, serialized) Csymex.Result.t
val produce : serialized -> t -> t Csymex.t
