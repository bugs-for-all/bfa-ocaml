open Typed
open T

module type S = sig
  type t
  type serialized

  val pp : Format.formatter -> t -> unit

  (** Prettier but expensive printing. *)
  val pp_pretty : ignore_freed:bool -> Format.formatter -> t -> unit

  val empty : t

  val load :
    [< sptr ] Typed.t ->
    Tree_block.Ctype.ctype ->
    t ->
    ( cval Typed.t * t,
      [> `NullDereference
      | `OutOfBounds
      | `UninitializedMemoryAccess
      | `UseAfterFree ]
      * Cerb_location.t,
      serialized )
    Csymex.Result.t

  val store :
    [< sptr ] Typed.t ->
    Tree_block.Ctype.ctype ->
    cval Typed.t ->
    t ->
    ( unit * t,
      [> `NullDereference | `OutOfBounds | `UseAfterFree ] * Cerb_location.t,
      serialized )
    Csymex.Result.t

  val alloc :
    sint Typed.t ->
    t ->
    ([> sptr ] Typed.t * t, 'a * Cerb_location.t, serialized) Csymex.Result.t

  val alloc_ty :
    Tree_block.Ctype.ctype ->
    t ->
    ([> sptr ] Typed.t * t, 'a * Cerb_location.t, serialized) Csymex.Result.t

  val free :
    [< sptr ] Typed.t ->
    t ->
    ( unit * t,
      [> `InvalidFree | `UseAfterFree ] * Cerb_location.t,
      serialized )
    Csymex.Result.t

  val copy_nonoverlapping :
    dst:[< sptr ] Typed.t ->
    src:[< sptr ] Typed.t ->
    size:sint Typed.t ->
    t ->
    ( unit * t,
      [> `NullDereference | `OutOfBounds | `UseAfterFree ] * Cerb_location.t,
      serialized )
    Csymex.Result.t

  val produce : serialized -> t -> t Csymex.t
  val consume : serialized -> t -> (t, 'err, serialized) Csymex.Result.t
end
