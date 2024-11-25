open Typed
open T

type t [@@deriving show]

(** Prettier but expensive printing. *)
val pp_pretty : ignore_freed:bool -> Format.formatter -> t -> unit

val empty : t

val load :
  [< sptr ] Typed.t ->
  Tree_block.Ctype.ctype ->
  t ->
  ( cval Typed.t * t,
    [> `MissingResource
    | `NullDereference
    | `OutOfBounds
    | `UninitializedMemoryAccess
    | `UseAfterFree ]
    * Cerb_location.t )
  Csymex.Result.t

val store :
  [< sptr ] Typed.t ->
  Tree_block.Ctype.ctype ->
  cval Typed.t ->
  t ->
  ( unit * t,
    [> `MissingResource | `NullDereference | `OutOfBounds | `UseAfterFree ]
    * Cerb_location.t )
  Csymex.Result.t

val alloc :
  sint Typed.t ->
  t ->
  ([> sptr ] Typed.t * t, 'a * Cerb_location.t) Csymex.Result.t

val alloc_ty :
  Tree_block.Ctype.ctype ->
  t ->
  ([> sptr ] Typed.t * t, 'a * Cerb_location.t) result Csymex.t

val free :
  [< sptr ] Typed.t ->
  t ->
  ( unit * t,
    [> `InvalidFree | `MissingResource | `UseAfterFree ] * Cerb_location.t )
  Result.t

val copy_nonoverlapping :
  dst:[< sptr ] Typed.t ->
  src:[< sptr ] Typed.t ->
  size:sint Typed.t ->
  t ->
  ( unit * t,
    [> `NullDereference | `OutOfBounds | `UseAfterFree | `MissingResource ]
    * Cerb_location.t )
  Result.t
