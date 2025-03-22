open Typed
open T

module type S = sig
  type t
  type serialized
  type 'a err

  val add_to_call_trace : 'a err -> Call_trace.element -> 'a err
  val pp : Format.formatter -> t -> unit

  (** Prettier but expensive printing. *)
  val pp_pretty : ignore_freed:bool -> Format.formatter -> t -> unit

  val empty : t

  val load :
    [< sptr ] Typed.t ->
    Charon.Types.ty ->
    t ->
    ( Charon_util.rust_val * t,
      [> `NullDereference
      | `OutOfBounds
      | `UninitializedMemoryAccess
      | `UseAfterFree ]
      err,
      serialized list )
    Rustsymex.Result.t

  val store :
    [< sptr ] Typed.t ->
    Charon.Types.ty ->
    Charon_util.rust_val ->
    t ->
    ( unit * t,
      [> `NullDereference | `OutOfBounds | `UseAfterFree ] err,
      serialized list )
    Rustsymex.Result.t

  val alloc :
    sint Typed.t ->
    t ->
    ([> sptr ] Typed.t * t, [> ] err, serialized list) Rustsymex.Result.t

  val alloc_ty :
    Charon.Types.ty ->
    t ->
    ([> sptr ] Typed.t * t, [> ] err, serialized list) Rustsymex.Result.t

  val free :
    [< sptr ] Typed.t ->
    t ->
    ( unit * t,
      [> `InvalidFree | `UseAfterFree ] err,
      serialized list )
    Rustsymex.Result.t

  val uninit :
    [< sptr ] Typed.t ->
    t ->
    ( unit * t,
      [> `NullDereference | `UseAfterFree ] err,
      serialized list )
    Rustsymex.Result.t

  val copy_nonoverlapping :
    dst:[< sptr ] Typed.t ->
    src:[< sptr ] Typed.t ->
    size:sint Typed.t ->
    t ->
    ( unit * t,
      [> `NullDereference | `OutOfBounds | `UseAfterFree ] err,
      serialized list )
    Rustsymex.Result.t

  val error : 'a -> t -> ('ok, 'a err, serialized list) Rustsymex.Result.t
  val produce : serialized -> t -> t Rustsymex.t

  val consume :
    serialized -> t -> (t, [> ] err, serialized list) Rustsymex.Result.t

  val store_str_global :
    string ->
    Charon_util.rust_val ->
    t ->
    (unit * t, [> ] err, serialized list) Rustsymex.Result.t

  val store_global :
    Charon.Types.global_decl_id ->
    Charon_util.rust_val ->
    t ->
    (unit * t, [> ] err, serialized list) Rustsymex.Result.t

  val load_str_global :
    string ->
    t ->
    ( Charon_util.rust_val option * t,
      [> ] err,
      serialized list )
    Rustsymex.Result.t

  val load_global :
    Charon.Types.global_decl_id ->
    t ->
    ( Charon_util.rust_val option * t,
      [> ] err,
      serialized list )
    Rustsymex.Result.t
end
