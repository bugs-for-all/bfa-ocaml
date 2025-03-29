open Typed
open T

module type S = sig
  module Sptr : Sptr.S

  (* state *)
  type t
  type serialized
  type 'a err

  val add_to_call_trace : 'a err -> Call_trace.element -> 'a err
  val pp : t Fmt.t

  (** Prettier but expensive printing. *)
  val pp_pretty : ignore_freed:bool -> t Fmt.t

  val empty : t

  val load :
    ?is_move:bool ->
    Sptr.t ->
    Charon.Types.ty ->
    t ->
    ( Sptr.t Charon_util.rust_val * t,
      [> `NullDereference
      | `OutOfBounds
      | `UninitializedMemoryAccess
      | `UseAfterFree
      | `UBTransmute ]
      err,
      serialized list )
    Rustsymex.Result.t

  val store :
    Sptr.t ->
    Charon.Types.ty ->
    Sptr.t Charon_util.rust_val ->
    t ->
    ( unit * t,
      [> `NullDereference | `OutOfBounds | `UseAfterFree ] err,
      serialized list )
    Rustsymex.Result.t

  val alloc :
    sint Typed.t ->
    t ->
    (Sptr.t * t, [> ] err, serialized list) Rustsymex.Result.t

  val alloc_ty :
    Charon.Types.ty ->
    t ->
    (Sptr.t * t, [> ] err, serialized list) Rustsymex.Result.t

  val free :
    Sptr.t ->
    t ->
    ( unit * t,
      [> `InvalidFree | `UseAfterFree ] err,
      serialized list )
    Rustsymex.Result.t

  val uninit :
    Sptr.t ->
    Charon.Types.ty ->
    t ->
    ( unit * t,
      [> `NullDereference | `OutOfBounds | `UseAfterFree ] err,
      serialized list )
    Rustsymex.Result.t

  val copy_nonoverlapping :
    dst:Sptr.t ->
    src:Sptr.t ->
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
    Sptr.t ->
    t ->
    (unit * t, [> ] err, serialized list) Rustsymex.Result.t

  val store_global :
    Charon.Types.global_decl_id ->
    Sptr.t ->
    t ->
    (unit * t, [> ] err, serialized list) Rustsymex.Result.t

  val load_str_global :
    string ->
    t ->
    (Sptr.t option * t, [> ] err, serialized list) Rustsymex.Result.t

  val load_global :
    Charon.Types.global_decl_id ->
    t ->
    (Sptr.t option * t, [> ] err, serialized list) Rustsymex.Result.t
end
