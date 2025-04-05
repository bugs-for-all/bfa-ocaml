open Charon_util
open Typed
open T
open Rustsymex
open Charon

module type S = sig
  module Sptr : Sptr.S

  type full_ptr := Sptr.t full_ptr

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
    full_ptr ->
    Types.ty ->
    t ->
    ( Sptr.t rust_val * t,
      [> `NullDereference
      | `OutOfBounds
      | `UninitializedMemoryAccess
      | `UseAfterFree
      | `UBTransmute
      | `UBTreeBorrow ]
      err,
      serialized list )
    Result.t

  val store :
    full_ptr ->
    Types.ty ->
    Sptr.t rust_val ->
    t ->
    ( unit * t,
      [> `NullDereference | `OutOfBounds | `UBTreeBorrow | `UseAfterFree ] err,
      serialized list )
    Result.t

  val alloc :
    sint Typed.t -> t -> (full_ptr * t, [> ] err, serialized list) Result.t

  val alloc_ty :
    Types.ty -> t -> (full_ptr * t, [> ] err, serialized list) Result.t

  val free :
    full_ptr ->
    t ->
    (unit * t, [> `InvalidFree | `UseAfterFree ] err, serialized list) Result.t

  val copy_nonoverlapping :
    dst:full_ptr ->
    src:full_ptr ->
    size:sint Typed.t ->
    t ->
    ( unit * t,
      [> `NullDereference | `OutOfBounds | `UseAfterFree ] err,
      serialized list )
    Result.t

  val uninit :
    full_ptr ->
    Types.ty ->
    t ->
    ( unit * t,
      [> `NullDereference | `OutOfBounds | `UseAfterFree ] err,
      serialized list )
    Result.t

  val error : 'a -> t -> ('ok, 'a err, serialized list) Result.t

  val store_str_global :
    string -> full_ptr -> t -> (unit * t, [> ] err, serialized list) Result.t

  val store_global :
    Types.global_decl_id ->
    full_ptr ->
    t ->
    (unit * t, [> ] err, serialized list) Result.t

  val load_str_global :
    string -> t -> (full_ptr option * t, [> ] err, serialized list) Result.t

  val load_global :
    Types.global_decl_id ->
    t ->
    (full_ptr option * t, [> ] err, serialized list) Result.t

  val borrow :
    full_ptr ->
    Expressions.borrow_kind ->
    t ->
    ( full_ptr * t,
      [> `NullDereference | `UseAfterFree ] err,
      serialized list )
    Result.t

  val protect :
    full_ptr ->
    Charon.Types.ref_kind ->
    t ->
    ( full_ptr * t,
      [> `NullDereference | `UseAfterFree ] err,
      serialized list )
    Result.t

  val unprotect :
    full_ptr ->
    t ->
    ( unit * t,
      [> `NullDereference | `UseAfterFree ] err,
      serialized list )
    Result.t
end
