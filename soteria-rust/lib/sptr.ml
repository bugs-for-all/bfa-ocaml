open Charon
open Typed
open T
open Typed.Infix

module type S = sig
  (** pointer type *)
  type t

  val pp : t Fmt.t
  val null_ptr : t

  (** Pointer equality *)
  val sem_eq : t -> t -> sbool Typed.t

  (** If this pointer is at a null location, i.e. has no provenance *)
  val is_at_null_loc : t -> sbool Typed.t

  (** If these two pointers are at the same location (ie. same allocation) *)
  val is_same_loc : t -> t -> sbool Typed.t

  (** The distance, in bytes, between two pointers -- assumes they are at the
      same location. *)
  val distance : t -> t -> sint Typed.t

  (** The symbolic constraints needed for the pointer to be valid. *)
  val constraints : t -> sbool Typed.t

  (** [offset ~ty ptr off] Offsets [ptr] by the size of [ty] * [off]. [ty]
      defaults to u8. *)
  val offset : ?ty:Charon.Types.ty -> t -> sint Typed.t -> t

  (** Project a pointer to a field of the given type. *)
  val project :
    Types.ty -> Expressions.field_proj_kind -> Types.field_id -> t -> t
end

(** A pointer that can perform pointer arithmetics -- all pointers are a pair of
    location and offset, along with an optional metadata. *)
module ArithPtr : S with type t = T.sptr Typed.t * Tree_borrow.tag = struct
  type t = T.sptr Typed.t * Tree_borrow.tag

  let pp fmt (ptr, tag) =
    Fmt.pf fmt "%a[%a]" Typed.ppa ptr Tree_borrow.pp_tag tag

  let null_ptr = (Typed.Ptr.null, Tree_borrow.zero)
  let sem_eq (ptr1, _) (ptr2, _) = ptr1 ==@ ptr2
  let is_at_null_loc (ptr, _) = Typed.Ptr.is_at_null_loc ptr

  let is_same_loc (ptr1, _) (ptr2, _) =
    Typed.Ptr.loc ptr1 ==@ Typed.Ptr.loc ptr2

  let distance (ptr1, _) (ptr2, _) = Typed.Ptr.ofs ptr1 -@ Typed.Ptr.ofs ptr2

  let constraints =
    let offset_constrs = Layout.int_constraints Values.Isize in
    fun (ptr, _) ->
      let ofs = Typed.Ptr.ofs ptr in
      Typed.conj (offset_constrs ofs)

  let offset ?(ty = Types.TLiteral (TInteger U8)) (ptr, tag) off =
    let { size; _ } : Layout.layout = Layout.layout_of ty in
    let ptr' = Typed.Ptr.add_ofs ptr (Typed.int size *@ off) in
    (ptr', tag)

  let project ty kind field ptr =
    let field = Types.FieldId.to_int field in
    let layout, field =
      match kind with
      | Expressions.ProjAdt (adt_id, Some variant) ->
          (* Skip discriminator, so field + 1 *)
          (Layout.of_enum_variant adt_id variant, field + 1)
      | ProjAdt (_, None) | ProjTuple _ -> (Layout.layout_of ty, field)
    in
    let off = Array.get layout.members_ofs field in
    offset ptr (Typed.int off)
end
