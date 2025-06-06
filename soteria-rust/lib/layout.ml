open Charon
open Typed
open Typed.Infix
open Typed.Syntax
open Rustsymex
open Charon_util

exception CantComputeLayout of string * Types.ty
exception InvalidLayout

module Archi = struct
  let word_size = 8
end

type layout = {
  size : int;
  align : int;
  members_ofs : int Array.t;
      (** Array of offset in layout for field with given index. For enums,
          includes the discriminator at index 0 *)
}

let pp_layout fmt { size; align; members_ofs } =
  Format.fprintf fmt "{ size = %d; align = %d; members_ofs = [%a] }" size align
    Fmt.(array ~sep:comma int)
    members_ofs

module Session = struct
  (* TODO: allow different caches for different crates *)
  (** Cache of (type or variant) -> layout *)
  let layout_cache : ((Types.ty, Types.variant) Either.t, layout) Hashtbl.t =
    Hashtbl.create 128

  let get_or_compute_cached_layout ty f =
    match Hashtbl.find_opt layout_cache ty with
    | Some layout -> layout
    | None ->
        let layout = f () in
        Hashtbl.add layout_cache ty layout;
        layout

  let get_or_compute_cached_layout_ty ty =
    get_or_compute_cached_layout (Left ty)

  let get_or_compute_cached_layout_var var =
    get_or_compute_cached_layout (Right var)
end

let is_int : Types.ty -> bool = function
  | TLiteral (TInteger _) -> true
  | _ -> false

let size_of_int_ty : Types.integer_type -> int = function
  | I128 | U128 -> 16
  | I64 | U64 -> 8
  | I32 | U32 -> 4
  | I16 | U16 -> 2
  | I8 | U8 -> 1
  | Isize | Usize -> Archi.word_size

let size_of_literal_ty : Types.literal_type -> int = function
  | TInteger int_ty -> size_of_int_ty int_ty
  | TBool -> 1
  | TChar -> 4
  | TFloat F16 -> 2
  | TFloat F32 -> 4
  | TFloat F64 -> 8
  | TFloat F128 -> 16

(* TODO: this is not really accurate, but good enough for now.
   See https://doc.rust-lang.org/reference/type-layout.html#r-layout.primitive.align *)
let align_of_literal_ty : Types.literal_type -> int = size_of_literal_ty
let empty_generics = TypesUtils.empty_generic_args

(** If this is a dynamically sized type (requiring a fat pointer) *)
let rec is_dst : Types.ty -> bool = function
  | TAdt (TBuiltin TSlice, _) | TAdt (TBuiltin TStr, _) -> true
  | TAdt (TAdtId id, _) when Crate.is_struct id -> (
      match List.last_opt (Crate.as_struct id) with
      | None -> false
      | Some last -> is_dst Types.(last.field_ty))
  | _ -> false

let size_to_fit ~size ~align =
  let ( % ) = Stdlib.( mod ) in
  if size % align = 0 then size else size + align - (size % align)

let max_array_len sub_size =
  let isize_bits = Archi.word_size * 8 in
  if sub_size = 0 then Z.of_int isize_bits
  else Z.((one lsl isize_bits) / of_int sub_size)

let rec layout_of (ty : Types.ty) : layout =
  Session.get_or_compute_cached_layout_ty ty @@ fun () ->
  match ty with
  (* Literals *)
  | TLiteral ty ->
      let size = size_of_literal_ty ty in
      let align = align_of_literal_ty ty in
      { size; align; members_ofs = [||] }
  (* Fat pointers *)
  | TAdt (TBuiltin TBox, { types = [ sub_ty ]; _ })
  | TRef (_, sub_ty, _)
  | TRawPtr (sub_ty, _)
    when is_dst sub_ty ->
      {
        size = Archi.word_size * 2;
        align = Archi.word_size;
        members_ofs = [||];
      }
  (* Refs, pointers, boxes *)
  | TAdt (TBuiltin TBox, _) | TRef (_, _, _) | TRawPtr (_, _) ->
      { size = Archi.word_size; align = Archi.word_size; members_ofs = [||] }
  (* Dynamically sized types -- we assume they have a size of 0. In truth, these types should
     simply never be allocated directly, and instead can only be obtained hidden behind
     references; however we must be able to compute their layout, to get e.g. the offset of
     the tail in a DST struct.
     FIXME: Maybe we should mark the layout as a DST, and ensure a DST layout's size is never
     used for an allocation. *)
  | TAdt (TBuiltin (TStr as ty), generics)
  | TAdt (TBuiltin (TSlice as ty), generics) ->
      let sub_ty =
        if ty = TSlice then List.hd generics.types else TLiteral (TInteger U8)
      in
      let sub_layout = layout_of sub_ty in
      { size = 0; align = sub_layout.align; members_ofs = [||] }
  (* Tuples *)
  | TAdt (TTuple, { types; _ }) -> layout_of_members types
  (* Custom ADTs (struct, enum, etc.) *)
  | TAdt (TAdtId id, _) -> (
      let adt = Crate.get_adt id in
      match adt.type_kind with
      | Struct fields -> layout_of_members @@ field_tys fields
      | Enum [] -> { size = 0; align = 1; members_ofs = [||] }
      (* fieldless enums with one variant are zero-sized *)
      | Enum [ { fields = []; _ } ] ->
          { size = 0; align = 1; members_ofs = [||] }
      | Enum variants ->
          let layouts = List.map of_variant variants in
          List.fold_left
            (fun acc l -> if l.size > acc.size then l else acc)
            (List.hd layouts) (List.tl layouts)
      | Opaque ->
          let msg = Fmt.str "Opaque %a " Crate.pp_name adt.item_meta.name in
          raise (CantComputeLayout (msg, ty))
      | Union fs ->
          let layouts = List.map layout_of @@ Charon_util.field_tys fs in
          let hd = List.hd layouts in
          let tl = List.tl layouts in
          let size, align =
            List.fold_left
              (fun (size, align) l -> (max size l.size, max align l.align))
              (hd.size, hd.align) tl
          in
          let size = size_to_fit ~size ~align in
          (* All fields in the union start at 0 and overlap *)
          let members_ofs = Array.init (List.length fs) (fun _ -> 0) in
          { size; align; members_ofs }
      | TDeclError _ -> raise (CantComputeLayout ("DeclError", ty))
      | Alias _ -> raise (CantComputeLayout ("Alias", ty)))
  (* Arrays *)
  | TAdt (TBuiltin TArray, { types = [ ty ]; const_generics = [ size ]; _ }) ->
      let len = Charon_util.int_of_const_generic size in
      let sub_layout = layout_of ty in
      if Z.(of_int len > max_array_len sub_layout.size) then raise InvalidLayout;
      let members_ofs = Array.init len (fun i -> i * sub_layout.size) in
      { size = len * sub_layout.size; align = sub_layout.align; members_ofs }
  | TAdt (TBuiltin TArray, _) -> failwith "Invalid TArray shape"
  (* Never -- zero sized type *)
  | TNever -> { size = 0; align = 1; members_ofs = [||] }
  (* Function definitions -- zero sized type *)
  | TFnDef _ -> { size = 0; align = 1; members_ofs = [||] }
  (* Function pointers (can point to a function or a state-less closure). *)
  | TArrow _ ->
      { size = Archi.word_size; align = Archi.word_size; members_ofs = [||] }
  (* FIXME: this is wrong but at least some more code runs... *)
  | TDynTrait _ -> { size = 0; align = 1; members_ofs = [||] }
  (* Others (unhandled for now) *)
  | TVar _ -> raise (CantComputeLayout ("De Bruijn variable", ty))
  | TError _ -> raise (CantComputeLayout ("Error", ty))
  | TTraitType (tref, ty_name) -> layout_of @@ resolve_trait_ty tref ty_name

and layout_of_members members =
  let rec aux members_ofs (layout : layout) = function
    | [] -> (List.rev members_ofs, layout)
    | ty :: rest ->
        let { size = curr_size; align = curr_align; _ } = layout in
        let { size; align; _ } = layout_of ty in
        let mem_ofs = size_to_fit ~size:curr_size ~align in
        let new_size = mem_ofs + size in
        let new_align = max align curr_align in
        aux (mem_ofs :: members_ofs)
          { size = new_size; align = new_align; members_ofs = [||] }
          rest
  in
  let members_ofs, { size; align; members_ofs = _ } =
    aux [] { size = 0; align = 1; members_ofs = [||] } members
  in
  {
    size = size_to_fit ~size ~align;
    align;
    members_ofs = Array.of_list members_ofs;
  }

and of_variant (variant : Types.variant) =
  Session.get_or_compute_cached_layout_var variant @@ fun () ->
  let discr_ty = Types.TLiteral (TInteger variant.discriminant.int_ty) in
  let members = discr_ty :: field_tys variant.fields in
  layout_of_members members

and of_enum_variant adt_id variant =
  let variants = Crate.as_enum adt_id in
  let variant = Types.VariantId.nth variants variant in
  of_variant variant

and resolve_trait_ty (tref : Types.trait_ref) ty_name =
  match tref.trait_id with
  | TraitImpl (impl, _) ->
      let impl = Crate.get_trait_impl impl in
      let _, ty = List.find (fun (n, _) -> ty_name = n) impl.types in
      ty
  | _ ->
      raise (CantComputeLayout ("Trait type", Types.TTraitType (tref, ty_name)))

let offset_in_array ty idx =
  let sub_layout = layout_of ty in
  idx * sub_layout.size

let layout_of_s ty =
  try return @@ layout_of ty
  with CantComputeLayout (msg, ty') ->
    Fmt.kstr Rustsymex.not_impl
      "Cannot yet compute size of %s:@.%a@.Occurred when computing:@.%a" msg
      pp_ty ty' pp_ty ty

let size_of_s ty =
  let open Rustsymex.Syntax in
  let+ { size; _ } = layout_of_s ty in
  Typed.int size

let align_of_s ty =
  let open Rustsymex.Syntax in
  let+ { align; _ } = layout_of_s ty in
  Typed.nonzero align

let is_signed : Types.integer_type -> bool = function
  | I128 | I64 | I32 | I16 | I8 | Isize -> true
  | U128 | U64 | U32 | U16 | U8 | Usize -> false

let min_value_z : Types.integer_type -> Z.t = function
  | U128 | U64 | U32 | U16 | U8 | Usize -> Z.zero
  | Isize -> Z.neg (Z.shift_left Z.one ((8 * Archi.word_size) - 1))
  | I128 -> Z.neg (Z.shift_left Z.one 127)
  | I64 -> Z.neg (Z.shift_left Z.one 63)
  | I32 -> Z.neg (Z.shift_left Z.one 31)
  | I16 -> Z.neg (Z.shift_left Z.one 15)
  | I8 -> Z.neg (Z.shift_left Z.one 7)

let min_value int_ty = Typed.int_z (min_value_z int_ty)

let max_value_z : Types.integer_type -> Z.t = function
  | U128 -> Z.pred (Z.shift_left Z.one 128)
  | U64 -> Z.pred (Z.shift_left Z.one 64)
  | U32 -> Z.pred (Z.shift_left Z.one 32)
  | U16 -> Z.pred (Z.shift_left Z.one 16)
  | U8 -> Z.pred (Z.shift_left Z.one 8)
  | Usize -> Z.pred (Z.shift_left Z.one (8 * Archi.word_size))
  | I128 -> Z.pred (Z.shift_left Z.one 127)
  | I64 -> Z.pred (Z.shift_left Z.one 63)
  | I32 -> Z.pred (Z.shift_left Z.one 31)
  | I16 -> Z.pred (Z.shift_left Z.one 15)
  | I8 -> Z.pred (Z.shift_left Z.one 7)
  | Isize -> Z.pred (Z.shift_left Z.one ((8 * Archi.word_size) - 1))

let max_value int_ty = Typed.nonzero_z (max_value_z int_ty)

let size_to_uint : int -> Types.ty = function
  | 1 -> TLiteral (TInteger U8)
  | 2 -> TLiteral (TInteger U16)
  | 4 -> TLiteral (TInteger U32)
  | 8 -> TLiteral (TInteger U64)
  | 16 -> TLiteral (TInteger U128)
  | _ -> failwith "Invalid integer size"

let lit_to_unsigned lit = size_to_uint @@ size_of_literal_ty lit

let int_constraints ty =
  let min = min_value ty in
  let max = max_value ty in
  fun x -> [ min <=@ x; x <=@ max ]

let constraints :
    Types.literal_type -> [< T.cval ] Typed.t -> T.sbool Typed.t list = function
  | TInteger ity -> (
      let constrs = int_constraints ity in
      fun x ->
        match Typed.cast_checked x Typed.t_int with
        | None -> [ Typed.v_false ]
        | Some x -> constrs x)
  | TBool -> (
      fun x ->
        match Typed.cast_checked x Typed.t_int with
        | None -> [ Typed.v_false ]
        (* Maybe worth checking which of these is better (if it matters at all)
          | Some x -> [ x ==@ 0s ||@ (x ==@ 1s) ]) *)
        | Some x -> [ 0s <=@ x; x <=@ 1s ])
  | TChar -> (
      (* A char is a ‘Unicode scalar value’, which is any ‘Unicode code point’ other than
       a surrogate code point. This has a fixed numerical definition: code points are in
       the range 0 to 0x10FFFF, inclusive. Surrogate code points, used by UTF-16, are in
       the range 0xD800 to 0xDFFF.
       https://doc.rust-lang.org/std/primitive.char.html *)
      let codepoint_min = Typed.zero in
      let codepoint_max = Typed.int 0x10FFFF in
      let surrogate_min = Typed.int 0xD800 in
      let surrogate_max = Typed.int 0xDFFF in
      fun x ->
        match Typed.cast_checked x Typed.t_int with
        | None -> [ Typed.v_false ]
        | Some x ->
            [
              codepoint_min <=@ x;
              x <=@ codepoint_max;
              Typed.not (surrogate_min <=@ x &&@ (x <=@ surrogate_max));
            ])
  | TFloat (F16 | F32 | F64 | F128) -> fun _ -> []

let nondet_literal_ty (ty : Types.literal_type) : T.cval Typed.t Rustsymex.t =
  let rty =
    match ty with
    | TInteger _ | TBool | TChar -> Typed.t_int
    | TFloat F16 -> Typed.t_f16
    | TFloat F32 -> Typed.t_f32
    | TFloat F64 -> Typed.t_f64
    | TFloat F128 -> Typed.t_f128
  in
  let constrs = constraints ty in
  Rustsymex.nondet ~constrs rty

(** [nondet ~extern ~init ty] returns a nondeterministic value for [ty], along
    with some "state". It receives a function [extern] to get an optional
    external function that computes the arbitrary value; it tries using it, and
    otherwise guesses the valid values. [init] is the initial "state", that is
    modified and returned by [extern]. *)
let rec nondet ty : 'a rust_val Rustsymex.t =
  let open Rustsymex.Syntax in
  match ty with
  | Types.TLiteral lit ->
      let+ cval = nondet_literal_ty lit in
      Base cval
  | TAdt (TTuple, { types; _ }) ->
      let+ fields = nondets types in
      Tuple fields
  | TAdt (TBuiltin TArray, { const_generics = [ len ]; types = [ ty ]; _ }) ->
      let size = Charon_util.int_of_const_generic len in
      let+ fields = nondets @@ List.init size (fun _ -> ty) in
      Array fields
  | TAdt (TAdtId t_id, _) -> (
      let type_decl = Crate.get_adt t_id in
      match type_decl.type_kind with
      | Enum variants -> (
          let disc_ty = (List.hd variants).discriminant.int_ty in
          let* disc_val = nondet_literal_ty (Values.TInteger disc_ty) in
          let* res =
            match_on variants ~constr:(fun (v : Types.variant) ->
                disc_val ==@ value_of_scalar v.discriminant)
          in
          match res with
          | None -> vanish ()
          | Some variant ->
              let discr = value_of_scalar variant.discriminant in
              let+ fields = nondets @@ Charon_util.field_tys variant.fields in
              Enum (discr, fields))
      | Struct fields ->
          let+ fields = nondets @@ Charon_util.field_tys fields in
          Struct fields
      | ty ->
          Rustsymex.not_impl
            (Fmt.str "nondet: unsupported type %a" Types.pp_type_decl_kind ty))
  | ty -> Rustsymex.not_impl (Fmt.str "nondet: unsupported type %a" pp_ty ty)

and nondets tys =
  let open Rustsymex.Syntax in
  Rustsymex.fold_list tys ~init:[] ~f:(fun fields ty ->
      let+ f = nondet ty in
      f :: fields)

let zeroed_lit : Types.literal_type -> T.cval Typed.t = function
  | TInteger _ | TBool | TChar -> 0s
  | TFloat F16 -> Typed.f16 0.0
  | TFloat F32 -> Typed.f32 0.0
  | TFloat F64 -> Typed.f64 0.0
  | TFloat F128 -> Typed.f128 0.0

let rec zeroed ~(null_ptr : 'a) : Types.ty -> 'a rust_val option =
  let zeroeds tys = Monad.OptionM.all (zeroed ~null_ptr) tys in
  function
  | TLiteral lit_ty -> ( try Some (Base (zeroed_lit lit_ty)) with _ -> None)
  | TRawPtr _ -> Some (Ptr (null_ptr, None))
  | TRef _ -> None
  | TAdt (TTuple, { types; _ }) ->
      zeroeds types |> Option.map (fun fields -> Tuple fields)
  | TAdt (TBuiltin TArray, { types = [ ty ]; const_generics = [ len ]; _ }) ->
      let len = int_of_const_generic len in
      zeroed ~null_ptr ty
      |> Option.map (fun v -> Array (List.init len (fun _ -> v)))
  | TAdt (TAdtId t_id, _) -> (
      let adt = Crate.get_adt t_id in
      match adt.type_kind with
      | Struct fields ->
          fields
          |> Charon_util.field_tys
          |> zeroeds
          |> Option.map (fun fields -> Struct fields)
      | Enum vars ->
          (vars
          |> List.find_opt (fun (v : Types.variant) ->
                 Z.equal Z.zero v.discriminant.value)
          |> Option.bind)
          @@ fun (v : Types.variant) ->
          v.fields
          |> Charon_util.field_tys
          |> zeroeds
          |> Option.map (fun fs -> Enum (value_of_scalar v.discriminant, fs))
      | Union fs ->
          let layouts =
            List.mapi
              (fun i (f : Types.field) -> (f.field_ty, i, layout_of f.field_ty))
              fs
          in
          let ty, i, _ =
            List.fold_left
              (fun ((_, _, accl) as acc) ((_, _, l) as cur) ->
                if l.size > accl.size then cur else acc)
              (List.hd layouts) (List.tl layouts)
          in
          let field = Types.FieldId.of_int i in
          zeroed ~null_ptr ty |> Option.map (fun v -> Union (field, v))
      | k ->
          Fmt.failwith "Unhandled zeroed ADT kind: %a" Types.pp_type_decl_kind k
      )
  | ty -> Fmt.failwith "Unhandled zeroed type: %a" pp_ty ty

let rec is_inhabited : Types.ty -> bool = function
  | TNever -> false
  | TAdt (TAdtId id, _) -> (
      let adt = Crate.get_adt id in
      match adt.type_kind with
      | Struct fs -> List.for_all is_inhabited @@ Charon_util.field_tys fs
      | Union fs -> List.exists is_inhabited @@ Charon_util.field_tys fs
      | Enum [] -> false
      | Enum vars ->
          List.exists
            (fun (v : Types.variant) ->
              List.for_all is_inhabited @@ Charon_util.field_tys v.fields)
            vars
      | _ -> true)
  | TAdt (TTuple, { types; _ }) -> List.for_all is_inhabited types
  | _ -> true

(** Returns the given type as it's unique representant if it's a ZST; otherwise
    [None].
    FIXME: @giltho: this plays awfuly with polymorphism. *)
let rec as_zst : Types.ty -> 'a rust_val option =
  let as_zsts tys = Monad.OptionM.all as_zst tys in
  function
  | TNever -> Some (Tuple [])
  | TAdt (TBuiltin TArray, { const_generics = [ len ]; _ })
    when int_of_const_generic len = 0 ->
      Some (Array [])
  | TAdt (TAdtId id, _) -> (
      let adt = Crate.get_adt id in
      match adt.type_kind with
      | Struct fs ->
          as_zsts @@ Charon_util.field_tys fs
          |> Option.map (fun fs -> Struct fs)
      | Union _ -> None
      | Enum [] -> Some (Enum (0s, []))
      | Enum [ { fields = []; discriminant; _ } ] ->
          Some (Enum (Typed.int_z discriminant.value, []))
      | Enum _ -> None
      | _ -> None)
  | TAdt (TTuple, { types; _ }) ->
      as_zsts types |> Option.map (fun fs -> Tuple fs)
  | TFnDef fn_binder ->
      let generics = fn_binder.binder_value.fun_generics in
      let id = fn_binder.binder_value.fun_id in
      Some (ConstFn { func = FunId (FRegular id); generics })
  | _ -> None

(** Apply the compiler-attribute to the given value *)
let apply_attribute v attr =
  let open Rustsymex.Syntax in
  match (v, attr) with
  | ( Base v,
      Meta.AttrUnknown
        { path = "rustc_layout_scalar_valid_range_start"; args = Some min } ) ->
      let min = int_of_string min in
      let* v = cast_checked ~ty:Typed.t_int v in
      if%sat v >=@ Typed.int min then Result.ok ()
      else Result.error (`StdErr "rustc_layout_scalar_valid_range_start")
  | ( Base v,
      AttrUnknown
        { path = "rustc_layout_scalar_valid_range_end"; args = Some max_s } ) ->
      let max = Z.of_string max_s in
      let* v = cast_checked ~ty:Typed.t_int v in
      if%sat v <=@ Typed.int_z max then Result.ok ()
      else Result.error (`StdErr "rustc_layout_scalar_valid_range_end")
  | _ -> Result.ok ()

let apply_attributes v attributes =
  Result.fold_list attributes ~f:(fun () -> apply_attribute v) ~init:()

let rec is_unsafe_cell : Types.ty -> bool = function
  | TAdt (TAdtId adt_id, _) -> (
      let adt = Crate.get_adt adt_id in
      if adt.item_meta.lang_item = Some "unsafe_cell" then true
      else
        match adt.type_kind with
        | Struct fs | Union fs -> List.exists is_unsafe_cell (field_tys fs)
        | Enum vs ->
            Iter.exists is_unsafe_cell
            @@ Iter.flat_map_l (fun (v : Types.variant) -> field_tys v.fields)
            @@ Iter.of_list vs
        | _ -> false)
  | TAdt (TBuiltin (TArray | TSlice), { types = [ ty ]; _ }) ->
      is_unsafe_cell ty
  | TAdt (TTuple, { types; _ }) -> List.exists is_unsafe_cell types
  | _ -> false

(** Traverses the given type and rust value, and returns all findable references
    with their type (ignores pointers). This is needed e.g. when needing to get
    the pointers along with the size of their pointee, in particular in nested
    cases. *)
let rec ref_tys_in (v : 'a rust_val) (ty : Types.ty) :
    ('a full_ptr * Types.ty) list =
  match (v, ty) with
  | Ptr ptr, (TAdt (TBuiltin TBox, _) | TRef _) -> [ (ptr, get_pointee ty) ]
  | Base _, _ -> []
  | Struct vs, TAdt (TAdtId adt_id, _) ->
      let fields = Crate.as_struct adt_id in
      List.concat_map2 ref_tys_in vs (field_tys fields)
  | Array vs, TAdt (TBuiltin (TArray | TSlice), { types = [ ty ]; _ }) ->
      List.concat_map (fun v -> ref_tys_in v ty) vs
  | Tuple vs, TAdt (TTuple, { types; _ }) ->
      List.concat_map2 ref_tys_in vs types
  | Enum (d, vs), TAdt (TAdtId adt_id, _) -> (
      match Typed.kind d with
      | Int d -> (
          let variants = Crate.as_enum adt_id in
          let v =
            List.find_opt
              (fun (v : Types.variant) -> Z.equal d v.discriminant.value)
              variants
          in
          match v with
          | Some v ->
              List.concat_map2 ref_tys_in vs (field_tys Types.(v.fields))
          | None -> [])
      | _ -> [])
  | Union (fid, v), TAdt (TAdtId adt_id, _) ->
      let fields = Crate.as_union adt_id in
      let field = Types.FieldId.nth fields fid in
      ref_tys_in v field.field_ty
  | _ -> []
