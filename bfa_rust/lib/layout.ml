module Utils_ = Utils
open Charon
open Typed
open Typed.Infix
open Typed.Syntax
open Rustsymex
open Charon_util

exception CantComputeLayout of string * Types.ty

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
  let current_crate : UllbcAst.crate ref =
    ref
      UllbcAst.
        {
          name = "";
          options =
            {
              ullbc = true;
              lib = false;
              bin = None;
              mir_promoted = false;
              mir_optimized = false;
              crate_name = None;
              input_file = None;
              read_llbc = None;
              dest_dir = None;
              dest_file = None;
              use_polonius = false;
              no_code_duplication = false;
              extract_opaque_bodies = false;
              translate_all_methods = false;
              included = [];
              opaque = [];
              exclude = [];
              remove_associated_types = [];
              hide_marker_traits = false;
              no_cargo = false;
              rustc_args = [];
              cargo_args = [];
              monomorphize = true;
              skip_borrowck = true;
              abort_on_error = false;
              error_on_warnings = false;
              no_serialize = false;
              print_original_ullbc = false;
              print_ullbc = false;
              print_built_llbc = false;
              print_llbc = false;
              no_merge_goto_chains = false;
              only_cargo = false;
            };
          declarations = [];
          type_decls = Types.TypeDeclId.Map.empty;
          fun_decls = FunDeclId.Map.empty;
          global_decls = GlobalDeclId.Map.empty;
          trait_decls = TraitDeclId.Map.empty;
          trait_impls = TraitImplId.Map.empty;
        }

  (** Cache of (type or variant) -> layout *)
  let layout_cache : ((Types.ty, Types.variant) Either.t, layout) Hashtbl.t =
    Hashtbl.create 128

  let set_crate c = current_crate := c
  let get_crate () = !current_crate

  let get_adt adt_id =
    let crate = get_crate () in
    Std_types.get_adt ~crate adt_id

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

  let pp_name ft name =
    let ctx = PrintUllbcAst.Crate.crate_to_fmt_env (get_crate ()) in
    let str = PrintTypes.name_to_string ctx name in
    Fmt.pf ft "%s" str
end

let is_int : Types.ty -> bool = function
  | TLiteral (TInteger _) -> true
  | _ -> false

let to_zeros : Types.literal_type -> T.cval Typed.t = function
  | TInteger _ -> 0s
  | t ->
      Fmt.failwith "to_zeros: unsupported literal type %a" Types.pp_literal_type
        t

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

(** If a pointer/reference to the given type requires a fat pointer *)
let is_fat_ptr : Types.ty -> bool = function
  | TAdt (TBuiltin TSlice, _) | TAdt (TBuiltin TStr, _) -> true
  | _ -> false

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
    when is_fat_ptr sub_ty ->
      {
        size = Archi.word_size * 2;
        align = Archi.word_size;
        members_ofs = [||];
      }
  | TAdt (TBuiltin TSlice, _) ->
      (* Slices should be hidden behind references *)
      raise (CantComputeLayout ("Raw slice", ty))
  (* Refs, pointers, boxes *)
  | TAdt (TBuiltin TBox, _) | TRef (_, _, _) | TRawPtr (_, _) ->
      { size = Archi.word_size; align = Archi.word_size; members_ofs = [||] }
  (* Tuples *)
  | TAdt (TTuple, { types = []; _ }) ->
      (* unit () *)
      (* TODO: this actually has size 0, which makes things... awkward *)
      { size = 1; align = 1; members_ofs = [||] }
  | TAdt (TTuple, { types; _ }) -> layout_of_members types
  (* Custom ADTs (struct, enum, etc.) *)
  | TAdt (TAdtId id, _) -> (
      let adt = Session.get_adt id in
      match adt.kind with
      | Struct fields -> layout_of_members @@ field_tys fields
      | Enum [] -> { size = 0; align = 1; members_ofs = [||] }
      | Enum variants ->
          let layouts = List.map of_variant variants in
          List.fold_left
            (fun acc l -> if l.size > acc.size then l else acc)
            (List.hd layouts) (List.tl layouts)
      | Opaque ->
          let msg = Fmt.str "Opaque %a " Session.pp_name adt.item_meta.name in
          raise (CantComputeLayout (msg, ty))
      | TError _ -> raise (CantComputeLayout ("Error", ty))
      | Alias _ -> raise (CantComputeLayout ("Alias", ty))
      | Union _ -> raise (CantComputeLayout ("Union", ty)))
  (* Arrays *)
  | TAdt (TBuiltin TArray, generics) ->
      let ty, size =
        match generics with
        | { types = [ ty ]; const_generics = [ size ]; _ } -> (ty, size)
        | _ -> failwith "Unexpected TArray generics"
      in
      let size = Charon_util.int_of_const_generic size in
      let sub_layout = layout_of ty in
      let members_ofs = Array.init size (fun i -> i * sub_layout.size) in
      { size = size * sub_layout.size; align = sub_layout.align; members_ofs }
  (* Never -- zero sized type *)
  | TNever -> { size = 0; align = 0; members_ofs = [||] }
  (* Others (unhandled for now) *)
  | TAdt (TBuiltin TStr, _) -> raise (CantComputeLayout ("String", ty))
  | TVar _ -> raise (CantComputeLayout ("De Bruijn variable", ty))
  | TTraitType (_, _) -> raise (CantComputeLayout ("Trait type", ty))
  | TDynTrait _ -> raise (CantComputeLayout ("dyn trait", ty))
  | TArrow _ -> raise (CantComputeLayout ("Arrow", ty))

and layout_of_members members =
  let ( % ) = Stdlib.( mod ) in
  let rec aux members_ofs (layout : layout) = function
    | [] -> (List.rev members_ofs, layout)
    | ty :: rest ->
        let { size = curr_size; align = curr_align; _ } = layout in
        let { size; align; _ } = layout_of ty in
        let mem_ofs = curr_size + ((align - (curr_size % align)) % align) in
        let new_size = mem_ofs + size in
        let new_align = Int.max align curr_align in
        aux (mem_ofs :: members_ofs)
          { size = new_size; align = new_align; members_ofs = [||] }
          rest
  in
  let members_ofs, { size; align; members_ofs = _ } =
    aux [] { size = 0; align = 1; members_ofs = [||] } members
  in
  {
    size = size + ((align - (size % align)) % align);
    align;
    members_ofs = Array.of_list members_ofs;
  }

and of_variant (variant : Types.variant) =
  Session.get_or_compute_cached_layout_var variant @@ fun () ->
  let discr_ty = Types.TLiteral (TInteger variant.discriminant.int_ty) in
  let members = discr_ty :: field_tys variant.fields in
  layout_of_members members

and of_enum_variant adt_id variant =
  let adt = Session.get_adt adt_id in
  let variants =
    match adt with { kind = Enum variants; _ } -> variants | _ -> assert false
  in
  let variant : Types.variant = Types.VariantId.nth variants variant in
  of_variant variant

and of_adt_id id =
  let adt = Session.get_adt id in
  match adt.kind with
  | Struct fields -> layout_of_members @@ field_tys fields
  | Enum _ ->
      Fmt.failwith
        "Enum cannot be used in Layout.of_adt_id, use Layout.of_enum_variant \
         instead"
  | k -> Fmt.failwith "Unhandled ADT in of_adt_id: %a" Types.pp_type_decl_kind k

let offset_in_array ty idx =
  let sub_layout = layout_of ty in
  idx * sub_layout.size

let size_of_s ty =
  try
    let { size; _ } = layout_of ty in
    return (Typed.int size)
  with CantComputeLayout (msg, ty) ->
    Fmt.kstr Rustsymex.not_impl "Cannot yet compute size of %s:\n%a" msg
      Types.pp_ty ty

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

let max_value int_ty = Typed.int_z (max_value_z int_ty)

let int_constraints ty =
  let min = min_value ty in
  let max = max_value ty in
  fun x -> [ min <=@ x; x <=@ max ]

let constraints :
    Types.literal_type -> (T.cval Typed.t -> T.sbool Typed.t list) option =
  function
  | TInteger ity ->
      let constrs = int_constraints ity in
      Some
        (fun x ->
          match Typed.cast_checked x Typed.t_int with
          | None -> [ Typed.v_false ]
          | Some x -> constrs x)
  | TBool ->
      Some
        (fun x ->
          match Typed.cast_checked x Typed.t_int with
          | None -> [ Typed.v_false ]
          (* Maybe worth checking which of these is better (if it matters at all)
          | Some x -> [ x ==@ 0s ||@ (x ==@ 1s) ]) *)
          | Some x -> [ 0s <=@ x; x <=@ 1s ])
  | TChar ->
      (* A char is a ‘Unicode scalar value’, which is any ‘Unicode code point’ other than
       a surrogate code point. This has a fixed numerical definition: code points are in
       the range 0 to 0x10FFFF, inclusive. Surrogate code points, used by UTF-16, are in
       the range 0xD800 to 0xDFFF.
       https://doc.rust-lang.org/std/primitive.char.html *)
      let codepoint_min = Typed.zero in
      let codepoint_max = Typed.int 0x10FFFF in
      let surrogate_min = Typed.int 0xD800 in
      let surrogate_max = Typed.int 0xDFFF in
      Some
        (fun x ->
          match Typed.cast_checked x Typed.t_int with
          | None -> [ Typed.v_false ]
          | Some x ->
              [
                codepoint_min <=@ x;
                x <=@ codepoint_max;
                Typed.not (surrogate_min <=@ x &&@ (x <=@ surrogate_max));
              ])
  | ty ->
      L.info (fun m ->
          m "No constraints implemented for type %a" Types.pp_literal_type ty);
      None

let nondet_literal_ty : Types.literal_type -> T.cval Typed.t Rustsymex.t =
  function
  | (TInteger _ | TBool | TChar) as ty ->
      let constrs = Option.get @@ constraints ty in
      Rustsymex.nondet ~constrs Typed.t_int
  | ty ->
      Rustsymex.not_impl
        (Fmt.str "nondet_literal_ty: unsupported type %a" Types.pp_literal_type
           ty)

let rec nondet : Types.ty -> 'a rust_val Rustsymex.t =
  let open Rustsymex.Syntax in
  function
  | TLiteral lit ->
      let+ cval = nondet_literal_ty lit in
      Base cval
  | TAdt (TAdtId t_id, _) -> (
      let type_decl = Session.get_adt t_id in
      match type_decl.kind with
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
              let+ fields =
                Rustsymex.fold_list variant.fields ~init:[] ~f:(fun fields ty ->
                    let+ f = nondet ty.field_ty in
                    f :: fields)
              in
              let fields = List.rev fields in
              Enum (discr, fields))
      | Struct fields ->
          let+ fields =
            Rustsymex.fold_list fields ~init:[] ~f:(fun fields ty ->
                let+ f = nondet ty.field_ty in
                f :: fields)
          in
          Struct fields
      | ty ->
          Rustsymex.not_impl
            (Fmt.str "nondet: unsupported type %a" Types.pp_type_decl_kind ty))
  | TAdt (TTuple, { types; _ }) ->
      let+ fields =
        Rustsymex.fold_list types ~init:[] ~f:(fun fields ty ->
            let+ f = nondet ty in
            f :: fields)
      in
      Tuple fields
  | ty ->
      Rustsymex.not_impl (Fmt.str "nondet: unsupported type %a" Types.pp_ty ty)
