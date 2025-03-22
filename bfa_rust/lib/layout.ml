module Utils_ = Utils
open Charon
open Rustsymex.Syntax
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

type sint = Typed.T.sint Typed.t
type cval = Typed.T.cval Typed.t
type sbool = Typed.T.sbool Typed.t

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

let to_zeros : Types.literal_type -> cval = function
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
  let rec aux members_ofs (layout : layout) = function
    | [] -> (List.rev members_ofs, layout)
    | ty :: rest ->
        let { size = curr_size; align = curr_align; _ } = layout in
        let { size; align; _ } = layout_of ty in
        let mem_ofs = curr_size + ((align - (curr_size mod align)) mod align) in
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
    size = size + ((align - (size mod align)) mod align);
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

let constraints : Types.literal_type -> (cval -> sbool list) option = function
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

let nondet_literal_ty : Types.literal_type -> cval Rustsymex.t =
  let open Rustsymex.Syntax in
  function
  | (TInteger _ | TBool | TChar) as ty ->
      let constrs = Option.get @@ constraints ty in
      let+ res = Rustsymex.nondet ~constrs Typed.t_int in
      (res :> cval)
  | ty ->
      Rustsymex.not_impl
        (Fmt.str "nondet_literal_ty: unsupported type %a" Types.pp_literal_type
           ty)

let rec nondet : Types.ty -> rust_val Rustsymex.t =
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

type cval_info = {
  value : cval;
  ty : Types.literal_type;
  size : sint;
  offset : sint;
}

(** Converts a Rust value of the given type into a list of C values, along with
    their size and offset *)
let rec rust_to_cvals ?(offset = 0s) (v : rust_val) (ty : Types.ty) :
    cval_info list =
  let illegal_pair () =
    L.err (fun m ->
        m "Wrong pair of rust_value and Charon.ty: %a / %a" pp_rust_val v
          Types.pp_ty ty);
    failwith "Wrong pair of rust_value and Charon.ty"
  in
  let chain_cvals layout vals types =
    Utils_.List_ex.map2i
      (fun i value ty ->
        let offset = (Array.get layout.members_ofs i |> Typed.int) +@ offset in
        rust_to_cvals ~offset value ty)
      vals types
    |> List.flatten
  in

  match (v, ty) with
  (* Literals *)
  | Base value, TLiteral ty ->
      let size = Typed.int (size_of_literal_ty ty) in
      [ { value; ty; size; offset } ]
  | _, TLiteral _ -> illegal_pair ()
  (* References / Pointers *)
  | Ptr value, TAdt (TBuiltin TBox, _)
  | Ptr value, TRef _
  | Ptr value, TRawPtr _ ->
      let size = Typed.int Archi.word_size in
      [ { value :> cval; ty = TInteger Isize; size; offset } ]
  (* References / Pointers obtained from casting *)
  | Base value, TAdt (TBuiltin TBox, _)
  | Base value, TRef _
  | Base value, TRawPtr _ ->
      let size = Typed.int Archi.word_size in
      [ { value; ty = TInteger Isize; size; offset } ]
  (* Fat pointers *)
  | FatPtr (value, sl_size), TAdt (TBuiltin TBox, { types = [ sub_ty ]; _ })
  | FatPtr (value, sl_size), TRef (_, sub_ty, _)
  | FatPtr (value, sl_size), TRawPtr (sub_ty, _)
    when is_fat_ptr sub_ty ->
      let size = Typed.int Archi.word_size in
      let isize = Values.TInteger Isize in
      [
        { value :> cval; ty = isize; size; offset };
        { value = sl_size; ty = isize; size; offset = offset +@ size };
      ]
  | _, TAdt (TBuiltin TBox, _) | _, TRawPtr _ | _, TRef _ -> illegal_pair ()
  (* Tuples *)
  | Tuple vs, TAdt (TTuple, { types; _ }) ->
      if List.compare_lengths vs types <> 0 then
        Fmt.failwith "Mistmached rust_val / TTuple lengths: %d/%d"
          (List.length vs) (List.length types)
      else chain_cvals (layout_of ty) vs types
  | Tuple _, _ | _, TAdt (TTuple, _) -> illegal_pair ()
  (* Structs *)
  | Struct vals, TAdt (TAdtId t_id, _) ->
      let type_decl = Session.get_adt t_id in
      let fields =
        match type_decl.kind with
        | Struct fields -> field_tys fields
        | _ ->
            Fmt.failwith "Unexpected type declaration in struct value: %a"
              Types.pp_type_decl type_decl
      in
      chain_cvals (layout_of ty) vals fields
  | Struct _, _ -> illegal_pair ()
  (* Enums *)
  | Enum (disc, vals), TAdt (TAdtId t_id, _) ->
      let type_decl = Session.get_adt t_id in
      let disc_z =
        match Typed.kind disc with
        | Int d -> d
        | k ->
            Fmt.failwith "Unexpected enum discriminant kind: %a"
              Svalue.pp_t_kind k
      in
      let variant =
        match type_decl.kind with
        | Enum variants ->
            List.find
              (fun v -> Z.equal disc_z Types.(v.discriminant.value))
              variants
        | _ ->
            Fmt.failwith "Unexpected ADT type for enum: %a" Types.pp_type_decl
              type_decl
      in
      let disc_ty = Types.TLiteral (TInteger variant.discriminant.int_ty) in
      chain_cvals (of_variant variant) (Base disc :: vals)
        (disc_ty :: field_tys variant.fields)
  | Enum _, _ -> illegal_pair ()
  (* Arrays *)
  | Array vals, TAdt (TBuiltin TArray, { types = [ sub_ty ]; _ }) ->
      let layout = layout_of ty in
      let size = Array.length layout.members_ofs in
      if List.length vals <> size then failwith "Array length mismatch"
      else chain_cvals layout vals (List.init size (fun _ -> sub_ty))
  | Array _, _ | _, TAdt (TBuiltin TArray, _) -> illegal_pair ()
  (* Rest *)
  | _ ->
      L.err (fun m ->
          m "Unhandled rust_value and Charon.ty: %a / %a" pp_rust_val v
            Types.pp_ty ty);
      failwith "Unhandled rust_value and Charon.ty"

type aux_ret = (Types.literal_type * sint * sint) list * parse_callback
and parse_callback = cval list -> callback_return
and parser_return = [ `Done of rust_val | `More of aux_ret ]
and callback_return = parser_return Rustsymex.t

(** Converts a Rust type into a list of C blocks, along with their size and
    offset; once these are read, symbolically decides whether we must keep
    reading. *)
let rust_of_cvals ?offset ty : parser_return =
  (* Base case, parses all types. *)
  let rec aux offset : Types.ty -> parser_return = function
    | TLiteral ty ->
        `More
          ( [ (ty, Typed.int (size_of_literal_ty ty), offset) ],
            function
            | [ v ] -> return (`Done (Base v))
            | _ -> failwith "Expected one cval" )
    | TAdt (TBuiltin TBox, { types = [ sub_ty ]; _ })
    | TRef (_, sub_ty, _)
    | TRawPtr (sub_ty, _)
      when is_fat_ptr sub_ty ->
        let callback = function
          | [ ptr; len ] ->
              let* ptr =
                of_opt_not_impl ~msg:"Slice value 1 should be a pointer"
                  (Typed.cast_checked ptr Typed.t_ptr)
              in
              let+ len =
                of_opt_not_impl ~msg:"Slice value 2 should be an integer"
                  (Typed.cast_checked len Typed.t_int)
              in
              `Done (FatPtr (ptr, len))
          | _ -> failwith "Expected two cvals"
        in
        let ptr_size = Typed.int Archi.word_size in
        let isize = Values.TInteger Isize in
        `More
          ( [ (isize, ptr_size, offset); (isize, ptr_size, offset +@ ptr_size) ],
            callback )
    | TAdt (TBuiltin TBox, _) | TRef _ | TRawPtr _ ->
        `More
          ( [ (TInteger Isize, Typed.int Archi.word_size, offset) ],
            function
            | [ v ] -> (
                match Typed.cast_checked v Typed.t_ptr with
                | Some v_ptr -> return (`Done (Ptr v_ptr))
                (* This should only happen on weird pointer casting. *)
                | None -> return (`Done (Base v)))
            | _ -> failwith "Expected one cval" )
    | TAdt (TTuple, { types; _ }) as ty ->
        let layout = layout_of ty in
        aux_fields ~f:(fun fs -> Tuple fs) ~layout offset types
    | TAdt (TAdtId t_id, _) as ty -> (
        let type_decl = Session.get_adt t_id in
        match (type_decl : Types.type_decl) with
        | { kind = Enum variants; _ } -> aux_enum offset variants
        | { kind = Struct fields; _ } ->
            let layout = layout_of ty in
            fields
            |> field_tys
            |> aux_fields ~f:(fun fs -> Struct fs) ~layout offset
        | _ ->
            Fmt.failwith "Unhandled type declaration in rust_of_cvals: %a"
              Types.pp_type_decl type_decl)
    | TAdt (TBuiltin TArray, { types = [ sub_ty ]; _ }) as ty ->
        let layout = layout_of ty in
        let size = Array.length layout.members_ofs in
        let fields = List.init size (fun _ -> sub_ty) in
        aux_fields ~f:(fun fs -> Array fs) ~layout offset fields
    | ty -> Fmt.failwith "Unhandled Charon.ty: %a" Types.pp_ty ty
  (* Parses a list of fields (for structs and tuples) *)
  and aux_fields ~f ~layout offset fields : parser_return =
    let base_offset = offset +@ (offset %@ Typed.nonzero layout.align) in
    let rec mk_callback to_parse parsed : parser_return =
      match to_parse with
      | [] -> `Done (f (List.rev parsed))
      | ty :: rest -> (
          let idx = List.length parsed in
          let offset = Array.get layout.members_ofs idx |> Typed.int in
          let offset = base_offset +@ offset in
          match aux offset ty with
          | `More (blocks, callback) ->
              wrap_callback rest parsed blocks callback
          | `Done value -> mk_callback rest (value :: parsed))
    and wrap_callback to_parse parsed blocks callback =
      `More
        ( blocks,
          fun values ->
            let+ res = callback values in
            match res with
            | `More (blocks, callback) ->
                wrap_callback to_parse parsed blocks callback
            | `Done value -> mk_callback to_parse (value :: parsed) )
    in
    mk_callback fields []
  (* Parses what enum variant we're handling *)
  and aux_enum offset (variants : Types.variant list) : parser_return =
    let disc = (List.hd variants).discriminant in
    let disc_ty = Values.TInteger disc.int_ty in
    let disc_align = Typed.nonzero (align_of_literal_ty disc_ty) in
    let disc_size = Typed.int (size_of_literal_ty disc_ty) in
    let offset = offset +@ (offset %@ disc_align) in
    let callback cval : callback_return =
      let cval = List.hd cval in
      let* res =
        match_on variants ~constr:(fun (v : Types.variant) ->
            cval ==@ value_of_scalar v.discriminant)
      in
      match res with
      | Some var ->
          (* skip discriminant *)
          let discr = value_of_scalar var.discriminant in
          let ({ members_ofs = mems; _ } as layout) = of_variant var in
          let members_ofs = Array.sub mems 1 (Array.length mems - 1) in
          let layout = { layout with members_ofs } in
          let parser =
            var.fields
            |> field_tys
            |> aux_fields ~f:(fun fs -> Enum (discr, fs)) ~layout offset
          in
          return parser
      | None ->
          Fmt.kstr not_impl
            "Vanishing rust_of_cvals, as no variant matches variant id %a"
            Typed.ppa cval
    in
    `More ([ (TInteger disc.int_ty, disc_size, offset) ], callback)
  in
  let off = Option.value ~default:0s offset in
  aux off ty
