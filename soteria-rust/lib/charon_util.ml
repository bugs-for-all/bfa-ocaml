open Charon
open Typed

type meta = T.cval Typed.t option
and 'ptr full_ptr = 'ptr * meta [@@deriving show { with_path = false }]

type 'ptr rust_val =
  | Base of T.cval Typed.t
  | Ptr of 'ptr full_ptr
      (** pointer, parametric to enable Ruxt, with optional meta *)
  | Enum of T.cval Typed.t * 'ptr rust_val list  (** discriminant * values *)
  | Struct of 'ptr rust_val list  (** contains ordered fields *)
  | Tuple of 'ptr rust_val list
  | Array of 'ptr rust_val list
  | Union of Types.field_id * 'ptr rust_val  (** field and value of union *)
  | ConstFn of Expressions.fn_ptr

let rec pp_rust_val pp_ptr fmt =
  let pp_rust_val = pp_rust_val pp_ptr in
  function
  | Base v -> Fmt.pf fmt "%a" Typed.ppa v
  | Ptr (p, None) -> Fmt.pf fmt "Ptr(%a)" pp_ptr p
  | Ptr (p, Some meta) -> Fmt.pf fmt "Ptr(%a, %a)" pp_ptr p Typed.ppa meta
  | Enum (disc, vals) ->
      Fmt.pf fmt "Enum(%a: %a)" Typed.ppa disc
        (Fmt.list ~sep:(Fmt.any ", ") pp_rust_val)
        vals
  | Struct fields ->
      Fmt.pf fmt "{%a}" (Fmt.list ~sep:(Fmt.any ", ") pp_rust_val) fields
  | Tuple vals ->
      Fmt.pf fmt "(%a)" (Fmt.list ~sep:(Fmt.any ", ") pp_rust_val) vals
  | Array vals ->
      Fmt.pf fmt "[%a]" (Fmt.list ~sep:(Fmt.any ", ") pp_rust_val) vals
  | Union (field_id, v) ->
      Fmt.pf fmt "Union(%a: %a)" Types.pp_field_id field_id pp_rust_val v
  | ConstFn fn_ptr -> Fmt.pf fmt "FnPtr(%a)" Expressions.pp_fn_ptr fn_ptr

let ppa_rust_val ft rv = pp_rust_val (Fmt.any "?") ft rv
let unit_ = Tuple []

let value_of_scalar : Values.scalar_value -> T.cval Typed.t = function
  | { value = v; _ } -> int_z v

let type_of_operand : Expressions.operand -> Types.ty = function
  | Constant c -> c.ty
  | Copy p | Move p -> p.ty

let lit_to_string = PrintValues.literal_type_to_string

let rec pp_ty fmt : Types.ty -> unit = function
  | TAdt (TAdtId id, _) ->
      let adt = Crate.get_adt id in
      Fmt.pf fmt "%a" Crate.pp_name adt.item_meta.name
  | TAdt (TTuple, { types = tys; _ }) ->
      Fmt.pf fmt "(%a)" (Fmt.list ~sep:(Fmt.any ", ") pp_ty) tys
  | TAdt (TBuiltin TBox, { types = [ ty ]; _ }) -> Fmt.pf fmt "Box<%a>" pp_ty ty
  | TAdt
      ( TBuiltin TArray,
        { types = [ ty ]; const_generics = [ CgValue (VScalar len) ]; _ } ) ->
      Fmt.pf fmt "[%a; %a]" pp_ty ty Z.pp_print len.value
  | TAdt (TBuiltin TSlice, { types = [ ty ]; _ }) -> Fmt.pf fmt "[%a]" pp_ty ty
  | TAdt (TBuiltin TStr, _) -> Fmt.string fmt "str"
  | TLiteral lit -> Fmt.string fmt @@ PrintValues.literal_type_to_string lit
  | TNever -> Fmt.string fmt "!"
  | TRef (_, ty, RMut) -> Fmt.pf fmt "&mut %a" pp_ty ty
  | TRef (_, ty, RShared) -> Fmt.pf fmt "&%a" pp_ty ty
  | TRawPtr (ty, RMut) -> Fmt.pf fmt "*mut %a" pp_ty ty
  | TRawPtr (ty, RShared) -> Fmt.pf fmt "*const %a" pp_ty ty
  | TArrow { binder_value = ins, out; _ } ->
      Fmt.pf fmt "fn (%a) -> %a" Fmt.(list ~sep:(any ", ") pp_ty) ins pp_ty out
  | ty -> Fmt.pf fmt "%a" Types.pp_ty ty

let as_ptr = function
  | Ptr ptr -> ptr
  | v ->
      Fmt.failwith "Unexpected rust_val kind, expected a pointer, got: %a"
        ppa_rust_val v

let as_base = function
  | Enum (v, []) | Base v -> v
  | v ->
      Fmt.failwith "Unexpected rust_val kind, expected a base value got: %a"
        ppa_rust_val v

let as_base_of ~ty = function
  | Enum (v, []) | Base v -> (
      match Typed.cast_checked v ty with
      | Some v -> v
      | None ->
          Fmt.failwith "Unexpected rust_val type, expected %a, got %a (%a)"
            Typed.ppa_ty ty Typed.ppa v Svalue.pp_ty (Typed.get_ty v))
  | v ->
      Fmt.failwith "Unexpected rust_val kind, expected a base value got: %a"
        ppa_rust_val v

let int_of_const_generic : Types.const_generic -> int = function
  | CgValue (VScalar v) -> Z.to_int v.value
  | cg ->
      Fmt.failwith "int_of_const_generic: unhandled const: %a"
        Types.pp_const_generic cg

let field_tys = List.map (fun (f : Types.field) -> f.field_ty)

let empty_span : Meta.span =
  {
    span =
      {
        beg_loc = { line = 0; col = 0 };
        end_loc = { line = 0; col = 0 };
        file = { name = Virtual ""; contents = None };
      };
    generated_from_span = None;
  }

let fields_of_tys : Types.ty list -> Types.field list =
  List.map (fun field_ty : Types.field ->
      {
        span = empty_span;
        attr_info =
          { attributes = []; inline = None; rename = None; public = true };
        field_name = None;
        field_ty;
      })

let mk_array_ty ty len : Types.ty =
  TAdt
    ( TBuiltin TArray,
      {
        types = [ ty ];
        const_generics =
          [ CgValue (VScalar { value = Z.of_int len; int_ty = Isize }) ];
        regions = [];
        trait_refs = [];
      } )

let decl_has_attr (decl : 'a GAst.gfun_decl) attr =
  List.exists
    (function Meta.AttrUnknown { path; _ } -> path = attr | _ -> false)
    decl.item_meta.attr_info.attributes

let decl_get_attr (decl : 'a GAst.gfun_decl) attr =
  List.find_map
    (function
      | Meta.AttrUnknown { path; args } when path = attr -> args | _ -> None)
    decl.item_meta.attr_info.attributes

let get_pointee : Types.ty -> Types.ty = function
  | TRef (_, ty, _)
  | TRawPtr (ty, _)
  | TAdt (TBuiltin TBox, { types = [ ty ]; _ }) ->
      ty
  | _ -> failwith "Non-pointer type given to get_pointee"

let float_precision : Values.float_type -> Svalue.FloatPrecision.t = function
  | F16 -> F16
  | F32 -> F32
  | F64 -> F64
  | F128 -> F128

let pp_span ft ({ span = { file; beg_loc; end_loc }; _ } : Meta.span) =
  let clean_filename name =
    let parts = String.split_on_char '/' name in
    if List.compare_length_with parts 3 <= 0 then name
    else
      let last_3 = List.rev (List.take 3 (List.rev parts)) in
      "../" ^ String.concat "/" last_3
  in
  let pp_filename ft ({ name; _ } : Meta.file) =
    match name with
    | Local name -> Fmt.string ft (clean_filename name)
    | Virtual name -> Fmt.pf ft "%s (virtual)" (clean_filename name)
  in
  if beg_loc.line = end_loc.line then
    Fmt.pf ft "%a:%d:%d-%d" pp_filename file beg_loc.line beg_loc.col
      end_loc.col
  else
    Fmt.pf ft "%a:%d:%d-%d:%d" pp_filename file beg_loc.line beg_loc.col
      end_loc.line end_loc.col
