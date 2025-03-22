open Charon
open Typed

type rust_val =
  | Base of T.cval Typed.t
  | Ptr of T.sptr Typed.t
  | FatPtr of T.sptr Typed.t * T.cval Typed.t  (** ptr * metadata *)
  | Enum of T.cval Typed.t * rust_val list  (** discriminant * values *)
  | Struct of rust_val list  (** contains ordered fields *)
  | Tuple of rust_val list
  | Array of rust_val list
[@@deriving show { with_path = false }]

let unit_ = Tuple []

let value_of_scalar : Values.scalar_value -> T.cval Typed.t = function
  | { value = v; _ } -> int_z v

let type_of_operand : Expressions.operand -> Types.ty = function
  | Constant c -> c.ty
  | Copy p | Move p -> p.ty

let lit_to_string : Values.literal_type -> string = function
  | TInteger Isize -> "isize"
  | TInteger I8 -> "i8"
  | TInteger I16 -> "i16"
  | TInteger I32 -> "i32"
  | TInteger I64 -> "i64"
  | TInteger I128 -> "i128"
  | TInteger Usize -> "usize"
  | TInteger U8 -> "u8"
  | TInteger U16 -> "u16"
  | TInteger U32 -> "u32"
  | TInteger U64 -> "u64"
  | TInteger U128 -> "u128"
  | TFloat F16 -> "f16"
  | TFloat F32 -> "f32"
  | TFloat F64 -> "f64"
  | TFloat F128 -> "f128"
  | TChar -> "char"
  | TBool -> "bool"

let as_ptr = function
  | Ptr ptr -> ptr
  | FatPtr (ptr, _) -> ptr
  | v ->
      Fmt.failwith "Unexpected rust_val kind, expected a pointer, got: %a"
        pp_rust_val v

let as_base_of ~ty = function
  | Base v -> (
      match Typed.cast_checked v ty with
      | Some v -> v
      | None ->
          Fmt.failwith "Unexpected rust_val type, expected %a, got %a"
            Typed.ppa_ty ty Typed.ppa v)
  | v ->
      Fmt.failwith "Unexpected rust_val kind, expected a base value got: %a"
        pp_rust_val v

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
