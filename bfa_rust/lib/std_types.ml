open Charon
module NameMatcherMap = Charon.NameMatcher.NameMatcherMap
module T = Typed.T

let struct_of tys = Types.Struct (Charon_util.fields_of_tys tys)
let mut_ref_of ty = Types.TRef (RErased, ty, RMut)
let mut_ptr_of ty = Types.TRawPtr (ty, RMut)
let gargs_of = TypesUtils.mk_generic_args_from_types

(* Actual types *)
let fmtarguments_ty = Types.Struct []

let string_ty =
  (* Slice of chars (ie. buffer + size), and the capacity -- really this should be a vec,
     but this is an equivalent and simpler representation (more or less) *)
  struct_of
    [
      mut_ref_of (TAdt (TBuiltin TSlice, gargs_of [ TLiteral TChar ]));
      TLiteral (TInteger Isize);
    ]

let iter_of ty = struct_of [ mut_ptr_of ty; mut_ptr_of ty ]

let std_type_map =
  [
    ("core::fmt::Arguments", fmtarguments_ty);
    ("alloc::string::String", string_ty);
    ("core::str::iter::Chars", iter_of (TLiteral TChar));
  ]
  |> List.map (fun (p, v) -> (NameMatcher.parse_pattern p, v))
  |> NameMatcherMap.of_list

let clean_name =
  List.filter @@ function Types.PeIdent ("<'_>", _) -> false | _ -> true

let match_config =
  NameMatcher.{ map_vars_to_vars = false; match_with_trait_decl_refs = false }

let type_eval ~(crate : UllbcAst.crate) (t : Types.type_decl) =
  let ctx = NameMatcher.ctx_from_crate crate in
  (* Unfortunately, some type names end with a region identifier,
     like "core::fmt::Arguments::<'_>". NameMatcher doesn't allow "<'_>" in the
     pattern, so we manually filter it out beforehand. *)
  let name = clean_name t.item_meta.name in
  NameMatcherMap.find_opt ctx match_config name std_type_map

let get_adt ~(crate : UllbcAst.crate) adt_id =
  let adt = Types.TypeDeclId.Map.find adt_id crate.type_decls in
  if adt.kind <> Opaque then adt
  else
    let opt_kind = type_eval ~crate adt in
    let kind = Option.value ~default:adt.kind opt_kind in
    { adt with kind }
