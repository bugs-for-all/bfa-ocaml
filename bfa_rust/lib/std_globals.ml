open Charon
module NameMatcherMap = Charon.NameMatcher.NameMatcherMap
open Charon_util
module T = Typed.T

let global_map =
  let open Values in
  [
    ("core::num::{usize}::MIN", Base (Layout.min_value Usize));
    ("core::num::{usize}::MAX", Base (Layout.max_value Usize));
    ("core::num::{u8}::MIN", Base (Layout.min_value U8));
    ("core::num::{u8}::MAX", Base (Layout.max_value U8));
    ("core::num::{u16}::MIN", Base (Layout.min_value U16));
    ("core::num::{u16}::MAX", Base (Layout.max_value U16));
    ("core::num::{u32}::MIN", Base (Layout.min_value U32));
    ("core::num::{u32}::MAX", Base (Layout.max_value U32));
    ("core::num::{u64}::MIN", Base (Layout.min_value U64));
    ("core::num::{u64}::MAX", Base (Layout.max_value U64));
    ("core::num::{u128}::MIN", Base (Layout.min_value U128));
    ("core::num::{u128}::MAX", Base (Layout.max_value U128));
    ("core::num::{isize}::MIN", Base (Layout.min_value Isize));
    ("core::num::{isize}::MAX", Base (Layout.max_value Isize));
    ("core::num::{i8}::MIN", Base (Layout.min_value I8));
    ("core::num::{i8}::MAX", Base (Layout.max_value I8));
    ("core::num::{i16}::MIN", Base (Layout.min_value I16));
    ("core::num::{i16}::MAX", Base (Layout.max_value I16));
    ("core::num::{i32}::MIN", Base (Layout.min_value I32));
    ("core::num::{i32}::MAX", Base (Layout.max_value I32));
    ("core::num::{i64}::MIN", Base (Layout.min_value I64));
    ("core::num::{i64}::MAX", Base (Layout.max_value I64));
    ("core::num::{i128}::MIN", Base (Layout.min_value I128));
    ("core::num::{i128}::MAX", Base (Layout.max_value I128));
  ]
  |> List.map (fun (p, v) -> (NameMatcher.parse_pattern p, v))
  |> NameMatcherMap.of_list

let match_config =
  NameMatcher.{ map_vars_to_vars = false; match_with_trait_decl_refs = false }

let global_eval ~crate (g : UllbcAst.global_decl) =
  let ctx = NameMatcher.ctx_from_crate crate in
  NameMatcherMap.find_opt ctx match_config g.item_meta.name global_map
