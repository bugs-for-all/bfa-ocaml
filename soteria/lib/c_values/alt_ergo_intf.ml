module L = Soteria_logs.Logs.L
module AE = AltErgoLib
module Expr = AE.Expr
module Sy = AE.Symbols
module AESolver_container = (val AE.Sat_solver.get_current ())
module AESolver = AESolver_container.Make (AE.Theory.Main_Default)
module F = AE.Frontend
module FS = AE.Frontend.Make (AESolver)
module D = Dolmen.Std
module DE = D.Expr

type t = FS.env

let dummy_loc = AE.Loc.dummy
(* let initialize_solver : (Simple_smt.solver -> unit) ref = ref (fun _ -> ()) *)

(* let register_solver_init f =
  let old = !initialize_solver in
  let f' solver =
    old solver;
    f solver
  in
  initialize_solver := f' *)

module Encoding = struct
  module Ptr = struct
    let record_constr = AE.Uid.of_string "mk-ptr"
    let ty_name = AE.Uid.of_string "Ptr"
    let loc_field = AE.Uid.of_string "loc"
    let ofs_field = AE.Uid.of_string "ofs"

    let access_field field expr =
      Expr.mk_term (Sy.Op (Sy.Access field)) [ expr ] Tint

    let get_loc = access_field loc_field
    let get_ofs = access_field ofs_field

    let ty =
      AE.Ty.trecord ~record_constr [] ty_name
        [ (loc_field, AE.Ty.Tint); (ofs_field, AE.Ty.Tint) ]
  end

  let encode_type (ty : Svalue.ty) : AE.Ty.t =
    let open AE.Ty in
    match ty with
    | TBool -> Tbool
    | TInt -> Tint
    | TLoc -> Tint
    | TFloat _ -> failwith "FIXME: float in alt-ergo"
    | TPointer -> Ptr.ty
    | TSeq _ -> failwith "FIXME: seq in alt-ergo"
    | TBitVector size -> Tbitv size

  let memo_encode_value_tbl : Expr.t Hashtbl.Hint.t = Hashtbl.Hint.create 1023

  let rec encode_value_memo (v : Svalue.t) : Expr.t =
    match Hashtbl.Hint.find_opt memo_encode_value_tbl v.Hashcons.tag with
    | Some k -> k
    | None ->
        let k = encode_value v in
        Hashtbl.Hint.add memo_encode_value_tbl v.Hashcons.tag k;
        k

  and encode_value (value : Svalue.t) : Expr.t =
    let open Expr in
    match value.node.kind with
    | Var v ->
        let id = Svalue.Var.to_string v in
        let sy = AE.Symbols.var (AE.Var.of_string id) in
        mk_term sy [] (encode_type value.node.ty)
    | Int z -> Ints.of_Z z
    | Bool b -> if b then vrai else faux
    | BitVec z ->
        let size = Svalue.size_of_bv value.node.ty in
        BV.of_Z ~size z
    | Float _ -> failwith "FIXME: float in alt-ergo"
    | Seq _ -> failwith "FIXME: seq in alt-ergo"
    | Ptr (l, o) ->
        let loc = encode_value_memo l in
        let ofs = encode_value_memo o in
        mk_record [ loc; ofs ] Ptr.ty
    | Ite (c, t, e) ->
        mk_ite (encode_value_memo c) (encode_value_memo t) (encode_value_memo e)
    | Unop (unop, v) -> (
        let v = encode_value_memo v in
        match unop with
        | Not -> Core.not v
        | GetPtrLoc -> Ptr.get_loc v
        | GetPtrOfs -> Ptr.get_ofs v
        | IntOfBool -> mk_ite v (Ints.of_int 1) (Ints.of_int 0)
        | BvOfInt -> BV.int2bv (Svalue.size_of_bv value.node.ty) v
        | IntOfBv _signed -> BV.int2bv (Svalue.size_of_bv value.node.ty) v
        | BvExtract (from_, to_) -> BV.extract from_ to_ v
        | FAbs | FIs _ | FRound _ | FloatOfBv | BvOfFloat _ ->
            failwith "FIXME: float in alt-ergo")
    | Binop (binop, v1, v2) -> (
        let v1 = encode_value_memo v1 in
        let v2 = encode_value_memo v2 in
        match binop with
        | Eq -> Core.eq v1 v2
        | Leq -> Ints.( <= ) v1 v2
        | Lt -> Ints.( < ) v1 v2
        | And -> Core.and_ v1 v2
        | Or -> Core.or_ v1 v2
        | Plus -> Ints.( + ) v1 v2
        | Minus -> Ints.( - ) v1 v2
        | Times -> Ints.( * ) v1 v2
        | Div -> Ints.( / ) v1 v2
        | Mod -> Ints.( mod ) v1 v2
        | Rem -> failwith "FIXME: rem in alt-ergo"
        | BitAnd -> BV.bvand v1 v2
        | BitOr -> BV.bvor v1 v2
        | BitXor -> BV.bvxor v1 v2
        | BitShl -> BV.bvshl v1 v2
        | BitShr -> BV.bvlshr v1 v2
        | BvPlus -> BV.bvadd v1 v2
        | BvMinus -> BV.bvsub v1 v2
        | Svalue.Binop.FEq | Svalue.Binop.FLeq | Svalue.Binop.FLt
        | Svalue.Binop.FPlus | Svalue.Binop.FMinus | Svalue.Binop.FTimes
        | Svalue.Binop.FDiv | Svalue.Binop.FRem ->
            failwith "FIXME: float in alt-ergo")
    | Nop (Distinct, vs) ->
        let vs = List.map encode_value_memo vs in
        (* No idea if this is correct... *)
        mk_distinct ~iff:true vs
end

let init () =
  let context = F.init_all_used_context () in
  FS.init_env context

let push env n = FS.push n env
let pop env n = FS.pop n env

let add_constraint env v =
  let expr = Encoding.encode_value v in
  FS.assume ("", expr, false) env

let check_sat (env : t) : Soteria_symex.Solver.result =
  match env.res with
  | `Unsat -> Unsat
  | `Sat -> Sat
  | `Unknown ->
      L.smt (fun m -> m "Unknown, model: %a" FS.print_model env.sat_env);
      Unknown

let declare_var (env : t) (v : Soteria_symex.Var.t) (ty : Svalue.ty) =
  let id = Soteria_symex.Var.to_string v |> AE.Hstring.make in
  let ty = Encoding.encode_type ty in
  let typed = (id, [], ty) in
  let cmd : AE.Commands.sat_tdecl =
    { st_loc = dummy_loc; st_decl = Decl typed }
  in
  FS.process_decl env cmd
