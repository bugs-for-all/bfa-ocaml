open Charon
open Charon_util
open Generated_Types
module Sptr = State.Sptr
module TyMap = Types.TypeDeclId.Map
open Rustsymex.Syntax

type var = Typed.T.cval Typed.t * literal_type

type t = {
  ret : (var list * Sptr.t rust_val) Rustsymex.t;
  state : State.serialized;
}

type ctx = t list TyMap.t

let empty_ctx = TyMap.empty

let ctx_update ty summ ctx =
  match ty with
  | TAdt (TAdtId id, _) ->
      let opt_cons = function
        | None -> Some [ summ ]
        (* PEDRO: Check if summ is already in summs *)
        | Some summs -> Some (summ :: summs)
      in
      TyMap.update id opt_cons ctx
  | _ -> ctx

let ctx_get ty ctx =
  match ty with
  | TAdt (TAdtId id, _) -> (
      match TyMap.find_opt id ctx with None -> [] | Some summs -> summs)
  | TLiteral lit ->
      let ret =
        let+ cval = Layout.nondet_literal_ty lit in
        ([ (cval, lit) ], Base cval)
      in
      [ { ret; state = State.serialize State.empty } ]
  | _ -> []

let ctx_update_res f tys ctx =
  let rec aux summs ctx = function
    | [] -> Result.bind ctx (f @@ List.rev summs)
    | hd :: tl ->
        List.fold_left (fun acc summ -> aux (summ :: summs) acc tl) ctx hd
  in
  aux [] (Result.ok ctx) (List.map (fun ty -> ctx_get ty ctx) tys)

let subst prev next =
  let as_var v =
    match Typed.kind v with Var x -> x | _ -> failwith "Expected a variable"
  in
  let subst_var var =
    if Svalue.Var.equal var (as_var prev) then as_var next else var
  in
  Typed.subst subst_var

let rec subst_val prev next rv =
  let subst_val = subst_val prev next in
  let subst_vals vs = List.map subst_val vs in
  match rv with
  | Base v -> Base (subst prev next v)
  | Ptr _ -> rv
  | Enum (disc, vals) -> Enum (disc, subst_vals vals)
  | Struct fields -> Struct (subst_vals fields)
  | Tuple vals -> Tuple (subst_vals vals)
  | Array vals -> Array (subst_vals vals)
  | Union (field_id, v) -> Union (field_id, subst_val v)

let subst_ret vars rv pcs =
  let sym_ret = Rustsymex.return ([], rv, pcs) in
  let update_ret sym_ret (prev, lit) =
    let* vars, ret, pcs = sym_ret in
    let+ next = Layout.nondet_literal_ty lit in
    let ret = subst_val prev next ret in
    let pcs = List.map (fun pc -> subst prev next pc) pcs in
    ((next, lit) :: vars, ret, pcs)
  in
  let* vars, ret, pcs = List.fold_left update_ret sym_ret vars in
  let+ () = Rustsymex.assume pcs in
  (vars, ret)
