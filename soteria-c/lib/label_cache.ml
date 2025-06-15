open Cerb_frontend
open Cerb_frontend.AilSyntax

type stmt = GenTypes.genTypeCategory statement
type t = (Symbol.sym, (Symbol.sym * stmt) Dynarray.t) Hashtbl.t

module Sym_set = Symbol_std.Set

let rec free_syms_expr acc expr =
  let (AnnotatedExpression (_, _, _, expr)) = expr in
  match expr with
  | AilEident sym -> Sym_set.add sym acc
  | AilEva_start (e, id) -> free_syms_expr (Sym_set.add id acc) e
  | AilEva_arg (e, _)
  | AilEva_end e
  | AilEunary (_, e)
  | AilEcast (_, _, e)
  | AilEassert e
  | AilEcompound (_, _, e)
  | AilEannot (_, e)
  | AilEsizeof_expr e
  | AilEprint_type e
  | AilErvalue e
  | AilEarray_decay e
  | AilEfunction_decay e
  | AilEatomic e
  | AilEmemberof (e, _)
  | AilEmemberofptr (e, _)
  | AilEbmc_assume e ->
      free_syms_expr acc e
  | AilEbinary (e1, _, e2)
  | AilEassign (e1, e2)
  | AilEcompoundAssign (e1, _, e2)
  | AilEva_copy (e1, e2) ->
      free_syms_expr (free_syms_expr acc e1) e2
  | AilEcond (e1, e2_opt, e3) ->
      let acc = free_syms_expr (free_syms_expr acc e1) e3 in
      Option.fold ~none:acc ~some:(free_syms_expr acc) e2_opt
  | AilEcall (e1, es) -> List.fold_left free_syms_expr acc (e1 :: es)
  | AilEstruct (_ty_sym, members) ->
      List.fold_left
        (fun acc (_, e) -> Option.fold ~none:acc ~some:(free_syms_expr acc) e)
        acc members
  | AilEarray (_, _, eopts) ->
      List.fold_left
        (fun acc e_opt ->
          Option.fold ~none:acc ~some:(free_syms_expr acc) e_opt)
        acc eopts
  | AilEunion (_ty_sym, _field, e_opt) ->
      Option.fold ~none:acc ~some:(free_syms_expr acc) e_opt
  | AilEgeneric (e, assocs) ->
      let acc = free_syms_expr acc e in
      List.fold_left
        (fun acc assoc ->
          match assoc with
          | AilGAtype (_, e) | AilGAdefault e -> free_syms_expr acc e)
        acc assocs
  | AilEgcc_statement (bindings, stmts) ->
      let exclude = set_of_bindings bindings in
      let res = List.fold_left free_syms_stmt acc stmts in
      Sym_set.diff res exclude
  | AilEbuiltin _ | AilEstr _ | AilEconst _ | AilEsizeof _ | AilEalignof _
  | AilEreg_load _ | AilEoffsetof _ | AilEinvalid _ ->
      acc

and free_syms_stmt acc stmt =
  match stmt.node with
  | AilSexpr e | AilSreturn e | AilSreg_store (_, e) -> free_syms_expr acc e
  | AilSwhile (e, stmt, _) | AilSdo (stmt, e, _) | AilSswitch (e, stmt) ->
      free_syms_stmt (free_syms_expr acc e) stmt
  | AilScase (_, stmt)
  | AilScase_rangeGNU (_, _, stmt)
  | AilSdefault stmt
  | AilSmarker (_, stmt) (* CN stuff *)
  | AilSlabel (_, stmt, _) ->
      free_syms_stmt acc stmt
  | AilSif (e, s1, s2) ->
      let acc = free_syms_expr acc e in
      let acc = free_syms_stmt acc s1 in
      free_syms_stmt acc s2
  | AilSskip | AilSbreak | AilScontinue | AilSreturnVoid | AilSgoto _ -> acc
  | AilSdeclaration l ->
      List.fold_left
        (fun acc (_, e_opt) ->
          Option.fold ~none:acc ~some:(free_syms_expr acc) e_opt)
        acc l
  | AilSblock (bindings, stmtl) ->
      let exclude = set_of_bindings bindings in
      let res = List.fold_left free_syms_stmt acc stmtl in
      Sym_set.diff res exclude
  | AilSpar stmtl -> List.fold_left free_syms_stmt acc stmtl
