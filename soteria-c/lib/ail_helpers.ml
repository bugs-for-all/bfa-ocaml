type _ Effect.t += Get_prog : Ail_tys.linked_program Effect.t

let get_prog () = Effect.perform Get_prog

let run_with_prog (prog : Ail_tys.linked_program) f =
  try f () with effect Get_prog, k -> Effect.Deep.continue k prog

let sym_is_id sym id =
  let open Cerb_frontend.Symbol in
  match sym with Symbol (_digest, _i, SD_Id id') -> id = id' | _ -> false

let pp_sym_hum ft sym =
  match sym with
  | Cerb_frontend.Symbol.Symbol (_digest, _i, SD_Id id) -> Fmt.string ft id
  | _ -> Symbol_std.pp ft sym

let rec resolve_sym ?(prog = get_prog ()) sym =
  match Pmap.lookup sym prog.symmap with
  | None -> sym
  | Some sym' ->
      if Symbol_std.equal sym sym' then sym else resolve_sym ~prog sym'

(* TODO: handle qualifiers! *)
let get_param_tys fid =
  let fid = resolve_sym fid in
  List.find_map
    (fun (id, (_, _, decl)) ->
      if Cerb_frontend.Symbol.equal_sym id fid then
        match decl with
        | Cerb_frontend.AilSyntax.Decl_function
            (_proto, _ret_qual, ptys, _is_variadic, _is_inline, _is_noreturn) ->
            Some (List.map (fun (_, ty, _) -> ty) ptys)
        | _ -> None
      else None)
    (get_prog ()).sigma.declarations

let find_fun_name ~(prog : Ail_tys.linked_program) to_find =
  List.find_map
    (fun ((sym, _) as decl) ->
      if sym_is_id sym to_find then Some decl else None)
    prog.sigma.function_definitions

let find_fun_loc to_find =
  List.find_map
    (fun (id, (loc, _, _, _, _)) ->
      if Cerb_frontend.Symbol.equal_sym id to_find then Some loc else None)
    (get_prog ()).sigma.function_definitions

let find_fun_def (to_find : Cerb_frontend.Symbol.sym) =
  let prog = get_prog () in
  let to_find = resolve_sym ~prog to_find in
  List.find_opt
    (fun (id, _) -> Cerb_frontend.Symbol.equal_sym id to_find)
    prog.sigma.function_definitions

let find_obj_def ~(prog : Ail_tys.linked_program)
    (to_find : Cerb_frontend.Symbol.sym) =
  let to_find = resolve_sym ~prog to_find in
  List.find_map
    (fun (id, e) ->
      if Cerb_frontend.Symbol.equal_sym id to_find then Some e else None)
    prog.sigma.object_definitions

let find_obj_decl ~(prog : Ail_tys.linked_program)
    (to_find : Cerb_frontend.Symbol.sym) =
  let to_find = resolve_sym ~prog to_find in
  List.find_map
    (fun (id, (_, _, decl)) ->
      if Cerb_frontend.Symbol.equal_sym id to_find then
        match decl with
        | Cerb_frontend.AilSyntax.Decl_object (sto, al, quals, ty) ->
            Some (sto, al, quals, ty)
        | _ -> None
      else None)
    prog.sigma.declarations

(** Returns true if the expression is guaranteed to be side-effect free. *)
let rec sure_side_effect_free (e : Ail_tys.expr) =
  let (AnnotatedExpression (_, _, _, e)) = e in
  match e with
  | AilEunary (_, e) -> sure_side_effect_free e
  | AilEbinary (e1, _, e2) ->
      sure_side_effect_free e1 && sure_side_effect_free e2
  | AilEcond (e1, e2opt, e3) ->
      sure_side_effect_free e1
      && Option.fold ~none:true ~some:sure_side_effect_free e2opt
      && sure_side_effect_free e3
  | AilEconst _ | AilEident _
  (* Loading from a stack variable is side-effect free, it supposedly cannot fail. *)
  | AilErvalue (AnnotatedExpression (_, _, _, AilEident _)) ->
      true
  | AilEcast (_, _, e) -> sure_side_effect_free e
  (* TODO: add more if relevant *)
  | _ -> false
