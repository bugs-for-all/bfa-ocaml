(* TODO: handle qualifiers! *)
let get_param_tys ~prog fid =
  List.find_map
    (fun (id, (_, _, decl)) ->
      if Cerb_frontend.Symbol.equal_sym id fid then
        match decl with
        | Cerb_frontend.AilSyntax.Decl_function
            (_proto, _ret_qual, ptys, _is_variadic, _is_inline, _is_noreturn) ->
            Some (List.map (fun (_, ty, _) -> ty) ptys)
        | _ -> None
      else None)
    prog.Cerb_frontend.AilSyntax.declarations

let find_fun ~prog to_find =
  List.find_map
    (fun ((id, _) as decl) ->
      let name = Cerb_frontend.Pp_symbol.to_string id in
      if String.starts_with ~prefix:to_find name then Some decl else None)
    prog.Cerb_frontend.AilSyntax.function_definitions
