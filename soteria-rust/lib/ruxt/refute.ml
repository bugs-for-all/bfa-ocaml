module Wpst_interp = Interp.Make (State)
module Compo_res = Soteria_symex.Compo_res
open Charon

let try_refute summ_ctx summs (fundef : UllbcAst.fun_decl) =
  (* Construct precondition state and symbolically execute function *)
  let process =
    let open Rustsymex.Syntax in
    let empty_pre = Rustsymex.return ([], [], State.empty) in
    let extend_pre pre (summ : Summary.t) =
      let* vars, args, state = pre in
      let* arg_vars, arg = summ.ret in
      let+ state = State.produce summ.state state in
      (arg_vars @ vars, arg :: args, state)
    in
    let* vars, args, state = List.fold_left extend_pre empty_pre summs in
    let++ rv, state =
      (* PEDRO: Leaks are being ignored for now due to handling of globals in state.ml *)
      Wpst_interp.exec_fun ~ignore_leaks:true ~args ~state fundef
    in
    (vars, rv, state)
  in
  (* For each successful outcome, the summary context will be updated. *)
  let extend_ctx res_ctx outcome =
    let ( let** ) = Result.bind in
    let** ctx = res_ctx in
    match outcome with
    (* Successful termination: update the summary context *)
    | Compo_res.Ok (vars, rv, state), pcs ->
        let ty = fundef.signature.output in
        let state = State.serialize state in
        let ret = Summary.subst_ret vars rv pcs in
        let summ = Summary.{ ret; state } in
        Result.ok @@ Summary.ctx_update ty summ ctx
    (* Unsuccessful termination: found a type unsoundness *)
    | _ -> Result.error `TypeUnsound
  in
  Rustsymex.run process |> List.fold_left extend_ctx (Result.ok summ_ctx)

let find_unsoundness ?(fuel = 5) (crate : UllbcAst.crate) =
  (* Filter out all unsafe functions *)
  let safe_decls =
    let is_safe _ (fundef : UllbcAst.fun_decl) =
      not fundef.signature.is_unsafe
    in
    Types.FunDeclId.Map.filter is_safe crate.fun_decls
  in
  (* The try_refute procedure is called for each function *)
  let try_refute _ (entry_point : UllbcAst.fun_decl) acc =
    let ( let** ) = Result.bind in
    let** summ_ctx = acc in
    let f summs ctx = try_refute ctx summs entry_point in
    let tys = entry_point.signature.inputs in
    Summary.ctx_update_res f tys summ_ctx
  in
  (* The meta-loop iterates over all safe functions *)
  let rec meta_loop summ_ctx fuel =
    if fuel <= 0 then false (* Fuel exhausted *)
    else
      let res =
        (* Call try_refute on all functions and accumulate the results *)
        try Types.FunDeclId.Map.fold try_refute safe_decls (Result.ok summ_ctx)
        with exn ->
          let msg =
            Fmt.str "Exn: %a@\nTrace: %s" Fmt.exn exn
              (Printexc.get_backtrace ())
          in
          raise (Driver.ExecutionError msg)
      in
      match res with
      | Ok summ_ctx -> meta_loop summ_ctx (fuel - 1)
      | Error _ -> true (* Type unsoundness found *)
  in
  (* Run the algorithm starting from an empty summary context *)
  meta_loop Summary.empty_ctx fuel

let find_unsoundness_and_print log_level solver_config no_compile clean
    file_name =
  let open Driver in
  Solver_config.set solver_config;
  Soteria_logs.Config.check_set_and_lock log_level;
  Cleaner.init ~clean ();
  try
    let plugin =
      Plugin.merge_ifs
        [ (true, Plugin.default); (true, Plugin.kani); (true, Plugin.miri) ]
    in
    let crate = parse_ullbc_of_file ~no_compile ~plugin file_name in
    let open Syntaxes.FunctionWrap in
    let@ () = Crate.with_crate crate in
    let ret, msg =
      if find_unsoundness crate then (1, "Found type unsoundness!\n")
      else (0, "No type unsoundness found!\n")
    in
    Fmt.pr "%s" msg;
    exit ret
  with
  | ExecutionError e ->
      Fmt.pr "Fatal: %s" e;
      exit 2
  | CharonError e ->
      Fmt.pr "Fatal (Charon): %s" e;
      exit 3
