open Syntaxes.FunctionWrap
open Soteria_logs.Logs
open Soteria_terminal
module Bi_interp = Interp.Make (Bi_state)
open Ail_tys

module Summaries = struct
  module H = Hashtbl.Make (Ail_helpers.Symbol_std)
end

let generate_summaries_for ~prog (fundef : fundef) =
  let open Syntaxes.List in
  let fid, (floc, _, _, _, _) = fundef in
  let section_name =
    "Generate summaries for " ^ Cerb_frontend.Symbol.show_symbol fid
  in
  let@ () = with_section section_name in
  L.info (fun m -> m "%s" section_name);
  let* arg_tys =
    match Ail_helpers.get_param_tys ~prog fid with
    | None ->
        L.info (fun m ->
            m "No argument types found for %a at loc %a" Fmt_ail.pp_sym fid
              Fmt_ail.pp_loc floc);
        []
    | Some arg_tys -> [ arg_tys ]
  in
  let process =
    let open Csymex.Syntax in
    let* args = Csymex.all Layout.nondet_c_ty arg_tys in
    let* result = Bi_interp.exec_fun fundef ~args Bi_state.empty in
    match result with
    | Ok (ret, bi_state) -> Csymex.return (args, Ok ret, bi_state)
    | Error (err, bi_state) -> Csymex.return (args, Error err, bi_state)
    | Missing _ -> Csymex.vanish ()
  in
  let+ (args, ret, bi_state), pc =
    let@ () = with_section "Running symbolic execution" in
    Csymex.run process
  in
  let@ () = with_section "Building summary" in
  let pre, post = Bi_state.to_spec bi_state in
  Summary.make ~args ~ret ~pre ~post ~pc ()

let generate_all_summaries ~functions_to_analyse prog =
  let order = Call_graph.weak_topological_order (Call_graph.of_prog prog) in
  let should_analyse =
    match functions_to_analyse with
    | None -> fun _ -> true
    | Some l -> fun fid -> List.exists (Ail_helpers.sym_is_id fid) l
  in
  let count = ref 0 in
  let to_analyse =
    List.filter
      (fun f ->
        should_analyse f
        &&
        (* Count how many function we should analyse *)
        (incr count;
         true))
      order
  in
  let@ () = Progress_bar.run ~msg:"Generating summaries" ~total:!count () in
  ListLabels.filter_map to_analyse ~f:(fun fid ->
      let open Syntaxes.Option in
      let res =
        let+ fundef = Ail_helpers.find_fun_def ~prog fid in
        let summaries = generate_summaries_for ~prog fundef in
        (fid, summaries)
      in
      Progress_bar.signal_progress 1;
      res)
