module Call_trace = Soteria_terminal.Call_trace
module SState = State (* Clashes with Cerb_frontend.State *)
open Cerb_frontend
open Syntaxes.FunctionWrap
module Wpst_interp = Interp.Make (SState)

let as_nonempty_list functions_to_analyse =
  match functions_to_analyse with [] -> None | _ -> Some functions_to_analyse

let impl_name =
  match Sys.getenv_opt "IMPL_NAME" with
  | Some impl -> impl
  | None -> "gcc_4.9.0_x86_64-apple-darwin10.8.0"

let set_cerb_conf () =
  let open Cerb_global in
  set_cerb_conf ~backend_name:"soteria-c" ~exec:false Random ~concurrency:false
    Basic ~defacto:false ~permissive:false ~agnostic:false
    ~ignore_bitfields:false

let io : Cerb_backend.Pipeline.io_helpers =
  let open Cerb_backend.Pipeline in
  let return = Exception.except_return in
  let pass_message =
    let ref = ref 0 in
    fun str ->
      Cerb_debug.print_success (Printf.sprintf "%i. %s" !ref str);
      incr ref;
      return ()
  in
  let set_progress _ = return () in
  let run_pp opts doc =
    run_pp opts doc;
    return ()
  in
  let print_endline str =
    print_endline str;
    return ()
  in
  let print_debug n mk_str =
    Cerb_debug.print_debug n [] mk_str;
    return ()
  in
  let warn ?(always = false) mk_str =
    Cerb_debug.warn ~always [] mk_str;
    return ()
  in
  { pass_message; set_progress; run_pp; print_endline; print_debug; warn }

module Frontend = struct
  let lift_parsing_result res =
    match res with
    | Exception.Result (_, (_, ast)) -> Ok ast
    | Exception (loc, err) ->
        let msg =
          Printf.sprintf "Failed to parse AIL: %s"
            (Pp_errors.to_string (loc, err))
        in
        Error (`ParsingError msg, Call_trace.singleton ~loc ())

  let frontend = ref (fun ~cpp_cmd:_ _ -> failwith "Frontend not set")

  let include_libc () =
    let root_includes = Cerb_runtime.in_runtime "libc/include" in
    let posix = Filename.concat root_includes "posix" in
    "-I" ^ root_includes ^ " -I" ^ posix

  let init () =
    let result =
      let open Cerb_backend.Pipeline in
      let include_soteria_c_h =
        let filename =
          Filename.concat !Config.current.auto_include_path "soteria-c.h"
        in
        if Sys.file_exists filename then "-include " ^ filename ^ " "
        else (
          L.warn (fun m -> m "%s not found" filename);
          "")
      in
      let ( let* ) = Exception.except_bind in
      let conf cpp_cmd =
        {
          debug_level = 0;
          pprints = [];
          astprints = [];
          ppflags = [];
          ppouts = [];
          typecheck_core = false;
          rewrite_core = false;
          sequentialise_core = false;
          cpp_cmd;
          cpp_stderr = false;
          cpp_save = None;
        }
      in
      set_cerb_conf ();
      Ocaml_implementation.(set HafniumImpl.impl);
      let* stdlib = load_core_stdlib () in
      let* impl = load_core_impl stdlib impl_name in
      Exception.Result
        (fun ~cpp_cmd filename ->
          let cpp_cmd =
            cpp_cmd
            ^ " -E -C -Werror -nostdinc "
            ^ include_soteria_c_h
            ^ include_libc ()
          in
          c_frontend (conf cpp_cmd, io) (stdlib, impl) ~filename)
    in
    let () = Cerb_colour.do_colour := false in
    match result with
    | Exception.Result f -> frontend := f
    | Exception.Exception err ->
        Fmt.failwith "Failed to initialize frontend: %s"
          (Pp_errors.to_string err)

  let () = Initialize_analysis.register_once_initialiser init

  let frontend ?cwd ~cpp_cmd filename =
    L.debug (fun m -> m "Parsing %s" filename);
    try
      let cerb_res =
        match cwd with
        | None -> !frontend ~cpp_cmd filename
        | Some dir ->
            Sys.with_working_dir dir @@ fun () -> !frontend ~cpp_cmd filename
      in
      lift_parsing_result cerb_res
    with Sys_error err ->
      let pos = Cerb_position.(set_source (filename, 0) dummy) in
      let loc = Cerb_location.point pos in

      Error (`ParsingError err, Call_trace.singleton ~loc ())

  let simple_frontend ~includes filename =
    let cmd = "cc" :: List.map (fun s -> "-I" ^ s) includes in
    let cpp_cmd = String.concat " " cmd in
    L.trace (fun m -> m "Cpp_cmd: %s" cpp_cmd);
    frontend ~cpp_cmd filename
end

let parse_ail_raw_default ~includes file =
  Frontend.simple_frontend ~includes file

let parse_and_link_ail ~includes files =
  let open Syntaxes.Result in
  let parse_and_signal file =
    let res = parse_ail_raw_default ~includes file in
    Soteria_terminal.Progress_bar.signal_progress 1;
    res
  in
  let@ () =
    Soteria_terminal.Progress_bar.run ~color:`Yellow ~msg:"Parsing files"
      ~total:(List.length files) ()
  in
  match files with
  | [] -> Error (`ParsingError "No files to parse?", Call_trace.empty)
  | files ->
      let* parsed =
        if !Config.current.no_ignore_parse_failures then
          Monad.ResultM.all parse_and_signal files
        else
          let parsed =
            List.filter_map
              (fun file ->
                match parse_and_signal file with
                | Ok ast -> Some ast
                | Error (msg, _loc) ->
                    let msg =
                      match msg with
                      | `ParsingError s -> s
                      | _ -> "Unknown error"
                    in
                    L.warn (fun m ->
                        m "Ignoring file that did not parse correctly: %s@\n%s"
                          file msg);
                    None)
              files
          in
          Ok parsed
      in
      Ail_linking.link parsed

let parse_compilation_item (item : Compilation_database.cmd) =
  Frontend.frontend ~cwd:item.directory
    ~cpp_cmd:(String.concat " " item.command)
    item.file

let is_main (def : Cabs.function_definition) =
  let decl = match def with FunDef (_, _, _, decl, _) -> decl in
  match decl with
  | Cabs.Declarator (_, DDecl_identifier (_, Identifier (_, name))) ->
      String.equal name "main"
  | _ -> false

let pp_err_and_call_trace ft (err, call_trace) =
  Fmt.pf ft "@[%a with trace@ %a@]" Error.pp err
    (Call_trace.pp Fmt_ail.pp_loc)
    call_trace

let resolve_entry_point (linked : Ail_tys.linked_program) =
  let open Syntaxes.Result in
  let* entry_point =
    Result.of_opt
      ~err:(`ParsingError "No entry point function", Call_trace.empty)
      linked.entry_point
  in
  linked.sigma.function_definitions
  |> List.find_opt (fun (id, _) -> Symbol.equal_sym id entry_point)
  |> Result.of_opt ~err:(`ParsingError "Entry point not found", Call_trace.empty)

let with_function_context prog f =
  let open Effect.Deep in
  let fctx = Fun_ctx.of_linked_program prog in
  try f () with effect Interp.Get_fun_ctx, k -> continue k fctx

let exec_main ~includes file_names =
  let open Syntaxes.Result in
  let result =
    let* linked = parse_and_link_ail ~includes file_names in
    if !Config.current.parse_only then Ok []
    else
      let* entry_point = resolve_entry_point linked in
      let sigma = linked.sigma in
      let () = Initialize_analysis.reinit sigma in
      let symex =
        let open Csymex.Syntax in
        let** state = Wpst_interp.init_prog_state linked in
        L.debug (fun m -> m "@[<2>Initial state:@ %a@]" SState.pp state);
        Wpst_interp.exec_fun entry_point ~prog:linked ~args:[] state
      in
      let@ () = with_function_context linked in
      Ok (Csymex.run symex)
  in
  match result with Ok v -> v | Error e -> [ (Error e, []) ]

let temp_file = lazy (Filename.temp_file "soteria_c" ".c")

let generate_errors content =
  let (lazy file_name) = temp_file in
  let () =
    let oc = open_out file_name in
    output_string oc content;
    close_out oc
  in
  match parse_and_link_ail ~includes:[] [ file_name ] with
  | Error e -> [ e ]
  | Ok prog ->
      let@ () = with_function_context prog in
      let summaries =
        Abductor.generate_all_summaries ~functions_to_analyse:None prog
      in
      let results =
        List.concat_map
          (fun (fid, summaries) ->
            let@ () =
              Soteria_logs.Logs.with_section
                ("Anaysing summaries for function" ^ Symbol.show_symbol fid)
            in
            List.concat_map (Summary.manifest_bugs ~prog ~fid) summaries)
          summaries
      in
      List.sort_uniq Stdlib.compare results

(** {2 Entry points} *)

(* Helper for all main entry points *)
let initialise log_config term_config solver_config config =
  Soteria_logs.Config.check_set_and_lock log_config;
  Soteria_terminal.Config.set_and_lock term_config;
  Solver_config.set solver_config;
  Config.set config;
  Initialize_analysis.init_once ()

(* Entry point function *)
let exec_main_and_print log_config term_config solver_config config includes
    file_names =
  (* The following line is not set as an initialiser so that it is executed before initialising z3 *)
  initialise log_config term_config solver_config config;
  let result = exec_main ~includes file_names in
  if not !Config.current.parse_only then
    let pp_state ft state = SState.pp_serialized ft (SState.serialize state) in
    Fmt.pr
      "@[<v 2>Symex terminated with the following outcomes:@ %a@]@\n\
       Executed %d statements"
      Fmt.Dump.(
        list @@ fun ft (r, _) ->
        (Soteria_symex.Compo_res.pp ~ok:(pair Typed.ppa pp_state)
           ~err:pp_err_and_call_trace
           ~miss:(Fmt.Dump.list SState.pp_serialized))
          ft r)
      result
      (Stats.get_executed_statements ())

let dump_summaries ~prog results =
  match !Config.current.dump_summaries_file with
  | None -> ()
  | Some file ->
      let pp_summary ~fid ft summary =
        Fmt.pf ft "@[<v 2>%a@]" Summary.pp (Summary.analyse ~prog ~fid summary)
      in
      let@ oc = Channels.with_out_file file in
      let ft = Format.formatter_of_out_channel oc in
      List.iter
        (fun (fid, summaries) ->
          Fmt.pf ft "@[<v 2>Summaries for %a:@ %a@]@ @." Fmt_ail.pp_sym fid
            (Fmt.list ~sep:Fmt.sp (pp_summary ~fid))
            summaries)
        results

let analyse_summaries ~prog results =
  let total =
    Iter.of_list results
    |> Iter.map (fun (_, l) -> List.length l)
    |> Iter.fold ( + ) 0
  in
  let open Syntaxes.List in
  let@ () =
    Soteria_terminal.Progress_bar.run ~color:`Magenta
      ~msg:"Analysing summaries " ~total ()
  in
  let+ fid, summaries = results in
  let results =
    let+ summary = summaries in
    let res = Summary.analyse ~prog ~fid summary in
    Soteria_terminal.Progress_bar.signal_progress 1;
    res
  in
  (fid, results)

let generate_summaries ~functions_to_analyse prog =
  let results =
    let@ () = with_function_context prog in
    Abductor.generate_all_summaries ~functions_to_analyse prog
  in
  Csymex.dump_unsupported ();
  let results = analyse_summaries ~prog results in
  dump_summaries ~prog results;
  Fmt.pr "@\n@?";
  let found_bugs = ref false in
  results
  |> List.iter (fun (fid, summaries) ->
         let bugs =
           List.concat_map (Summary.manifest_bugs ~prog ~fid) summaries
         in
         if not (List.is_empty bugs) then (
           found_bugs := true;
           List.iter
             (fun (error, call_trace) ->
               Error.Diagnostic.print_diagnostic ~fid ~call_trace ~error;
               Fmt.pr "@\n@?")
             (List.sort_uniq Stdlib.compare bugs)));
  if not !found_bugs then
    Fmt.pr "%a@.@?" Soteria_terminal.Color.pp_ok "No bugs found"

(* Entry point function *)
let lsp config () =
  Config.set config;
  Initialize_analysis.init_once ();
  Soteria_c_lsp.run ~generate_errors ()

(* Entry point function *)
let show_ail logs_config term_config (includes : string list)
    (files : string list) =
  Soteria_logs.Config.check_set_and_lock logs_config;
  Soteria_terminal.Config.set_and_lock term_config;
  Initialize_analysis.init_once ();
  match parse_and_link_ail ~includes files with
  | Ok { symmap; sigma; entry_point } ->
      Fmt.pr "@[<v 2>Extern idmap:@ %a@]@\n@\n"
        Fmt.(
          iter_bindings ~sep:semi Pmap.iter
            (hbox
               (pair ~sep:(any " ->@ ") Fmt_ail.pp_id
                  (Dump.pair Fmt_ail.pp_sym Fmt_ail.pp_id_kind))))
        sigma.AilSyntax.extern_idmap;

      Fmt.pr "@[<v 2>Declarations:@ %a@]@\n@\n"
        (Fmt.list ~sep:Fmt.semi (fun ft (sym, (_, _, decl)) ->
             let declstr =
               match decl with
               | AilSyntax.Decl_object _ -> "object"
               | Decl_function _ -> "function"
             in
             Fmt.pf ft "@[<h>%a ->@ %s@]" Fmt_ail.pp_sym sym declstr))
        sigma.declarations;

      Fmt.pr "@[<v 2>Object definitions:@ %a@]@\n@\n"
        (Fmt.list ~sep:Fmt.sp (Fmt.Dump.pair Fmt_ail.pp_sym Fmt_ail.pp_expr))
        sigma.object_definitions;

      Fmt.pr "@[<v 2>Function definitions:@ %a@]@\n@\n"
        (Fmt.list ~sep:Fmt.sp (fun ft (sym, _) -> Fmt_ail.pp_sym ft sym))
        sigma.function_definitions;

      Fmt.pr "@[<v 2> Symmap:@ %a@]@\n@\n"
        Fmt.(
          iter_bindings ~sep:semi Pmap.iter
            (hbox Fmt_ail.(pair ~sep:(any " ->@ ") pp_sym pp_sym)))
        symmap;

      Fmt.pr "@[<v>%a@]" Fmt_ail.pp_program (entry_point, sigma)
  | Error err -> Fmt.pr "%a@." pp_err_and_call_trace err

(* Entry point function *)
let generate_all_summaries log_config term_config solver_config config includes
    functions_to_analyse file_names =
  (* TODO: generate a compilation database directly, to simplify the interface in this file. *)
  let functions_to_analyse = as_nonempty_list functions_to_analyse in
  initialise log_config term_config solver_config config;
  let prog =
    let@ () = Soteria_logs.Logs.with_section "Parsing and Linking" in
    parse_and_link_ail ~includes file_names
    |> Result.get_or ~err:(fun e ->
           Fmt.epr "%a@\n@?" pp_err_and_call_trace e;
           failwith "Failed to parse AIL")
  in
  if not !Config.current.parse_only then
    generate_summaries ~functions_to_analyse prog

(* Entry point function *)
let capture_db log_config term_config solver_config config json_file
    functions_to_analyse =
  let open Syntaxes.Result in
  let functions_to_analyse = as_nonempty_list functions_to_analyse in
  initialise log_config term_config solver_config config;
  let linked_prog =
    let@ () =
      Soteria_logs.Logs.with_section "Parsing and Linking from database"
    in
    let db = Compilation_database.from_file json_file in
    let parse_and_signal item =
      let res = parse_compilation_item item in
      Soteria_terminal.Progress_bar.signal_progress 1;
      res
    in
    let db_size = List.length db in
    let@ () =
      Soteria_terminal.Progress_bar.run ~color:`Yellow
        ~msg:"Parsing files       " ~total:db_size ()
    in
    let* ails =
      if !Config.current.no_ignore_parse_failures then
        Monad.ResultM.all parse_and_signal db
      else
        let ails =
          List.filter_map
            (fun item ->
              match parse_and_signal item with
              | Ok ail -> Some ail
              | Error (`ParsingError msg, _loc) ->
                  L.debug (fun m ->
                      m "Ignoring file that did not parse correctly: %s@\n%s"
                        item.file msg);
                  None)
            db
        in
        let () =
          let parsed = List.length ails in
          if parsed < db_size then
            L.warn (fun m ->
                m
                  "Some files failed to parse, successfully parsed %d out of \
                   %d files"
                  parsed db_size)
        in
        Ok ails
    in
    Ail_linking.link ails
  in
  let linked_prog =
    Result.get_or
      ~err:(function
        | `ParsingError msg, _ ->
            Fmt.epr "%s@\n@?" msg;
            failwith "Failed to parse AIL"
        | `LinkError msg, _ ->
            let msg =
              if not !Config.current.no_ignore_parse_failures then
                "All files failed to parse, no analysis will be performed"
              else msg
            in
            Fmt.pr "Error: %a@\n@?" Soteria_terminal.Color.pp_err msg;
            if !Config.current.no_ignore_parse_failures then
              failwith "Failed to link AIL"
            else Ail_tys.empty_linked_program)
      linked_prog
  in
  if not !Config.current.parse_only then
    generate_summaries ~functions_to_analyse linked_prog
