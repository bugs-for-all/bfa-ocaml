open Cerb_frontend
module Wpst_interp = Interp.Make (Heap)

let setup_console_log level =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup_stderr_log ~log_lsp level =
  Logs.set_level level;
  let fmt_reporter = Logs.format_reporter ~app:Fmt.stderr ~dst:Fmt.stderr () in
  let reporter =
    if log_lsp then fmt_reporter
    else
      (* A reporter that filters out logging from SMT. *)
      Logs.
        {
          report =
            (fun src level ~over k msgf ->
              if Src.equal src Z3solver.log_src then (
                over ();
                k ())
              else fmt_reporter.report src level ~over k msgf);
        }
  in
  Logs.set_reporter reporter

(** Copying most of the wrapper from RefinedC *)

let impl_name =
  match Sys.getenv_opt "IMPL_NAME" with
  | Some impl -> impl
  | None -> "gcc_4.9.0_x86_64-apple-darwin10.8.0"

let set_cerb_conf () =
  let open Cerb_global in
  set_cerb_conf ~backend_name:"bfa-c" ~exec:false Random ~concurrency:false
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
  let frontend = ref (fun _ -> failwith "Frontend not set")
  let includes = ref ""
  let add_include s = includes := !includes ^ "-I " ^ s ^ " "
  let add_includes ss = List.iter add_include ss
  let libc () = Cerb_runtime.in_runtime "libc/include "

  let init () =
    let result =
      let open Cerb_backend.Pipeline in
      let cpp_cmd =
        "cc -E -C -Werror -nostdinc -undef " ^ "-I" ^ libc () ^ !includes
      in
      let ( let* ) = Exception.except_bind in
      let conf =
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
        (fun filename -> c_frontend (conf, io) (stdlib, impl) ~filename)
    in
    match result with
    | Exception.Result f -> frontend := f
    | Exception.Exception err ->
        let msg = Pp_errors.to_string err in
        frontend := fun _ -> failwith ("Failed to initialize frontend: " ^ msg)

  let () = Initialize_analysis.register_once_initialiser init
  let frontend filename = !frontend filename
end

let parse_ail_raw file =
  match Frontend.frontend file with
  | Result (_, (_, ast)) -> Ok ast
  | Exception (loc, err) ->
      let msg =
        Printf.sprintf "Failed to parse ail: %s"
          (Pp_errors.to_string (loc, err))
      in
      Error (`ParsingError msg, Call_trace.singleton ~loc ())

let is_main (def : Cabs.function_definition) =
  let decl = match def with FunDef (_, _, _, decl, _) -> decl in
  match decl with
  | Cabs.Declarator (_, DDecl_identifier (_, Identifier (_, name))) ->
      String.equal name "main"
  | _ -> false

let pp_err ft (err, call_trace) =
  Format.open_hbox ();
  let () =
    match err with
    | `NullDereference -> Fmt.string ft "NullDereference"
    | `OutOfBounds -> Fmt.string ft "OutOfBounds"
    | `UninitializedMemoryAccess -> Fmt.string ft "UninitializedMemoryAccess"
    | `UseAfterFree -> Fmt.string ft "UseAfterFree"
    | `DivisionByZero -> Fmt.string ft "DivisionByZero"
    | `ParsingError s -> Fmt.pf ft "ParsingError: %s" s
    | `UBPointerComparison -> Fmt.string ft "UBPointerComparison"
    | `UBPointerArithmetic -> Fmt.string ft "UBPointerArithmetic"
    | `DoubleFree -> Fmt.string ft "DoubleFree"
    | `InvalidFree -> Fmt.string ft "InvalidFree"
    | `Memory_leak -> Fmt.string ft "Memory leak"
    | `FailedAssert -> Fmt.string ft "Failed assertion"
  in
  Fmt.pf ft " with trace %a" Call_trace.pp call_trace;
  Format.close_box ()

let parse_ail file_name =
  let open Syntaxes.Result in
  let* entry_point, sigma = parse_ail_raw file_name in
  let* entry_point =
    match entry_point with
    | None -> Error (`ParsingError "No entry point function", Call_trace.empty)
    | Some e -> Ok e
  in
  let+ entry_point =
    let entry_opt =
      sigma.function_definitions
      |> List.find_opt (fun (id, _) -> Symbol.equal_sym id entry_point)
    in
    match entry_opt with
    | None -> Error (`ParsingError "Entry point not found", Call_trace.empty)
    | Some e -> Ok e
  in
  (entry_point, sigma)

let exec_main file_name =
  let open Syntaxes.Result in
  let result =
    let* entry_point, sigma = parse_ail file_name in
    let () = Initialize_analysis.reinit sigma in
    let symex =
      Wpst_interp.exec_fun ~prog:sigma ~args:[] ~state:Heap.empty entry_point
    in
    Ok (Csymex.run symex)
  in
  match result with Ok v -> v | Error e -> [ (Error e, []) ]

(* Entry point function *)
let exec_main_and_print log_level smt_file file_name =
  (* The following line is not set as an initialiser so that it is executed before initialising z3 *)
  Z3solver.set_smt_file smt_file;
  setup_console_log log_level;
  Initialize_analysis.init_once ();
  let result = exec_main file_name in
  let pp_heap ft heap = Heap.pp_serialized ft (Heap.serialize heap) in
  L.app (fun m ->
      m
        "@[<v 2>Symex terminated with the following outcomes:@ %a@]@\n\
         Executed %d statements"
        Fmt.Dump.(
          list @@ fun ft (r, _) ->
          (Bfa_symex.Compo_res.pp ~ok:(pair Typed.ppa pp_heap) ~err:pp_err
             ~miss:(Fmt.Dump.list Heap.pp_serialized))
            ft r)
        result
        (Stats.get_executed_statements ()))

let temp_file = lazy (Filename.temp_file "bfa_c" ".c")

let generate_errors content =
  let (lazy file_name) = temp_file in
  let () =
    let oc = open_out file_name in
    output_string oc content;
    close_out oc
  in
  match parse_ail_raw file_name with
  | Error e -> [ e ]
  | Ok (_, prog) ->
      let summaries = Abductor.generate_all_summaries prog in
      let results =
        List.concat_map
          (fun (fid, summaries) ->
            List.concat_map (Summary.analyse_summary ~prog ~fid) summaries)
          summaries
      in
      List.sort_uniq Stdlib.compare results

(* Entry point function *)
let lsp () =
  setup_stderr_log ~log_lsp:true (Some Logs.Debug);
  Initialize_analysis.init_once ();
  Bfa_c_lsp.run ~generate_errors ()

(* Entry point function *)
let show_ail file_name =
  setup_console_log (Some Debug);
  Initialize_analysis.init_once ();
  match parse_ail_raw file_name with
  | Ok prog -> Fmt.pr "@[<v>%a@]" Fmt_ail.pp_program prog
  | Error err -> Fmt.pr "%a@." pp_err err

let exec_main_bi file_name =
  match parse_ail file_name with
  | Ok (entry_point, prog) ->
      let () = Initialize_analysis.reinit prog in
      Abductor.generate_summaries_for ~prog entry_point
  | Error (`ParsingError s, call_trace) ->
      Fmt.failwith "Failed to parse AIL at loc %a: %s" Call_trace.pp call_trace
        s

(* Entry point function *)
let generate_main_summary file_name =
  setup_console_log (Some Debug);
  Initialize_analysis.init_once ();
  let results = exec_main_bi file_name in
  let pp_summary = Summary.pp pp_err in
  let printer = Fmt.list ~sep:Fmt.sp pp_summary in
  Fmt.pr "@[<v>%a@]@." printer results

let exec_fun_bi file_name fun_name =
  match parse_ail_raw file_name with
  | Ok (_entry_point, prog) ->
      let () = Initialize_analysis.reinit prog in
      let fundef =
        match Ail_helpers.find_fun_name ~prog fun_name with
        | Some fundef -> fundef
        | None -> Fmt.failwith "Couldn't find function %s" fun_name
      in
      let fid, _ = fundef in
      let results = Abductor.generate_summaries_for ~prog fundef in
      List.map
        (fun summary -> (summary, Summary.analyse_summary ~prog ~fid summary))
        results
  | Error (`ParsingError s, call_trace) ->
      Fmt.failwith "Failed to parse AIL at loc %a: %s" Call_trace.pp call_trace
        s

(* Entry point function *)

let generate_summary_for include_args file_name fun_name =
  Frontend.add_includes include_args;
  setup_console_log (Some Debug);
  Initialize_analysis.init_once ();
  let results = exec_fun_bi file_name fun_name in
  let pp_summary ft (summary, analysis) =
    Fmt.pf ft "@[<v 2>%a@ manifest bugs: @[<h>%a@]@]" (Summary.pp pp_err)
      summary (Fmt.Dump.list pp_err) analysis
  in
  Fmt.pr "@[<v>%a@]@." (Fmt.list ~sep:Fmt.sp pp_summary) results

let generate_all_summaries log_level dump_unsupported_file includes file_name =
  Csymex.unsupported_file := dump_unsupported_file;
  Frontend.add_includes includes;
  setup_console_log log_level;
  Initialize_analysis.init_once ();
  let prog =
    match parse_ail_raw file_name with
    | Error e -> Fmt.failwith "Failed to parse AIL: %a" pp_err e
    | Ok (_, prog) -> prog
  in
  let results = Abductor.generate_all_summaries prog in
  Csymex.dump_unsupported ();
  let pp_summary ~fid ft summary =
    Fmt.pf ft "@[<v 2>%a@ manifest bugs: @[<h>%a@]@]" (Summary.pp pp_err)
      summary (Fmt.Dump.list pp_err)
      (Summary.analyse_summary ~prog ~fid summary)
  in
  List.iter
    (fun (fid, summaries) ->
      Fmt.pr "@[<v 2>Summaries for %a:@ %a@]@ @." Fmt_ail.pp_sym fid
        (Fmt.list ~sep:Fmt.sp (pp_summary ~fid))
        summaries)
    results
