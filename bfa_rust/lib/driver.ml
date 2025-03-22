module Wpst_interp = Interp.Make (Heap)
module Compo_res = Bfa_symex.Compo_res
open Utils.ExecResult

module Cleaner = struct
  let files = ref []
  let touched file = files := file :: !files
  let cleanup () = List.iter Sys.remove !files
end

let setup_console_log level =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

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
    | `UBAbort -> Fmt.string ft "UBAbort"
    | `DoubleFree -> Fmt.string ft "DoubleFree"
    | `InvalidFree -> Fmt.string ft "InvalidFree"
    | `Memory_leak -> Fmt.string ft "Memory leak"
    | `FailedAssert -> Fmt.string ft "Failed assertion"
    | `Overflow -> Fmt.string ft "Overflow"
    | `StdErr msg -> Fmt.pf ft "Std error: %s" msg
    | `Panic msg -> Fmt.pf ft "Panic: %s" msg
  in
  Fmt.pf ft " with trace %a" Call_trace.pp call_trace;
  Format.close_box ()

(** Given a Rust file, parse it into LLBC, using Charon. *)
let parse_ullbc_of_file ~no_compile file_name =
  let parent_folder = Filename.dirname file_name in
  let output = Printf.sprintf "%s.llbc.json" file_name in
  let*> () =
    match no_compile with
    | true -> Ok 0
    | false ->
        (* load from env *)
        let+ kani_lib =
          try
            let path = Sys.getenv "KANI_LIB_PATH" in
            if String.ends_with ~suffix:"/" path then Ok path
            else Ok (path ^ "/")
          with Not_found -> Fatal "KANI_LIB_PATH not set"
        in
        let peek f s =
          f s;
          s
        in
        String.concat " "
          [
            Fmt.str "cd %s &&" parent_folder;
            "charon --ullbc";
            Fmt.str "--input %s" file_name;
            Fmt.str "--dest-file %s" output;
            (* We can't enable this because it removes statements we care about... *)
            (* "--mir_optimized"; *)
            (* We don't care about our implementation *)
            "--opaque=kani";
            "--translate-all-methods";
            "--monomorphize";
            "--extract-opaque-bodies";
            (* Go through rustc to allow injecting the kani deps *)
            "--no-cargo";
            (* i.e. not always a binary! *)
            "--rustc-arg=--crate-type=lib";
            "--rustc-arg=-Zunstable-options";
            "--rustc-arg=-Zcrate-attr=feature\\(register_tool\\)";
            "--rustc-arg=-Zcrate-attr=register_tool\\(kanitool\\)";
            (* Code often hides kani proofs behind a cfg *)
            "--rustc-arg=--cfg=kani";
            (* Not sure this is needed *)
            "--rustc-arg=--extern=kani";
            "--rustc-arg=--extern=std";
            (* The below is cursed and should be fixed !!! *)
            Fmt.str
              "--rustc-arg=-L%skani/target/aarch64-apple-darwin/debug/deps"
              kani_lib;
            Fmt.str "--rustc-arg=-L%skani/target/debug/deps" kani_lib;
          ]
        |> peek (fun s -> L.debug (fun g -> g "Running command: %s" s))
        |> Sys.command
        |> peek (function 0 -> Cleaner.touched output | _ -> ())
  in
  let* crate =
    try
      output |> Yojson.Basic.from_file |> Charon.UllbcOfJson.crate_of_json |> ok
    with
    | Sys_error _ -> Fatal "File doesn't exist"
    | _ -> Fatal "Failed to parse ULLBC"
  in
  (* save crate to local file *)
  let () =
    match (crate, no_compile) with
    | Ok crate, false ->
        let crate_file = Printf.sprintf "%s.crate" file_name in
        let oc = open_out_bin crate_file in
        let str = Charon.PrintUllbcAst.Crate.crate_to_string crate in
        output_string oc str;
        close_out oc;
        Cleaner.touched crate_file
    | _ -> ()
  in
  of_result_fatal crate

let exec_main (crate : Charon.UllbcAst.crate) =
  let open Charon in
  Layout.Session.set_crate crate;
  let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
  let* entry_points =
    Types.FunDeclId.Map.values crate.fun_decls
    |> List.filter (fun (decl : UllbcAst.blocks UllbcAst.gfun_decl) ->
           (match List.rev decl.item_meta.name with
           | PeIdent ("main", _) :: _ -> true
           | _ -> false)
           || List.exists
                (function
                  | Meta.AttrUnknown { path; _ } -> path = "kanitool::proof"
                  | _ -> false)
                decl.item_meta.attr_info.attributes)
    |> function
    | [] -> Fatal "No main function found"
    | l -> Ok l
  in
  let exec_fun = Wpst_interp.exec_fun ~crate ~args:[] ~state:Heap.empty in
  let outcomes =
    entry_points
    |> List.map (fun (entry_point : UllbcAst.fun_decl) ->
           L.info (fun g ->
               g "Executing entry point: %s"
                 (PrintTypes.name_to_string ctx entry_point.item_meta.name));
           try
             let exec_fun = exec_fun entry_point in
             let branches = Rustsymex.run exec_fun in
             let outcomes = List.map fst branches in
             if Option.is_some !Rustsymex.not_impl_happened then (
               let msg = Option.get !Rustsymex.not_impl_happened in
               Rustsymex.not_impl_happened := None;
               Fmt.kstr fatal "A not_impl was triggered: %s" msg)
             else if List.is_empty branches then Fatal "Execution vanished"
             else if List.exists Compo_res.is_missing outcomes then
               Fatal "Miss encountered in WPST"
             else
               let errors = Compo_res.only_errors outcomes in
               if List.is_empty errors then
                 branches
                 |> List.filter_map (function
                      | Compo_res.Ok ok, pcs -> Some (ok, pcs)
                      | _ -> None)
                 |> ok
               else Fmt.kstr error "Errors: %a" Fmt.(Dump.list pp_err) errors
           with exn ->
             Fmt.kstr fatal "Exn: %a@\nTrace: %s" Fmt.exn exn
               (Printexc.get_backtrace ()))
  in
  combine outcomes
  |> map_full List.flatten (String.concat "\n\n") (String.concat "\n\n")

let exec_main_and_print log_level smt_file no_compile clean_up file_name =
  Z3solver.set_smt_file smt_file;
  setup_console_log log_level;
  let res =
    let* crate = parse_ullbc_of_file ~no_compile file_name in
    exec_main crate
  in
  if clean_up then Cleaner.cleanup ();
  match res with
  | Ok res ->
      let open Fmt in
      let n = List.length res in
      let pp_pc ft pc =
        pf ft "@[%a@]" (list ~sep:(any " /\\@, ") Typed.ppa) pc
      in
      let pp_info ft (_, pc) = pf ft "PC: @[%a@]" pp_pc pc in
      Fmt.pr "Done. - Ran %i branches\n%a\n" n
        (list ~sep:(any "@\n@\n") pp_info)
        res;
      exit 0
  | Error e ->
      L.err (fun f -> f "Error: %s" e);
      exit 1
  | Fatal e ->
      L.err (fun f -> f "Fatal: %s" e);
      exit 2
