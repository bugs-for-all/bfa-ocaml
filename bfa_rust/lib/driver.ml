module Wpst_interp = Interp.Make (Heap)
module Compo_res = Bfa_symex.Compo_res

exception ExecutionError of string
exception CharonError of string

module Cleaner = struct
  let files = ref []
  let touched file = files := file :: !files
  let cleanup () = List.iter Sys.remove !files
  let init ~clean () = if clean then at_exit cleanup
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
    | `UBTransmute -> Fmt.string ft "UBTransmute"
    | `UBTreeBorrow -> Fmt.string ft "UBTreeBorrow"
    | `DoubleFree -> Fmt.string ft "DoubleFree"
    | `InvalidFree -> Fmt.string ft "InvalidFree"
    | `Memory_leak -> Fmt.string ft "Memory leak"
    | `FailedAssert -> Fmt.string ft "Failed assertion"
    | `Overflow -> Fmt.string ft "Overflow"
    | `StdErr msg -> Fmt.pf ft "Std error: %s" msg
    | `Panic msg -> Fmt.pf ft "Panic: %s" msg
    | `MetaExpectedError -> Fmt.string ft "MetaExpectedError"
  in
  Fmt.pf ft " with trace %a" Call_trace.pp call_trace;
  Format.close_box ()

let exec_cmd cmd =
  L.debug (fun g -> g "Running command: %s" cmd);
  Sys.command cmd

let find_kani_lib ~no_compile () =
  let path =
    try
      let path = Sys.getenv "KANI_LIB_PATH" in
      if String.ends_with ~suffix:"/" path then
        String.sub path 0 (String.length path - 1)
      else path
    with Not_found -> List.hd Runtime_sites.Sites.kani_lib
  in
  match no_compile with
  | true -> path
  | false ->
      let res =
        Fmt.kstr exec_cmd
          "cd %s/kani && charon --only-cargo --lib --input ./src/ > /dev/null \
           2>/dev/null"
          path
      in
      if res <> 0 && res <> 255 then
        raise
          (ExecutionError
             ("Couldn't compile Kani lib: error " ^ Int.to_string res));
      path

(** Given a Rust file, parse it into LLBC, using Charon. *)
let parse_ullbc_of_file ~no_compile file_name =
  let file_name =
    if Filename.is_relative file_name then
      Filename.concat (Sys.getcwd ()) file_name
    else file_name
  in
  let parent_folder = Filename.dirname file_name in
  let output = Printf.sprintf "%s.llbc.json" file_name in
  (if not no_compile then
     let kani_args =
       let kani_lib = find_kani_lib ~no_compile () in
       [
         "--opaque=kani";
         "--rustc-arg=-Zcrate-attr=feature\\(register_tool\\)";
         "--rustc-arg=-Zcrate-attr=register_tool\\(kanitool\\)";
         (* Code often hides kani proofs behind a cfg *)
         "--rustc-arg=--cfg=kani";
         "--rustc-arg=--extern=kani";
         (* The below is cursed and should be fixed !!! *)
         Fmt.str "--rustc-arg=-L%s/kani/target/aarch64-apple-darwin/debug/deps"
           kani_lib;
         Fmt.str "--rustc-arg=-L%s/kani/target/debug/deps" kani_lib;
       ]
     in
     let miri_args = [ "--opaque=miri_extern" ] in
     let res =
       String.concat " "
       @@ [
            Fmt.str "cd %s &&" parent_folder;
            "charon --ullbc";
            Fmt.str "--input %s" file_name;
            Fmt.str "--dest-file %s" output;
            (* We can't enable this because it removes statements we care about... *)
            (* "--mir_optimized"; *)
            (* We don't care about our implementation *)
            "--translate-all-methods";
            "--monomorphize";
            "--extract-opaque-bodies";
            (* Go through rustc to allow injecting the kani deps *)
            "--no-cargo";
            (* i.e. not always a binary! *)
            "--rustc-arg=--crate-type=lib";
            "--rustc-arg=-Zunstable-options";
            (* Not sure this is needed *)
            "--rustc-arg=--extern=std";
            "--rustc-arg=--extern=core";
            (* No warning *)
            "--rustc-arg=-Awarnings";
          ]
       @ kani_args
       @ miri_args
       |> exec_cmd
     in
     if res = 0 then Cleaner.touched output
     else
       let msg = Fmt.str "Failed compilation to ULLBC: code %d" res in
       raise (CharonError msg));
  let crate =
    try
      output |> Yojson.Basic.from_file |> Charon.UllbcOfJson.crate_of_json
    with
    | Sys_error _ -> raise (CharonError "File doesn't exist")
    | _ -> raise (CharonError "Failed to parse ULLBC")
  in
  match crate with
  | Ok crate ->
      if not no_compile then (
        (* save crate to local file *)
        let crate_file = Printf.sprintf "%s.crate" file_name in
        let str = Charon.PrintUllbcAst.Crate.crate_to_string crate in
        let oc = open_out_bin crate_file in
        output_string oc str;
        close_out oc;
        Cleaner.touched crate_file);
      crate
  | Error err -> raise (CharonError err)

let exec_main (crate : Charon.UllbcAst.crate) =
  let module List_ex = Utils.List_ex in
  let open Charon in
  Layout.Session.set_crate crate;
  let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
  let entry_points =
    Types.FunDeclId.Map.values crate.fun_decls
    |> List.filter_map (fun (decl : UllbcAst.blocks UllbcAst.gfun_decl) ->
           let is_main =
             match List.rev decl.item_meta.name with
             | PeIdent ("main", _) :: _ -> true
             | _ -> false
           in
           if is_main then Some (decl, false)
           else
             let is_proof = Charon_util.decl_has_attr decl "kanitool::proof" in
             if is_proof then
               let should_err =
                 Charon_util.decl_has_attr decl "kanitool::should_panic"
               in
               Some (decl, should_err)
             else None)
  in
  if List.is_empty entry_points then
    raise (ExecutionError "No entry points found");
  let exec_fun = Wpst_interp.exec_fun ~crate ~args:[] ~state:Heap.empty in
  let outcomes =
    entry_points
    |> List.map @@ fun ((entry_point : UllbcAst.fun_decl), should_err) ->
       L.info (fun g ->
           g "Executing entry point: %s"
             (PrintTypes.name_to_string ctx entry_point.item_meta.name));
       let branches =
         try Rustsymex.run @@ exec_fun entry_point
         with exn ->
           let msg =
             Fmt.str "Exn: %a@\nTrace: %s" Fmt.exn exn
               (Printexc.get_backtrace ())
           in
           raise (ExecutionError msg)
       in
       let branches =
         if not should_err then branches
         else
           List.map
             (function
               | Compo_res.Ok _, pcs ->
                   let trace =
                     Call_trace.singleton ~loc:entry_point.item_meta.span ()
                   in
                   (Compo_res.Error (`MetaExpectedError, trace), pcs)
               | Compo_res.Error _, pcs ->
                   (Compo_res.Ok (Charon_util.Base Typed.zero, Heap.empty), pcs)
               | v -> v)
             branches
       in
       let outcomes = List.map fst branches in
       if Option.is_some !Rustsymex.not_impl_happened then
         let msg = Option.get !Rustsymex.not_impl_happened in
         raise (ExecutionError msg)
       else if List.is_empty branches then
         raise (ExecutionError "Execution vanished")
       else if List.exists Compo_res.is_missing outcomes then
         raise (ExecutionError "Miss encountered in WPST")
       else
         let errors = Compo_res.only_errors outcomes in
         if List.is_empty errors then
           branches
           |> List.filter_map (function
                | Compo_res.Ok _, pcs -> Some pcs
                | _ -> None)
           |> Result.ok
         else Fmt.error "Errors: %a" Fmt.(Dump.list pp_err) errors
  in
  List_ex.join_results outcomes
  |> Result.map List.flatten
  |> Result.map_error (String.concat "\n\n")

let exec_main_and_print log_level smt_file no_compile clean file_name =
  Z3solver.set_smt_file smt_file;
  setup_console_log log_level;
  Cleaner.init ~clean ();
  try
    let crate = parse_ullbc_of_file ~no_compile file_name in
    let res = exec_main crate in
    match res with
    | Ok res ->
        let open Fmt in
        let n = List.length res in
        let pp_pc ft pc = pf ft "%a" (list ~sep:(any " /\\@, ") Typed.ppa) pc in
        let pp_info ft pc = pf ft "PC: @.  @[<-1>%a@]" pp_pc pc in
        Fmt.pr "Done. - Ran %i branches\n%a\n" n
          (list ~sep:(any "@\n@\n") pp_info)
          res;
        exit 0
    | Error e ->
        L.err (fun f -> f "Error: %s" e);
        exit 1
  with
  | ExecutionError e ->
      L.err (fun f -> f "Fatal: %s" e);
      exit 2
  | CharonError e ->
      L.err (fun f -> f "Fatal (Charon): %s" e);
      exit 3
