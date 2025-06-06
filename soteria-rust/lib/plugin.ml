open Charon
open Cmd

exception PluginError of string

type fun_decl = UllbcAst.fun_decl
type entry_point = { fun_decl : fun_decl; expect_error : bool }

let mk_entry_point ?(expect_error = false) fun_decl =
  Some { fun_decl; expect_error }

let cargo =
  "RUSTC=$(charon toolchain-path)/bin/rustc $(charon toolchain-path)/bin/cargo"

let get_host =
  let host = ref None in
  fun () ->
    match !host with
    | Some h -> h
    | None -> (
        let info = Fmt.kstr exec_and_read "%s -vV" cargo in
        match List.find_opt (String.starts_with ~prefix:"host") info with
        | Some s ->
            let host_ = String.sub s 6 (String.length s - 6) in
            host := Some host_;
            host_
        | None -> raise (PluginError "Couldn't find target host"))

let lib_path name =
  let root =
    match Sys.getenv_opt "RUSTERIA_PLUGINS" with
    | Some root -> root
    | None -> List.hd Runtime_sites.Sites.plugins
  in
  root ^ "/" ^ name

let compile_lib path =
  let target = get_host () in
  let verbosity =
    if Soteria_logs.(Config.should_log Level.Trace) then "--verbose"
    else "> /dev/null 2>/dev/null"
  in
  let res =
    Fmt.kstr exec_cmd "cd %s && %s build --lib --target %s %s" path cargo target
      verbosity
  in
  if res <> 0 && res <> 255 then
    let msg = Fmt.str "Couldn't compile lib at %s: error %d" path res in
    raise (PluginError msg)

(* List of patterns that currently cause generic errors (and thus crashes in monomorphisation).
   We tell Charon to not translate these, to avoid the crash, and we just hope the item is not
   encountered at runtime. *)
let known_generic_errors =
  [
    "alloc::borrow::ToOwned::to_owned";
    "alloc::boxed::_::from_raw";
    "alloc::boxed::_::into_raw";
    "alloc::raw_vec::_::try_allocate_in";
    "alloc::string::_::from";
    "alloc::raw_vec::finish_grow";
    "core::fmt::Display::fmt";
    "core::ptr::null_mut";
    "core::ptr::metadata::Thin";
    "std::path::_::from";
  ]

type plugin = {
  mk_cmd : unit -> Cmd.charon_cmd;
  get_entry_point : fun_decl -> entry_point option;
}

let default =
  let mk_cmd () =
    let std_lib = lib_path "std" in
    let target = get_host () in
    let opaques = List.map (( ^ ) "--opaque ") known_generic_errors in
    compile_lib std_lib;
    mk_cmd
      ~charon:
        ([
           "--ullbc";
           "--translate-all-methods";
           "--extract-opaque-bodies";
           "--monomorphize";
           "--mir promoted";
         ]
        @ opaques)
      ~rustc:
        [
          (* i.e. not always a binary! *)
          "--crate-type=lib";
          "-Zunstable-options";
          (* No warning *)
          "-Awarnings";
          (* include our std and rusteria crates *)
          "--extern=rusteria";
          Fmt.str "-L%s/target/%s/debug/deps" std_lib target;
          Fmt.str "-L%s/target/debug/deps" std_lib;
          Fmt.str "--extern noprelude:std=%s/target/%s/debug/libstd.rlib"
            std_lib target;
        ]
      ()
  in
  let get_entry_point (decl : fun_decl) =
    match List.last_opt decl.item_meta.name with
    | Some (PeIdent ("main", _)) -> mk_entry_point decl
    | _ -> None
  in
  { mk_cmd; get_entry_point }

let kani =
  let mk_cmd () =
    let lib = lib_path "kani" in
    let target = get_host () in
    compile_lib lib;
    mk_cmd
      ~rustc:
        [
          "-Zcrate-attr=feature\\(register_tool\\)";
          "-Zcrate-attr=register_tool\\(kanitool\\)";
          "--cfg=kani";
          "--extern=kani";
          Fmt.str "-L%s/target/%s/debug/deps" lib target;
          Fmt.str "-L%s/target/debug/deps" lib;
        ]
      ()
  in
  let get_entry_point (decl : fun_decl) =
    if Charon_util.decl_has_attr decl "kanitool::proof" then
      let expect_error =
        Charon_util.decl_has_attr decl "kanitool::should_panic"
      in
      mk_entry_point ~expect_error decl
    else None
  in
  { mk_cmd; get_entry_point }

let miri =
  let mk_cmd () =
    let lib = lib_path "miri" in
    let target = get_host () in
    compile_lib lib;
    mk_cmd
      ~rustc:
        [
          "--cfg=miri";
          "--extern=miristd";
          Fmt.str "-L%s/target/%s/debug/deps" lib target;
          Fmt.str "-L%s/target/debug/deps" lib;
        ]
      ()
  in
  let get_entry_point (decl : fun_decl) =
    match List.last decl.item_meta.name with
    | PeIdent ("miri_start", _) -> mk_entry_point decl
    | _ -> None
  in
  { mk_cmd; get_entry_point }

type root_plugin = {
  mk_cmd : input:string -> output:string -> unit -> Cmd.charon_cmd;
  get_entry_point : fun_decl -> entry_point option;
}

let merge_ifs (plugins : (bool * plugin) list) =
  let plugins =
    List.filter_map
      (fun (enabled, plugin) -> if enabled then Some plugin else None)
      plugins
  in

  let mk_cmd ~input ~output () =
    let init = mk_cmd ~charon:[ "--dest-file " ^ output ] ~rustc:[ input ] () in
    List.map (fun (p : plugin) -> p.mk_cmd ()) plugins
    |> List.fold_left concat_cmd init
  in
  let get_entry_point decl =
    let rec aux acc rest =
      match (acc, rest) with
      | None, (p : plugin) :: rest -> aux (p.get_entry_point decl) rest
      | _ -> acc
    in
    aux None plugins
  in
  { mk_cmd; get_entry_point }
