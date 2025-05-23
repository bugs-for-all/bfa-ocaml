open Shexp_process
open Stdlib (* Shexp overrides `List`... *)
open Cmdliner

let ( |. ) f g = pipe f g

let dest_dir_arg position =
  Arg.(
    required
    & pos position (some string) None
    & info [] ~docv:"DEST_DIR" ~doc:"Path to the destination directory")

module Infer_Dylibs = struct
  let infer_dylibs exe =
    run "otool" [ "-L"; exe ]
    |. run "awk" [ "{print $1}" ]
    |. run "tail" [ "-n"; "+2" ]
    |. read_all
    |> eval
    |> print_string

  let exe_arg =
    Arg.(
      required
      & pos 0 (some file) None
      & info [] ~docv:"EXE" ~doc:"Path to the executable")

  let term = Term.(const infer_dylibs $ exe_arg)
  let cmd = Cmd.v (Cmd.info "infer-dylibs") term
end

module Copy_files = struct
  let ignored_regexp = Str.quote "libSystem.B.dylib" |> Str.regexp

  let should_ignore lib =
    try Str.search_forward ignored_regexp lib 0 >= 0 with Not_found -> false

  let copy_files list_file dest_dir =
    let () = mkdir ~p:() dest_dir |> eval in
    let copy_file f =
      let dest = Filename.concat dest_dir (Filename.basename f) in
      if Sys.file_exists dest then (
        Printf.printf "%s already exists, removing it\n" dest;
        eval (rm dest));
      let () = run "cp" [ f; dest ] |> eval in
      Printf.printf "Copied %s to %s\n" f dest
    in
    let ic = open_in list_file in
    try
      while true do
        let il = input_line ic |> String.trim in
        (* We take care of trailing whitespace / new lines *)
        if should_ignore il then Printf.printf "Ignoring %s\n" il
        else if il <> "" then copy_file il
        else raise End_of_file
      done
    with End_of_file -> close_in ic

  let list_file_arg =
    Arg.(
      required
      & pos 0 (some file) None
      & info [] ~docv:"LIST_FILE"
          ~doc:
            "Path to the file that contains the list of files to copy. One \
             file per line")

  let term = Term.(const copy_files $ list_file_arg $ dest_dir_arg 1)
  let cmd = Cmd.v (Cmd.info "copy-files") term
end

module Copy_cerb_runtime = struct
  let copy_cerb_runtime dest_dir =
    let ( / ) = Filename.concat in
    let path = Cerb_runtime.in_runtime "" in
    let dest_dir = dest_dir / "cerberus-lib" / "runtime" in
    let () = run "mkdir" [ "-p"; dest_dir ] |> eval in
    let () = run "cp" [ "-r"; path; dest_dir ] |> eval in
    let () = run "rm" [ "-rf"; Filename.concat dest_dir "bmc" ] |> eval in
    Printf.printf "Copied Cerb runtime from %s to %s\n" path dest_dir

  let term = Term.(const copy_cerb_runtime $ dest_dir_arg 0)
  let cmd = Cmd.v (Cmd.info "copy-cerb-runtime") term
end

module Copy_soteria_c_auto_includes = struct
  let copy_includes dest_dir =
    let ( / ) = Filename.concat in
    let path =
      Stdlib.List.nth Soteria_c_lib.Auto_include_site.Sites.includes 0
    in
    let path = path / "soteria-c.h" in
    let dest_dir = dest_dir / "soteria-c" in
    let () = run "mkdir" [ "-p"; dest_dir ] |> eval in
    let () = run "cp" [ path; dest_dir ] |> eval in
    Printf.printf "Copied %s to %s\n" path dest_dir

  let term = Term.(const copy_includes $ dest_dir_arg 0)
  let cmd = Cmd.v (Cmd.info "copy-soteria-c-auto-includes") term
end

(* Main command *)

let cmd =
  Cmd.group (Cmd.info "package")
    [
      Infer_Dylibs.cmd;
      Copy_files.cmd;
      Copy_cerb_runtime.cmd;
      Copy_soteria_c_auto_includes.cmd;
    ]

let () = exit @@ Cmd.eval cmd
