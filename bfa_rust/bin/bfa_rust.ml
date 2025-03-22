open Cmdliner

let file_arg =
  let doc = "FILE" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let dump_smt_arg =
  let doc = "Dump the SMT queries to the given file" in
  Arg.(
    value
    & opt (some string) None
    & info [ "dump-smt-to"; "dump-smt" ] ~docv:"SMT_FILE" ~doc)

let no_compile_flag =
  let doc = "Do not compile the Rust code, as it is already compiled" in
  Arg.(value & flag & info [ "no-compile" ] ~doc)

let cleanup_flag =
  let doc = "Clean up compiles files after execution" in
  Arg.(value & flag & info [ "clean" ] ~doc)

module Exec_main = struct
  let term =
    Term.(
      const Bfa_rust_lib.Driver.exec_main_and_print
      $ Logs_cli.level ()
      $ dump_smt_arg
      $ no_compile_flag
      $ cleanup_flag
      $ file_arg)

  let cmd = Cmd.v (Cmd.info "exec-main") term
end

let cmd = Cmd.group (Cmd.info "bfa-rust") [ Exec_main.cmd ]
let () = exit @@ Cmd.eval cmd
