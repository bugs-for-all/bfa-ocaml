open Cmdliner

let file_arg =
  let doc = "FILE" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let no_compile_flag =
  let doc = "Do not compile the Rust code, as it is already compiled" in
  Arg.(value & flag & info [ "no-compile" ] ~doc)

let cleanup_flag =
  let doc = "Clean up compiles files after execution" in
  Arg.(value & flag & info [ "clean" ] ~doc)

let ignore_leaks_flag =
  let doc = "Ignore memory leaks" in
  Arg.(value & flag & info [ "ignore-leaks" ] ~doc)

let with_kani_flag =
  let doc = "Use the Kani library" in
  Arg.(value & flag & info [ "kani" ] ~doc)

let with_miri_flag =
  let doc = "Use the Miri library" in
  Arg.(value & flag & info [ "miri" ] ~doc)

module Exec_main = struct
  let term =
    Term.(
      const Soteria_rust_lib.Driver.exec_main_and_print
      $ Soteria_logs.Cli.term
      $ Soteria_c_values.Solver_config.Cli.term
      $ no_compile_flag
      $ cleanup_flag
      $ ignore_leaks_flag
      $ with_kani_flag
      $ with_miri_flag
      $ file_arg)

  let cmd = Cmd.v (Cmd.info "exec-main") term
end

module RUXt = struct
  let term =
    Term.(
      const Soteria_rust_lib.Ruxt.Refute.find_unsoundness_and_print
      $ Soteria_logs.Cli.term
      $ Soteria_c_values.Solver_config.Cli.term
      $ no_compile_flag
      $ cleanup_flag
      $ file_arg)

  let cmd = Cmd.v (Cmd.info "ruxt") term
end

let cmd = Cmd.group (Cmd.info "soteria-rust") [ Exec_main.cmd; RUXt.cmd ]
let () = exit @@ Cmd.eval cmd
