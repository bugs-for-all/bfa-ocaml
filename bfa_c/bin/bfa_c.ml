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

let version_arg =
  let doc = "Print version information" in
  Arg.(value & flag & info [ "version" ] ~doc)

module Exec_main = struct
  let term =
    Term.(
      const Bfa_c_lib.Driver.exec_main_and_print
      $ Logs_cli.level ()
      $ dump_smt_arg
      $ file_arg)

  let cmd = Cmd.v (Cmd.info "exec-main") term
end

module Lsp = struct
  let lsp show_version =
    if show_version then print_endline "dev" else Bfa_c_lib.Driver.lsp ()

  let term = Term.(const lsp $ version_arg)
  let cmd = Cmd.v (Cmd.info "lsp") term
end

module Show_ail = struct
  let term = Term.(const Bfa_c_lib.Driver.show_ail $ file_arg)
  let cmd = Cmd.v (Cmd.info "show-ail") term
end

module Bi_main = struct
  let term = Term.(const Bfa_c_lib.Driver.generate_main_summary $ file_arg)
  let cmd = Cmd.v (Cmd.info "bi-main") term
end

module Generate_summary = struct
  let fun_name_arg =
    let doc = "FUNCTION" in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"FUNCTION" ~doc)

  let term =
    Term.(const Bfa_c_lib.Driver.generate_summary_for $ file_arg $ fun_name_arg)

  let cmd = Cmd.v (Cmd.info "gen-summary") term
end

let cmd =
  Cmd.group (Cmd.info "bfa-c")
    [ Exec_main.cmd; Lsp.cmd; Show_ail.cmd; Bi_main.cmd; Generate_summary.cmd ]

let () = exit @@ Cmd.eval cmd
