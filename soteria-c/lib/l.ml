let src = Logs.Src.create "Soteria.soteria_C"

include (val Logs.src_log src)

type current_logger = Fmt | Stderr | Html

let current_logger = ref Fmt
let get_current_logger () = !current_logger

module Logger = struct
  type t = (module Soteria_symex.Logging.Symex_log_reporter)

  let logger () =
    match !current_logger with
    | Fmt ->
        (module Soteria_symex.Logging.Fmt_log_reporter
        : Soteria_symex.Logging.Symex_log_reporter)
    | Stderr -> failwith "Stderr logger not implemented"
    | Html ->
        (module Soteria_html_logs : Soteria_symex.Logging.Symex_log_reporter)

  let init s =
    let module Logger = (val logger ()) in
    Logger.init s
end
