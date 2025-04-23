let src = Logs.Src.create "Soteria.Symex"

include (val Logs.src_log src : Logs.LOG)

module type Symex_log_reporter = sig
  (** The string is an initialisation information, potentially a file name *)
  include Incremental.Mutable with type init_data := string

  val with_logger_setup : use:bool -> string -> (unit -> 'a) -> 'a
end

module Fmt_log_reporter : Symex_log_reporter = struct
  type t = unit

  let init _ = ()
  let backtrack_n _ _ = ()
  let save _ = ()
  let reset _ = ()

  let with_logger_setup ~use _ f =
    if use then (
      Logs.set_reporter (Logs_fmt.reporter ());
      Fmt_tty.setup_std_outputs ());
    f ()
end
