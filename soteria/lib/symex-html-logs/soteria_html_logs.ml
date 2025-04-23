let html_to_string el = Htmlit.El.to_string ~doctype:false el

type t = { channel : out_channel; mutable depth_counter : int }
type _ Effect.t += Get_logger : t Effect.t

let get_logger () = Effect.perform Get_logger

let header =
  {|
    <!DOCTYPE html>
    <html lang="en">
    <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>Symex Log</title>
    </head>
    <body>
      <h1>Symex Log</h1>
    |}

let footer = {|
    </body>
    </html>
  |}

let depth_level_opening = {|<details>|}
let depth_level_closing = {|</details>|}

let depth_level_title title =
  Htmlit.El.summary [ Htmlit.El.txt title ] |> html_to_string

let finalise t =
  for _ = 0 to t.depth_counter do
    Printf.fprintf t.channel "%s\n" depth_level_closing
  done;
  Printf.fprintf t.channel "%s" footer;
  close_out t.channel

let init file_name =
  let channel = open_out file_name in
  let depth_counter = 0 in
  Printf.fprintf channel "%s\n%s" header depth_level_opening;
  let logger = { channel; depth_counter } in
  let () = at_exit (fun () -> finalise logger) in
  logger

let reset t =
  for _ = 0 to t.depth_counter do
    Printf.fprintf t.channel "%s\n" depth_level_closing
  done;
  Printf.fprintf t.channel "%s\n" depth_level_opening;
  flush t.channel

let save t =
  Printf.fprintf t.channel "%s\n%s" depth_level_opening (depth_level_title "");
  flush t.channel;
  t.depth_counter <- t.depth_counter + 1

let backtrack_n t n =
  for _ = 0 to n - 1 do
    Printf.fprintf t.channel "%s\n" depth_level_closing
  done;
  flush t.channel;
  t.depth_counter <- t.depth_counter - n

let reporter =
  let open Logs in
  let report :
      'a 'b.
      src -> level -> over:(unit -> unit) -> (unit -> 'b) -> ('a, 'b) msgf -> 'b
      =
   fun _src _level ~over k msgf ->
    let k _ =
      over ();
      k ()
    in
    (* TODO: I could potentially leverage tags or headers to attach a title to branching patterns! *)
    msgf @@ fun ?header:_ ?tags:_ fmt ->
    fmt
    |> Format.kasprintf (fun msg_txt ->
           let msg = Htmlit.El.p [ Htmlit.El.txt msg_txt ] |> html_to_string in
           Printf.kfprintf k (get_logger ()).channel "%s\n" msg)
  in
  { report }

let with_logger_setup ~use init_data f =
  if use then
    let () = Logs.set_reporter reporter in
    let t = init init_data in
    try f () with effect Get_logger, k -> Effect.Deep.continue k t
  else f ()
