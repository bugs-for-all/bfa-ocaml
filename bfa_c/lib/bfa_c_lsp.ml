let cerb_loc_to_range loc =
  let open Lsp.Types in
  let (start_l, start_c), (end_l, end_c) =
    Option.value ~default:((0, 0), (0, 0)) (Cerb_location.to_cartesian_user loc)
  in
  Range.
    {
      start = { character = start_c; line = start_l };
      end_ = { character = end_c; line = end_l };
    }

(* This gathers all the reasons why the analysis gave up. *)
let get_abort_diagnostics () =
  let open Syntaxes.List in
  let+ msg, loc = Csymex.flush_give_up () in
  let range = cerb_loc_to_range loc in
  Lsp.Types.Diagnostic.create ~message:(`String msg) ~severity:Information
    ~range ~source:"bfa" ()

let error_to_diagnostic_opt ~uri (err, call_trace) =
  let open Lsp.Types in
  let open DiagnosticSeverity in
  let severity, message =
    (* When we hide non-bug things, some patterns will return None *)
    match err with
    | `NullDereference -> (Error, "Null Dereference")
    | `OutOfBounds -> (Error, "Out of Bounds")
    | `UninitializedMemoryAccess -> (Error, "Uninitialized Memory Access")
    | `ParsingError s -> (Error, s)
    | `UseAfterFree -> (Error, "Use After Free")
    | `DivisionByZero -> (Error, "Division By Zero")
    | `UBPointerComparison ->
        (Error, "Undefined Behavior for Pointer Comparison")
    | `UBPointerArithmetic ->
        (Error, "Undefined Behavior for Pointer Arithmetic")
    | `InvalidFree -> (Error, "Invalid Pointer passed to free")
    | `Memory_leak -> (Warning, "Memory leak")
    | `FailedAssert -> (Error, "FailedAssert")
  in
  let parens_if_non_empty = function "" -> "" | s -> " (" ^ s ^ ")" in
  let range, relatedInformation, msg_addendum =
    match (call_trace : Call_trace.t) with
    | [] -> (cerb_loc_to_range Cerb_location.unknown, None, "")
    | [ { loc; msg } ] -> (cerb_loc_to_range loc, None, parens_if_non_empty msg)
    | { loc; msg } :: locs ->
        let related_info =
          List.map
            (fun Call_trace.{ loc; msg } ->
              let location =
                Location.create ~range:(cerb_loc_to_range loc) ~uri
              in
              DiagnosticRelatedInformation.create ~location ~message:msg)
            locs
        in
        (cerb_loc_to_range loc, Some related_info, parens_if_non_empty msg)
  in
  Lsp.Types.Diagnostic.create
    ~message:(`String (message ^ msg_addendum))
    ~severity ~range ?relatedInformation ~source:"bfa" ()

class bfa_lsp_server generate_errors =
  object (self)
    inherit Linol_eio.Jsonrpc2.server as super
    val mutable debug_mode = false

    (* one env per document *)

    method spawn_query_handler f = Linol_eio.spawn f

    (* We define here a helper method that will:
       - process a document
       - store the state resulting from the processing
       - return the diagnostics from the new state
    *)
    method private _on_doc ~(notify_back : Linol_eio.Jsonrpc2.notify_back)
        (uri : Lsp.Types.DocumentUri.t) (contents : string) =
      let errors = generate_errors contents in
      let diags = List.map (error_to_diagnostic_opt ~uri) errors in
      let diags =
        if debug_mode then get_abort_diagnostics () @ diags else diags
      in
      notify_back#send_diagnostic diags

    (* We now override the [on_notify_doc_did_open] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back d ~content : unit Linol_eio.t =
      self#_on_doc ~notify_back d.uri content

    (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old
        ~new_content =
      self#_on_doc ~notify_back d.uri new_content

    (* On document closes, we remove the state associated to the file from the global
       hashtable state, to avoid leaking memory. *)
    method on_notif_doc_did_close ~notify_back:_ _d : unit Linol_eio.t = ()

    method! on_unknown_notification ~notify_back notif =
      match notif.method_ with
      | "bfa/toggleDebugMode" ->
          L.debug (fun m -> m "Toggling debug mode");
          debug_mode <- not debug_mode
      | _ -> super#on_unknown_notification ~notify_back notif
  end

let run ~generate_errors () =
  Eio_main.run @@ fun env ->
  let s = new bfa_lsp_server generate_errors in
  let server = Linol_eio.Jsonrpc2.create_stdio ~env s in
  let task () =
    let shutdown () = s#get_status = `ReceivedExit in
    Linol_eio.Jsonrpc2.run ~shutdown server
  in
  match task () with
  | () -> ()
  | exception e ->
      let e = Printexc.to_string e in
      Printf.eprintf "error: %s\n%!" e;
      exit 1
