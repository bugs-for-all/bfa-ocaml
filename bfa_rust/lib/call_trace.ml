module Meta = Charon.Meta

type element = { loc : Meta.span; msg : string }

let make_element ~loc ?(msg = "") () : element = { loc; msg }

let pp_span ft ({ span = { file; beg_loc; end_loc }; _ } : Meta.span) =
  let pp_filename ft ({ name; _ } : Meta.file) =
    match name with
    | Local name -> Fmt.string ft name
    | Virtual name -> Fmt.pf ft "%s (virtual)" name
  in
  if beg_loc.line = end_loc.line then
    Fmt.pf ft "%a:%d:%d-%d" pp_filename file beg_loc.line beg_loc.col
      end_loc.col
  else
    Fmt.pf ft "%a:%d:%d-%d:%d" pp_filename file beg_loc.line beg_loc.col
      end_loc.line end_loc.col

let pp_element ft { loc; msg } = (Fmt.Dump.pair pp_span Fmt.string) ft (loc, msg)

type t = element list

let empty : t = []
let singleton ~loc ?(msg = "") () : t = [ make_element ~loc ~msg () ]
let pp : t Fmt.t = Fmt.Dump.list pp_element
