open Cerb_frontend.AilSyntax
open Ail_tys
open Cerb_frontend.Pp_ail

let pp_to_fmt (pprinter : 'a -> PPrint.document) : 'a Fmt.t =
 fun ft a ->
  let buffer = Buffer.create 1023 in
  PPrint.ToBuffer.pretty 0.5 80 buffer (pprinter a);
  Fmt.pf ft "%s" (Buffer.contents buffer)

let pp_id_kind ft kind =
  Fmt.string ft
    (match kind with
    | IK_declaration -> "decl"
    | IK_definition -> "def"
    | IK_tentative -> "tent")

let pp_loc = Fmt.of_to_string Cerb_location.location_to_string
let pp_id = pp_to_fmt (Cerb_frontend.Pp_symbol.pp_identifier ~clever:true)
let pp_sym = Fmt.of_to_string Cerb_frontend.Pp_symbol.to_string
let pp_ty = pp_to_fmt Cerb_frontend.Pp_core_ctype.pp_ctype
let pp_ty_ ft t = pp_ty ft (Ctype ([], t))
let pp_int_ty = pp_to_fmt Cerb_frontend.Pp_ail.pp_integerType
let pp_arithop = pp_to_fmt pp_arithmeticOperator
let pp_binop = pp_to_fmt pp_binaryOperator
let pp_unop = pp_to_fmt pp_unaryOperator
let pp_constant = pp_to_fmt pp_constant
let pp_expr : expr Fmt.t = pp_to_fmt (fun e -> pp_expression e)
let pp_stmt : stmt Fmt.t = pp_to_fmt (fun s -> pp_statement s)
let pp_program : program Fmt.t = pp_to_fmt (pp_program ~show_include:true)
