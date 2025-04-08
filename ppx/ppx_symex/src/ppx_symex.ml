open Ppxlib

let ext ext =
  let open Ppx_symex_expander.If_sat in
  Extension.declare_with_path_arg
    (Extension_name.to_string ext)
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc:_ ~path:_ ~arg:_ expr -> expand ~ext expr)

(* Registring [if%sat] *)
let () =
  let open Ppx_symex_expander.If_sat in
  Driver.register_transformation
    (Extension_name.to_string Sat)
    ~extensions:[ ext Sat ];
  Driver.register_transformation
    (Extension_name.to_string Sat1)
    ~extensions:[ ext Sat1 ]

(* Register [0s], [1s] etc. *)
let () =
  let open Ppx_symex_expander.Sym_constants in
  let kind = Context_free.Rule.Constant_kind.Integer in
  let rule = Context_free.Rule.constant kind suffix rewriter in
  Driver.register_transformation ~rules:[ rule ] "sym_constants"
