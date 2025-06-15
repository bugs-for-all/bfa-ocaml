open Cerb_frontend
open AilSyntax

type sym = Symbol.sym
type nonrec constant = constant
type ctype = Ctype.ctype
type expr = GenTypes.genTypeCategory expression
type stmt = GenTypes.genTypeCategory statement
type fundef = GenTypes.genTypeCategory sigma_function_definition
type nonrec sigma = GenTypes.genTypeCategory sigma
type program = GenTypes.genTypeCategory ail_program
type extern_symmap = (Symbol.sym, Symbol.sym) Pmap.map

type linked_program = {
  sigma : sigma;
  entry_point : Symbol.sym option;
  symmap : extern_symmap;
  label_cache : Label_cache.t;
}

let empty_linked_program =
  {
    sigma = empty_sigma;
    entry_point = None;
    symmap = Pmap.empty Symbol.compare_sym;
    label_cache = Hashtbl.create 0;
  }
