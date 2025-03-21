open Cerb_frontend
open AilSyntax

type nonrec constant = constant
type expr = GenTypes.genTypeCategory expression
type stmt = GenTypes.genTypeCategory statement
type fundef = GenTypes.genTypeCategory sigma_function_definition
type nonrec sigma = GenTypes.genTypeCategory sigma
type program = GenTypes.genTypeCategory ail_program
type extern_symmap = (Symbol.sym, Symbol.sym) Pmap.map
type linked_program = { ail : program; symmap : extern_symmap }
