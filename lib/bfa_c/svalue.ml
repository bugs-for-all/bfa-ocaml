open Hashcons

module Var_name = struct
  type t = int

  let next = ref 0
  let to_string i = "|$V" ^ string_of_int i ^ "|"
  let pp = Fmt.of_to_string to_string

  let fresh () =
    let r = !next in
    incr next;
    r

  let equal = Int.equal
end

type t_ptr

type ty =
  | TBool
  | TInt
  | TPointer
  | TVoid
  (* | BitVec of int *)
  | TSeq of ty
[@@deriving eq, show]

module Unop = struct
  type t = Not [@@deriving eq, show]
end

module Binop = struct
  type t =
    | (* Bool *) And
    | (* Comparison *) Eq
    | Geq
    | Gt
    | Leq
    | Lt
    | Plus
    | Minus
    | Times
    | Div
  [@@deriving eq, show { with_path = false }]
end

let pp_hash_consed pp_node ft t = pp_node ft t.node
let equal_hash_consed _ t1 t2 = Int.equal t1.tag t2.tag

type t_node =
  | Var of (Var_name.t * ty)
  | Bool of bool
  | Int of Z.t [@printer Fmt.of_to_string Z.to_string]
  | Ptr of (t * t)
  (* | BitVec of (Z.t * int) *)
  | Seq of t list
  | Unop of (Unop.t * t)
  | Binop of (Binop.t * t * t)
  | Void

and t = t_node hash_consed [@@deriving show { with_path = false }, eq]

let pp ft t = pp_t_node ft t.node
let equal a b = Int.equal a.tag b.tag

module Hcons = Hashcons.Make (struct
  type t = t_node

  let equal = equal_t_node
  let hash = Hashtbl.hash
end)

let table = create 1024
let hashcons = hashcons table

let fresh ty =
  let v = Var_name.fresh () in
  hashcons (Var (v, ty))

let int_z z = hashcons (Int z)
let int i = int_z (Z.of_int i)
let ptr l o = hashcons (Ptr (l, o))
let seq s = hashcons (Seq s)
let zero = int_z Z.zero
let one = int_z Z.one
let v_true = hashcons (Bool true)
let v_false = hashcons (Bool false)
let void = hashcons Void

let bool b =
  (* avoid hashconsing re-alloc *)
  if b then v_true else v_false

let sem_eq v1 v2 =
  if equal v1 v2 then v_true (* Start with a syntactic check *)
  else hashcons (Binop (Eq, v1, v2))

let and_ v1 v2 =
  match (v1.node, v2.node) with
  | Bool b1, Bool b2 -> bool (b1 && b2)
  | _ -> hashcons (Binop (And, v1, v2))

let not sv =
  if equal sv v_true then v_false
  else if equal sv v_false then v_true
  else hashcons (Unop (Not, sv))

(** [out_cons] is the outcome constructor, [f] is the function to apply to the int values, [b] is the binop *)
let lift_int_binop ~out_cons ~f ~binop v1 v2 =
  match (v1.node, v2.node) with
  | Int i1, Int i2 -> out_cons (f i1 i2)
  | _ -> hashcons (Binop (binop, v1, v2))

let geq = lift_int_binop ~out_cons:bool ~f:Z.geq ~binop:Geq
let leq = lift_int_binop ~out_cons:bool ~f:Z.leq ~binop:Leq
let lt = lift_int_binop ~out_cons:bool ~f:Z.lt ~binop:Lt
let gt = lift_int_binop ~out_cons:bool ~f:Z.gt ~binop:Gt
let plus = lift_int_binop ~out_cons:int_z ~f:Z.add ~binop:Plus
let minus = lift_int_binop ~out_cons:int_z ~f:Z.sub ~binop:Minus
let times = lift_int_binop ~out_cons:int_z ~f:Z.mul ~binop:Times
let div = lift_int_binop ~out_cons:int_z ~f:Z.div ~binop:Div

module Infix = struct
  let ( #== ) = sem_eq
  let ( #> ) = gt
  let ( #>= ) = geq
  let ( #< ) = lt
  let ( #<= ) = leq
  let ( #&& ) = and_
  let ( #+ ) = plus
  let ( #- ) = minus
  let ( #* ) = times
  let ( #/ ) = div
end
