open Hashcons

module Var = struct
  type t = int

  let[@inline] of_int i = i
  let to_string i = "|" ^ string_of_int i ^ "|"
  let of_string s = int_of_string (String.sub s 1 (String.length s - 2))
  let pp = Fmt.of_to_string to_string
  let equal = Int.equal
  let compare = Int.compare
end

type ty = TBool | TInt | TLoc | TPointer | TSeq of ty
[@@deriving eq, show, ord]

let t_bool = TBool
let t_int = TInt
let t_loc = TLoc
let t_ptr = TPointer
let t_seq ty = TSeq ty

module Nop = struct
  type t = | [@@deriving eq, show, ord]
end

module Unop = struct
  type t = Not | GetPtrLoc | GetPtrOfs | IntOfBool [@@deriving eq, show, ord]
end

module Binop = struct
  type t =
    (* Bool *)
    | And
    | Or
    (* Comparison *)
    | Eq
    | Geq
    | Gt
    | Leq
    | Lt
    (* Arith *)
    | Plus
    | Minus
    | Times
    | Div
  [@@deriving eq, show { with_path = false }, ord]
end

let pp_hash_consed pp_node ft t = pp_node ft t.node
let equal_hash_consed _ t1 t2 = Int.equal t1.tag t2.tag
let compare_hash_consed _ t1 t2 = Int.compare t1.tag t2.tag

type t_kind =
  | Var of Var.t
  | Bool of bool
  | Int of Z.t [@printer Fmt.of_to_string Z.to_string]
  | Ptr of t * t
  (* | BitVec of (Z.t * int) *)
  | Seq of t list
  | Unop of Unop.t * t
  | Binop of Binop.t * t * t
(* | Nop of Nop.t * t list *)

and t_node = { kind : t_kind; ty : ty }
and t = t_node hash_consed [@@deriving show { with_path = false }, eq, ord]

let hash t = t.hkey

let rec iter_vars (sv : t) (f : Var.t * ty -> unit) : unit =
  match sv.node.kind with
  | Var v -> f (v, sv.node.ty)
  | Bool _ | Int _ -> ()
  | Ptr (l, r) | Binop (_, l, r) ->
      iter_vars l f;
      iter_vars r f
  | Unop (_, sv) -> iter_vars sv f
  | Seq t -> List.iter (fun sv -> iter_vars sv f) t

let pp_full ft t = pp_t_node ft t.node

let rec pp ft t =
  let open Fmt in
  match t.node.kind with
  | Var v -> pf ft "V%a" Var.pp v
  | Bool b -> pf ft "%b" b
  | Int z -> pf ft "%a" Z.pp_print z
  | Ptr (l, o) -> pf ft "&(%a, %a)" pp l pp o
  | Seq l -> pf ft "%a" (brackets (list ~sep:comma pp)) l
  | Unop (op, v) -> pf ft "%a(%a)" Unop.pp op pp v
  | Binop (op, v1, v2) -> pf ft "(%a %a %a)" pp v1 Binop.pp op pp v2

let[@inline] equal a b = Int.equal a.tag b.tag

module Hcons = Hashcons.Make (struct
  type t = t_node

  let equal = equal_t_node

  (* We could do a lot more efficient in terms of hashing probably,
     if this ever becomes a bottleneck. *)
  let hash { kind; ty } =
    let hty = Hashtbl.hash ty in
    match kind with
    | Var _ | Bool _ | Int _ -> Hashtbl.hash (kind, hty)
    | Ptr (l, r) -> Hashtbl.hash (l.hkey, r.hkey, hty)
    | Seq l -> Hashtbl.hash (List.map (fun sv -> sv.hkey) l, hty)
    | Unop (op, v) -> Hashtbl.hash (op, v.hkey, hty)
    | Binop (op, l, r) -> Hashtbl.hash (op, l.hkey, r.hkey, hty)
end)

let table = Hcons.create 1023
let hashcons = Hcons.hashcons table
let ( <| ) kind ty : t = hashcons { kind; ty }
let mk_var v ty = Var v <| ty

let rec subst subst_var sv =
  match sv.node.kind with
  | Var v -> mk_var (subst_var v) sv.node.ty
  | Bool _ | Int _ -> sv
  | Ptr (l, r) ->
      let l' = subst subst_var l in
      let r' = subst subst_var r in
      if equal l l' && equal r r' then sv else Ptr (l', r') <| TPointer
  | Seq l ->
      let changed = ref false in
      let l' =
        List.map
          (fun sv ->
            let new_sv = subst subst_var sv in
            if equal new_sv sv then changed := true;
            new_sv)
          l
      in
      if !changed then Seq l' <| sv.node.ty else sv
  | Unop (op, v) ->
      let v' = subst subst_var v in
      if equal v v' then sv else Unop (op, v') <| sv.node.ty
  | Binop (op, l, r) ->
      let l' = subst subst_var l in
      let r' = subst subst_var r in
      if equal l l' && equal r r' then sv else Binop (op, l', r') <| sv.node.ty

(** {2 Booleans}  *)

let v_true = Bool true <| TBool
let v_false = Bool false <| TBool

let bool b =
  (* avoid re-alloc and re-hashconsing *)
  if b then v_true else v_false

let and_ v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Bool b1, Bool b2 -> bool (b1 && b2)
  | Bool false, _ | _, Bool false -> v_false
  | Bool true, _ -> v2
  | _, Bool true -> v1
  | _ -> Binop (And, v1, v2) <| TBool

let rec sem_eq v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Int z1, Int z2 -> bool (Z.equal z1 z2)
  | Bool b1, Bool b2 -> bool (b1 = b2)
  | Ptr (l1, o1), Ptr (l2, o2) -> and_ (sem_eq l1 l2) (sem_eq o1 o2)
  | _ ->
      if equal v1 v2 then v_true (* Start with a syntactic check *)
      else Binop (Eq, v1, v2) <| TBool

let sem_eq_untyped v1 v2 =
  if equal_ty v1.node.ty v2.node.ty then sem_eq v1 v2 else v_false

let or_ v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Bool b1, Bool b2 -> bool (b1 || b2)
  | Bool true, _ | _, Bool true -> v_true
  | Bool false, _ -> v2
  | _, Bool false -> v1
  | _ -> Binop (Or, v1, v2) <| TBool

let not sv =
  if equal sv v_true then v_false
  else if equal sv v_false then v_true
  else
    match sv.node.kind with
    | Unop (Not, sv) -> sv
    | _ -> Unop (Not, sv) <| TBool

let rec split_ands (sv : t) (f : t -> unit) : unit =
  match sv.node.kind with
  | Binop (And, s1, s2) ->
      split_ands s1 f;
      split_ands s2 f
  | _ -> f sv

let distinct l =
  let current = ref [] in
  Utils.List_ex.iter_self_cross_product l (fun (i, j) ->
      current := not (sem_eq i j) :: !current);
  !current

(** {2 Integers}  *)

let int_z z = Int z <| TInt
let int i = int_z (Z.of_int i)
let zero = int_z Z.zero
let one = int_z Z.one

let int_of_bool b =
  match b.node.kind with
  | Bool true -> one
  | Bool false -> zero
  | _ -> Unop (IntOfBool, b) <| TInt

let bool_of_int sv =
  match sv.node.kind with
  | Int z -> bool (Stdlib.not (Z.equal z Z.zero))
  | Unop (IntOfBool, sv') -> sv'
  | _ -> not (sem_eq sv zero)

(** [out_cons] is the outcome constructor, [f] is the function to apply to the int values, [b] is the binop *)
let lift_int_binop ~out_cons ~out_ty ~f ~binop v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Int i1, Int i2 -> out_cons (f i1 i2)
  | _ -> Binop (binop, v1, v2) <| out_ty

let geq = lift_int_binop ~out_cons:bool ~out_ty:TBool ~f:Z.geq ~binop:Geq
let leq = lift_int_binop ~out_cons:bool ~out_ty:TBool ~f:Z.leq ~binop:Leq
let lt = lift_int_binop ~out_cons:bool ~out_ty:TBool ~f:Z.lt ~binop:Lt
let gt = lift_int_binop ~out_cons:bool ~out_ty:TBool ~f:Z.gt ~binop:Gt
let plus = lift_int_binop ~out_cons:int_z ~out_ty:TInt ~f:Z.add ~binop:Plus
let minus = lift_int_binop ~out_cons:int_z ~out_ty:TInt ~f:Z.sub ~binop:Minus
let times = lift_int_binop ~out_cons:int_z ~out_ty:TInt ~f:Z.mul ~binop:Times
let div = lift_int_binop ~out_cons:int_z ~out_ty:TInt ~f:Z.div ~binop:Div

(* Negates a boolean that is in integer form (i.e. 0 for false, anything else is true) *)
let not_int_bool sv =
  match sv.node.kind with
  | Int z -> int_z (if Z.equal z Z.zero then Z.one else Z.zero)
  | Unop (IntOfBool, sv') -> int_of_bool (not sv')
  | _ -> int_of_bool (sem_eq sv one)

(** {2 Pointers} *)

module Ptr = struct
  let mk l o = Ptr (l, o) <| TPointer

  let loc p =
    match p.node.kind with Ptr (l, _) -> l | _ -> Unop (GetPtrLoc, p) <| TLoc

  let loc_of_int i = Int (Z.of_int i) <| TLoc

  let ofs p =
    match p.node.kind with Ptr (_, o) -> o | _ -> Unop (GetPtrOfs, p) <| TInt

  let null_loc = Int Z.zero <| TLoc
  let null = mk null_loc zero
  let is_null p = sem_eq p null
  let is_at_null_loc p = sem_eq (loc p) null_loc
end

(** {2 Sequences} *)

module SSeq = struct
  let mk ~inner_ty l = Seq l <| TSeq inner_ty
end

(** {2 Infix operators}  *)

module Infix = struct
  let int_z = int_z
  let int = int
  let ptr = Ptr.mk
  let seq = SSeq.mk
  let ( #== ) = sem_eq
  let ( #==? ) = sem_eq_untyped
  let ( #> ) = gt
  let ( #>= ) = geq
  let ( #< ) = lt
  let ( #<= ) = leq
  let ( #&& ) = and_
  let ( #|| ) = or_
  let ( #+ ) = plus
  let ( #- ) = minus
  let ( ~- ) x = minus zero x
  let ( #* ) = times
  let ( #/ ) = div
end

module Syntax = struct
  module Sym_int_syntax = struct
    let mk_int = int
    let zero = zero
    let one = one
  end
end
