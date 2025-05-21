open Hashcons
module Var = Soteria_symex.Var

type ty = TBool | TInt | TLoc | TPointer | TSeq of ty
[@@deriving eq, show { with_path = false }, ord]

let t_bool = TBool
let t_int = TInt
let t_loc = TLoc
let t_ptr = TPointer
let t_seq ty = TSeq ty

module Nop = struct
  type t = Distinct [@@deriving eq, show { with_path = false }, ord]
end

module Unop = struct
  type t = Not | GetPtrLoc | GetPtrOfs | IntOfBool
  [@@deriving eq, show { with_path = false }, ord]
end

module Binop = struct
  type t =
    (* Bool *)
    | And
    | Or
    (* Comparison *)
    | Eq
    | Leq
    | Lt
    (* Arith *)
    | Plus
    | Minus
    | Times
    | Div
    | Mod (* Modulo, not remainder *)
    (* Bitwise *)
    | BitAnd
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
  | Nop of Nop.t * t list

and t_node = { kind : t_kind; ty : ty }
and t = t_node hash_consed [@@deriving show { with_path = false }, eq, ord]

let hash t = t.hkey
let kind t = t.node.kind

let rec iter_vars (sv : t) (f : Var.t * ty -> unit) : unit =
  match sv.node.kind with
  | Var v -> f (v, sv.node.ty)
  | Bool _ | Int _ -> ()
  | Ptr (l, r) | Binop (_, l, r) ->
      iter_vars l f;
      iter_vars r f
  | Unop (_, sv) -> iter_vars sv f
  | Nop (_, l) | Seq l -> List.iter (fun sv -> iter_vars sv f) l

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
  | Nop (op, l) -> pf ft "%a(%a)" Nop.pp op (list ~sep:comma pp) l

let[@inline] equal a b = Int.equal a.tag b.tag

let rec sure_neq a b =
  (not (equal_ty a.node.ty b.node.ty))
  ||
  match (a.node.kind, b.node.kind) with
  | Int a, Int b -> not (Z.equal a b)
  | Bool a, Bool b -> a <> b
  | Ptr (la, oa), Ptr (lb, ob) -> sure_neq la lb || sure_neq oa ob
  | _ -> false

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
    | Nop (op, l) -> Hashtbl.hash (op, List.map (fun sv -> sv.hkey) l, hty)
end)

let table = Hcons.create 1023
let hashcons = Hcons.hashcons table
let ( <| ) kind ty : t = hashcons { kind; ty }
let mk_var v ty = Var v <| ty

(** We put commutative binary operators in some sort of normal form where
    element with the smallest id is on the LHS, to increase cache hits. *)
let mk_commut_binop op l r =
  if l.tag <= r.tag then Binop (op, l, r) else Binop (op, r, l)

(* TODO: substitution will break normal forms. *)
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
            if not (equal new_sv sv) then changed := true;
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
  | Nop (op, l) ->
      let changed = ref false in
      let l' =
        List.map
          (fun sv ->
            let new_sv = subst subst_var sv in
            if not (equal new_sv sv) then changed := true;
            new_sv)
          l
      in
      if !changed then Nop (op, l') <| sv.node.ty else sv

(** {2 Constants} *)

let v_true = Bool true <| TBool
let v_false = Bool false <| TBool
let int_z z = Int z <| TInt
let int i = int_z (Z.of_int i)
let zero = int_z Z.zero
let one = int_z Z.one

(** {2 Booleans} *)

let as_bool t =
  if equal t v_true then Some true
  else if equal t v_false then Some false
  else None

let bool b =
  (* avoid re-alloc and re-hashconsing *)
  if b then v_true else v_false

let and_ v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Bool b1, Bool b2 -> bool (b1 && b2)
  | Bool false, _ | _, Bool false -> v_false
  | Bool true, _ -> v2
  | _, Bool true -> v1
  | _ -> mk_commut_binop And v1 v2 <| TBool

let conj l = List.fold_left and_ v_true l

let rec sem_eq v1 v2 =
  if equal v1 v2 then v_true
  else
    match (v1.node.kind, v2.node.kind) with
    | Int z1, Int z2 -> bool (Z.equal z1 z2)
    | Bool b1, Bool b2 -> bool (b1 = b2)
    | Ptr (l1, o1), Ptr (l2, o2) -> and_ (sem_eq l1 l2) (sem_eq o1 o2)
    | _, Binop (Plus, v2, v3) when equal v1 v2 -> sem_eq v3 zero
    | _, Binop (Plus, v2, v3) when equal v1 v3 -> sem_eq v2 zero
    | Binop (Plus, v1, v3), _ when equal v1 v2 -> sem_eq v3 zero
    | Binop (Plus, v1, v3), _ when equal v3 v2 -> sem_eq v1 zero
    | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v1 v3 ->
        sem_eq v2 v4
    | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v1 v4 ->
        sem_eq v2 v3
    | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v2 v3 ->
        sem_eq v1 v4
    | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v2 v4 ->
        sem_eq v1 v3
    | _ -> mk_commut_binop Eq v1 v2 <| TBool

let sem_eq_untyped v1 v2 =
  if equal_ty v1.node.ty v2.node.ty then sem_eq v1 v2 else v_false

let or_ v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Bool b1, Bool b2 -> bool (b1 || b2)
  | Bool true, _ | _, Bool true -> v_true
  | Bool false, _ -> v2
  | _, Bool false -> v1
  | _ -> mk_commut_binop Or v1 v2 <| TBool

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
  (* [Distinct l] when l is empty or of size 1 is always true *)
  match l with
  | [] | _ :: [] -> v_true
  | l -> Nop (Distinct, l) <| TBool

(** {2 Integers} *)

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

let rec lt v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Int i1, Int i2 -> bool (Z.lt i1 i2)
  | _, _ when equal v1 v2 -> v_false
  | _, Binop (Plus, v2, v3) when equal v1 v2 -> lt zero v3
  | _, Binop (Plus, v2, v3) when equal v1 v3 -> lt zero v2
  | Binop (Plus, v1, v3), _ when equal v1 v2 -> lt v3 zero
  | Binop (Plus, v1, v3), _ when equal v3 v2 -> lt v1 zero
  | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v1 v3 -> lt v2 v4
  | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v2 v3 -> lt v1 v4
  | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v1 v4 -> lt v2 v3
  | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v2 v4 -> lt v1 v3
  | _ -> Binop (Lt, v1, v2) <| TBool

let gt v1 v2 = lt v2 v1

let rec plus v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v1 zero -> v2
  | _, _ when equal v2 zero -> v1
  | Int i1, Int i2 -> int_z (Z.add i1 i2)
  | Binop (Plus, v1, { node = { kind = Int i2; _ }; _ }), Int i3 ->
      plus v1 (int_z (Z.add i2 i3))
  | Binop (Plus, { node = { kind = Int i1; _ }; _ }, v2), Int i3 ->
      plus (int_z (Z.add i1 i3)) v2
  | Int i1, Binop (Plus, v1, { node = { kind = Int i2; _ }; _ }) ->
      plus (int_z (Z.add i1 i2)) v1
  | Int i1, Binop (Plus, { node = { kind = Int i2; _ }; _ }, v2) ->
      plus (int_z (Z.add i1 i2)) v2
  | _ -> mk_commut_binop Plus v1 v2 <| TInt

let minus v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v2 zero -> v1
  | Int i1, Int i2 -> int_z (Z.sub i1 i2)
  | _ -> Binop (Minus, v1, v2) <| TInt

let times v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v1 zero || equal v2 zero -> zero
  | _, _ when equal v1 one -> v2
  | _, _ when equal v2 one -> v1
  | Int i1, Int i2 -> int_z (Z.mul i1 i2)
  | _ -> mk_commut_binop Times v1 v2 <| TInt

let rec leq v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Int i1, Int i2 -> bool (Z.leq i1 i2)
  | _, _ when equal v1 v2 -> v_true
  | _, Binop (Plus, v2, v3) when equal v1 v2 -> leq zero v3
  | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v1 v3 -> leq v2 v4
  | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v2 v3 -> leq v1 v4
  | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v1 v4 -> leq v2 v3
  | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v2 v4 -> leq v1 v3
  | _ -> Binop (Leq, v1, v2) <| TBool

let bit_and v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Int i1, Int i2 -> int_z (Z.logand i1 i2)
  | _ -> mk_commut_binop Binop.BitAnd v1 v2 <| TInt

let geq v1 v2 = leq v2 v1

let div v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v2 one -> v1
  | Int i1, Int i2 -> int_z (Z.div i1 i2)
  | _ -> Binop (Div, v1, v2) <| TInt

let mod_ v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v2 one -> zero
  | Int i1, Int i2 ->
      (* OCaml's mod computes the remainer... *)
      let rem = Z.( mod ) i1 i2 in
      if Z.lt rem Z.zero then int_z (Z.add rem i2) else int_z rem
  | _ -> Binop (Mod, v1, v2) <| TInt

(* Negates a boolean that is in integer form (i.e. 0 for false, anything else is true) *)
let not_int_bool sv =
  match sv.node.kind with
  | Int z -> int_z (if Z.equal z Z.zero then Z.one else Z.zero)
  | Unop (IntOfBool, sv') -> int_of_bool (not sv')
  | _ -> int_of_bool (sem_eq sv zero)

(** {2 Pointers} *)

module Ptr = struct
  let mk l o = Ptr (l, o) <| TPointer

  let loc p =
    match p.node.kind with Ptr (l, _) -> l | _ -> Unop (GetPtrLoc, p) <| TLoc

  let null_loc = Int Z.zero <| TLoc
  let is_null_loc l = sem_eq l null_loc
  let loc_of_z z = Int z <| TLoc
  let loc_of_int i = loc_of_z (Z.of_int i)

  let ofs p =
    match p.node.kind with Ptr (_, o) -> o | _ -> Unop (GetPtrOfs, p) <| TInt

  let null = mk null_loc zero
  let is_null p = sem_eq p null
  let is_at_null_loc p = is_null_loc (loc p)
end

(** {2 Sequences} *)

module SSeq = struct
  let mk ~inner_ty l = Seq l <| TSeq inner_ty
end

(** {2 Infix operators} *)

module Infix = struct
  let int_z = int_z
  let int = int
  let ptr = Ptr.mk
  let seq = SSeq.mk
  let ( ==@ ) = sem_eq
  let ( ==?@ ) = sem_eq_untyped
  let ( >@ ) = gt
  let ( >=@ ) = geq
  let ( <@ ) = lt
  let ( <=@ ) = leq
  let ( &&@ ) = and_
  let ( ||@ ) = or_
  let ( +@ ) = plus
  let ( -@ ) = minus
  let ( ~- ) x = minus zero x
  let ( *@ ) = times
  let ( /@ ) = div
  let ( &@ ) = bit_and
end

module Syntax = struct
  module Sym_int_syntax = struct
    let mk_int = int
    let zero = zero
    let one = one
  end
end
