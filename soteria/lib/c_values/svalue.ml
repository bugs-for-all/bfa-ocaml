open Hashcons
module Var = Soteria_symex.Var

module FloatPrecision = struct
  type t = F16 | F32 | F64 | F128
  [@@deriving eq, show { with_path = false }, ord]

  let size = function F16 -> 16 | F32 -> 32 | F64 -> 64 | F128 -> 128

  let of_size = function
    | 16 -> F16
    | 32 -> F32
    | 64 -> F64
    | 128 -> F128
    | _ -> failwith "Invalid float size"
end

module FloatClass = struct
  type t = Normal | Subnormal | Zero | Infinite | NaN
  [@@deriving eq, show { with_path = false }, ord]

  let as_fpclass = function
    | Normal -> FP_normal
    | Subnormal -> FP_subnormal
    | Zero -> FP_zero
    | Infinite -> FP_infinite
    | NaN -> FP_nan
end

module FloatRoundingMode = struct
  type t = NearestTiesToEven | NearestTiesToAway | Ceil | Floor | Truncate
  [@@deriving eq, show { with_path = false }, ord]
end

type ty =
  | TBool
  | TInt
  | TFloat of FloatPrecision.t
  | TLoc
  | TPointer
  | TSeq of ty
  | TBitVector of int
[@@deriving eq, show { with_path = false }, ord]

let t_bool = TBool
let t_int = TInt
let t_f fp = TFloat fp
let t_f16 = t_f F16
let t_f32 = t_f F32
let t_f64 = t_f F64
let t_f128 = t_f F128
let t_loc = TLoc
let t_ptr = TPointer
let t_seq ty = TSeq ty

(* NOTE: Currently, we do not have BitVectors as values, and we do not expose them to the
   interpreter. BVs are merely an artefact of the fact we can't do all operations we wish to
   only on Int+Float. As such, we must ensure that BV-typed values never leak! They may only
   exist as intermediate values. *)
let t_bv n = TBitVector n
let is_float = function TFloat _ -> true | _ -> false

let precision_of_f = function
  | TFloat p -> p
  | ty -> Fmt.failwith "Not a float: %a" pp_ty ty

let size_of_bv = function TBitVector n -> n | _ -> failwith "Not a bitvector"

module Nop = struct
  type t = Distinct [@@deriving eq, show { with_path = false }, ord]
end

module Unop = struct
  let equal_fpclass = ( = )
  let compare_fpclass = compare

  type t =
    | Not
    | FAbs
    | GetPtrLoc
    | GetPtrOfs
    | IntOfBool
    | BvOfFloat of int (* target bitvec size *)
    | BvOfInt
    | FloatOfBv
    | IntOfBv of bool (* signed *)
    | BvExtract of int * int (* from * to *)
    | FIs of FloatClass.t
    | FRound of FloatRoundingMode.t
  [@@deriving eq, ord]

  let pp ft = function
    | Not -> Fmt.string ft "!"
    | FAbs -> Fmt.string ft "abs."
    | GetPtrLoc -> Fmt.string ft "loc"
    | GetPtrOfs -> Fmt.string ft "ofs"
    | IntOfBool -> Fmt.string ft "b2i"
    | BvOfFloat n -> Fmt.pf ft "f2bv(%d)" n
    | BvOfInt -> Fmt.string ft "i2bv"
    | FloatOfBv -> Fmt.string ft "bv2f"
    | IntOfBv _ -> Fmt.string ft "bv2i"
    | BvExtract (from, to_) -> Fmt.pf ft "extract[%d-%d]" from to_
    | FIs fc -> Fmt.pf ft "fis(%a)" FloatClass.pp fc
    | FRound mode -> Fmt.pf ft "fround(%a)" FloatRoundingMode.pp mode
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
    | Rem
    | Mod (* Modulo, not remainder *)
    (* Float comparison *)
    | FEq
    | FLeq
    | FLt
    (* Float arith *)
    | FPlus
    | FMinus
    | FTimes
    | FDiv
    | FRem
    (* Bitwise Binary operators *)
    | BvPlus
    | BvMinus
    | BitAnd
    | BitOr
    | BitXor
    | BitShl
    | BitShr
  [@@deriving eq, show { with_path = false }, ord]

  let pp ft = function
    | And -> Fmt.string ft "&&"
    | Or -> Fmt.string ft "||"
    | Eq -> Fmt.string ft "=="
    | Leq -> Fmt.string ft "<="
    | Lt -> Fmt.string ft "<"
    | Plus -> Fmt.string ft "+"
    | Minus -> Fmt.string ft "-"
    | Times -> Fmt.string ft "*"
    | Div -> Fmt.string ft "/"
    | Rem -> Fmt.string ft "rem"
    | Mod -> Fmt.string ft "mod"
    | FEq -> Fmt.string ft "==."
    | FLeq -> Fmt.string ft "<=."
    | FLt -> Fmt.string ft "<."
    | FPlus -> Fmt.string ft "+."
    | FMinus -> Fmt.string ft "-."
    | FTimes -> Fmt.string ft "*."
    | FDiv -> Fmt.string ft "/."
    | FRem -> Fmt.string ft "rem."
    | BvPlus -> Fmt.string ft "+b"
    | BvMinus -> Fmt.string ft "-b"
    | BitAnd -> Fmt.string ft "&"
    | BitOr -> Fmt.string ft "|"
    | BitXor -> Fmt.string ft "^"
    | BitShl -> Fmt.string ft "<<"
    | BitShr -> Fmt.string ft ">>"
end

let pp_hash_consed pp_node ft t = pp_node ft t.node
let equal_hash_consed _ t1 t2 = Int.equal t1.tag t2.tag
let compare_hash_consed _ t1 t2 = Int.compare t1.tag t2.tag

type t_kind =
  | Var of Var.t
  | Bool of bool
  | Int of Z.t [@printer Fmt.of_to_string Z.to_string]
  | Float of string
  | Ptr of t * t
  | BitVec of Z.t [@printer Fmt.of_to_string (Z.format "%#x")]
  | Seq of t list
  | Unop of Unop.t * t
  | Binop of Binop.t * t * t
  | Nop of Nop.t * t list
  | Ite of t * t * t

and t_node = { kind : t_kind; ty : ty }
and t = t_node hash_consed [@@deriving show { with_path = false }, eq, ord]

let hash t = t.hkey
let kind t = t.node.kind

let rec iter_vars (sv : t) (f : Var.t * ty -> unit) : unit =
  match sv.node.kind with
  | Var v -> f (v, sv.node.ty)
  | Bool _ | Int _ | Float _ | BitVec _ -> ()
  | Ptr (l, r) | Binop (_, l, r) ->
      iter_vars l f;
      iter_vars r f
  | Unop (_, sv) -> iter_vars sv f
  | Nop (_, l) | Seq l -> List.iter (fun sv -> iter_vars sv f) l
  | Ite (c, t, e) ->
      iter_vars c f;
      iter_vars t f;
      iter_vars e f

let pp_full ft t = pp_t_node ft t.node

let rec pp ft t =
  let open Fmt in
  match t.node.kind with
  | Var v -> pf ft "V%a" Var.pp v
  | Bool b -> pf ft "%b" b
  | Int z when Z.(z > of_int 2048 || z < of_int (-2048)) ->
      pf ft "%s" (Z.format "%#x" z)
  | Int z -> pf ft "%a" Z.pp_print z
  | Float f -> pf ft "%sf" f
  | BitVec bv -> pf ft "%s" (Z.format "%#x" bv)
  | Ptr (l, o) -> pf ft "&(%a, %a)" pp l pp o
  | Seq l -> pf ft "%a" (brackets (list ~sep:comma pp)) l
  | Ite (c, t, e) -> pf ft "(%a ? %a : %a)" pp c pp t pp e
  | Unop (Not, { node = { kind = Binop (Eq, v1, v2); _ }; _ }) ->
      pf ft "(%a != %a)" pp v1 pp v2
  | Unop (op, v) -> pf ft "%a(%a)" Unop.pp op pp v
  | Binop (op, v1, v2) -> pf ft "(%a %a %a)" pp v1 Binop.pp op pp v2
  | Nop (op, l) -> (
      let rec aux = function
        | acc, [] -> acc
        | Some l, { node = { kind = Var v; _ }; _ } :: rest ->
            aux (Some (Var.to_int v :: l), rest)
        | _, _ -> None
      in
      let range = aux (Some [], l) in
      let range =
        Option.bind range (fun l ->
            let l = List.sort Int.compare l in
            let min = List.hd l in
            let max = List.hd @@ List.rev l in
            if max - min + 1 = List.length l then Some (min, max) else None)
      in
      match range with
      | Some (min, max) -> pf ft "%a(V|%d-%d|)" Nop.pp op min max
      | None -> pf ft "%a(%a)" Nop.pp op (list ~sep:comma pp) l)

let[@inline] equal a b = Int.equal a.tag b.tag

let rec sure_neq a b =
  (not (equal_ty a.node.ty b.node.ty))
  ||
  match (a.node.kind, b.node.kind) with
  | Int a, Int b | BitVec a, BitVec b -> not (Z.equal a b)
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
    | Var _ | Bool _ | Int _ | Float _ | BitVec _ -> Hashtbl.hash (kind, hty)
    | Ptr (l, r) -> Hashtbl.hash (l.hkey, r.hkey, hty)
    | Seq l -> Hashtbl.hash (List.map (fun sv -> sv.hkey) l, hty)
    | Unop (op, v) -> Hashtbl.hash (op, v.hkey, hty)
    | Binop (op, l, r) -> Hashtbl.hash (op, l.hkey, r.hkey, hty)
    | Nop (op, l) -> Hashtbl.hash (op, List.map (fun sv -> sv.hkey) l, hty)
    | Ite (c, t, e) -> Hashtbl.hash (c.hkey, t.hkey, e.hkey, hty)
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
  | Bool _ | Int _ | Float _ | BitVec _ -> sv
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
  | Ite (c, t, e) ->
      let c' = subst subst_var c in
      let t' = subst subst_var t in
      let e' = subst subst_var e in
      if equal c c' && equal t t' && equal e e' then sv
      else Ite (c', t', e') <| sv.node.ty

(** {2 Booleans} *)

let v_true = Bool true <| TBool
let v_false = Bool false <| TBool

(** {2 Integers} *)

let int_z z = Int z <| TInt
let int i = int_z (Z.of_int i)
let zero = int_z Z.zero
let one = int_z Z.one

(** {2 Bitvectors} *)

let bitvec n bv = BitVec bv <| t_bv n

(** {2 Floats} *)

let float fp f = Float f <| t_f fp
let float_f fp f = Float (Float.to_string f) <| t_f fp
let float_like v f = Float (Float.to_string f) <| v.node.ty

let fp_of v =
  match v.node.ty with
  | TFloat fp -> fp
  | _ -> Fmt.failwith "Unsupported float type"

let f16 f = float_f F16 f
let f32 f = float_f F32 f
let f64 f = float_f F64 f
let f128 f = float_f F128 f

(** {2 Utils} *)

let rec bv_extract from_ to_ v =
  let size = to_ - from_ + 1 in
  match v.node.kind with
  | BitVec bv ->
      let to_ = to_ + 1 in
      let bv = Z.((bv asr from_) land pred (one lsl to_)) in
      bitvec size bv
  | Binop (((BitAnd | BitOr | BitXor) as bop), v1, v2) ->
      let v1 = bv_extract from_ to_ v1 in
      let v2 = bv_extract from_ to_ v2 in
      mk_commut_binop bop v1 v2 <| t_bv size
  | Unop (BvOfInt, v) when from_ = 0 -> Unop (BvOfInt, v) <| t_bv size
  | _ -> Unop (BvExtract (from_, to_), v) <| t_bv size

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
  | _, _ when equal v1 v2 -> v1
  | Bool false, _ | _, Bool false -> v_false
  | Bool true, _ -> v2
  | _, Bool true -> v1
  | _ -> mk_commut_binop And v1 v2 <| TBool

let conj l = List.fold_left and_ v_true l

let rec not sv =
  if equal sv v_true then v_false
  else if equal sv v_false then v_true
  else
    match sv.node.kind with
    | Unop (Not, sv) -> sv
    | Binop (Lt, v1, v2) -> Binop (Leq, v2, v1) <| TBool
    | Binop (Leq, v1, v2) -> Binop (Lt, v2, v1) <| TBool
    | Binop (Or, v1, v2) -> mk_commut_binop And (not v1) (not v2) <| TBool
    | _ -> Unop (Not, sv) <| TBool

let rec sem_eq v1 v2 =
  if equal v1 v2 && Stdlib.not (is_float v1.node.ty) then v_true
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
    | Binop (Plus, v1, { node = { kind = Int x; _ }; _ }), Int y
    | Binop (Plus, { node = { kind = Int x; _ }; _ }, v1), Int y
    | Int y, Binop (Plus, v1, { node = { kind = Int x; _ }; _ })
    | Int y, Binop (Plus, { node = { kind = Int x; _ }; _ }, v1) ->
        sem_eq v1 (int_z @@ Z.sub y x)
    | Int y, Binop (Times, { node = { kind = Int x; _ }; _ }, v1)
    | Int y, Binop (Times, v1, { node = { kind = Int x; _ }; _ })
    | Binop (Times, v1, { node = { kind = Int x; _ }; _ }), Int y
    | Binop (Times, { node = { kind = Int x; _ }; _ }, v1), Int y ->
        if Z.equal Z.zero x then bool (Z.equal Z.zero y)
        else if Z.(equal zero (rem y x)) then sem_eq v1 (int_z Z.(y / x))
        else v_false
    | Unop (IntOfBool, v1), Int z -> if Z.equal Z.zero z then not v1 else v1
    (* Reduce  (X & #x...N) = #x...M to (X & #xN) = #xM *)
    | Binop (BitAnd, _, _), _ | _, Binop (BitAnd, _, _) -> (
        let rec msb_of v =
          match v.node.kind with
          | BitVec v when Z.(v > zero) -> Some (Z.log2up v)
          | BitVec v when Z.(equal v zero) -> Some 0
          | Binop (BitAnd, bv1, bv2) ->
              Option.merge min (msb_of bv1) (msb_of bv2)
          | _ -> None
        in
        let current_size = size_of_bv v1.node.ty in
        let msb = Option.map2 max (msb_of v1) (msb_of v2) in
        match msb with
        | Some msb when msb <> current_size - 1 ->
            let v1 = bv_extract 0 msb v1 in
            let v2 = bv_extract 0 msb v2 in
            sem_eq v1 v2
        | _ ->
            (* regular sem_eq *)
            if equal v1 v2 then v_true else mk_commut_binop Eq v1 v2 <| TBool)
    | Unop (IntOfBv _, bv1), Unop (IntOfBv _, bv2) -> sem_eq bv1 bv2
    | Unop (IntOfBv _, bv), Int n | Int n, Unop (IntOfBv _, bv) ->
        let size = size_of_bv bv.node.ty in
        let n = if Z.geq n Z.zero then n else Z.neg n in
        sem_eq bv (BitVec n <| t_bv size)
    | _ -> mk_commut_binop Eq v1 v2 <| TBool

let sem_eq_untyped v1 v2 =
  if equal_ty v1.node.ty v2.node.ty then sem_eq v1 v2 else v_false

let or_ v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Bool true, _ | _, Bool true -> v_true
  | Bool false, _ -> v2
  | _, Bool false -> v1
  | _ -> mk_commut_binop Or v1 v2 <| TBool

let rec split_ands (sv : t) (f : t -> unit) : unit =
  match sv.node.kind with
  | Binop (And, s1, s2) ->
      split_ands s1 f;
      split_ands s2 f
  | _ -> f sv

let distinct l =
  (* [Distinct l] when l is empty or of size 1 is always true *)
  match l with
  | [] | [ _ ] -> v_true
  | l ->
      let cross_product = List.to_seq l |> Seq.self_cross_product in
      let sure_distinct =
        Seq.for_all (fun (a, b) -> sure_neq a b) cross_product
      in
      if sure_distinct then v_true else Nop (Distinct, l) <| TBool

let ite guard if_ else_ =
  match guard.node.kind with
  | Bool true -> if_
  | Bool false -> else_
  | _ -> Ite (guard, if_, else_) <| if_.node.ty

(** {2 Integers} *)

let int_of_bool b =
  if equal v_true b then one
  else if equal v_false b then zero
  else Unop (IntOfBool, b) <| TInt

(* Negates a boolean that is in integer form (i.e. 0 for false, anything else is true) *)
let not_int_bool sv =
  match sv.node.kind with
  | Int z -> if Z.equal z Z.zero then one else zero
  | Unop (IntOfBool, sv') -> int_of_bool (not sv')
  | _ -> int_of_bool (sem_eq sv zero)

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
  | Binop (Plus, v1, { node = { kind = Int x; _ }; _ }), Int y
  | Binop (Plus, { node = { kind = Int x; _ }; _ }, v1), Int y ->
      lt v1 (int_z @@ Z.sub y x)
  | Int y, Binop (Plus, v1, { node = { kind = Int x; _ }; _ })
  | Int y, Binop (Plus, { node = { kind = Int x; _ }; _ }, v1) ->
      lt (int_z @@ Z.sub y x) v1
  | Binop (Minus, v1, { node = { kind = Int x; _ }; _ }), Int y ->
      lt v1 (int_z @@ Z.add y x)
  | Binop (Minus, { node = { kind = Int x; _ }; _ }, v1), Int y ->
      lt (int_z @@ Z.sub x y) v1
  | Int y, Binop (Minus, v1, { node = { kind = Int x; _ }; _ }) ->
      lt (int_z @@ Z.add y x) v1
  | Int y, Binop (Minus, { node = { kind = Int x; _ }; _ }, v1) ->
      lt v1 (int_z @@ Z.sub x y)
  | Int y, Binop (Times, { node = { kind = Int x; _ }; _ }, v1')
  | Int y, Binop (Times, v1', { node = { kind = Int x; _ }; _ }) ->
      if Z.equal Z.zero x then bool (Z.lt y Z.zero)
      else
        let op = if Z.divisible y x || Z.(y > zero) then lt else leq in
        if Z.(zero < x) then op (int_z Z.(y / x)) v1'
        else op v1' (int_z Z.(y / x))
  | Binop (Times, v1', { node = { kind = Int x; _ }; _ }), Int y
  | Binop (Times, { node = { kind = Int x; _ }; _ }, v1'), Int y ->
      if Z.equal Z.zero x then bool (Z.lt Z.zero y)
      else
        let op = if Z.divisible y x || Z.(y < zero) then lt else leq in
        if Z.(zero < x) then op v1' (int_z Z.(y / x))
        else op (int_z Z.(y / x)) v1'
  | _ -> Binop (Lt, v1, v2) <| TBool

and leq v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Int i1, Int i2 -> bool (Z.leq i1 i2)
  | _, _ when equal v1 v2 -> v_true
  | _, Binop (Plus, v2, v3) when equal v1 v2 -> leq zero v3
  | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v1 v3 -> leq v2 v4
  | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v2 v3 -> leq v1 v4
  | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v1 v4 -> leq v2 v3
  | Binop (Plus, v1, v2), Binop (Plus, v3, v4) when equal v2 v4 -> leq v1 v3
  | Binop (Plus, v1, { node = { kind = Int x; _ }; _ }), Int y
  | Binop (Plus, { node = { kind = Int x; _ }; _ }, v1), Int y ->
      leq v1 (int_z @@ Z.sub y x)
  | Int y, Binop (Plus, v1, { node = { kind = Int x; _ }; _ })
  | Int y, Binop (Plus, { node = { kind = Int x; _ }; _ }, v1) ->
      leq (int_z @@ Z.sub y x) v1
  | Binop (Minus, v1, { node = { kind = Int x; _ }; _ }), Int y ->
      leq v1 (int_z @@ Z.add y x)
  | Binop (Minus, { node = { kind = Int x; _ }; _ }, v1), Int y ->
      leq (int_z @@ Z.sub x y) v1
  | Int y, Binop (Minus, v1, { node = { kind = Int x; _ }; _ }) ->
      leq (int_z @@ Z.add y x) v1
  | Int y, Binop (Minus, { node = { kind = Int x; _ }; _ }, v1) ->
      leq v1 (int_z @@ Z.sub x y)
  | Int y, Binop (Times, { node = { kind = Int x; _ }; _ }, v1')
  | Int y, Binop (Times, v1', { node = { kind = Int x; _ }; _ }) ->
      if Z.equal Z.zero x then bool (Z.lt y Z.zero)
      else
        let op = if Z.divisible y x || Z.(y < zero) then leq else lt in
        if Z.(zero < x) then op (int_z Z.(y / x)) v1'
        else op v1' (int_z Z.(y / x))
  | Binop (Times, v1', { node = { kind = Int x; _ }; _ }), Int y
  | Binop (Times, { node = { kind = Int x; _ }; _ }, v1'), Int y ->
      if Z.equal Z.zero x then bool (Z.lt y Z.zero)
      else
        let op = if Z.divisible y x || Z.(y > zero) then leq else lt in
        if Z.(zero < x) then op v1' (int_z Z.(y / x))
        else op (int_z Z.(y / x)) v1'
  | _ -> Binop (Leq, v1, v2) <| TBool

let geq v1 v2 = leq v2 v1
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

let rec minus v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v2 zero -> v1
  | Int i1, Int i2 -> int_z (Z.sub i1 i2)
  | Binop (Minus, { node = { kind = Int i2; _ }; _ }, v1), Int i1 ->
      minus (int_z (Z.sub i2 i1)) v1
  | Binop (Minus, v1, { node = { kind = Int i2; _ }; _ }), Int i1 ->
      minus v1 (int_z (Z.sub i2 i1))
  | _ -> Binop (Minus, v1, v2) <| TInt

let times v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v1 zero || equal v2 zero -> zero
  | _, _ when equal v1 one -> v2
  | _, _ when equal v2 one -> v1
  | Int i1, Int i2 -> int_z (Z.mul i1 i2)
  | _ -> mk_commut_binop Times v1 v2 <| TInt

let div v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v2 one -> v1
  | Int i1, Int i2 -> int_z (Z.div i1 i2)
  | _ -> Binop (Div, v1, v2) <| TInt

let rec is_mod v n =
  match v.node.kind with
  | Int i1 -> Z.equal (Z.( mod ) i1 n) Z.zero
  | Binop (Plus, v2, v3) -> is_mod v2 n && is_mod v3 n
  | Binop (Minus, v2, v3) -> is_mod v2 n && is_mod v3 n
  | Binop (Times, v2, v3) -> is_mod v2 n || is_mod v3 n
  | _ -> false

let rec rem v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, Int i2 when is_mod v1 i2 -> int_z Z.zero
  | Int i1, Int i2 -> int_z (Z.rem i1 i2)
  | Binop (Times, v1, n), Binop (Times, v2, m) when equal n m ->
      times n (rem v1 v2)
  | Binop (Times, n, v1), Binop (Times, m, v2) when equal n m ->
      times n (rem v1 v2)
  | _ -> Binop (Rem, v1, v2) <| v1.node.ty

let mod_ v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v2 one -> zero
  | _, Int i2 when is_mod v1 i2 -> int_z Z.zero
  | Int i1, Int i2 ->
      (* OCaml's mod computes the remainer... *)
      let rem = Z.( mod ) i1 i2 in
      if Z.lt rem Z.zero then int_z (Z.add rem i2) else int_z rem
  | _ -> Binop (Mod, v1, v2) <| TInt

let neg v =
  match (v.node.ty, v.node.kind) with
  | TInt, Int i -> int_z (Z.neg i)
  | TFloat fp, _ -> Binop (FMinus, float fp "0.0", v) <| v.node.ty
  | _ -> minus zero v

(** {2 Bit vectors} *)

let raw_bit_and n v1 v2 =
  let covers_bitwidth z =
    Z.(z > one && popcount (succ z) = 1 && log2 (succ z) = n)
  in
  match (v1.node.kind, v2.node.kind) with
  | BitVec mask, _ when Z.(equal mask zero) -> v1
  | _, BitVec mask when Z.(equal mask zero) -> v2
  | BitVec mask, _ when covers_bitwidth mask -> v2
  | _, BitVec mask when covers_bitwidth mask -> v1
  | _, _ -> mk_commut_binop BitAnd v1 v2 <| t_bv n

let raw_bit_or n v1 v2 = mk_commut_binop BitOr v1 v2 <| t_bv n
let raw_bit_xor n v1 v2 = mk_commut_binop BitXor v1 v2 <| t_bv n
let raw_bit_shl n v1 v2 = Binop (BitShl, v1, v2) <| t_bv n
let raw_bit_shr n v1 v2 = Binop (BitShr, v1, v2) <| t_bv n
let raw_bv_plus n v1 v2 = mk_commut_binop BvPlus v1 v2 <| t_bv n
let raw_bv_minus n v1 v2 = Binop (BvMinus, v1, v2) <| t_bv n

let bv_of_float n v =
  match (v.node.ty, v.node.kind, n) with
  | TFloat _, Unop (FloatOfBv, v), _ -> v
  | TFloat F32, Float f, 32 ->
      let z = Z.of_int32 (Int32.bits_of_float (Float.of_string f)) in
      bitvec n z
  | TFloat F64, Float f, 64 ->
      let z = Z.of_int64 (Int64.bits_of_float (Float.of_string f)) in
      bitvec n z
  | _, _, _ -> Unop (BvOfFloat n, v) <| t_bv n

let rec bv_of_int n v =
  let bv_of_int = bv_of_int n in
  let is_2pow z = Z.(z > one && popcount z = 1) in
  match v.node.kind with
  | Unop (IntOfBv _, v) ->
      if size_of_bv v.node.ty = n then v
      else if size_of_bv v.node.ty > n then bv_extract 0 (n - 1) v
      else failwith "can't extend a bitvector yet"
  | Int z ->
      let z = if Z.geq z Z.zero then z else Z.neg z in
      bitvec n z
  | Binop (Mod, v, { node = { kind = Int mask; _ }; _ }) when is_2pow mask ->
      raw_bit_and n (bv_of_int v) (bitvec n (Z.pred mask))
  | Binop (Mod, { node = { kind = Int mask; _ }; _ }, v) when is_2pow mask ->
      raw_bit_and n (bv_of_int v) (bitvec n (Z.pred mask))
  | Binop (Times, { node = { kind = Int mask; _ }; _ }, v) when is_2pow mask ->
      let fac = bitvec n (Z.of_int (Z.log2 mask)) in
      raw_bit_shl n (bv_of_int v) fac
  | Binop (Div, v, { node = { kind = Int mask; _ }; _ }) when is_2pow mask ->
      let fac = bitvec n (Z.of_int (Z.log2 mask)) in
      raw_bit_shr n (bv_of_int v) fac
  | Binop (Plus, l, r) -> raw_bv_plus n (bv_of_int l) (bv_of_int r)
  | Binop (Minus, l, r) -> raw_bv_minus n (bv_of_int l) (bv_of_int r)
  | _ -> Unop (BvOfInt, v) <| t_bv n

let rec int_of_bv signed v =
  (* Tests if z is of the form 1+0+ *)
  let is_left_mask z =
    if Z.(z <= one) then false
    else
      let size = Z.log2up z in
      let zeroes = size - Z.popcount z in
      if zeroes = 0 then false
      else if size <> size_of_bv v.node.ty then false
      else
        let mask = Z.pred @@ Z.shift_left Z.one zeroes in
        Z.(Z.equal (z land mask) zero)
  in
  match v.node.kind with
  | Unop (BvOfInt, v) -> v
  | Binop (BvPlus, l, r) -> plus (int_of_bv signed l) (int_of_bv signed r)
  | Binop (BvMinus, l, r) -> minus (int_of_bv signed l) (int_of_bv signed r)
  | Binop (BitShl, l, { node = { kind = Int n; _ }; _ }) ->
      let pow = int_z @@ Z.shift_left Z.one (Z.to_int n) in
      times l pow
  | Binop (BitShr, l, { node = { kind = Int n; _ }; _ }) ->
      let pow = int_z @@ Z.shift_right Z.one (Z.to_int n) in
      div l pow
  | Binop (BitAnd, v, { node = { kind = BitVec mask; _ }; _ })
    when is_left_mask mask ->
      (* left mask, of the form 1+0+. e.g. for 1111 1000, this is equivalent to dividing by 2^3,
         and then re-multiplying, avoiding the bitvector.  *)
      let zeroes = Z.log2 mask - Z.popcount mask in
      let pow = int_z @@ Z.shift_left Z.one zeroes in
      let v = int_of_bv signed v in
      times (div v pow) pow
  | _ -> Unop (IntOfBv signed, v) <| t_int

let float_of_bv v =
  match (v.node.ty, v.node.kind) with
  | _, Unop (BvOfFloat _, v) -> v
  | TBitVector n, _ -> Unop (FloatOfBv, v) <| t_f (FloatPrecision.of_size n)
  | _ -> failwith "Expected a bitvector value in float_of_bv"

let float_of_int fp v =
  match (v.node.kind, fp) with
  (* We force the integer to a float, to account for precision loss.
     Ideally we should do this for every float precision, but we would need support for
     f16, f32 and f128 in OCaml.  *)
  | Int i, FloatPrecision.F64 -> float fp (string_of_float (Z.to_float i))
  | _ -> float_of_bv (bv_of_int (FloatPrecision.size fp) v)

let int_of_float n v =
  match v.node.kind with
  | Float f -> int_z (Z.of_float (Float.of_string f))
  | _ -> int_of_bv true (bv_of_float n v)

let bit_and ~size ~signed v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Int i1, Int i2 -> int_z (Z.( land ) i1 i2)
  | Bool b1, Bool b2 -> bool (b1 && b2)
  | _ ->
      let v1_bv = bv_of_int size v1 in
      let v2_bv = bv_of_int size v2 in
      let bv = raw_bit_and size v1_bv v2_bv in
      int_of_bv signed bv

let bit_or ~size ~signed v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Int i1, Int i2 -> int_z (Z.( lor ) i1 i2)
  | Bool b1, Bool b2 -> bool (b1 || b2)
  | _ ->
      let v1_bv = bv_of_int size v1 in
      let v2_bv = bv_of_int size v2 in
      let bv = raw_bit_or size v1_bv v2_bv in
      int_of_bv signed bv

let bit_xor ~size ~signed v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Int i1, Int i2 -> int_z (Z.( lxor ) i1 i2)
  | Bool b1, Bool b2 -> bool (b1 <> b2)
  | _ ->
      let v1_bv = bv_of_int size v1 in
      let v2_bv = bv_of_int size v2 in
      let bv = raw_bit_xor size v1_bv v2_bv in
      int_of_bv signed bv

let bit_shl ~size ~signed v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Int i1, Int i2 ->
      let shifted = Z.( lsl ) i1 (Z.to_int i2) in
      let masked = Z.( land ) shifted (Z.pred (Z.( lsl ) Z.one size)) in
      int_z masked
  | _, Int i2 ->
      let max = Z.( lsl ) Z.one size in
      let shifted = times v1 (int_z (Z.( lsl ) Z.one (Z.to_int i2))) in
      let masked = mod_ shifted (int_z max) in
      masked
  | _ ->
      let v1_bv = bv_of_int size v1 in
      let v2_bv = bv_of_int size v2 in
      let bv = raw_bit_shl size v1_bv v2_bv in
      int_of_bv signed bv

let bit_shr ~size ~signed v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Int i1, Int i2 ->
      let shifted = Z.( asr ) i1 (Z.to_int i2) in
      let masked = Z.( land ) shifted (Z.pred (Z.( lsl ) Z.one size)) in
      int_z masked
  | _ ->
      let v1_bv = bv_of_int size v1 in
      let v2_bv = bv_of_int size v2 in
      let bv = raw_bit_shr size v1_bv v2_bv in
      int_of_bv signed bv

(** {2 Floating point ops} *)

let eq_f v1 v2 = mk_commut_binop FEq v1 v2 <| TBool

let lt_f v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Float f1, Float f2 -> bool (float_of_string f1 < float_of_string f2)
  | _ -> Binop (FLt, v1, v2) <| TBool

let leq_f v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Float f1, Float f2 -> bool (float_of_string f1 <= float_of_string f2)
  | _ -> Binop (FLeq, v1, v2) <| TBool

let gt_f v1 v2 = lt_f v2 v1
let geq_f v1 v2 = leq_f v2 v1
let plus_f v1 v2 = mk_commut_binop FPlus v1 v2 <| v1.node.ty
let minus_f v1 v2 = Binop (FMinus, v1, v2) <| v1.node.ty
let div_f v1 v2 = Binop (FDiv, v1, v2) <| v1.node.ty
let times_f v1 v2 = mk_commut_binop FTimes v1 v2 <| v1.node.ty
let rem_f v1 v2 = Binop (FRem, v1, v2) <| v1.node.ty

let abs_f v =
  match v.node.kind with
  | Unop (FAbs, _) -> v
  | _ -> Unop (FAbs, v) <| v.node.ty

(* FIXME: all of these reductions are unsound for floats that aren't F64, I think *)
let is_floatclass fc =
 fun sv ->
  match sv.node.kind with
  | Float f ->
      bool (FloatClass.as_fpclass fc = classify_float (float_of_string f))
  | _ -> Unop (FIs fc, sv) <| TBool

let is_normal = is_floatclass Normal
let is_subnormal = is_floatclass Subnormal
let is_infinite = is_floatclass Infinite
let is_nan = is_floatclass NaN
let is_zero = is_floatclass Zero
let float_round rm sv = Unop (FRound rm, sv) <| sv.node.ty

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

  let decompose p =
    match p.node.kind with Ptr (l, o) -> (l, o) | _ -> (loc p, ofs p)

  let add_ofs p o =
    let loc, ofs = decompose p in
    mk loc (plus ofs o)

  let null = mk null_loc zero
  let is_null p = sem_eq p null
  let is_at_null_loc p = is_null_loc (loc p)
end

(** {2 Sequences} *)

module SSeq = struct
  let mk ~seq_ty l = Seq l <| seq_ty

  let inner_ty ty =
    match ty with TSeq ty -> ty | _ -> failwith "Expected a sequence type"
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
  let ( ~- ) = neg
  let ( *@ ) = times
  let ( /@ ) = div
  let ( %@ ) = mod_
  let ( ==.@ ) = eq_f
  let ( >.@ ) = gt_f
  let ( >=.@ ) = geq_f
  let ( <.@ ) = lt_f
  let ( <=.@ ) = leq_f
  let ( +.@ ) = plus_f
  let ( -.@ ) = minus_f
  let ( *.@ ) = times_f
  let ( /.@ ) = div_f
end

module Syntax = struct
  module Sym_int_syntax = struct
    let mk_int = int
    let zero = zero
    let one = one
  end
end
