open Svalue

type _ Effect.t += Eval_var : Var.t * Svalue.ty -> t Effect.t

let eval_var (v : Var.t) (ty : Svalue.ty) : t =
  Effect.perform (Eval_var (v, ty))

let rec eval (x : t) : t =
  match x.node.kind with
  | Var v -> eval_var v x.node.ty
  | Bool _ | Int _ | Float _ | BitVec _ -> x
  | Ptr (l, o) ->
      let nl = eval l in
      let no = eval o in
      if l == nl && o == no then x else Ptr.mk (eval l) (eval o)
  | Unop (unop, v) -> (
      let nv = eval v in
      if v == nv then x
      else
        match unop with
        | Unop.Not -> not nv
        | FAbs -> abs_f nv
        | GetPtrLoc -> Ptr.loc nv
        | GetPtrOfs -> Ptr.ofs nv
        | IntOfBool -> int_of_bool nv
        | BvOfFloat n -> bv_of_float n nv
        | BvOfInt ->
            let n = size_of_bv x.node.ty in
            bv_of_int n nv
        | IntOfBv signed -> int_of_bv signed nv
        | FloatOfBv -> float_of_bv nv
        | BvExtract (from, to_) -> bv_extract from to_ nv
        | BvExtend by ->
            let n = size_of_bv x.node.ty in
            bv_extend (n + by) x
        | FIs fc -> is_floatclass fc nv
        | FRound rm -> float_round rm nv)
  | Binop (binop, v1, v2) -> (
      (* TODO: for binops that may short-circuit such as || or &&,
    we could do this without evaluating both sides, and deciding if any
      of either side evaluates properly to e.g. true/false *)
      let nv1 = eval v1 in
      let nv2 = eval v2 in
      if v1 == nv1 && v2 == nv2 then x
      else
        match binop with
        | Binop.And -> and_ nv1 nv2
        | Or -> or_ nv1 nv2
        | Eq -> sem_eq nv1 nv2
        | Leq -> leq nv1 nv2
        | Lt -> lt nv1 nv2
        | Plus -> plus nv1 nv2
        | Minus -> minus nv1 nv2
        | Times -> times nv1 nv2
        | Div -> div nv1 nv2
        | Rem -> rem nv1 nv2
        | Mod -> mod_ nv1 nv2
        | FEq -> eq_f nv1 nv2
        | FLeq -> leq_f nv1 nv2
        | FLt -> lt_f nv1 nv2
        | FPlus -> plus_f nv1 nv2
        | FMinus -> minus_f nv1 nv2
        | FTimes -> times_f nv1 nv2
        | FDiv -> div_f nv1 nv2
        | FRem -> rem_f nv1 nv2
        | BvPlus ->
            let n = size_of_bv x.node.ty in
            raw_bv_plus n nv1 nv2
        | BvMinus ->
            let n = size_of_bv x.node.ty in
            raw_bv_minus n nv1 nv2
        | BitAnd ->
            let n = size_of_bv x.node.ty in
            raw_bit_and n nv1 nv2
        | BitOr ->
            let n = size_of_bv x.node.ty in
            raw_bit_or n nv1 nv2
        | BitXor ->
            let n = size_of_bv x.node.ty in
            raw_bit_xor n nv1 nv2
        | BitShl ->
            let n = size_of_bv x.node.ty in
            raw_bit_shl n nv1 nv2
        | BitShr ->
            let n = size_of_bv x.node.ty in
            raw_bit_shr n nv1 nv2)
  | Nop (nop, l) -> (
      let l, changed = List.map_changed eval l in
      if Stdlib.not changed then x else match nop with Distinct -> distinct l)
  | Ite (guard, then_, else_) ->
      let guard = eval guard in
      if equal guard v_true then eval then_
      else if equal guard v_false then eval else_
      else ite guard (eval then_) (eval else_)
  | Seq l ->
      let l, changed = List.map_changed eval l in
      if Stdlib.not changed then x else Svalue.SSeq.mk ~seq_ty:x.node.ty l

let eval ~(eval_var : Var.t -> Svalue.ty -> t option) (x : t) : t option =
  try Some (eval x) with
  | Division_by_zero -> None
  | effect Eval_var (v, ty), k -> (
      match eval_var v ty with
      | Some v -> Effect.Deep.continue k v
      | None -> None)
