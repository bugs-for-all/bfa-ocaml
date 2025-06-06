open Rustsymex
open Rustsymex.Syntax
open Typed.Infix
open Typed.Syntax
open Charon
open Charon_util
module T = Typed.T

module Make (Heap : Heap_intf.S) = struct
  module Core = Core.M (Heap)
  module Std_funs = Builtins.Eval.M (Heap)
  module Sptr = Heap.Sptr
  module Encoder = Encoder.Make (Sptr)

  let pp_rust_val = pp_rust_val Sptr.pp

  exception Unsupported of (string * Meta.span)

  type full_ptr = Sptr.t Charon_util.full_ptr
  type state = Heap.t
  type store = (full_ptr option * Types.ty) Store.t

  let pp_full_ptr = Charon_util.pp_full_ptr Sptr.pp

  type ('err, 'fixes) fun_exec =
    args:Sptr.t rust_val list ->
    state:state ->
    (Sptr.t rust_val * state, 'err, 'fixes) Result.t

  let get_variable var_id (store : store) state =
    match Store.find_value var_id store with
    | Some ptr ->
        L.debug (fun m ->
            m "Variable %a has pointer %a" Expressions.pp_var_id var_id
              pp_full_ptr ptr);
        Result.ok (ptr, state)
    | None -> Heap.error `DeadVariable state

  let get_variable_and_ty var_id (store : store) state =
    match Store.find_opt var_id store with
    | Some ptr_and_ty -> Result.ok (ptr_and_ty, state)
    | None -> Heap.error `DeadVariable state

  let alloc_stack (locals : GAst.locals) args st :
      (store * (full_ptr * Types.ty) list * state, 'e, 'm) Result.t =
    if List.compare_length_with args locals.arg_count <> 0 then
      Fmt.failwith "Function expects %d arguments, but got %d" locals.arg_count
        (List.length args);
    (* create a store with all types *)
    let store =
      List.fold_left
        (fun st (local : GAst.local) ->
          Store.add local.index (None, local.var_ty) st)
        Store.empty locals.locals
    in
    (* allocate arguments and return value, updating store *)
    let alloc_locs = List.take (1 + locals.arg_count) locals.locals in
    let tys =
      List.map (fun ({ var_ty; _ } : GAst.local) -> var_ty) alloc_locs
    in
    let** ptrs, st = Heap.alloc_tys tys st in
    let tys_ptrs = List.combine alloc_locs ptrs in
    let store =
      List.fold_left
        (fun store ((local : GAst.local), ptr) ->
          Store.add local.index (Some ptr, local.var_ty) store)
        store tys_ptrs
    in
    (* store values for the arguments *)
    let tys_ptrs = List.tl tys_ptrs in
    let++ protected, st =
      Result.fold_list tys_ptrs ~init:([], st)
        ~f:(fun (protected, st) ({ index; var_ty = ty; _ }, ptr) ->
          let index = Expressions.LocalId.to_int index in
          let value = List.nth args (index - 1) in
          (* Passed references must be protected! *)
          let** value, protected', st =
            match (value, ty) with
            | Ptr ptr, TRef (_, subty, mut) ->
                let++ ptr', st = Heap.protect ptr subty mut st in
                (Ptr ptr', (ptr', subty) :: protected, st)
            | _ -> Result.ok (value, protected, st)
          in
          let** (), st = Heap.store ptr ty value st in
          (* Ensure all passed references are valid, even nested ones! *)
          let ptr_tys = Layout.ref_tys_in value ty in
          let++ (), st =
            Result.fold_list ptr_tys ~init:((), st)
              ~f:(fun ((), st) (ptr, ty) -> Heap.tb_load ptr ty st)
          in
          (protected', st))
    in
    (store, protected, st)

  (** [dealloc_store ?protected_address store protected st] Deallocates the
      locations in [st] used for the variables in [store]; if
      [protected_address] is provided, will not deallocate that location (this
      is used e.g. for globals, that return a &'static reference). Will also
      remove the protectors from the pointers [protected] that were given at the
      function's entry. *)
  let dealloc_store ?protected_address store protected st =
    let** (), st =
      Result.fold_list protected ~init:((), st) ~f:(fun ((), st) (ptr, ty) ->
          Heap.unprotect ptr ty st)
    in
    Result.fold_list (Store.bindings store) ~init:((), st)
      ~f:(fun ((), st) (_, (ptr, _)) ->
        match (ptr, protected_address) with
        | None, _ -> Result.ok ((), st)
        | Some ptr, None -> Heap.free ptr st
        | Some ((ptr, _) as fptr), Some protect ->
            if%sat Sptr.sem_eq ptr protect then Result.ok ((), st)
            else Heap.free fptr st)

  let resolve_constant (const : Expressions.constant_expr) state =
    match const.value with
    | CLiteral (VScalar scalar) ->
        Result.ok (Base (value_of_scalar scalar), state)
    | CLiteral (VBool b) ->
        Result.ok (Base (if b then Typed.one else Typed.zero), state)
    | CLiteral (VChar c) -> Result.ok (Base (Typed.int (Uchar.to_int c)), state)
    | CLiteral (VFloat { float_value; float_ty }) ->
        let fp = float_precision float_ty in
        Result.ok (Base (Typed.float fp float_value), state)
    | CLiteral (VStr str) -> (
        let** ptr_opt, state = Heap.load_str_global str state in
        match ptr_opt with
        | Some v -> Result.ok (Ptr v, state)
        | None ->
            (* We "cheat" and model strings as an array of chars, with &str a slice *)
            let len = String.length str in
            let chars =
              String.to_bytes str
              |> Bytes.fold_left
                   (fun l c -> Base (Typed.int (Char.code c)) :: l)
                   []
              |> List.rev
            in
            let char_arr = Array chars in
            let str_ty : Types.ty = mk_array_ty (TLiteral (TInteger U8)) len in
            let** (ptr, _), state = Heap.alloc_ty str_ty state in
            let ptr = (ptr, Some (Typed.int len)) in
            let** (), state = Heap.store ptr str_ty char_arr state in
            let++ (), state = Heap.store_str_global str ptr state in
            (Ptr ptr, state))
    | CFnPtr fn_ptr -> Result.ok (ConstFn fn_ptr, state)
    | CLiteral (VByteStr _) -> not_impl "TODO: resolve const ByteStr"
    | CTraitConst _ -> not_impl "TODO: resolve const TraitConst"
    | CRawMemory _ -> not_impl "TODO: resolve const RawMemory"
    | COpaque msg -> Fmt.kstr not_impl "Opaque constant: %s" msg
    | CVar _ -> not_impl "TODO: resolve const Var (mono error)"

  (** Resolves a place to a pointer, in the form of a rust_val. We use rust_val
      rather than T.sptr Typed.t, to be able to handle fat pointers; however
      there is the guarantee that this function returns either a Base or a
      FatPointer value. *)
  let rec resolve_place ~store state ({ kind; ty } : Expressions.place) :
      (full_ptr * state, 'e, 'm) Result.t =
    match kind with
    (* Just a local *)
    | PlaceLocal v -> get_variable v store state
    (* Dereference a pointer *)
    | PlaceProjection (base, Deref) -> (
        let** ptr, state = resolve_place ~store state base in
        L.debug (fun f ->
            f "Dereferencing ptr %a of %a" pp_full_ptr ptr pp_ty base.ty);
        let** v, state = Heap.load ptr base.ty state in
        match v with
        | Ptr v ->
            L.debug (fun f ->
                f "Dereferenced pointer %a to pointer %a" pp_full_ptr ptr
                  pp_full_ptr v);
            let pointee = Charon_util.get_pointee base.ty in
            let++ () = Heap.check_ptr_align (fst v) pointee state in
            (v, state)
        | Base off ->
            let* off = cast_checked ~ty:Typed.t_int off in
            let ptr = Sptr.null_ptr_of off in
            Result.ok ((ptr, None), state)
        | _ -> not_impl "Unexpected value when dereferencing place")
    | PlaceProjection (base, Field (kind, field)) ->
        let** (ptr, meta), state = resolve_place ~store state base in
        L.debug (fun f ->
            f "Projecting field %a (kind %a) for %a" Types.pp_field_id field
              Expressions.pp_field_proj_kind kind Sptr.pp ptr);
        let** ptr' =
          Sptr.project base.ty kind field ptr |> Heap.lift_err state
        in
        L.debug (fun f ->
            f
              "Dereferenced ADT projection %a, field %a, with pointer %a to \
               pointer %a"
              Expressions.pp_field_proj_kind kind Types.pp_field_id field
              Sptr.pp ptr Sptr.pp ptr');
        if not @@ Layout.is_inhabited ty then Heap.error `RefToUninhabited state
        else Result.ok ((ptr', meta), state)
    | PlaceProjection (base, ProjIndex (idx, from_end)) ->
        let** (ptr, meta), state = resolve_place ~store state base in
        let len =
          match (meta, base.ty) with
          (* Array with static size *)
          | None, TAdt (TBuiltin TArray, { const_generics = [ len ]; _ }) ->
              Typed.int @@ Charon_util.int_of_const_generic len
          | Some len, TAdt (TBuiltin TSlice, _) -> Typed.cast len
          | _ -> Fmt.failwith "Index projection: unexpected arguments"
        in
        let** idx, state = eval_operand ~store state idx in
        let idx = as_base_of ~ty:Typed.t_int idx in
        let idx = if from_end then len -@ idx else idx in
        if%sat 0s <=@ idx &&@ (idx <@ len) then (
          let** ptr' = Sptr.offset ~ty ptr idx |> Heap.lift_err state in
          L.debug (fun f ->
              f "Projected %a, index %a, to pointer %a" Sptr.pp ptr Typed.ppa
                idx Sptr.pp ptr');
          Result.ok ((ptr', None), state))
        else Heap.error `OutOfBounds state
    | PlaceProjection (base, Subslice (from, to_, from_end)) ->
        let** (ptr, meta), state = resolve_place ~store state base in
        let ty, len =
          match (meta, base.ty) with
          (* Array with static size *)
          | ( None,
              TAdt
                ( TBuiltin TArray,
                  { const_generics = [ len ]; types = [ ty ]; _ } ) ) ->
              (ty, Typed.int @@ Charon_util.int_of_const_generic len)
          | Some len, TAdt (TBuiltin TSlice, { types = [ ty ]; _ }) ->
              (ty, Typed.cast len)
          | _ -> Fmt.failwith "Index projection: unexpected arguments"
        in
        let** from, state = eval_operand ~store state from in
        let** to_, state = eval_operand ~store state to_ in
        let from = as_base_of ~ty:Typed.t_int from in
        let to_ = as_base_of ~ty:Typed.t_int to_ in
        let to_ = if from_end then len -@ to_ else to_ in
        if%sat 0s <=@ from &&@ (from <=@ to_) &&@ (to_ <=@ len) then (
          let** ptr' = Sptr.offset ~ty ptr from |> Heap.lift_err state in
          let slice_len = to_ -@ from in
          L.debug (fun f ->
              f "Projected %a, slice %a..%a%s, to pointer %a, len %a" Sptr.pp
                ptr Typed.ppa from Typed.ppa to_
                (if from_end then "(from end)" else "")
                Sptr.pp ptr' Typed.ppa slice_len);
          Result.ok ((ptr', Some slice_len), state))
        else Heap.error `OutOfBounds state

  and resolve_function ~store (fnop : GAst.fn_operand) state :
      (('err, 'fixes) fun_exec * state, 'err, 'fixes) Result.t =
    match fnop with
    | FnOpRegular { func = FunId (FRegular fid); _ }
    | FnOpRegular { func = TraitMethod (_, _, fid); _ } -> (
        let fundef = Crate.get_fun fid in
        L.info (fun g ->
            g "Resolved function call to %a" Crate.pp_name fundef.item_meta.name);
        match Std_funs.std_fun_eval fundef exec_fun with
        | Some fn -> Result.ok (fn, state)
        | None -> Result.ok (exec_fun fundef, state))
    | FnOpRegular { func = FunId (FBuiltin fn); generics } ->
        let fn = Std_funs.builtin_fun_eval fn generics in
        Result.ok (fn, state)
    | FnOpMove place ->
        let** fn_ptr_ptr, state = resolve_place ~store state place in
        let** fn_ptr, state =
          Heap.load ~is_move:true fn_ptr_ptr place.ty state
        in
        let fn_ptr = as_ptr fn_ptr in
        let** fn, state = Heap.lookup_fn fn_ptr state in
        let fnop : GAst.fn_operand = FnOpRegular fn in
        resolve_function ~store fnop state

  (** Resolves a global into a *pointer* Rust value to where that global is *)
  and resolve_global (g : Types.global_decl_id) state =
    let decl = Crate.get_global g in
    let** v_opt, state = Heap.load_global g state in
    match v_opt with
    | Some v -> Result.ok (v, state)
    | None ->
        (* Same as with strings -- here we need to somehow cache where we store the globals *)
        let fundef = Crate.get_fun decl.body in
        L.info (fun g ->
            g "Resolved global init call to %a" Crate.pp_name
              fundef.item_meta.name);
        let global_fn =
          match Std_funs.std_fun_eval fundef exec_fun with
          | Some fn -> fn
          | None -> exec_fun fundef
        in
        (* First we allocate the global and store it in the heap  *)
        let** ptr, state = Heap.alloc_ty decl.ty state in
        let** (), state = Heap.store_global g ptr state in
        (* And only after we compute it; this enables recursive globals *)
        let** v, state = global_fn ~args:[] ~state in
        let++ (), state = Heap.store ptr decl.ty v state in
        (ptr, state)

  and eval_operand ~store state (op : Expressions.operand) =
    match op with
    | Constant c ->
        let++ v, state = resolve_constant c state in
        (v, state)
    | (Move loc | Copy loc) when not (Layout.is_inhabited loc.ty) ->
        Heap.error `RefToUninhabited state
    | Move loc | Copy loc -> (
        let ty = loc.ty in
        match Layout.as_zst ty with
        | Some zst ->
            let** _, state = resolve_place ~store state loc in
            Result.ok (zst, state)
        | None ->
            let** ptr, state = resolve_place ~store state loc in
            let is_move =
              (* TODO: properly detect if ty has the Copy trait, in which case is_move is
             always false. *)
              match (op, ty) with
              | _, TLiteral _ -> false
              | Move _, _ -> true
              | _ -> false
            in
            let++ v, state = Heap.load ~is_move ptr ty state in
            (v, state))

  and eval_operand_list ~store state ops =
    let++ vs, state =
      Result.fold_list ops ~init:([], state) ~f:(fun (acc, state) op ->
          let++ new_res, state = eval_operand ~store state op in
          (new_res :: acc, state))
    in
    (List.rev vs, state)

  and eval_rvalue ~store state (expr : Expressions.rvalue) =
    let eval_operand = eval_operand ~store in
    match expr with
    | Use op -> eval_operand state op
    | RvRef (place, borrow) ->
        let** ptr, state = resolve_place ~store state place in
        let++ ptr', state = Heap.borrow ptr place.ty borrow state in
        (Ptr ptr', state)
    | Global { global_id; _ } ->
        let** ptr, state = resolve_global global_id state in
        let decl = Crate.get_global global_id in
        Heap.load ptr decl.ty state
    | GlobalRef ({ global_id; _ }, _mut) ->
        (* TODO: handle mutability *)
        let++ ptr, state = resolve_global global_id state in
        (Ptr ptr, state)
    | UnaryOp (op, e) -> (
        let** v, state = eval_operand state e in
        match op with
        | Not ->
            let v = as_base_of ~ty:Typed.t_int v in
            let* v' =
              match type_of_operand e with
              | TLiteral TBool -> return (Typed.not_int_bool v)
              | TLiteral
                  (TInteger ((Usize | U8 | U16 | U32 | U64 | U128) as ty)) ->
                  let max = Layout.max_value ty in
                  return (max -@ v)
              | TLiteral (TInteger (Isize | I8 | I16 | I32 | I64 | I128)) ->
                  return (~-v -@ 1s)
              | ty ->
                  Fmt.kstr not_impl "Unexpect type in UnaryOp.Neg: %a" pp_ty ty
            in
            Result.ok (Base v', state)
        | Neg -> (
            match type_of_operand e with
            | TLiteral (TInteger _) ->
                let v = as_base_of ~ty:Typed.t_int v in
                Result.ok (Base ~-v, state)
            | TLiteral (TFloat _) ->
                let* v =
                  of_opt_not_impl ~msg:"Expected a float type"
                  @@ Typed.cast_float (as_base v)
                in
                Result.ok (Base (Typed.float_like v 0.0 -.@ v), state)
            | _ -> not_impl "Invalid type for Neg")
        | PtrMetadata -> (
            match v with
            | Ptr (_, None) -> Result.ok (Tuple [], state)
            | Ptr (_, Some v) -> Result.ok (Base v, state)
            | _ -> not_impl "Invalid value for PtrMetadata")
        | ArrayToSlice (_, _, len) -> (
            match v with
            | Ptr (ptr, None) ->
                let len = Typed.int @@ int_of_const_generic len in
                Result.ok (Ptr (ptr, Some len), state)
            | _ -> not_impl "Invalid value for ArrayToSlice")
        | Cast (CastRawPtr (_from, _to)) -> Result.ok (v, state)
        | Cast (CastTransmute (from_ty, to_ty)) ->
            let++ v =
              Heap.lift_err state
              @@ Encoder.transmute ~verify_ptr:(Heap.is_valid_ptr state)
                   ~from_ty ~to_ty v
            in
            (v, state)
        | Cast (CastScalar (from_ty, to_ty)) ->
            let++ v =
              Heap.lift_err state
              @@ Encoder.transmute ~verify_ptr:(Heap.is_valid_ptr state)
                   ~from_ty:(TLiteral from_ty) ~to_ty:(TLiteral to_ty) v
            in
            (v, state)
        | Cast (CastUnsize (from_ty, _)) ->
            let rec get_size = function
              | Types.TRawPtr (ty, _)
              | TRef (_, ty, _)
              | TAdt (TBuiltin TBox, { types = [ ty ]; _ }) ->
                  get_size ty
              | TAdt (TAdtId id, _) -> (
                  let type_decl = Crate.get_adt id in
                  match type_decl.type_kind with
                  | Struct (_ :: _ as fields) ->
                      get_size (List.last fields).field_ty
                  | _ -> not_impl "Couldn't get size in CastUnsize")
              | TAdt (TBuiltin TArray, { const_generics = [ size ]; _ }) ->
                  return size
              | _ -> not_impl "Couldn't get size in CastUnsize"
            in
            let* size = get_size from_ty in
            let ptr, _ = as_ptr v in
            let size = Typed.int @@ int_of_const_generic size in
            Result.ok (Ptr (ptr, Some size), state)
        | Cast (CastFnPtr (_from, _to)) ->
            let* fn_ptr =
              match v with
              | ConstFn fn_ptr -> return fn_ptr
              | Struct [] ->
                  let ty = type_of_operand e in
                  let fn_opt : Expressions.fn_ptr option =
                    match ty with
                    | TAdt (TAdtId id, gargs) -> (
                        let adt = Crate.get_adt id in
                        match (adt.type_kind, adt.kind) with
                        | Struct [], ClosureItem { fun_id; _ } ->
                            Some
                              {
                                func = FunId (FRegular fun_id);
                                generics = gargs;
                              }
                        | _ -> None)
                    | _ -> None
                  in
                  of_opt_not_impl ~msg:"Invalid argument to CastFnPtr" fn_opt
              | _ -> not_impl "Invalid argument to CastFnPtr"
            in
            let++ ptr, state = Heap.declare_fn fn_ptr state in
            (Ptr ptr, state))
    | BinaryOp (op, e1, e2) -> (
        let** v1, state = eval_operand state e1 in
        let** v2, state = eval_operand state e2 in
        match (v1, v2) with
        | Base v1, Base v2 -> (
            match op with
            | Ge | Gt | Lt | Le -> (
                let* v1, v2, ty = cast_checked2 v1 v2 in
                match Typed.untype_type ty with
                | Svalue.TInt ->
                    let op =
                      match op with
                      | Ge -> Typed.geq
                      | Gt -> Typed.gt
                      | Lt -> Typed.lt
                      | Le -> Typed.leq
                      | _ -> assert false
                    in
                    let v = op v1 v2 |> Typed.int_of_bool in
                    Result.ok (Base v, state)
                | TFloat _ ->
                    let op =
                      match op with
                      | Ge -> Typed.geq_f
                      | Gt -> Typed.gt_f
                      | Lt -> Typed.lt_f
                      | Le -> Typed.leq_f
                      | _ -> assert false
                    in
                    let v1, v2 = (Typed.cast v1, Typed.cast v2) in
                    let v = op v1 v2 |> Typed.int_of_bool in
                    Result.ok (Base v, state)
                | TPointer -> Heap.error `UBPointerComparison state
                | _ -> assert false)
            | Eq | Ne ->
                let* v1, v2, _ = cast_checked2 v1 v2 in
                let++ res = Core.equality_check v1 v2 state in
                let res = if op = Eq then res else Typed.not_int_bool res in
                (Base (res :> T.cval Typed.t), state)
            | Add | Sub | Mul | Div | Rem | Shl | Shr ->
                let* ty =
                  match type_of_operand e1 with
                  | TLiteral ty -> return ty
                  | ty ->
                      Fmt.kstr not_impl "Unexpected type in binop: %a" pp_ty ty
                in
                let++ res = Core.eval_lit_binop op ty v1 v2 state in
                (Base res, state)
            | CheckedAdd | CheckedSub | CheckedMul ->
                let* ty =
                  match type_of_operand e1 with
                  | TLiteral ty -> return ty
                  | ty ->
                      Fmt.kstr not_impl "Unexpected type in binop: %a" pp_ty ty
                in
                let++ res = Core.eval_checked_lit_binop op ty v1 v2 state in
                (res, state)
            | WrappingAdd | WrappingSub | WrappingMul -> (
                match type_of_operand e1 with
                | TLiteral (TInteger ty as litty) ->
                    let** v = Core.safe_binop op litty v1 v2 state in
                    let+ res = Core.wrap_value ty v in
                    Soteria_symex.Compo_res.Ok (Base res, state)
                | TLiteral ty ->
                    (* Wrapping operations can apply to floating points too, but in that case
                       it is equivalent to the regular operation. *)
                    let++ res = Core.eval_lit_binop op ty v1 v2 state in
                    (Base res, state)
                | ty ->
                    Fmt.kstr not_impl "Unexpected type in binop: %a" pp_ty ty)
            | Cmp ->
                let* v1, v2, ty = cast_checked2 v1 v2 in
                if Typed.equal_ty ty Typed.t_ptr then
                  Heap.error `UBPointerComparison state
                else
                  let v = Typed.minus v1 v2 in
                  let* cmp = Core.cmp_of_int v in
                  Result.ok (Base cmp, state)
            | Offset ->
                (* if offset is done on integers, we just add the (size(T) * rhs) *)
                let* v1, v2, ty = cast_checked2 v1 v2 in
                if Typed.equal_ty ty Typed.t_ptr then
                  Fmt.kstr not_impl
                    "Offset cannot be used on non-int values: %a / %a" Typed.ppa
                    v1 Typed.ppa v2
                else
                  let pointee = Charon_util.get_pointee (type_of_operand e1) in
                  let* size = Layout.size_of_s pointee in
                  let res = v1 +@ (v2 *@ size) in
                  Result.ok (Base res, state)
            | BitOr | BitAnd | BitXor ->
                let* ity =
                  match type_of_operand e1 with
                  | TLiteral (TInteger ity) -> return ity
                  | TLiteral TBool -> return Values.U8
                  | TLiteral TChar -> return Values.U32
                  | ty ->
                      Fmt.kstr not_impl
                        "Unsupported type for bitwise operation: %a" pp_ty ty
                in
                let size = 8 * Layout.size_of_int_ty ity in
                let signed = Layout.is_signed ity in
                let* v1 = cast_checked ~ty:Typed.t_int v1 in
                let* v2 = cast_checked ~ty:Typed.t_int v2 in
                let op =
                  match op with
                  | BitOr -> Typed.bit_or
                  | BitAnd -> Typed.bit_and
                  | BitXor -> Typed.bit_xor
                  | _ -> assert false
                in
                Result.ok (Base (op ~size ~signed v1 v2), state))
        | ((Ptr _ | Base _) as p1), ((Ptr _ | Base _) as p2) -> (
            match op with
            | Offset ->
                let* p, meta, v =
                  match (p1, p2) with
                  | Ptr (p, meta), Base v | Base v, Ptr (p, meta) ->
                      return (p, meta, v)
                  | _ -> not_impl "Invalid operands in offset"
                in
                let ty = Charon_util.get_pointee (type_of_operand e1) in
                let* v = cast_checked ~ty:Typed.t_int v in
                let++ p' = Sptr.offset ~ty p v |> Heap.lift_err state in
                (Ptr (p', meta), state)
            | _ ->
                let++ res = Core.eval_ptr_binop op p1 p2 state in
                (Base res, state))
        | v1, v2 ->
            Fmt.kstr not_impl
              "Unsupported values for binary operator (%a): %a / %a"
              Expressions.pp_binop op pp_rust_val v1 pp_rust_val v2)
    | NullaryOp (op, ty) -> (
        match op with
        | UbChecks ->
            (* See https://doc.rust-lang.org/std/intrinsics/fn.ub_checks.html
               From what I understand: our execution already checks for UB, so we should return
               false, to say we don't want to do UB checks at runtime. *)
            Result.ok (Base (Typed.int_of_bool Typed.v_true), state)
        | SizeOf ->
            let* size = Layout.size_of_s ty in
            Result.ok (Base size, state)
        | AlignOf ->
            let* align = Layout.align_of_s ty in
            Result.ok (Base align, state)
        | OffsetOf _ ->
            Fmt.kstr not_impl "Unsupported nullary operator: %a"
              Expressions.pp_nullop op)
    | Discriminant (place, kind) -> (
        let** (loc, _), state = resolve_place ~store state place in
        let variants = Crate.as_enum kind in
        match variants with
        (* enums with one fieldless variant are ZSTs, so we can't load their discriminant! *)
        | [ { fields = []; discriminant; _ } ] ->
            let discr = Typed.int_z discriminant.value in
            Result.ok (Base discr, state)
        | var :: _ ->
            let int_ty = var.discriminant.int_ty in
            let layout = Layout.of_variant var in
            let discr_ofs = Typed.int @@ Array.get layout.members_ofs 0 in
            let discr_ty = Types.TLiteral (TInteger int_ty) in
            let** loc = Sptr.offset loc discr_ofs |> Heap.lift_err state in
            Heap.load (loc, None) discr_ty state
        | [] -> Fmt.kstr not_impl "Unsupported discriminant for empty enums")
    (* Enum aggregate *)
    | Aggregate (AggregatedAdt (TAdtId t_id, Some v_id, None, _), vals) ->
        let variants = Crate.as_enum t_id in
        let variant = Types.VariantId.nth variants v_id in
        let discr = value_of_scalar variant.discriminant in
        let++ vals, state = eval_operand_list ~store state vals in
        (Enum (discr, vals), state)
    (* Union aggregate *)
    | Aggregate (AggregatedAdt (_, None, Some field, _), ops) ->
        let* op =
          match ops with
          | [ op ] -> return op
          | _ -> not_impl "union aggregate with >1 values?"
        in
        let++ value, state = eval_operand state op in
        (Union (field, value), state)
    (* Tuple aggregate *)
    | Aggregate (AggregatedAdt (TTuple, None, None, _), operands) ->
        let++ values, state = eval_operand_list ~store state operands in
        (Tuple values, state)
    (* Struct aggregate *)
    | Aggregate (AggregatedAdt (TAdtId t_id, None, None, _), operands) ->
        let type_decl = Crate.get_adt t_id in
        let** values, state = eval_operand_list ~store state operands in
        let++ () =
          match values with
          | [ v ] ->
              Heap.lift_err state
              @@ Layout.apply_attributes v
                   type_decl.item_meta.attr_info.attributes
          | _ -> Result.ok ()
        in
        (Struct values, state)
    (* Invalid aggregate (not sure, but seems like it) *)
    | Aggregate ((AggregatedAdt _ as v), _) ->
        Fmt.failwith "Invalid ADT aggregate kind: %a"
          Expressions.pp_aggregate_kind v
    (* Array aggregate *)
    | Aggregate (AggregatedArray (_ty, _size), operands) ->
        let++ values, state = eval_operand_list ~store state operands in
        (Array values, state)
    (* Raw pointer construction *)
    | Aggregate (AggregatedRawPtr (_, _), operands) -> (
        let** values, state = eval_operand_list ~store state operands in
        match values with
        | [ Ptr (ptr, _); Base meta ] -> Result.ok (Ptr (ptr, Some meta), state)
        | [ Base v; Base meta ] ->
            let* v = cast_checked ~ty:Typed.t_int v in
            let ptr = Sptr.null_ptr_of v in
            Result.ok (Ptr (ptr, Some meta), state)
        | _ ->
            Fmt.kstr not_impl "AggregatedRawPtr: invalid arguments %a"
              Fmt.(list ~sep:comma pp_rust_val)
              values)
    (* Array repetition *)
    | Repeat (value, _, len) ->
        let++ value, state = eval_operand state value in
        let len = int_of_const_generic len in
        let els = List.init len (fun _ -> value) in
        (Array els, state)
    (* Shallow init box -- just casts a ptr into a box *)
    | ShallowInitBox (ptr, _) -> eval_operand state ptr
    (* Raw pointer *)
    | RawPtr (place, _kind) ->
        let++ ptr, state = resolve_place ~store state place in
        (Ptr ptr, state)
    (* Length of a &[T;N] or &[T] *)
    | Len (place, _, size_opt) ->
        let** (_, meta), state = resolve_place ~store state place in
        let+ len =
          match (meta, size_opt) with
          | _, Some size -> return (Typed.int @@ int_of_const_generic size)
          | Some len, None -> return len
          | _ -> not_impl "Unexpected len rvalue"
        in
        Soteria_symex.Compo_res.Ok (Base len, state)

  and exec_stmt store state astmt :
      (store * state, 'err, Heap.serialized list) Rustsymex.Result.t =
    L.info (fun m -> m "Statement: %a" Crate.pp_statement astmt);
    L.trace (fun m ->
        m "Statement full:@.%a" UllbcAst.pp_raw_statement astmt.content);
    let { span = loc; content = stmt; _ } : UllbcAst.statement = astmt in
    let@ () = with_loc ~loc in
    match stmt with
    | Nop -> Result.ok (store, state)
    | Assign (({ ty; _ } as place), rval) ->
        let** ptr, state = resolve_place ~store state place in
        let** v, state = eval_rvalue ~store state rval in
        L.info (fun m -> m "Assigning %a <- %a" pp_full_ptr ptr pp_rust_val v);
        let++ (), state = Heap.store ptr ty v state in
        (store, state)
    | StorageLive local ->
        let** (ptr, ty), state = get_variable_and_ty local store state in
        let** (), state =
          match ptr with
          | None -> Result.ok ((), state)
          | Some ptr -> Heap.free ptr state
        in
        let++ ptr, state = Heap.alloc_ty ty state in
        let store = Store.add local (Some ptr, ty) store in
        (store, state)
    | StorageDead local -> (
        let** (ptr, ty), state = get_variable_and_ty local store state in
        match ptr with
        | Some ptr ->
            let++ (), state = Heap.free ptr state in
            let store = Store.add local (None, ty) store in
            (store, state)
        | None -> Result.ok (store, state))
    | Drop place ->
        (* TODO: this is probably super wrong, drop glue etc. *)
        let** place_ptr, state = resolve_place ~store state place in
        let++ (), state = Heap.uninit place_ptr place.ty state in
        (store, state)
    | Assert { cond; expected; on_failure } -> (
        let** cond, state = eval_operand ~store state cond in
        let* cond_int =
          match cond with
          | Base cond -> cast_checked cond ~ty:Typed.t_int
          | _ -> not_impl "Expected a base Rust value in assert"
        in
        let cond_bool = Typed.bool_of_int cond_int in
        let cond_bool =
          if expected = true then cond_bool else Typed.not cond_bool
        in
        if%sat cond_bool then Result.ok (store, state)
        else
          match on_failure with
          | UndefinedBehavior -> Heap.error `UBAbort state
          | UnwindTerminate -> Heap.error `UnwindTerminate state
          | Panic name ->
              let name = Option.map (Fmt.str "%a" Crate.pp_name) name in
              Heap.error (`Panic name) state)
    | CopyNonOverlapping { src; dst; count } ->
        let ty = get_pointee (type_of_operand src) in
        let** args, state =
          eval_operand_list ~store state [ src; dst; count ]
        in
        let++ _, state = Std_funs.Std.copy true ty ~args ~state in
        (store, state)
    | SetDiscriminant (_, _) ->
        not_impl "Unsupported statement: SetDiscriminant"
    | Deinit _ -> not_impl "Unsupported statement: Deinit"

  and exec_block ~(body : UllbcAst.expr_body) store state
      ({ statements; terminator } : UllbcAst.block) =
    let* () = Rustsymex.consume_fuel_steps 1 in
    let** store, state =
      Rustsymex.Result.fold_list statements ~init:(store, state)
        ~f:(fun (store, state) stmt -> exec_stmt store state stmt)
    in
    L.info (fun f -> f "Terminator: %a" Crate.pp_terminator terminator);
    let { span = loc; content = term; _ } : UllbcAst.terminator = terminator in
    let@ () = with_loc ~loc in
    match term with
    | Call ({ func; args; dest = { ty; _ } as place }, target, on_unwind) ->
        let** exec_fun, state = resolve_function ~store func state in
        let** args, state = eval_operand_list ~store state args in
        L.info (fun g ->
            g "Executing function with arguments [%a]"
              Fmt.(list ~sep:(any ", ") pp_rust_val)
              args);
        Heap.unwind_with (exec_fun ~args ~state)
          ~f:(fun (v, state) ->
            let** ptr, state = resolve_place ~store state place in
            L.info (fun m ->
                m "Returned %a from %a" pp_rust_val v Crate.pp_fn_operand func);
            let** (), state = Heap.store ptr ty v state in
            let block = UllbcAst.BlockId.nth body.body target in
            exec_block ~body store state block)
          ~fe:(fun (err, state) ->
            let** (), state' = Heap.add_error err state in
            let block = UllbcAst.BlockId.nth body.body on_unwind in
            exec_block ~body store state' block)
    | Goto b ->
        let block = UllbcAst.BlockId.nth body.body b in
        exec_block ~body store state block
    | Return ->
        let ptr, ty = Store.find Expressions.LocalId.zero store in
        let* ptr =
          of_opt_not_impl ~msg:"Return value unset, but returned" ptr
        in
        let** value, state = Heap.load ptr ty state in
        let ptr_tys = Layout.ref_tys_in value ty in
        let++ (), state =
          Result.fold_list ptr_tys ~init:((), state)
            ~f:(fun ((), st) (ptr, ty) -> Heap.tb_load ptr ty st)
        in
        (value, store, state)
    | Switch (discr, switch) -> (
        let** discr, state = eval_operand ~store state discr in
        match switch with
        | If (if_block, else_block) ->
            L.info (fun g ->
                g "Switch if/else %a/%a for %a" UllbcAst.pp_block_id if_block
                  UllbcAst.pp_block_id else_block pp_rust_val discr);
            let* block =
              (* if a base value, compare with 0 -- if a pointer, check for null *)
              match discr with
              | Base discr ->
                  if%sat [@lname "else case"] [@rname "if case"] discr ==@ 0s
                  then return else_block
                  else return if_block
              | Ptr (ptr, _) ->
                  if%sat [@lname "else case"] [@rname "if case"]
                    Sptr.is_at_null_loc ptr
                  then return else_block
                  else return if_block
              | _ ->
                  Fmt.kstr not_impl
                    "Expected base value for discriminant, got %a" pp_rust_val
                    discr
            in
            let block = UllbcAst.BlockId.nth body.body block in
            exec_block ~body store state block
        | SwitchInt (_, options, default) ->
            L.info (fun g ->
                let options =
                  List.map
                    (fun (v, b) -> (PrintValues.scalar_value_to_string v, b))
                    options
                in
                g "Switch options %a (else %a) for %a"
                  Fmt.(
                    list ~sep:comma
                    @@ pair ~sep:(any "->") string UllbcAst.pp_block_id)
                  options UllbcAst.pp_block_id default pp_rust_val discr);
            let compare_discr =
              match discr with
              | Base discr -> fun (v, _) -> discr ==@ value_of_scalar v
              | Ptr (ptr, _) ->
                  fun (v, _) ->
                    if Z.equal Z.zero v.value then Sptr.is_at_null_loc ptr
                    else failwith "Can't compare pointer with non-0 scalar"
              | _ ->
                  fun (v, _) ->
                    Fmt.failwith
                      "Didn't know how to compare discriminant %a with scalar \
                       %s"
                      pp_rust_val discr
                      (PrintValues.scalar_value_to_string v)
            in
            let* block = match_on options ~constr:compare_discr in
            let block = Option.fold ~none:default ~some:snd block in
            let block = UllbcAst.BlockId.nth body.body block in
            exec_block ~body store state block)
    | Abort kind -> (
        match kind with
        | UndefinedBehavior -> Heap.error `UBAbort state
        | UnwindTerminate -> Heap.error `UnwindTerminate state
        | Panic name ->
            let name = Option.map (Fmt.str "%a" Crate.pp_name) name in
            Heap.error (`Panic name) state)
    | UnwindResume -> Heap.pop_error state

  and exec_fun ~args ~state (fundef : UllbcAst.fun_decl) =
    (* Put arguments in store *)
    let GAst.{ item_meta = { span = loc; name; _ }; body; _ } = fundef in
    let* body =
      match body with
      | None -> Fmt.kstr not_impl "Function %a is opaque" Crate.pp_name name
      | Some body -> return body
    in
    let@ () = with_loc ~loc in
    L.info (fun m ->
        m "Calling %a with %a" Crate.pp_name name
          Fmt.(hbox @@ brackets @@ list ~sep:comma pp_rust_val)
          args);
    let** store, protected, state = alloc_stack body.locals args state in
    let starting_block = List.hd body.body in
    let exec_block = exec_block ~body store state starting_block in
    Heap.unwind_with exec_block
      ~f:(fun (value, store, state) ->
        let protected_address =
          match (fundef.signature.output, value) with
          | TRef (RStatic, _, RShared), Ptr (addr, _) -> Some addr
          | _ -> None
        in
        let++ (), state =
          dealloc_store ?protected_address store protected state
        in
        (value, state))
      ~fe:(fun (err, state) ->
        let err' =
          Heap.add_to_call_trace err
            (Call_trace.make_element ~loc ~msg:"Call trace" ())
        in
        let** (), state = dealloc_store store protected state in
        Result.error (err', state))

  (* re-define this for the export, nowhere else: *)
  let exec_fun ?(ignore_leaks = false) ~args ~state fundef =
    let+- err, _ =
      let** value, state = exec_fun ~args ~state fundef in
      if ignore_leaks then Result.ok (value, state)
      else
        let@ () = with_loc ~loc:fundef.item_meta.span in
        let++ (), state = Heap.leak_check state in
        (value, state)
    in
    err
end
