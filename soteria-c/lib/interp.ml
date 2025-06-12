open Soteria_symex.Compo_res
open Csymex
open Csymex.Syntax
open Typed.Infix
open Typed.Syntax
open Ail_tys
module Ctype = Cerb_frontend.Ctype
module AilSyntax = Cerb_frontend.AilSyntax
module T = Typed.T

(* It's a good occasion to play with effects: we have a global immutable state,
   and want to avoid passing it to every single function in the interpreter.
   TODO: If this works well, do the same thing for prog.
   *)
type _ Effect.t += Get_fun_ctx : Fun_ctx.t Effect.t

let get_fun_ctx () = Effect.perform Get_fun_ctx

module Make (State : State_intf.S) = struct
  module C_std = C_std.M (State)

  exception Unsupported of (string * Cerb_location.t)

  let type_of expr = Cerb_frontend.Translation_aux.ctype_of expr
  let unwrap_ctype (Ctype.Ctype (_, ty)) = ty

  let pointer_inner (Ctype.Ctype (_, ty)) =
    match ty with Pointer (_, ty) -> Some ty | _ -> None

  type state = State.t

  let cast_to_ptr (x : [< T.cval ] Typed.t) =
    match Typed.get_ty x with
    | TInt ->
        (* We can cast an integer to a pointer by assigning the "null" location *)
        Typed.Ptr.mk Typed.Ptr.null_loc (Typed.cast x)
    | TPointer ->
        (* Already a pointer *)
        Typed.cast x
    | _ -> failwith "Unreachable: not a C value"

  let cast_to_int (x : [< T.cval ] Typed.t) : [> T.sint ] Typed.t Csymex.t =
    match Typed.get_ty x with
    | TInt -> Csymex.return (Typed.cast x)
    | TPointer ->
        let x = Typed.cast x in
        if%sat Typed.Ptr.is_at_null_loc x then Csymex.return (Typed.Ptr.ofs x)
        else
          Fmt.kstr not_impl "Pointer to int that is not at null loc %a at %a"
            Typed.ppa x Fmt_ail.pp_loc (get_loc ())
    | _ -> failwith "cast_to_int a cval?"

  let cast_to_bool (x : [< T.cval ] Typed.t) : [> T.sbool ] Typed.t =
    let open Typed in
    match get_ty x with
    | TInt -> bool_of_int (cast x)
    | TPointer -> not (Ptr.is_null (cast x))
    | _ -> failwith "unreachable"

  type 'err fun_exec =
    prog:linked_program ->
    args:T.cval Typed.t list ->
    state:state ->
    (T.cval Typed.t * state, 'err, State.serialized list) Result.t

  let get_param_tys ~prog name =
    let ptys = Ail_helpers.get_param_tys ~prog name in
    Csymex.of_opt_not_impl ~msg:"Couldn't find function prototype" ptys

  let fold_bindings (bindings : AilSyntax.bindings) ~init ~f =
    ListLabels.fold_left bindings ~init
      ~f:(fun acc (pname, ((loc, duration, _is_register), align, _quals, ty)) ->
        (match duration with
        | AilSyntax.Static | Thread -> raise (Unsupported ("static/tread", loc))
        | _ -> ());
        if Option.is_some align then raise (Unsupported ("align", loc));
        f acc (pname, ty))

  let try_with_unsupported f =
    try Csymex.return (f ())
    with Unsupported (msg, loc) ->
      let@ () = with_loc ~loc in
      Csymex.not_impl msg

  let attach_bindings store (bindings : AilSyntax.bindings) =
    try_with_unsupported @@ fun () ->
    fold_bindings bindings ~init:store ~f:(fun store (pname, ty) ->
        Store.reserve pname ty store)

  let remove_bindings store (bindings : AilSyntax.bindings) =
    try_with_unsupported @@ fun () ->
    fold_bindings bindings ~init:store ~f:(fun store (pname, _) ->
        Store.remove pname store)

  (* We're assuming all bindings declared, and they have already been removed from the store. *)
  let free_bindings store state (bindings : AilSyntax.bindings) =
    Result.fold_list bindings ~init:state
      ~f:(fun state (pname, ((loc, _, _), _, _, _)) ->
        let@ () = with_loc_immediate ~loc in
        match Store.find_opt pname store with
        | Some { kind = Some (Stackptr ptr); _ } ->
            let++ (), state = State.free ptr state in
            state
        | _ -> Result.ok state)

  let mk_store params =
    ListLabels.fold_left params ~init:Store.empty
      ~f:(fun store (pname, ty, value) ->
        L.trace (fun m ->
            m "Putting variable to the store: %a" Fmt_ail.pp_sym pname);

        Store.add_value pname value ty store)

  let dealloc_store store st =
    Result.fold_list (Store.bindings store) ~init:st
      ~f:(fun st (_, { kind; _ }) ->
        match kind with
        | Some (Stackptr ptr) ->
            let++ (), st = State.free ptr st in
            st
        | _ -> Result.ok st)

  let get_stack_address (store : Store.t) (st : State.t) (sym : Ail_tys.sym) =
    match Store.find_opt sym store with
    | Some { kind = None; _ } | None -> Csymex.return (None, store, st)
    | Some { kind = Some (Stackptr ptr); _ } ->
        Csymex.return (Some ptr, store, st)
    | Some { kind = Some other; ty } -> (
        let+ res =
          let** ptr, st = State.alloc_ty ty st in
          let++ (), st =
            match other with
            | Value v -> State.store ptr ty v st
            | _ -> Result.ok ((), st)
          in
          (ptr, Store.add_stackptr sym ptr ty store, st)
        in
        match res with
        | Ok (ptr, store, st) -> (Some ptr, store, st)
        | Error _ | Missing _ ->
            failwith "BUG(unreachable): failed to allocate stack address")

  let try_immediate_load ~prog (store : Store.t) state e =
    let (AilSyntax.AnnotatedExpression (_, _, _, e)) = e in
    match e with
    | AilEident id -> (
        let id = Ail_helpers.resolve_sym ~prog id in
        match Store.find_opt id store with
        | Some { kind = Some (Value v); _ } -> Result.ok (Some v)
        | Some { kind = Some Uninit; _ } ->
            State.error `UninitializedMemoryAccess state
        | _ -> Result.ok None)
    | _ -> Result.ok None

  let try_immediate_store ~prog (store : Store.t) lvalue rval =
    let (AilSyntax.AnnotatedExpression (_, _, _, lvalue)) = lvalue in
    match lvalue with
    | AilEident id -> (
        let id = Ail_helpers.resolve_sym ~prog id in
        match Store.find_opt id store with
        | Some { kind = Some (Value _ | Uninit); ty } ->
            let store = Store.add_value id rval ty store in
            Some store
        | _ -> None)
    | _ -> None

  let value_of_constant (c : constant) : T.cval Typed.t Csymex.t =
    match c with
    | ConstantInteger (IConstant (z, _basis, _suff)) ->
        Csymex.return (Typed.int_z z)
    | ConstantNull -> Csymex.return Typed.Ptr.null
    | ConstantCharacter (pref, char) ->
        if Option.is_some pref then Csymex.not_impl "prefixed char"
        else
          let+ char =
            Constants.string_to_char char
            |> of_opt_not_impl ~msg:(Fmt.str "char constant %s" char)
          in
          Typed.int char
    | _ ->
        Fmt.kstr Csymex.not_impl "value of constant? %a" Fmt_ail.pp_constant c

  let debug_show ~prog:_ ~args:_ ~state =
    let loc = get_loc () in
    let str = (Fmt.to_to_string (State.pp_pretty ~ignore_freed:false)) state in
    Csymex.push_give_up (str, loc);
    Result.ok (0s, state)

  let unwrap_expr (AnnotatedExpression (_, _, _, e) : expr) = e

  let find_stub ~prog:_ (fname : Cerb_frontend.Symbol.sym) :
      'err fun_exec option =
    let (Symbol (_, _, descr)) = fname in
    match descr with
    | Cerb_frontend.Symbol.SD_Id name -> (
        match name with
        | "__soteria_nondet__" -> Some C_std.nondet_int_fun
        | "malloc" -> Some C_std.malloc
        | "calloc" -> Some C_std.calloc
        | "free" -> Some C_std.free
        | "memcpy" -> Some C_std.memcpy
        | "__assert__" -> Some C_std.assert_
        | "__soteria_debug_show" -> Some debug_show
        | _ -> None)
    | _ -> None

  let cast ~old_ty:(Ctype.Ctype (_, old_ty)) ~new_ty:(Ctype.Ctype (_, new_ty))
      (v : [> T.cval ] Typed.t) =
    let open Typed in
    match (old_ty, new_ty) with
    | Ctype.Basic (Integer _), Ctype.Pointer (_quals, _ty) -> (
        match get_ty v with
        | TInt -> return (Ptr.mk Ptr.null_loc (Typed.cast v))
        | TPointer -> return v
        | _ -> Fmt.failwith "BUG: not a valid C value: %a" Typed.ppa v)
    | Ctype.Pointer (_, _), Ctype.Pointer (_, _) -> return v
    | Ctype.Basic (Integer ity_left), Ctype.Basic (Integer ity_right) -> (
        let* v = cast_to_int v in
        let ity_left = Layout.normalise_int_ty ity_left in
        let ity_right = Layout.normalise_int_ty ity_right in
        match (ity_left, ity_right) with
        | Signed _, Unsigned _ ->
            let+ size_right =
              Layout.size_of_int_ty ity_right
              |> Csymex.of_opt_not_impl ~msg:"Size of int ty"
            in
            let size_right = Typed.nonzero size_right in
            Typed.mod_ v size_right
        | _, _ ->
            Fmt.kstr not_impl "Integer cast : %a -> %a" Fmt_ail.pp_int_ty
              ity_left Fmt_ail.pp_int_ty ity_right)
    | _ ->
        Fmt.kstr Csymex.not_impl "Cast %a -> %a" Fmt_ail.pp_ty_ old_ty
          Fmt_ail.pp_ty_ new_ty

  let rec equality_check ~state (v1 : [< Typed.T.cval ] Typed.t)
      (v2 : [< Typed.T.cval ] Typed.t) =
    match (Typed.get_ty v1, Typed.get_ty v2) with
    | TInt, TInt | TPointer, TPointer ->
        Result.ok (v1 ==@ v2 |> Typed.int_of_bool)
    | TPointer, TInt ->
        let v2 : T.sint Typed.t = Typed.cast v2 in
        if%sat Typed.(v2 ==@ zero) then
          Result.ok (v1 ==@ Typed.Ptr.null |> Typed.int_of_bool)
        else State.error `UBPointerComparison state
    | TInt, TPointer -> equality_check ~state v2 v1
    | _ ->
        Fmt.kstr not_impl "Unexpected types in cval equality: %a and %a"
          Typed.ppa v1 Typed.ppa v2

  let rec arith_add ~state (v1 : [< Typed.T.cval ] Typed.t)
      (v2 : [< Typed.T.cval ] Typed.t) =
    match (Typed.get_ty v1, Typed.get_ty v2) with
    | TInt, TInt ->
        let v1 = Typed.cast v1 in
        let v2 = Typed.cast v2 in
        Result.ok (v1 +@ v2)
    | TPointer, TInt ->
        let v1 : T.sptr Typed.t = Typed.cast v1 in
        let v2 : T.sint Typed.t = Typed.cast v2 in
        let loc = Typed.Ptr.loc v1 in
        let ofs = Typed.Ptr.ofs v1 +@ v2 in
        Result.ok (Typed.Ptr.mk loc ofs)
    | TInt, TPointer -> arith_add ~state v2 v1
    | TPointer, TPointer -> State.error `UBPointerArithmetic state
    | _ ->
        Fmt.kstr not_impl "Unexpected types in addition: %a and %a" Typed.ppa v1
          Typed.ppa v2

  let arith_sub ~state (v1 : [< Typed.T.cval ] Typed.t)
      (v2 : [< Typed.T.cval ] Typed.t) =
    match (Typed.get_ty v1, Typed.get_ty v2) with
    | TInt, TInt ->
        let v1 = Typed.cast v1 in
        let v2 = Typed.cast v2 in
        Result.ok (v1 -@ v2)
    | TPointer, TInt ->
        let v1 : T.sptr Typed.t = Typed.cast v1 in
        let v2 : T.sint Typed.t = Typed.cast v2 in
        let loc = Typed.Ptr.loc v1 in
        let ofs = Typed.Ptr.ofs v1 -@ v2 in
        Result.ok (Typed.Ptr.mk loc ofs)
    | TPointer, TPointer ->
        let v1 : T.sptr Typed.t = Typed.cast v1 in
        let v2 : T.sptr Typed.t = Typed.cast v2 in
        if%sat Typed.Ptr.loc v1 ==@ Typed.Ptr.loc v2 then
          Result.ok (Typed.Ptr.ofs v1 -@ Typed.Ptr.ofs v2)
        else State.error `UBPointerArithmetic state
    | _ ->
        Fmt.kstr not_impl "Unexpected types in addition: %a and %a" Typed.ppa v1
          Typed.ppa v2

  let arith_mul ~state (v1 : [< Typed.T.cval ] Typed.t)
      (v2 : [< Typed.T.cval ] Typed.t) =
    match (Typed.get_ty v1, Typed.get_ty v2) with
    | TInt, TInt ->
        let v1 = Typed.cast v1 in
        let v2 = Typed.cast v2 in
        Result.ok (v1 *@ v2)
    | TPointer, _ | _, TPointer -> State.error `UBPointerArithmetic state
    | _ ->
        Fmt.kstr not_impl "Unexpected types in multiplication: %a and %a"
          Typed.ppa v1 Typed.ppa v2

  let arith ~state (v1, t1) a_op (v2, t2) =
    match (a_op : AilSyntax.arithmeticOperator) with
    | Div -> (
        let* v1 = cast_to_int v1 in
        let* v2 = cast_to_int v2 in
        let* v2 = Csymex.check_nonzero v2 in
        match v2 with
        | Ok v2 -> Csymex.Result.ok (v1 /@ v2)
        | Error `NonZeroIsZero -> State.error `DivisionByZero state
        | Missing e -> (* Unreachable but still *) Csymex.Result.miss e)
    | Mul -> arith_mul ~state v1 v2
    | Add -> (
        match (t1 |> pointer_inner, t2 |> pointer_inner) with
        | Some _, Some _ -> State.error `UBPointerArithmetic state
        | Some ty, None ->
            let* factor = Layout.size_of_s ty in
            let** v2 = arith_mul ~state v2 factor in
            arith_add ~state v1 v2
        | None, Some ty ->
            let* factor = Layout.size_of_s ty in
            let** v1 = arith_mul ~state v1 factor in
            arith_add ~state v2 v1
        | None, None -> arith_add ~state v1 v2)
    | Sub -> (
        match (t1 |> pointer_inner, t2 |> pointer_inner) with
        | Some ty, None ->
            let* factor = Layout.size_of_s ty in
            let** v2 = arith_mul ~state v2 factor in
            arith_sub ~state v1 v2
        | None, Some _ -> State.error `UBPointerArithmetic state
        | Some _, Some _ | None, None -> arith_sub ~state v1 v2)
    | Band ->
        (* TODO: is it guaranteed that both have the same type? *)
        let* { bv_size; signed } =
          Layout.bv_info t1 |> of_opt_not_impl ~msg:"bv_info"
        in
        let* v1 = cast_to_int v1 in
        let* v2 = cast_to_int v2 in
        Result.ok (Typed.bit_and ~size:bv_size ~signed v1 v2)
    | _ ->
        Fmt.kstr not_impl "Unsupported arithmetic operator: %a"
          Fmt_ail.pp_arithop a_op

  let try_immediate_postfix_op ~prog ~apply_op store state lvalue =
    let** v_opt = try_immediate_load ~prog store state lvalue in
    match v_opt with
    | Some v ->
        (* Optimisation *)
        (* If the value is in direct access, we can just increment it and
          immediately update it in the store. The writing cannot fail. *)
        let++ res = apply_op ~state v in
        let store =
          try_immediate_store ~prog store lvalue res
          |> Option.get ~msg:"Immediate store failed after immediate load?"
        in
        Some (res, store, state)
    | None -> Result.ok None

  (* We do this in the untyped world *)
  let ineq_comparison ~state ~cmp_op left right =
    let cmp_op left right = cmp_op left right |> Typed.int_of_bool in
    match (Typed.get_ty left, Typed.get_ty right) with
    | TInt, TInt ->
        let left = Typed.cast left in
        let right = Typed.cast right in
        Result.ok (cmp_op left right)
    | TPointer, TPointer ->
        let left = Typed.cast left in
        let right = Typed.cast right in
        if%sat Typed.Ptr.loc left ==@ Typed.Ptr.loc right then
          Csymex.Result.ok (cmp_op (Typed.Ptr.ofs left) (Typed.Ptr.ofs right))
        else State.error `UBPointerComparison state
    | _ -> State.error `UBPointerComparison state

  let rec resolve_function ~(prog : linked_program) store state fexpr =
    let** (loc, fname), store, state =
      let (AilSyntax.AnnotatedExpression (_, _, loc, inner_expr)) = fexpr in
      match inner_expr with
      (* Special case when we can immediately resolve the function name *)
      | AilEfunction_decay (AnnotatedExpression (_, _, _, AilEident fname)) ->
          Result.ok ((loc, fname), store, state)
      | _ ->
          (* Some function pointer *)
          L.trace (fun m ->
              m "Resolving function pointer: %a" Fmt_ail.pp_expr fexpr);
          let** fptr, store, state = eval_expr ~prog store state fexpr in
          L.trace (fun m -> m "Function pointer is value: %a" Typed.ppa fptr);
          let fptr = cast_to_ptr fptr in
          if%sat
            Typed.not (Typed.Ptr.ofs fptr ==@ 0s)
            ||@ Typed.Ptr.is_at_null_loc fptr
          then State.error `InvalidFunctionPtr state
          else
            let fctx = get_fun_ctx () in
            let+ sym = Fun_ctx.get_sym (Typed.Ptr.loc fptr) fctx in
            Soteria_symex.Compo_res.Ok ((loc, sym), store, state)
    in
    let@ () = with_loc ~loc in
    let fundef_opt = Ail_helpers.find_fun_def ~prog fname in
    match fundef_opt with
    | Some fundef -> Csymex.Result.ok (exec_fun fundef, store, state)
    | None -> (
        match find_stub ~prog fname with
        | Some stub -> Csymex.Result.ok (stub, store, state)
        | None ->
            Fmt.kstr not_impl "Cannot call external function: %a" Fmt_ail.pp_sym
              fname)

  and eval_expr_list ~(prog : linked_program) (store : Store.t) (state : state)
      (el : expr list) =
    let++ vs, store, state =
      Csymex.Result.fold_list el ~init:([], store, state)
        ~f:(fun (acc, store, state) e ->
          let++ new_res, store, state = eval_expr ~prog store state e in
          (new_res :: acc, store, state))
    in
    (List.rev vs, store, state)

  and eval_expr ~(prog : linked_program) (store : Store.t) (state : state)
      (aexpr : expr) :
      ([> T.cval ] Typed.t * Store.t * State.t, 'err, 'fix) Result.t =
    let eval_expr = eval_expr ~prog in
    let (AnnotatedExpression (_, _, loc, expr)) = aexpr in
    let@ () = with_loc ~loc in
    match expr with
    | AilEconst c ->
        let+ v = value_of_constant c in
        Ok (v, store, state)
    | AilEcall (f, args) ->
        let** exec_fun, store, state = resolve_function ~prog store state f in
        let** args, store, state = eval_expr_list ~prog store state args in
        let++ v, state =
          let+- err = exec_fun ~prog ~args ~state in
          State.add_to_call_trace err
            (Soteria_terminal.Call_trace.mk_element ~loc ~msg:"Called from here"
               ())
        in
        L.debug (fun m -> m "returned %a from %a" Typed.ppa v Fmt_ail.pp_expr f);
        (v, store, state)
    | AilEunary (Address, e) -> (
        match unwrap_expr e with
        | AilEunary (Indirection, e) -> (* &*e <=> e *) eval_expr store state e
        | AilEident id -> (
            let id = Ail_helpers.resolve_sym ~prog id in
            let* ptr_opt, store, state = get_stack_address store state id in
            match ptr_opt with
            | Some ptr -> Result.ok ((ptr :> T.cval Typed.t), store, state)
            | None ->
                let+ ptr, state = State.get_global id state in
                Ok (ptr, store, state))
        | AilEmemberofptr (ptr, member) ->
            let** ptr_v, store, state = eval_expr store state ptr in
            let* ty_pointee =
              type_of ptr
              |> Cerb_frontend.AilTypesAux.referenced_type
              |> Csymex.of_opt_not_impl
                   ~msg:"Member of Pointer that isn't of type pointer"
            in
            let* mem_ofs = Layout.member_ofs member ty_pointee in
            let++ v = arith_add ~state ptr_v mem_ofs in
            (v, store, state)
        | _ -> Fmt.kstr not_impl "Unsupported address_of: %a" Fmt_ail.pp_expr e)
    | AilEunary (((PostfixIncr | PostfixDecr) as op), e) -> (
        let apply_op ~state v =
          let* operand =
            match pointer_inner (type_of e) with
            | Some ty -> Layout.size_of_s ty
            | None -> return 1s
          in
          match op with
          | PostfixIncr -> arith_add ~state v operand
          | PostfixDecr -> arith_sub ~state v operand
          | _ -> failwith "unreachable: postfix is not postfix??"
        in
        let** res_opt =
          try_immediate_postfix_op ~prog ~apply_op store state e
        in
        match res_opt with
        | Some res_triple -> Result.ok res_triple
        | None ->
            let** v, store, state = eval_expr store state e in
            let ptr = cast_to_ptr v in
            let** v, state = State.load ptr (type_of e) state in
            let** v_incr = apply_op ~state v in
            let++ (), state = State.store ptr (type_of e) v_incr state in
            (v, store, state))
    | AilEunary (op, e) -> (
        let** v, store, state = eval_expr store state e in
        match op with
        | PostfixDecr ->
            let ptr = cast_to_ptr v in
            let** v, state = State.load ptr (type_of e) state in
            let* incr_operand =
              match type_of e |> pointer_inner with
              | Some ty -> Layout.size_of_s ty
              | None -> return 1s
            in
            let** v_decr = arith_sub ~state v incr_operand in
            let++ (), state = State.store ptr (type_of e) v_decr state in
            (v, store, state)
        | Indirection -> Result.ok (v, store, state)
        | Address -> failwith "unreachable: address_of already handled"
        | Minus ->
            let* v = cast_to_int v in
            let++ v = arith_sub ~state Typed.zero v in
            (v, store, state)
        | _ ->
            Fmt.kstr not_impl "Unsupported unary operator %a" Fmt_ail.pp_unop op
        )
    | AilEbinary (e1, Or, e2) ->
        (* Or is short-circuiting. In case of side-effects on the RHS,
           not doing this properly might lead to unsoundnesses.
           We still optimize by returning a disjunction of the two
           expressions if the RHS is side-effect free. *)
        let** v1, store, state = eval_expr store state e1 in
        if Ail_helpers.sure_side_effect_free e2 then
          let** v2, store, state = eval_expr store state e2 in
          let b_res = cast_to_bool v1 ||@ cast_to_bool v2 in
          Result.ok (Typed.int_of_bool b_res, store, state)
        else
          if%sat cast_to_bool v1 then Result.ok (Typed.one, store, state)
          else
            let** v2, store, state = eval_expr store state e2 in
            let b_res = cast_to_bool v2 in
            Result.ok (Typed.int_of_bool b_res, store, state)
    | AilEbinary (e1, And, e2) ->
        (* Same as Or, we need to short-circuit *)
        let** v1, store, state = eval_expr store state e1 in
        if Ail_helpers.sure_side_effect_free e2 then
          let** v2, store, state = eval_expr store state e2 in
          let b_res = cast_to_bool v1 &&@ cast_to_bool v2 in
          Result.ok (Typed.int_of_bool b_res, store, state)
        else
          if%sat cast_to_bool v1 then
            let** v2, store, state = eval_expr store state e2 in
            let b_res = cast_to_bool v2 in
            Result.ok (Typed.int_of_bool b_res, store, state)
          else Result.ok (Typed.zero, store, state)
    | AilEbinary (e1, op, e2) ->
        let** v1, store, state = eval_expr store state e1 in
        let** v2, store, state = eval_expr store state e2 in
        let++ v =
          match op with
          | Ge -> ineq_comparison ~state ~cmp_op:( >=@ ) v1 v2
          | Gt -> ineq_comparison ~state ~cmp_op:( >@ ) v1 v2
          | Lt -> ineq_comparison ~state ~cmp_op:( <@ ) v1 v2
          | Le -> ineq_comparison ~state ~cmp_op:( <=@ ) v1 v2
          | Eq -> equality_check ~state v1 v2
          | Ne ->
              (* TODO: Semantics of Ne might be different from semantics of not eq? *)
              let++ res = equality_check ~state v1 v2 in
              Typed.not_int_bool res
          | Or | And -> failwith "Unreachable, handled earlier."
          | Arithmetic a_op ->
              arith ~state (v1, type_of e1) a_op (v2, type_of e2)
          | Comma -> Result.ok v2
        in
        (v, store, state)
    | AilErvalue e -> (
        (* Optimisation: If the expression to load is a variable that is
           immediately in the store (without heap indirection),
           we can just access it without heap shenanigans. *)
        let** v_opt = try_immediate_load ~prog store state e in
        match v_opt with
        | Some v -> Result.ok (v, store, state)
        | None ->
            (* The value is not an immediate store load *)
            let** lvalue, store, state = eval_expr store state e in
            let ty = type_of e in
            (* At this point, lvalue must be a pointer (including to the stack) *)
            let lvalue = cast_to_ptr lvalue in
            let++ v, state = State.load lvalue ty state in
            (v, store, state))
    | AilEident id -> (
        let id = Ail_helpers.resolve_sym ~prog id in
        let* ptr_opt, store, state = get_stack_address store state id in
        match ptr_opt with
        | Some v ->
            (* A pointer is a value *)
            let v = (v :> T.cval Typed.t) in
            Result.ok (v, store, state)
        | None ->
            (* If the variable isn't in the store, it must be a global variable. *)
            let+ ptr, state = State.get_global id state in
            Ok (ptr, store, state))
    | AilEassign (lvalue, rvalue) -> (
        (* Evaluate rvalue first *)
        let** rval, store, state = eval_expr store state rvalue in
        (* Optimisation: if the lvalue is a variable to which we assign directly,
           we don't need to do anything with the heap, we can simply immediately assign,
           obtaining a new store.  *)
        let store_opt = try_immediate_store ~prog store lvalue rval in
        match store_opt with
        | Some store -> Result.ok (rval, store, state)
        | None ->
            let** ptr, store, state = eval_expr store state lvalue in
            (* [ptr] is a necessarily a pointer, and [rval] is a memory value.
         I don't support pointer fragments for now, so let's say it's an *)
            let ptr = cast_to_ptr ptr in
            let ty = type_of lvalue in
            let++ (), state = State.store ptr ty rval state in
            (rval, store, state))
    | AilEcompoundAssign (lvalue, op, rvalue) -> (
        let** rval, store, state = eval_expr store state rvalue in
        let rty = type_of rvalue in
        let lty = type_of lvalue in
        let apply_op ~state v = arith ~state (v, lty) op (rval, rty) in
        let** immediate_result =
          try_immediate_postfix_op ~prog ~apply_op store state lvalue
        in
        match immediate_result with
        | Some res_triple -> Result.ok res_triple
        | None ->
            (* Otherwise we proceed as normal *)
            let** ptr, store, state = eval_expr store state lvalue in
            (* At this point, lvalue must be a pointer (including to the stack) *)
            let ptr = cast_to_ptr ptr in
            let** operand, state = State.load ptr lty state in
            let** res = apply_op ~state operand in
            let++ (), state = State.store ptr lty res state in
            (res, store, state))
    | AilEsizeof (_quals, ty) ->
        let+ res = Layout.size_of_s ty in
        Ok (res, store, state)
    | AilEmemberofptr (ptr, member) ->
        let** ptr_v, store, state = eval_expr store state ptr in
        let* ty_pointee =
          type_of ptr
          |> Cerb_frontend.AilTypesAux.referenced_type
          |> Csymex.of_opt_not_impl
               ~msg:"Member of Pointer that isn't of type pointer"
        in
        let* mem_ofs = Layout.member_ofs member ty_pointee in
        let++ v = arith_add ~state ptr_v mem_ofs in
        (v, store, state)
    | AilEmemberof (obj, member) ->
        let** ptr_v, store, state = eval_expr store state obj in
        let ty_obj = type_of obj in
        let* mem_ofs = Layout.member_ofs member ty_obj in
        let++ v = arith_add ~state ptr_v mem_ofs in
        (v, store, state)
    | AilEcast (_quals, new_ty, expr) ->
        let old_ty = type_of expr in
        let** v, store, state = eval_expr store state expr in
        let+ new_v = cast ~old_ty ~new_ty v in
        Ok (new_v, store, state)
    | AilEfunction_decay (AnnotatedExpression (_, _, _, fexpr) as outer_fexpr)
      -> (
        match fexpr with
        | AilEident id ->
            let id = Ail_helpers.resolve_sym ~prog id in
            let ctx = get_fun_ctx () in
            let+ floc = Fun_ctx.decay_fn_sym id ctx in
            Soteria_symex.Compo_res.Ok (Typed.Ptr.mk floc 0s, store, state)
        | _ ->
            Fmt.kstr not_impl "Unsupported function decay: %a" Fmt_ail.pp_expr
              outer_fexpr)
    | AilEinvalid (_ty, reason) ->
        Fmt.kstr not_impl "Cerberus could not parse an expression because %a"
          Fmt_ail.pp_invalid_reason reason
    | AilEcond (_, _, _)
    | AilEassert _
    | AilEoffsetof (_, _)
    | AilEgeneric (_, _)
    | AilEarray (_, _, _)
    | AilEstruct (_, _)
    | AilEunion (_, _, _)
    | AilEcompound (_, _, _)
    | AilEbuiltin _ | AilEstr _ | AilEsizeof_expr _
    | AilEalignof (_, _)
    | AilEannot (_, _)
    | AilEva_start (_, _)
    | AilEva_arg (_, _)
    | AilEva_copy (_, _)
    | AilEva_end _ | AilEprint_type _ | AilEbmc_assume _ | AilEreg_load _
    | AilEarray_decay _ | AilEatomic _
    | AilEgcc_statement (_, _) ->
        Fmt.kstr not_impl "Unsupported expr: %a" Fmt_ail.pp_expr aexpr

  (** Executing a statement returns an optional value outcome (if a return
      statement was hit), or *)
  and exec_stmt ~prog (store : Store.t) (state : state) (astmt : stmt) :
      ( T.cval Typed.t option * Store.t * state,
        'err,
        State.serialized list )
      Csymex.Result.t =
    L.debug (fun m -> m "Executing statement: %a" Fmt_ail.pp_stmt astmt);
    L.debug (fun m -> m "@[<v 2>STORE:@ %a@]" Store.pp store);
    let* () = Csymex.consume_fuel_steps 1 in
    Stats.incr_executed_statements ();
    let AilSyntax.{ loc; node = stmt; _ } = astmt in
    let@ () = with_loc ~loc in
    match stmt with
    | AilSskip -> Result.ok (None, store, state)
    | AilSreturn e ->
        let** v, store, state = eval_expr ~prog store state e in
        L.debug (fun m -> m "Returning: %a" Typed.ppa v);
        Result.ok (Some v, store, state)
    | AilSreturnVoid -> Result.ok (Some 0s, store, state)
    | AilSblock (bindings, stmtl) ->
        let* store = attach_bindings store bindings in
        (* Second result, corresponding to the block-scoped store, is discarded *)
        let** res, store, state =
          Csymex.Result.fold_list stmtl ~init:(None, store, state)
            ~f:(fun (res, store, state) stmt ->
              match res with
              | Some _ -> Csymex.Result.ok (res, store, state)
              | None -> exec_stmt ~prog store state stmt)
        in
        (* Cerberus is nice here, symbols inside the block have different names than
           the ones outside the block if there is shadowing going on.
           I.e. int x = 12; { int x = 13; }, the two `x`s have different symbols.
           So we can just remove the bindings of the inner block from the store entirely. *)
        let** state = free_bindings store state bindings in
        let+ store = remove_bindings store bindings in
        Ok (res, store, state)
    | AilSexpr e ->
        let** _, store, state = eval_expr ~prog store state e in
        Result.ok (None, store, state)
    | AilSif (cond, then_stmt, else_stmt) ->
        let** v, store, state = eval_expr ~prog store state cond in
        (* [v] must be an integer! (TODO: or NULL possibly...) *)
        let v = cast_to_bool v in
        if%sat v then exec_stmt ~prog store state then_stmt [@name "if branch"]
        else exec_stmt ~prog store state else_stmt [@name "else branch"]
    | AilSwhile (cond, stmt, _loopid) ->
        let rec loop store state =
          let** cond_v, store, state = eval_expr ~prog store state cond in
          if%sat cast_to_bool cond_v then
            let** res, store, state = exec_stmt ~prog store state stmt in
            match res with
            | Some _ -> Result.ok (res, store, state)
            | None -> loop store state
          else Result.ok (None, store, state)
        in
        loop store state
    | AilSdo (stmt, cond, _loop_id) ->
        let rec loop store state =
          let** res, store, state = exec_stmt ~prog store state stmt in
          match res with
          | Some _ -> Result.ok (res, store, state)
          | None ->
              let** cond_v, store, state = eval_expr ~prog store state cond in
              if%sat cast_to_bool cond_v then loop store state
              else Result.ok (None, store, state)
        in
        loop store state
    | AilSlabel (_label, stmt, _annot) ->
        (* TODO: keep track of labels in a record or something!! *)
        exec_stmt ~prog store state stmt
    | AilSdeclaration decls ->
        let++ store, st =
          Csymex.Result.fold_list decls ~init:(store, state)
            ~f:(fun (store, state) (pname, expr) ->
              match expr with
              | None ->
                  let store = Store.declare_uninit pname store in
                  Result.ok (store, state)
              | Some expr ->
                  let++ v, store, state = eval_expr ~prog store state expr in
                  let store = Store.declare_value pname v store in
                  (store, state))
        in
        (None, store, st)
    | _ -> Fmt.kstr not_impl "Unsupported statement: %a" Fmt_ail.pp_stmt astmt

  and exec_fun ~prog ~args ~state (fundef : fundef) =
    (* Put arguments in store *)
    let name, (loc, _, _, params, stmt) = fundef in
    let@ () = with_loc ~loc in
    L.debug (fun m -> m "Executing function %a" Fmt_ail.pp_sym name);
    L.trace (fun m ->
        m "Was given arguments: %a" (Fmt.Dump.list Typed.ppa) args);
    let* ptys = get_param_tys ~prog name in
    let ps = List.combine3 params ptys args in
    let store = mk_store ps in
    let** val_opt, store, state = exec_stmt ~prog store state stmt in
    let++ state = dealloc_store store state in
    (* We model void as zero, it should never be used anyway *)
    let value = Option.value ~default:0s val_opt in
    (value, state)

  let init_prog_state (prog : Ail_tys.linked_program) =
    (* Produce_zero will be useful when Kayvan allows for knowing when no declaration is given. *)
    let _produce_zero (ptr : [< T.sptr ] Typed.t) ty (state : State.t) =
      let loc = Typed.Ptr.loc ptr in
      let offset = Typed.Ptr.ofs ptr in
      let* len = Layout.size_of_s ty in
      let serialized : State.serialized =
        {
          heap = [ (loc, Freeable.Alive [ Tree_block.Zeros { offset; len } ]) ];
          globs = [];
        }
      in
      let+ state = State.produce serialized state in
      Soteria_symex.Compo_res.ok state
    in
    let produce_value (ptr : [< T.sptr ] Typed.t) ty expr (state : State.t) =
      let loc = Typed.Ptr.loc ptr in
      let offset = Typed.Ptr.ofs ptr in
      (* I somehow have to support global initialisation urgh.
       I might be able to extract some of that into interp *)
      let** v, _, state = eval_expr ~prog Store.empty state expr in
      let serialized : State.serialized =
        {
          heap =
            [ (loc, Freeable.Alive [ Tree_block.TypedVal { offset; ty; v } ]) ];
          globs = [];
        }
      in
      let+ state = State.produce serialized state in
      Soteria_symex.Compo_res.ok state
    in
    Csymex.Result.fold_list prog.sigma.object_definitions ~init:State.empty
      ~f:(fun (state : State.t) def ->
        let id, e = def in
        let* ty =
          match Ail_helpers.find_obj_decl ~prog id with
          | None -> Csymex.not_impl "Couldn't find object declaration"
          | Some (_, _, _, ty) -> Csymex.return ty
        in
        (* TODO: handle other parameters here *)
        let* ptr, state = State.get_global id state in
        produce_value ptr ty e state)
end
