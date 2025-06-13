open Soteria_symex.Compo_res
open Csymex
open Csymex.Syntax
open Typed.Infix
open Typed.Syntax
open Ail_tys
module Ctype = Cerb_frontend.Ctype
module AilSyntax = Cerb_frontend.AilSyntax
module T = Typed.T

module InterpM (State : State_intf.S) = struct
  type 'a t =
    Store.t ->
    State.t ->
    ('a * Store.t * State.t, Error.t State.err, State.serialized list) Result.t

  let ok x : 'a t = fun store state -> Result.ok (x, store, state)
  let error err : 'a t = fun _store state -> State.error err state
  let not_impl str : 'a t = fun _store _state -> Csymex.not_impl str
  let get_store () = fun store state -> Result.ok (store, store, state)

  let bind (x : 'a t) (f : 'a -> 'b t) : 'b t =
   fun store state ->
    let** y, store, state = x store state in
    (f y) store state

  let map (x : 'a t) (f : 'a -> 'b) : 'b t =
   fun store state ->
    let++ y, store, state = x store state in
    (f y, store, state)

  let fold_list x ~init ~f =
    Monad.foldM ~bind ~return:ok ~fold:Foldable.List.fold x ~init ~f

  let map_store f = fun store state -> Result.ok ((), f store, state)

  let lift_state_op f =
   fun store state ->
    let++ v, state = f state in
    (v, store, state)

  let lift_symex (s : 'a Csymex.t) : 'a t =
   fun store state ->
    let+ s = s in
    Ok (s, store, state)

  let lift_symex_res
      (s : ('a, Error.t State.err, State.serialized list) Result.t) : 'a t =
   fun store state ->
    let++ s = s in
    (s, store, state)

  let of_opt_not_impl ~msg x = lift_symex (of_opt_not_impl ~msg x)

  let with_loc ~loc f =
    let old_loc = !Csymex.current_loc in
    Csymex.current_loc := loc;
    map (f ()) @@ fun res ->
    current_loc := old_loc;
    res

  let with_extra_call_trace ~loc ~msg (x : 'a t) : 'a t =
   fun store state ->
    let+ res = x store state in
    match res with
    | Ok triple -> Ok triple
    | Error e ->
        let elem = Soteria_terminal.Call_trace.mk_element ~loc ~msg () in
        Error (State.add_to_call_trace e elem)
    | Missing f -> Missing f

  let if_not_ok do_this (x : 'a t) : 'a t =
   fun store state ->
    let+ res = x store state in
    match res with Ok _ -> res | _ -> do_this ()

  let run ~store ~state f = f store state

  module State = struct
    let load ptr ty = lift_state_op (State.load ptr ty)
    let store ptr ty v = lift_state_op (State.store ptr ty v)
    let alloc ?zeroed size = lift_state_op (State.alloc ?zeroed size)
    let alloc_ty ty = lift_state_op (State.alloc_ty ty)

    let get_global id =
      lift_state_op (fun state ->
          let+ v, state = State.get_global id state in
          Ok (v, state))
  end

  module Store = struct
    let find_opt sym =
     fun store state ->
      let binding = Store.find_opt sym store in
      Result.ok (binding, store, state)
  end

  module Syntax = struct
    let ( let* ) = bind
    let ( let+ ) = map
    let ( let^ ) x f = bind (lift_symex x) f

    module Symex_syntax = struct
      let branch_on ?left_branch_name ?right_branch_name guard ~then_ ~else_ =
       fun store state ->
        Csymex.branch_on ?left_branch_name ?right_branch_name guard
          ~then_:(fun () -> then_ () store state)
          ~else_:(fun () -> else_ () store state)
    end
  end
end

(* It's a good occasion to play with effects: we have a global immutable state,
   and want to avoid passing it to every single function in the interpreter.
   TODO: If this works well, do the same thing for prog.
   *)
type _ Effect.t += Get_fun_ctx : Fun_ctx.t Effect.t

let get_fun_ctx () = Effect.perform Get_fun_ctx

module Make (State : State_intf.S) = struct
  module InterpM = InterpM (State)
  open InterpM.Syntax
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
    let open Csymex.Syntax in
    match Typed.get_ty x with
    | TInt -> Csymex.return (Typed.cast x)
    | TPointer ->
        let x = Typed.cast x in
        if%sat Typed.Ptr.is_at_null_loc x then Csymex.return (Typed.Ptr.ofs x)
        else
          Fmt.kstr Csymex.not_impl
            "Pointer to int that is not at null loc %a at %a" Typed.ppa x
            Fmt_ail.pp_loc (get_loc ())
    | _ -> failwith "cast_to_int a cval?"

  let cast_to_bool (x : [< T.cval ] Typed.t) : [> T.sbool ] Typed.t =
    let open Typed in
    match get_ty x with
    | TInt -> bool_of_int (cast x)
    | TPointer -> not (Ptr.is_null (cast x))
    | _ -> failwith "unreachable"

  type 'err fun_exec =
    args:T.cval Typed.t list ->
    state ->
    (T.cval Typed.t * state, 'err, State.serialized list) Result.t

  let get_param_tys name =
    let ptys = Ail_helpers.get_param_tys name in
    Csymex.of_opt_not_impl ~msg:"Couldn't find function prototype" ptys

  let fold_bindings (bindings : AilSyntax.bindings) ~init ~f =
    ListLabels.fold_left bindings ~init
      ~f:(fun acc (pname, ((loc, duration, _is_register), align, _quals, ty)) ->
        (match duration with
        | AilSyntax.Static | Thread -> raise (Unsupported ("static/tread", loc))
        | _ -> ());
        if Option.is_some align then raise (Unsupported ("align", loc));
        f acc (pname, ty))

  let try_with_unsupported f store state =
    try (InterpM.map_store f) store state
    with Unsupported (msg, loc) ->
      let@ () = with_loc ~loc in
      Csymex.not_impl msg

  let attach_bindings (bindings : AilSyntax.bindings) =
    try_with_unsupported @@ fun store ->
    fold_bindings bindings ~init:store ~f:(fun store (pname, ty) ->
        Store.reserve pname ty store)

  let remove_bindings (bindings : AilSyntax.bindings) =
    try_with_unsupported @@ fun store ->
    fold_bindings bindings ~init:store ~f:(fun store (pname, _) ->
        Store.remove pname store)

  (* We're assuming all bindings declared, and they have already been removed from the store. *)
  let free_bindings (bindings : AilSyntax.bindings) : unit InterpM.t =
   fun store state ->
    let++ state =
      Result.fold_list bindings ~init:state
        ~f:(fun state (pname, ((loc, _, _), _, _, _)) ->
          let@ () = with_loc_immediate ~loc in
          match Store.find_opt pname store with
          | Some { kind = Some (Stackptr ptr); _ } ->
              let++ (), state = State.free ptr state in
              state
          | _ -> Result.ok state)
    in
    ((), store, state)

  let pp_bindings : AilSyntax.bindings Fmt.t =
    Fmt.Dump.iter List.iter Fmt.nop (Fmt.pair Fmt_ail.pp_sym Fmt.nop)

  let remove_and_free_bindings (bindings : AilSyntax.bindings) =
    let* () = free_bindings bindings in
    remove_bindings bindings

  let mk_store params =
    ListLabels.fold_left params ~init:Store.empty
      ~f:(fun store (pname, ty, value) ->
        L.trace (fun m ->
            m "Putting variable to the store: %a" Fmt_ail.pp_sym pname);

        Store.add_value pname value ty store)

  let dealloc_store store state =
    Result.fold_list (Store.bindings store) ~init:state
      ~f:(fun state (_, { kind; _ }) ->
        match kind with
        | Some (Stackptr ptr) ->
            let++ (), st = State.free ptr state in
            st
        | _ -> Result.ok state)

  let get_stack_address (sym : Ail_tys.sym) : T.sptr Typed.t option InterpM.t =
    let* binding = InterpM.Store.find_opt sym in
    match binding with
    | Some { kind = None; _ } | None -> InterpM.ok None
    | Some { kind = Some (Stackptr ptr); _ } -> InterpM.ok (Some ptr)
    | Some { kind = Some other; ty } ->
        let* ptr = InterpM.State.alloc_ty ty in
        let* () =
          match other with
          | Value v -> InterpM.State.store ptr ty v
          | _ -> InterpM.ok ()
        in
        let+ () = InterpM.map_store (Store.add_stackptr sym ptr ty) in
        Some ptr

  let try_immediate_load e (store : Store.t) state =
    let (AilSyntax.AnnotatedExpression (_, _, _, e)) = e in
    match e with
    | AilEident id -> (
        let id = Ail_helpers.resolve_sym id in
        match Store.find_opt id store with
        | Some { kind = Some (Value v); _ } -> Result.ok (Some v, store, state)
        | Some { kind = Some Uninit; _ } ->
            State.error `UninitializedMemoryAccess state
        | _ -> Result.ok (None, store, state))
    | _ -> Result.ok (None, store, state)

  let try_immediate_store lvalue rval (store : Store.t) state =
    let (AilSyntax.AnnotatedExpression (_, _, _, lvalue)) = lvalue in
    match lvalue with
    | AilEident id -> (
        let id = Ail_helpers.resolve_sym id in
        match Store.find_opt id store with
        | Some { kind = Some (Value _ | Uninit); ty } ->
            let store = Store.add_value id rval ty store in
            Result.ok (`Success, store, state)
        | _ -> Result.ok (`NotImmediate, store, state))
    | _ -> Result.ok (`NotImmediate, store, state)

  let value_of_constant (c : constant) : T.cval Typed.t Csymex.t =
    let open Csymex.Syntax in
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

  let debug_show ~args:_ state =
    let loc = get_loc () in
    let str = (Fmt.to_to_string (State.pp_pretty ~ignore_freed:false)) state in
    Csymex.push_give_up (str, loc);
    Result.ok (0s, state)

  let unwrap_expr (AnnotatedExpression (_, _, _, e) : expr) = e

  let find_stub (fname : Cerb_frontend.Symbol.sym) : 'err fun_exec option =
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
    let open Csymex.Syntax in
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

  let rec equality_check (v1 : [< Typed.T.cval ] Typed.t)
      (v2 : [< Typed.T.cval ] Typed.t) =
    match (Typed.get_ty v1, Typed.get_ty v2) with
    | TInt, TInt | TPointer, TPointer ->
        InterpM.ok (v1 ==@ v2 |> Typed.int_of_bool)
    | TPointer, TInt ->
        let v2 : T.sint Typed.t = Typed.cast v2 in
        if%sat Typed.(v2 ==@ zero) then
          InterpM.ok (v1 ==@ Typed.Ptr.null |> Typed.int_of_bool)
        else InterpM.error `UBPointerComparison
    | TInt, TPointer -> equality_check v2 v1
    | _ ->
        Fmt.kstr InterpM.not_impl "Unexpected types in cval equality: %a and %a"
          Typed.ppa v1 Typed.ppa v2

  let rec arith_add (v1 : [< Typed.T.cval ] Typed.t)
      (v2 : [< Typed.T.cval ] Typed.t) =
    match (Typed.get_ty v1, Typed.get_ty v2) with
    | TInt, TInt ->
        let v1 = Typed.cast v1 in
        let v2 = Typed.cast v2 in
        InterpM.ok (v1 +@ v2)
    | TPointer, TInt ->
        let v1 : T.sptr Typed.t = Typed.cast v1 in
        let v2 : T.sint Typed.t = Typed.cast v2 in
        let loc = Typed.Ptr.loc v1 in
        let ofs = Typed.Ptr.ofs v1 +@ v2 in
        InterpM.ok (Typed.Ptr.mk loc ofs)
    | TInt, TPointer -> arith_add v2 v1
    | TPointer, TPointer -> InterpM.error `UBPointerArithmetic
    | _ ->
        Fmt.kstr InterpM.not_impl "Unexpected types in addition: %a and %a"
          Typed.ppa v1 Typed.ppa v2

  let arith_sub (v1 : [< Typed.T.cval ] Typed.t)
      (v2 : [< Typed.T.cval ] Typed.t) =
    match (Typed.get_ty v1, Typed.get_ty v2) with
    | TInt, TInt ->
        let v1 = Typed.cast v1 in
        let v2 = Typed.cast v2 in
        InterpM.ok (v1 -@ v2)
    | TPointer, TInt ->
        let v1 : T.sptr Typed.t = Typed.cast v1 in
        let v2 : T.sint Typed.t = Typed.cast v2 in
        let loc = Typed.Ptr.loc v1 in
        let ofs = Typed.Ptr.ofs v1 -@ v2 in
        InterpM.ok (Typed.Ptr.mk loc ofs)
    | TPointer, TPointer ->
        let v1 : T.sptr Typed.t = Typed.cast v1 in
        let v2 : T.sptr Typed.t = Typed.cast v2 in
        if%sat Typed.Ptr.loc v1 ==@ Typed.Ptr.loc v2 then
          InterpM.ok (Typed.Ptr.ofs v1 -@ Typed.Ptr.ofs v2)
        else InterpM.error `UBPointerArithmetic
    | _ ->
        Fmt.kstr InterpM.not_impl "Unexpected types in addition: %a and %a"
          Typed.ppa v1 Typed.ppa v2

  let arith_mul (v1 : [< Typed.T.cval ] Typed.t)
      (v2 : [< Typed.T.cval ] Typed.t) =
    match (Typed.get_ty v1, Typed.get_ty v2) with
    | TInt, TInt ->
        let v1 = Typed.cast v1 in
        let v2 = Typed.cast v2 in
        InterpM.ok (v1 *@ v2)
    | TPointer, _ | _, TPointer -> InterpM.error `UBPointerArithmetic
    | _ ->
        Fmt.kstr InterpM.not_impl
          "Unexpected types in multiplication: %a and %a" Typed.ppa v1 Typed.ppa
          v2

  let arith (v1, t1) a_op (v2, t2) : [> T.sint ] Typed.t InterpM.t =
    match (a_op : AilSyntax.arithmeticOperator) with
    | Div -> (
        let^ v1 = cast_to_int v1 in
        let^ v2 = Csymex.bind (cast_to_int v2) Csymex.check_nonzero in
        match v2 with
        | Ok v2 -> InterpM.ok (v1 /@ v2)
        | Error `NonZeroIsZero -> InterpM.error `DivisionByZero
        | Missing _ -> failwith "Unreachable: check_nonzero returned miss")
    | Mul -> arith_mul v1 v2
    | Add -> (
        match (t1 |> pointer_inner, t2 |> pointer_inner) with
        | Some _, Some _ -> InterpM.error `UBPointerArithmetic
        | Some ty, None ->
            let^ factor = Layout.size_of_s ty in
            let* v2 = arith_mul v2 factor in
            arith_add v1 v2
        | None, Some ty ->
            let^ factor = Layout.size_of_s ty in
            let* v1 = arith_mul v1 factor in
            arith_add v2 v1
        | None, None -> arith_add v1 v2)
    | Sub -> (
        match (t1 |> pointer_inner, t2 |> pointer_inner) with
        | Some ty, None ->
            let^ factor = Layout.size_of_s ty in
            let* v2 = arith_mul v2 factor in
            arith_sub v1 v2
        | None, Some _ -> InterpM.error `UBPointerArithmetic
        | Some _, Some _ | None, None -> arith_sub v1 v2)
    | Band ->
        (* TODO: is it guaranteed that both have the same type? *)
        let* { bv_size; signed } =
          Layout.bv_info t1 |> InterpM.of_opt_not_impl ~msg:"bv_info"
        in
        let^ v1 = cast_to_int v1 in
        let^ v2 = cast_to_int v2 in
        InterpM.ok (Typed.bit_and ~size:bv_size ~signed v1 v2)
    | _ ->
        Fmt.kstr InterpM.not_impl "Unsupported arithmetic operator: %a"
          Fmt_ail.pp_arithop a_op

  let try_immediate_postfix_op ~apply_op lvalue =
    let* v_opt = try_immediate_load lvalue in
    match v_opt with
    | Some v -> (
        (* Optimisation *)
        (* If the value is in direct access, we can just increment it and
          immediately update it in the store. The writing cannot fail. *)
        let* res = apply_op v in
        let* store = try_immediate_store lvalue res in
        match store with
        | `NotImmediate ->
            failwith "Immediate store failed after immediate load succeeded"
        | `Success -> InterpM.ok (Some res))
    | None -> InterpM.ok None

  (* We do this in the untyped world *)
  let ineq_comparison ~cmp_op left right =
    let cmp_op left right = cmp_op left right |> Typed.int_of_bool in
    match (Typed.get_ty left, Typed.get_ty right) with
    | TInt, TInt ->
        let left = Typed.cast left in
        let right = Typed.cast right in
        InterpM.ok (cmp_op left right)
    | TPointer, TPointer ->
        let left = Typed.cast left in
        let right = Typed.cast right in
        if%sat Typed.Ptr.loc left ==@ Typed.Ptr.loc right then
          InterpM.ok (cmp_op (Typed.Ptr.ofs left) (Typed.Ptr.ofs right))
        else InterpM.error `UBPointerComparison
    | _ -> InterpM.error `UBPointerComparison

  let rec resolve_function fexpr : Error.t State.err fun_exec InterpM.t =
    let* loc, fname =
      let (AilSyntax.AnnotatedExpression (_, _, loc, inner_expr)) = fexpr in
      match inner_expr with
      (* Special case when we can immediately resolve the function name *)
      | AilEfunction_decay (AnnotatedExpression (_, _, _, AilEident fname)) ->
          InterpM.ok (loc, fname)
      | _ ->
          (* Some function pointer *)
          L.trace (fun m ->
              m "Resolving function pointer: %a" Fmt_ail.pp_expr fexpr);
          let* fptr = eval_expr fexpr in
          L.trace (fun m -> m "Function pointer is value: %a" Typed.ppa fptr);
          let fptr = cast_to_ptr fptr in
          if%sat
            Typed.not (Typed.Ptr.ofs fptr ==@ 0s)
            ||@ Typed.Ptr.is_at_null_loc fptr
          then InterpM.error `InvalidFunctionPtr
          else
            let fctx = get_fun_ctx () in
            let^ sym = Fun_ctx.get_sym (Typed.Ptr.loc fptr) fctx in
            InterpM.ok (loc, sym)
    in
    let@ () = InterpM.with_loc ~loc in

    let fundef_opt = Ail_helpers.find_fun_def fname in
    match fundef_opt with
    | Some fundef -> InterpM.ok (exec_fun fundef)
    | None -> (
        match find_stub fname with
        | Some stub -> InterpM.ok stub
        | None ->
            Fmt.kstr InterpM.not_impl "Cannot call external function: %a"
              Fmt_ail.pp_sym fname)

  and eval_expr_list (el : expr list) =
    let+ vs =
      InterpM.fold_list el ~init:[] ~f:(fun acc e ->
          let+ new_res = eval_expr e in
          new_res :: acc)
    in
    List.rev vs

  and eval_expr (aexpr : expr) : [> T.cval ] Typed.t InterpM.t =
    let (AnnotatedExpression (_, _, loc, expr)) = aexpr in
    let@ () = InterpM.with_loc ~loc in
    match expr with
    | AilEconst c ->
        let^ v = value_of_constant c in
        InterpM.ok v
    | AilEcall (f, args) ->
        let* exec_fun = resolve_function f in
        let* args = eval_expr_list args in
        let+ v =
          InterpM.with_extra_call_trace ~loc ~msg:"Called from here"
          @@ InterpM.lift_state_op
          @@ exec_fun ~args
        in
        L.debug (fun m -> m "returned %a from %a" Typed.ppa v Fmt_ail.pp_expr f);
        v
    | AilEunary (Address, e) -> (
        match unwrap_expr e with
        | AilEunary (Indirection, e) -> (* &*e <=> e *) eval_expr e
        | AilEident id -> (
            let id = Ail_helpers.resolve_sym id in
            let* ptr_opt = get_stack_address id in
            match ptr_opt with
            | Some ptr -> InterpM.ok (ptr :> T.cval Typed.t)
            | None -> InterpM.State.get_global id)
        | AilEmemberofptr (ptr, member) ->
            let* ptr_v = eval_expr ptr in
            let^ ty_pointee =
              type_of ptr
              |> Cerb_frontend.AilTypesAux.referenced_type
              |> Csymex.of_opt_not_impl
                   ~msg:"Member of Pointer that isn't of type pointer"
            in
            let^ mem_ofs = Layout.member_ofs member ty_pointee in
            arith_add ptr_v mem_ofs
        | _ ->
            Fmt.kstr InterpM.not_impl "Unsupported address_of: %a"
              Fmt_ail.pp_expr e)
    | AilEunary (((PostfixIncr | PostfixDecr) as op), e) -> (
        let apply_op v =
          let^ operand =
            match pointer_inner (type_of e) with
            | Some ty -> Layout.size_of_s ty
            | None -> return 1s
          in
          match op with
          | PostfixIncr -> arith_add v operand
          | PostfixDecr -> arith_sub v operand
          | _ -> failwith "unreachable: postfix is not postfix??"
        in
        let* res_opt = try_immediate_postfix_op ~apply_op e in
        match res_opt with
        | Some v -> InterpM.ok v
        | None ->
            let* v = eval_expr e in
            let ptr = cast_to_ptr v in
            let* v = InterpM.State.load ptr (type_of e) in
            let* v_incr = apply_op v in
            let+ () = InterpM.State.store ptr (type_of e) v_incr in
            v)
    | AilEunary (op, e) -> (
        let* v = eval_expr e in
        match op with
        | Indirection -> InterpM.ok v
        | Address -> failwith "unreachable: address_of already handled"
        | Minus ->
            let^ v = cast_to_int v in
            arith_sub Typed.zero v
        | _ ->
            Fmt.kstr InterpM.not_impl "Unsupported unary operator %a"
              Fmt_ail.pp_unop op)
    | AilEbinary (e1, Or, e2) ->
        (* Or is short-circuiting. In case of side-effects on the RHS,
           not doing this properly might lead to unsoundnesses.
           We still optimize by returning a disjunction of the two
           expressions if the RHS is side-effect free. *)
        let* v1 = eval_expr e1 in
        if Ail_helpers.sure_side_effect_free e2 then
          let+ v2 = eval_expr e2 in
          let b_res = cast_to_bool v1 ||@ cast_to_bool v2 in
          Typed.int_of_bool b_res
        else
          if%sat cast_to_bool v1 then InterpM.ok Typed.one
          else
            let+ v2 = eval_expr e2 in
            let b_res = cast_to_bool v2 in
            Typed.int_of_bool b_res
    | AilEbinary (e1, And, e2) ->
        (* Same as Or, we need to short-circuit *)
        let* v1 = eval_expr e1 in
        if Ail_helpers.sure_side_effect_free e2 then
          let+ v2 = eval_expr e2 in
          let b_res = cast_to_bool v1 &&@ cast_to_bool v2 in
          Typed.int_of_bool b_res
        else
          if%sat cast_to_bool v1 then
            let+ v2 = eval_expr e2 in
            let b_res = cast_to_bool v2 in
            Typed.int_of_bool b_res
          else InterpM.ok Typed.zero
    | AilEbinary (e1, op, e2) -> (
        let* v1 = eval_expr e1 in
        let* v2 = eval_expr e2 in
        match op with
        | Ge -> ineq_comparison ~cmp_op:( >=@ ) v1 v2
        | Gt -> ineq_comparison ~cmp_op:( >@ ) v1 v2
        | Lt -> ineq_comparison ~cmp_op:( <@ ) v1 v2
        | Le -> ineq_comparison ~cmp_op:( <=@ ) v1 v2
        | Eq -> equality_check v1 v2
        | Ne ->
            (* TODO: Semantics of Ne might be different from semantics of not eq? *)
            let+ res = equality_check v1 v2 in
            Typed.not_int_bool res
        | Or | And -> failwith "Unreachable, handled earlier."
        | Arithmetic a_op -> arith (v1, type_of e1) a_op (v2, type_of e2)
        | Comma -> InterpM.ok v2)
    | AilErvalue e -> (
        (* Optimisation: If the expression to load is a variable that is
           immediately in the store (without heap indirection),
           we can just access it without heap shenanigans. *)
        let* v_opt = try_immediate_load e in
        match v_opt with
        | Some v -> InterpM.ok v
        | None ->
            (* The value is not an immediate store load *)
            let* lvalue = eval_expr e in
            let ty = type_of e in
            (* At this point, lvalue must be a pointer (including to the stack) *)
            let lvalue = cast_to_ptr lvalue in
            InterpM.State.load lvalue ty)
    | AilEident id -> (
        let id = Ail_helpers.resolve_sym id in
        let* ptr_opt = get_stack_address id in
        match ptr_opt with
        | Some v ->
            (* A pointer is a value *)
            let v = (v :> T.cval Typed.t) in
            InterpM.ok v
        | None ->
            (* If the variable isn't in the store, it must be a global variable. *)
            InterpM.State.get_global id)
    | AilEassign (lvalue, rvalue) -> (
        (* Evaluate rvalue first *)
        let* rval = eval_expr rvalue in
        (* Optimisation: if the lvalue is a variable to which we assign directly,
           we don't need to do anything with the heap, we can simply immediately assign,
           obtaining a new store.  *)
        let* im_store_res = try_immediate_store lvalue rval in
        match im_store_res with
        | `Success -> InterpM.ok rval
        | `NotImmediate ->
            let* ptr = eval_expr lvalue in
            (* [ptr] is a necessarily a pointer, and [rval] is a memory value.
               I don't support pointer fragments for now, so let's say it's an *)
            let ptr = cast_to_ptr ptr in
            let ty = type_of lvalue in
            let+ () = InterpM.State.store ptr ty rval in
            rval)
    | AilEcompoundAssign (lvalue, op, rvalue) -> (
        let* rval = eval_expr rvalue in
        let rty = type_of rvalue in
        let lty = type_of lvalue in
        let apply_op v = arith (v, lty) op (rval, rty) in
        let* immediate_result = try_immediate_postfix_op ~apply_op lvalue in
        match immediate_result with
        | Some v -> InterpM.ok v
        | None ->
            (* Otherwise we proceed as normal *)
            let* ptr = eval_expr lvalue in
            (* At this point, lvalue must be a pointer (including to the stack) *)
            let ptr = cast_to_ptr ptr in
            let* operand = InterpM.State.load ptr lty in
            let* res = apply_op operand in
            let+ () = InterpM.State.store ptr lty res in
            res)
    | AilEsizeof (_quals, ty) ->
        let^ res = Layout.size_of_s ty in
        InterpM.ok res
    | AilEmemberofptr (ptr, member) ->
        let* ptr_v = eval_expr ptr in
        let^ ty_pointee =
          type_of ptr
          |> Cerb_frontend.AilTypesAux.referenced_type
          |> Csymex.of_opt_not_impl
               ~msg:"Member of Pointer that isn't of type pointer"
        in
        let^ mem_ofs = Layout.member_ofs member ty_pointee in
        arith_add ptr_v mem_ofs
    | AilEmemberof (obj, member) ->
        let* ptr_v = eval_expr obj in
        let ty_obj = type_of obj in
        let^ mem_ofs = Layout.member_ofs member ty_obj in
        arith_add ptr_v mem_ofs
    | AilEcast (_quals, new_ty, expr) ->
        let old_ty = type_of expr in
        let* v = eval_expr expr in
        InterpM.lift_symex @@ cast ~old_ty ~new_ty v
    | AilEfunction_decay (AnnotatedExpression (_, _, _, fexpr) as outer_fexpr)
      -> (
        match fexpr with
        | AilEident id ->
            let id = Ail_helpers.resolve_sym id in
            let ctx = get_fun_ctx () in
            let^ floc = Fun_ctx.decay_fn_sym id ctx in
            InterpM.ok (Typed.Ptr.mk floc 0s)
        | _ ->
            Fmt.kstr InterpM.not_impl "Unsupported function decay: %a"
              Fmt_ail.pp_expr outer_fexpr)
    | AilEinvalid (_ty, reason) ->
        Fmt.kstr InterpM.not_impl
          "Cerberus could not parse an expression because %a"
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
        Fmt.kstr InterpM.not_impl "Unsupported expr: %a" Fmt_ail.pp_expr aexpr

  (** Executing a statement returns an optional value outcome (if a return
      statement was hit), or *)
  and exec_stmt (astmt : stmt) : T.cval Typed.t option InterpM.t =
    let^ () = Csymex.consume_fuel_steps 1 in
    L.debug (fun m -> m "Executing statement: %a" Fmt_ail.pp_stmt astmt);
    let* () =
      let+ store = InterpM.get_store () in
      L.debug (fun m -> m "@[<v 2>STORE:@ %a@]" Store.pp store)
    in
    Stats.incr_executed_statements ();
    let AilSyntax.{ loc; node = stmt; _ } = astmt in
    let@ () = InterpM.with_loc ~loc in
    match stmt with
    | AilSskip -> InterpM.ok None
    | AilSreturn e ->
        let+ v = eval_expr e in
        L.debug (fun m -> m "Returning: %a" Typed.ppa v);
        Some v
    | AilSreturnVoid -> InterpM.ok (Some 0s)
    | AilSblock (bindings, stmtl) ->
        let* () = attach_bindings bindings in
        (* Second result, corresponding to the block-scoped store, is discarded *)
        let* res =
          InterpM.fold_list stmtl ~init:None ~f:(fun res stmt ->
              match res with Some _ -> InterpM.ok res | None -> exec_stmt stmt)
        in

        (* Cerberus is nice here, symbols inside the block have different names than
           the ones outside the block if there is shadowing going on.
           I.e. int x = 12; { int x = 13; }, the two `x`s have different symbols.
           So we can just remove the bindings of the inner block from the store entirely. *)
        let+ () = remove_and_free_bindings bindings in
        res
    | AilSexpr e ->
        let+ _ = eval_expr e in
        None
    | AilSif (cond, then_stmt, else_stmt) ->
        let* v = eval_expr cond in
        (* [v] must be an integer! (TODO: or NULL possibly...) *)
        let v = cast_to_bool v in
        if%sat v then exec_stmt then_stmt [@name "if branch"]
        else exec_stmt else_stmt [@name "else branch"]
    | AilSwhile (cond, stmt, _loopid) ->
        let rec loop () =
          let* cond_v = eval_expr cond in
          if%sat cast_to_bool cond_v then
            let* res = exec_stmt stmt in
            match res with Some _ -> InterpM.ok res | None -> loop ()
          else InterpM.ok None
        in
        loop ()
    | AilSdo (stmt, cond, _loop_id) ->
        let rec loop () =
          let* res = exec_stmt stmt in
          match res with
          | Some _ -> InterpM.ok res
          | None ->
              let* cond_v = eval_expr cond in
              if%sat cast_to_bool cond_v then loop () else InterpM.ok None
        in
        loop ()
    | AilSlabel (_label, stmt, _annot) ->
        (* TODO: keep track of labels in a record or something!! *)
        exec_stmt stmt
    | AilSdeclaration decls ->
        let+ () =
          InterpM.fold_list decls ~init:() ~f:(fun () (pname, expr) ->
              match expr with
              | None -> InterpM.map_store (Store.declare_uninit pname)
              | Some expr ->
                  let* v = eval_expr expr in
                  InterpM.map_store (Store.declare_value pname v))
        in
        None
    | _ ->
        Fmt.kstr InterpM.not_impl "Unsupported statement: %a" Fmt_ail.pp_stmt
          astmt

  and exec_fun (fundef : fundef) ~args state =
    let open Csymex.Syntax in
    (* Put arguments in store *)
    let name, (loc, _, _, params, stmt) = fundef in
    let@ () = with_loc ~loc in
    L.debug (fun m -> m "Executing function %a" Fmt_ail.pp_sym name);
    L.trace (fun m ->
        m "Was given arguments: %a" (Fmt.Dump.list Typed.ppa) args);
    let* ptys = get_param_tys name in
    let ps = List.combine3 params ptys args in
    let store = mk_store ps in
    let** val_opt, store, state = exec_stmt stmt store state in
    let++ state = dealloc_store store state in
    (* We model void as zero, it should never be used anyway *)
    let value = Option.value ~default:0s val_opt in
    (value, state)

  let init_prog_state (prog : Ail_tys.linked_program) =
    let open Csymex.Syntax in
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
      Ok state
    in
    let produce_value (ptr : [< T.sptr ] Typed.t) ty expr (state : State.t) =
      let loc = Typed.Ptr.loc ptr in
      let offset = Typed.Ptr.ofs ptr in
      (* I somehow have to support global initialisation urgh.
       I might be able to extract some of that into interp *)
      let** v, _, state = eval_expr expr Store.empty state in
      let serialized : State.serialized =
        {
          heap =
            [ (loc, Freeable.Alive [ Tree_block.TypedVal { offset; ty; v } ]) ];
          globs = [];
        }
      in
      let+ state = State.produce serialized state in
      Ok state
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
