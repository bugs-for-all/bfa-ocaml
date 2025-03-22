open Bfa_symex.Compo_res
open Rustsymex
open Rustsymex.Syntax
open Typed.Infix
open Typed.Syntax
module Utils_ = Utils
open Charon
open Charon_util
module T = Typed.T

module Make (Heap : Heap_intf.S) = struct
  module Std_funs = Std_funs.M (Heap)

  exception Unsupported of (string * Meta.span)

  type state = Heap.t
  type store = (T.sptr Typed.t option * Types.ty) Store.t

  let cast_checked ~ty x =
    match Typed.cast_checked x ty with
    | Some x -> Rustsymex.return x
    | None ->
        Fmt.kstr Rustsymex.not_impl "Failed to cast %a to %a" Typed.ppa x
          Typed.ppa_ty ty

  type 'err fun_exec =
    crate:UllbcAst.crate ->
    args:rust_val list ->
    state:state ->
    (rust_val * state, 'err, Heap.serialized list) Result.t

  let alloc_stack (locals : GAst.locals) args st =
    if List.length args <> locals.arg_count then
      Fmt.failwith "Function expects %d arguments, but got %d" locals.arg_count
        (List.length args);
    Rustsymex.Result.fold_list locals.vars ~init:(Store.empty, st)
      ~f:(fun (store, st) { index; var_ty = ty; _ } ->
        let** ptr, st = Heap.alloc_ty ty st in
        let store = Store.add index (Some ptr, ty) store in
        let index = Expressions.VarId.to_int index in
        if 0 < index && index <= locals.arg_count then
          let value = List.nth args (index - 1) in
          let++ (), st = Heap.store ptr ty value st in
          (store, st)
        else Result.ok (store, st))

  let dealloc_store ?protected_address store st =
    Rustsymex.Result.fold_list (Store.bindings store) ~init:st
      ~f:(fun st (_, (ptr, _)) ->
        match (ptr, protected_address) with
        | None, _ -> Result.ok st
        | Some ptr, None ->
            let++ (), st = Heap.free ptr st in
            st
        | Some ptr, Some protect ->
            if%sat ptr ==@ protect then Result.ok st
            else
              let++ (), st = Heap.free ptr st in
              st)

  let resolve_constant (const : Expressions.constant_expr) state =
    match const with
    | { value = CLiteral (VScalar scalar); _ } ->
        Result.ok (Base (value_of_scalar scalar), state)
    | { value = CLiteral (VBool b); _ } ->
        Result.ok (Base (if b then Typed.one else Typed.zero), state)
    | { value = CLiteral (VChar c); _ } ->
        Result.ok (Base (Typed.int (Char.code c)), state)
    | { value = CLiteral (VStr str); _ } -> (
        let** ptr_opt, state = Heap.load_str_global str state in
        match ptr_opt with
        | Some v -> Result.ok (v, state)
        | None ->
            (* We "cheat" and model strings as a vector of chars, with &str a slice *)
            let len = String.length str in
            let chars =
              String.to_seq str
              |> Seq.fold_left
                   (fun l c -> Base (Typed.int (Char.code c)) :: l)
                   []
              |> List.rev
            in
            let char_arr = Array chars in
            let str_ty : Types.ty = mk_array_ty (TLiteral TChar) len in
            let** ptr, state = Heap.alloc_ty str_ty state in
            let** (), state = Heap.store ptr str_ty char_arr state in
            let value = FatPtr (ptr, Typed.int len) in
            let++ (), state = Heap.store_str_global str value state in
            (value, state))
    | e ->
        Fmt.kstr not_impl "TODO: resolve_constant %a"
          Expressions.pp_constant_expr e

  (** Resolves a place to a pointer, in the form of a rust_val. We use rust_val
      rather than T.sptr Typed.t, to be able to handle fat pointers; however
      there is the guarantee that this function returns either a Base or a
      FatPointer value. *)
  let rec resolve_place ~store state ({ kind; _ } : Expressions.place) =
    match kind with
    (* Just a variable *)
    | PlaceBase var -> (
        let ptr = Store.find_value var store in
        match ptr with
        | Some ptr ->
            L.debug (fun f ->
                f "Found pointer %a of variable %a" Typed.ppa ptr
                  Expressions.pp_var_id var);
            Result.ok (Ptr ptr, state)
        | None ->
            Fmt.kstr not_impl "Variable %a not found in store"
              Expressions.pp_var_id var)
    (* Dereference a pointer *)
    | PlaceProjection (base, Deref) ->
        let** ptr, state = resolve_place ~store state base in
        L.debug (fun f ->
            f "Dereferencing ptr %a of %a" pp_rust_val ptr Types.pp_ty base.ty);
        let ptr_raw = as_ptr ptr in
        let** v, state = Heap.load ptr_raw base.ty state in
        L.debug (fun f ->
            f "Dereferenced pointer %a to pointer %a" pp_rust_val ptr
              pp_rust_val v);
        Result.ok (v, state)
    | PlaceProjection (base, Field (kind, field)) ->
        let** ptr, state = resolve_place ~store state base in
        L.debug (fun f ->
            f "Projecting field %a (kind %a) for %a" Types.pp_field_id field
              Expressions.pp_field_proj_kind kind pp_rust_val ptr);
        let ptr = as_ptr ptr in
        let field = Types.FieldId.to_int field in
        let layout, field =
          match kind with
          | ProjAdt (adt_id, Some variant) ->
              (* Skip discriminator, so field + 1 *)
              (Layout.of_enum_variant adt_id variant, field + 1)
          | ProjAdt (adt_id, None) -> (Layout.of_adt_id adt_id, field)
          | ProjTuple _arity -> (Layout.layout_of base.ty, field)
        in
        let offset = Array.get layout.members_ofs field in
        let ptr = Typed.Ptr.add_ofs ptr (Typed.int offset) in
        L.debug (fun f ->
            f
              "Dereferenced ADT projection %a, field %d, with pointer %a to \
               pointer %a"
              Expressions.pp_field_proj_kind kind field Typed.ppa ptr Typed.ppa
              ptr);
        Result.ok (Ptr ptr, state)

  let overflow_check state v (ty : Types.ty) =
    let* constraints =
      match ty with
      | TLiteral ty ->
          Rustsymex.of_opt_not_impl ~msg:"Constraints for overflow check"
            (Layout.constraints ty)
      | ty ->
          Fmt.kstr Rustsymex.not_impl "Unexpected type for overflow check: %a"
            Types.pp_ty ty
    in
    if%sat Typed.conj @@ constraints v then Rustsymex.Result.ok (v, state)
    else Heap.error `Overflow state

  let rec equality_check ~state (v1 : [< Typed.T.cval ] Typed.t)
      (v2 : [< Typed.T.cval ] Typed.t) =
    match (Typed.get_ty v1, Typed.get_ty v2) with
    | TInt, TInt | TPointer, TPointer ->
        Result.ok (v1 ==@ v2 |> Typed.int_of_bool, state)
    | TPointer, TInt ->
        let v2 : T.sint Typed.t = Typed.cast v2 in
        if%sat Typed.(v2 ==@ zero) then
          Result.ok (v1 ==@ Typed.Ptr.null |> Typed.int_of_bool, state)
        else Heap.error `UBPointerComparison state
    | TInt, TPointer -> equality_check ~state v2 v1
    | _ ->
        Fmt.kstr not_impl "Unexpected types in cval equality: %a and %a"
          Typed.ppa v1 Typed.ppa v2

  let arith_add ~state (v1 : [< Typed.T.cval ] Typed.t)
      (v2 : [< Typed.T.cval ] Typed.t) =
    match ((Typed.get_ty v1, v1), (Typed.get_ty v2, v2)) with
    | (TInt, v1), (TInt, v2) ->
        let v1 = Typed.cast v1 in
        let v2 = Typed.cast v2 in
        Result.ok (v1 +@ v2, state)
    | (TPointer, v1), (TInt, v2) | (TInt, v2), (TPointer, v1) ->
        let v1 : T.sptr Typed.t = Typed.cast v1 in
        let v2 : T.sint Typed.t = Typed.cast v2 in
        let v' = Typed.Ptr.add_ofs v1 v2 in
        Result.ok (v', state)
    | (TPointer, _), (TPointer, _) -> Heap.error `UBPointerArithmetic state
    | _ ->
        Fmt.kstr not_impl "Unexpected types in addition: %a and %a" Typed.ppa v1
          Typed.ppa v2

  let arith_mul ~state (v1 : [< Typed.T.cval ] Typed.t)
      (v2 : [< Typed.T.cval ] Typed.t) =
    match (Typed.get_ty v1, Typed.get_ty v2) with
    | TInt, TInt ->
        let v1 = Typed.cast v1 in
        let v2 = Typed.cast v2 in
        Result.ok (v1 *@ v2, state)
    | TPointer, _ | _, TPointer -> Heap.error `UBPointerArithmetic state
    | _ ->
        Fmt.kstr not_impl "Unexpected types in multiplication: %a and %a"
          Typed.ppa v1 Typed.ppa v2

  let rec resolve_function ~(crate : UllbcAst.crate) (fnop : GAst.fn_operand) :
      'err fun_exec Rustsymex.t =
    match fnop with
    | FnOpRegular { func = FunId (FRegular fid); _ }
    | FnOpRegular { func = TraitMethod (_, _, fid); _ } -> (
        let fundef = Expressions.FunDeclId.Map.find fid crate.fun_decls in
        L.info (fun g ->
            let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
            g "Resolved function call to %s"
              (PrintTypes.name_to_string ctx fundef.item_meta.name));
        match Std_funs.std_fun_eval ~crate fundef with
        | Some fn -> Rustsymex.return fn
        | None -> Rustsymex.return (exec_fun fundef))
    | FnOpRegular { func = FunId (FBuiltin fn); generics } ->
        Rustsymex.return @@ Std_funs.builtin_fun_eval ~crate fn generics
    | FnOpMove _ ->
        Fmt.kstr not_impl "Move function call is not supported: %a"
          GAst.pp_fn_operand fnop

  (** Resolves a global into a *pointer* Rust value to where that global is *)
  and resolve_global ~crate (g : Types.global_decl_id) state =
    let decl = UllbcAst.GlobalDeclId.Map.find g UllbcAst.(crate.global_decls) in
    let** v_opt, state = Heap.load_global g state in
    match v_opt with
    | Some v -> Result.ok (v, state)
    | None ->
        let** v, state =
          match Std_globals.global_eval ~crate decl with
          | Some global -> Result.ok (global, state)
          | None ->
              (* Same as with strings -- here we need to somehow cache where we store the globals *)
              let fundef =
                UllbcAst.FunDeclId.Map.find decl.body crate.fun_decls
              in
              L.info (fun g ->
                  let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
                  g "Resolved function call to %s"
                    (PrintTypes.name_to_string ctx fundef.item_meta.name));
              let global_fn =
                match Std_funs.std_fun_eval ~crate fundef with
                | Some fn -> fn
                | None -> exec_fun fundef
              in
              global_fn ~crate ~args:[] ~state
        in
        let** ptr, state = Heap.alloc_ty decl.ty state in
        let** (), state = Heap.store ptr decl.ty v state in
        let ptr = Ptr ptr in
        let++ (), state = Heap.store_global g ptr state in
        (ptr, state)

  and eval_operand ~crate:_ ~store state (op : Expressions.operand) =
    match op with
    | Constant c ->
        let++ v, state = resolve_constant c state in
        (v, state)
    | Move loc ->
        let ty = loc.ty in
        let** loc, state = resolve_place ~store state loc in
        let loc = as_ptr loc in
        let** v, state = Heap.load loc ty state in
        (* TODO: mark value as moved!!! !== freeing it, btw *)
        Result.ok (v, state)
    | Copy loc ->
        let ty = loc.ty in
        let** loc, state = resolve_place ~store state loc in
        let loc = as_ptr loc in
        let** v, state = Heap.load loc ty state in
        Result.ok (v, state)

  and eval_operand_list ~crate ~store state ops =
    let++ vs, state =
      Result.fold_list ops ~init:([], state) ~f:(fun (acc, state) op ->
          let++ new_res, state = eval_operand ~crate ~store state op in
          (new_res :: acc, state))
    in
    (List.rev vs, state)

  and eval_rvalue ~crate ~store state (expr : Expressions.rvalue) =
    let eval_operand = eval_operand ~crate ~store in
    match expr with
    | Use op -> eval_operand state op
    | RvRef (place, _borrow) -> resolve_place ~store state place
    | Global { global_id; _ } ->
        let decl =
          UllbcAst.GlobalDeclId.Map.find global_id UllbcAst.(crate.global_decls)
        in
        let** ptr, state = resolve_global ~crate global_id state in
        let ptr = as_ptr ptr in
        Heap.load ptr decl.ty state
    | GlobalRef ({ global_id; _ }, _mut) ->
        (* TODO: handle mutability *)
        resolve_global ~crate global_id state
    | UnaryOp (op, e) -> (
        let** v, _state = eval_operand state e in
        match op with
        | Not ->
            let v_int = as_base_of ~ty:Typed.t_int v in
            let v' = Base (Typed.not_int_bool v_int) in
            Result.ok (v', state)
        | Neg ->
            let v_int = as_base_of ~ty:Typed.t_int v in
            let v' = Base (0s -@ v_int) in
            Result.ok (v', state)
        | Cast (CastRawPtr (_from, _to)) -> Result.ok (v, state)
        | Cast (CastTransmute (from_ty, to_ty)) -> (
            match (from_ty, to_ty) with
            | TRawPtr _, TLiteral (TInteger Usize) ->
                let v_ptr = as_base_of ~ty:Typed.t_ptr v in
                (* TODO: is this right? Or do we want to convert the location to an integer?
                         (probably not...) *)
                Result.ok (Base v_ptr, state)
            | _ ->
                Fmt.kstr not_impl "Unsupported transmutation, from %a to %a"
                  Types.pp_ty from_ty Types.pp_ty to_ty)
        | Cast (CastScalar (TInteger from_ty, TInteger to_ty)) ->
            let from_size = Layout.size_of_int_ty from_ty in
            let to_size = Layout.size_of_int_ty to_ty in
            if Layout.is_signed from_ty = Layout.is_signed to_ty then
              (* same sign: *)
              if from_size <= to_size then
                (* to a larger number *)
                Result.ok (v, state)
              else if not @@ Layout.is_signed from_ty then
                (* to a smaller number (unsigned) *)
                let max_value = Layout.max_value_z to_ty in
                let v_int = as_base_of ~ty:Typed.t_int v in
                let v_int' = v_int %@ Typed.nonzero_z max_value in
                Result.ok (Base v_int', state)
              else
                (* to a smaller number (signed) *)
                not_impl "Unsupported: integer cast to a smaller signed number"
            else if from_size = to_size then
              (* same size *)
              let min = Typed.int_z @@ Layout.min_value_z from_ty in
              let v_int = as_base_of ~ty:Typed.t_int v in
              let v_int' =
                if Layout.is_signed from_ty then v_int -@ min else v_int +@ min
              in
              Result.ok (Base v_int', state)
            else
              not_impl
                "Unsupported: integer cast with different signedness and sign"
        | Cast
            (CastUnsize
               ( TAdt
                   ( TBuiltin TBox,
                     {
                       types =
                         [
                           TAdt
                             (TBuiltin TArray, { const_generics = [ size ]; _ });
                         ];
                       _;
                     } ),
                 TAdt
                   (TBuiltin TBox, { types = [ TAdt (TBuiltin TSlice, _) ]; _ })
               )) ->
            let ptr = as_ptr v in
            let size = Typed.int @@ int_of_const_generic size in
            Result.ok (FatPtr (ptr, size), state)
        | Cast kind ->
            Fmt.kstr not_impl "Unsupported cast kind: %a"
              Expressions.pp_cast_kind kind)
    | BinaryOp (op, e1, e2) ->
        let** v1, state = eval_operand state e1 in
        let** v2, state = eval_operand state e2 in
        let v1, v2 =
          match (v1, v2) with
          | Base v1, Base v2 -> (v1, v2)
          | _, _ ->
              Fmt.failwith "Expected base values in BinaryOp: %a/%a" pp_rust_val
                v1 pp_rust_val v2
        in
        let++ res, state =
          match op with
          | Ge ->
              (* TODO: comparison operators for pointers *)
              let* v1 = cast_checked v1 ~ty:Typed.t_int in
              let* v2 = cast_checked v2 ~ty:Typed.t_int in
              Result.ok (v1 >=@ v2 |> Typed.int_of_bool, state)
          | Gt ->
              let* v1 = cast_checked v1 ~ty:Typed.t_int in
              let* v2 = cast_checked v2 ~ty:Typed.t_int in
              Result.ok (v1 >@ v2 |> Typed.int_of_bool, state)
          | Lt ->
              let* v1 = cast_checked v1 ~ty:Typed.t_int in
              let* v2 = cast_checked v2 ~ty:Typed.t_int in
              Result.ok (v1 <@ v2 |> Typed.int_of_bool, state)
          | Le ->
              let* v1 = cast_checked v1 ~ty:Typed.t_int in
              let* v2 = cast_checked v2 ~ty:Typed.t_int in
              Result.ok (v1 <=@ v2 |> Typed.int_of_bool, state)
          | Eq ->
              let v1 = Typed.cast v1 in
              let v2 = Typed.cast v2 in
              equality_check ~state v1 v2
          | Ne ->
              let v1 = Typed.cast v1 in
              let v2 = Typed.cast v2 in
              let++ res, state = equality_check ~state v1 v2 in
              (Typed.not_int_bool res, state)
          | Div -> (
              let* v1 = cast_checked v1 ~ty:Typed.t_int in
              let* v2 = cast_checked v2 ~ty:Typed.t_int in
              let* v2 = Rustsymex.check_nonzero v2 in
              match v2 with
              | Ok v2 -> overflow_check state (v1 /@ v2) (type_of_operand e1)
              | Error `NonZeroIsZero -> Heap.error `DivisionByZero state
              | Missing e -> (* Unreachable but still *) Rustsymex.Result.miss e
              )
          | Rem -> (
              let* v1 = cast_checked v1 ~ty:Typed.t_int in
              let* v2 = cast_checked v2 ~ty:Typed.t_int in
              let* v2 = Rustsymex.check_nonzero v2 in
              match v2 with
              | Ok v2 -> (
                  match type_of_operand e1 with
                  | TLiteral (TInteger ty) ->
                      let min_val = Layout.min_value ty in
                      (* Overflow if left is MIN and right is -1 *)
                      if%sat
                        v1
                        ==@ min_val
                        &&@ ((v2 :> T.sint Typed.t) ==@ Typed.int (-1))
                      then Heap.error `Overflow state
                      else Rustsymex.Result.ok (v1 %@ v2, state)
                  | ty ->
                      Fmt.kstr not_impl "Unsupported type for rem: %a"
                        Types.pp_ty ty)
              | Error `NonZeroIsZero -> Heap.error `DivisionByZero state
              | Missing e -> (* Unreachable but still *) Rustsymex.Result.miss e
              )
          | Mul ->
              let v1 = Typed.cast v1 in
              let v2 = Typed.cast v2 in
              arith_mul ~state v1 v2
          | Add ->
              let v1 = Typed.cast v1 in
              let v2 = Typed.cast v2 in
              arith_add ~state v1 v2
          | Sub ->
              let v1 = Typed.cast v1 in
              let v2 = Typed.cast v2 in
              arith_add ~state v1 (Typed.minus 0s v2)
          | bop ->
              Fmt.kstr not_impl "Unsupported binary operator: %a"
                Expressions.pp_binop bop
        in
        (Base res, state)
    | NullaryOp (op, _ty) -> (
        match op with
        | UbChecks ->
            (* See https://doc.rust-lang.org/std/intrinsics/fn.ub_checks.html
               From what I understand: our execution already checks for UB, so we should return
               true to skip past code that manually does these checks. *)
            Result.ok (Base (Typed.int_of_bool Typed.v_true), state)
        | op ->
            Fmt.kstr not_impl "Unsupported nullary operator: %a"
              Expressions.pp_nullop op)
    | Discriminant (place, kind) ->
        let** loc, state = resolve_place ~store state place in
        let loc = as_ptr loc in
        let enum = Types.TypeDeclId.Map.find kind UllbcAst.(crate.type_decls) in
        let* discr_ofs, discr_ty =
          match enum.kind with
          | Enum (var :: _) ->
              let int_ty = var.discriminant.int_ty in
              let layout = Layout.of_variant var in
              let discr_ofs = Typed.int @@ Array.get layout.members_ofs 0 in
              return (discr_ofs, Types.TLiteral (TInteger int_ty))
          | Enum [] ->
              Fmt.kstr not_impl "Unsupported discriminant for empty enums"
          | k ->
              Fmt.failwith "Expected an enum for discriminant, got %a"
                Types.pp_type_decl_kind k
        in
        let loc = Typed.Ptr.add_ofs loc discr_ofs in
        Heap.load loc discr_ty state
    (* Enum aggregate *)
    | Aggregate (AggregatedAdt (TAdtId t_id, Some v_id, None, _), vals) ->
        let type_decl =
          Types.TypeDeclId.Map.find t_id UllbcAst.(crate.type_decls)
        in
        let variant =
          match (type_decl : Types.type_decl) with
          | { kind = Enum variants; _ } -> Types.VariantId.nth variants v_id
          | _ ->
              Fmt.failwith "Unexpected type declaration in enum aggregate: %a"
                Types.pp_type_decl type_decl
        in
        let discr = value_of_scalar variant.discriminant in
        let++ vals, state = eval_operand_list ~crate ~store state vals in
        (Enum (discr, vals), state)
    (* Union aggregate *)
    | Aggregate (AggregatedAdt (_, None, Some _, _), _) as v ->
        Fmt.kstr not_impl "Union rvalues not supported: %a"
          Expressions.pp_rvalue v
    (* Special case? unit (zero-tuple) *)
    | Aggregate (AggregatedAdt (TTuple, None, None, _), operands) ->
        let++ values, state = eval_operand_list ~crate ~store state operands in
        (Tuple values, state)
    (* Struct aggregate *)
    | Aggregate (AggregatedAdt (TAdtId _, None, None, _), operands) ->
        let++ values, state = eval_operand_list ~crate ~store state operands in
        (Struct values, state)
    (* Invalid aggregate (not sure, but seems like it) *)
    | Aggregate ((AggregatedAdt _ as v), _) ->
        Fmt.failwith "Invalid aggregate kind: %a" Expressions.pp_aggregate_kind
          v
    (* Array aggregate *)
    | Aggregate (AggregatedArray (_ty, _size), operands) ->
        let++ values, state = eval_operand_list ~crate ~store state operands in
        (Array values, state)
    | Aggregate (AggregatedClosure _, _) ->
        not_impl "Unsupported rvalue: aggregated closure"
    (* Raw pointer *)
    | RawPtr (place, _kind) ->
        let++ ptr, state = resolve_place ~store state place in
        (ptr, state)
    (* Length of a &[T;N] or &[T] *)
    | Len (place, _, size_opt) ->
        let** ptr, state = resolve_place ~store state place in
        let+ len =
          match (ptr, size_opt) with
          | Ptr _, Some size -> return (Typed.int @@ int_of_const_generic size)
          | FatPtr (_, len), None -> return len
          | _ -> not_impl "Unexpected len rvalue"
        in
        Bfa_symex.Compo_res.Ok (Base len, state)

  and exec_stmt ~crate store state astmt :
      (store * state, 'err, Heap.serialized list) Rustsymex.Result.t =
    L.info (fun m ->
        let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
        m "Statement: %s" (PrintUllbcAst.Ast.statement_to_string ctx "" astmt));
    L.debug (fun m ->
        m "Statement full:@.%a" UllbcAst.pp_raw_statement astmt.content);
    let* () = Rustsymex.consume_fuel_steps 1 in
    let { span = loc; content = stmt; _ } : UllbcAst.statement = astmt in
    let@ () = with_loc ~loc in
    match stmt with
    | Nop -> Result.ok (store, state)
    (* small optimisation, avoids parsing to-from rust. *)
    | Assign (({ ty; _ } as dst_place), Use (Copy src_place)) ->
        let** dst, state = resolve_place ~store state dst_place in
        let dst = as_ptr dst in
        let** src, state = resolve_place ~store state src_place in
        let src = as_ptr src in
        let* size = Layout.size_of_s ty in
        let++ (), state = Heap.copy_nonoverlapping ~dst ~src ~size state in
        (store, state)
    | Assign (({ ty; _ } as place), rval) ->
        let** ptr, state = resolve_place ~store state place in
        let ptr = as_ptr ptr in
        let** v, state = eval_rvalue ~crate ~store state rval in
        let++ (), state = Heap.store ptr ty v state in
        (store, state)
    | Call { func; args; dest = { kind = PlaceBase var; ty } } ->
        let* exec_fun = resolve_function ~crate func in
        let** args, state = eval_operand_list ~crate ~store state args in
        L.info (fun g ->
            g "Executing function with arguments [%a]"
              Fmt.(list ~sep:comma pp_rust_val)
              args);
        let** v, state =
          let+- err = exec_fun ~crate ~args ~state in
          Heap.add_to_call_trace err
            (Call_trace.make_element ~loc ~msg:"Call trace" ())
        in
        L.debug (fun m ->
            m "Returned %a from %a" pp_rust_val v GAst.pp_fn_operand func);
        let ptr =
          match Store.find_value var store with
          | Some ptr -> ptr
          | None ->
              failwith "Tried storing in a variable that was not allocated"
        in
        let++ (), state = Heap.store ptr ty v state in
        (store, state)
    | StorageDead var ->
        let* ptr, ty =
          (* TODO: maybe only mark as unusable and dont deallocate? maybe NOP? *)
          match Store.find_opt var store with
          | Some (Some ptr, ty) -> return (ptr, ty)
          | Some (None, _) ->
              Fmt.kstr not_impl
                "Variable %a already deallocated - UB? unreachable?"
                Expressions.pp_var_id var
          | None ->
              Fmt.kstr not_impl "Variable %a not found in store"
                Expressions.pp_var_id var
        in
        let++ (), state = Heap.free ptr state in
        let store = Store.add var (None, ty) store in
        (store, state)
    | FakeRead _ ->
        (* TODO: update tree borrow with read *)
        Result.ok (store, state)
    | Drop place ->
        (* TODO: this is probably super wrong, drop glue etc. *)
        let** place_ptr, state = resolve_place ~store state place in
        let place_ptr = as_ptr place_ptr in
        let++ (), state = Heap.uninit place_ptr state in
        (* let store =
          match place.kind with
          | PlaceBase var_id -> Store.add var_id (None, place.ty) store
          | _ -> store
        in *)
        (store, state)
    | Assert { cond; expected } ->
        let** cond, state = eval_operand ~crate ~store state cond in
        let* cond_int =
          match cond with
          | Base cond ->
              of_opt_not_impl ~msg:"Expected an integer assertion"
                (Typed.cast_checked cond Typed.t_int)
          | _ -> not_impl "Expected a base Rust value in assert"
        in
        let cond_bool = Typed.bool_of_int cond_int in
        let cond_bool =
          if expected = true then cond_bool else Typed.not cond_bool
        in
        if%sat cond_bool then Result.ok (store, state)
        else Heap.error `FailedAssert state
    | s ->
        Fmt.kstr not_impl "Unsupported statement: %a" UllbcAst.pp_raw_statement
          s

  and exec_block ~crate ~(body : UllbcAst.expr_body) store state
      ({ statements; terminator } : UllbcAst.block) =
    let** store, state =
      Rustsymex.Result.fold_list statements ~init:(store, state)
        ~f:(fun (store, state) stmt -> exec_stmt ~crate store state stmt)
    in
    L.info (fun f ->
        let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
        f "Terminator: %s"
          (PrintUllbcAst.Ast.terminator_to_string ctx "" terminator));
    let { span = loc; content = term; _ } : UllbcAst.terminator = terminator in
    let@ () = with_loc ~loc in
    match term with
    | Goto b ->
        let block = UllbcAst.BlockId.nth body.body b in
        exec_block ~crate ~body store state block
    | Return ->
        let value_ptr, value_ty = Store.find Expressions.VarId.zero store in
        let* value_ptr =
          match value_ptr with
          | Some x -> return x
          | None -> Fmt.kstr not_impl "Return value unset, but returned"
        in
        let++ value, _ = Heap.load value_ptr value_ty state in
        (value, store, state)
    | Switch (discr, switch) -> (
        let** discr, state = eval_operand ~crate ~store state discr in
        let discr =
          match discr with
          | Base discr -> discr
          | _ ->
              Fmt.failwith "Expected base value for discriminant, got %a"
                pp_rust_val discr
        in
        match switch with
        | If (if_block, else_block) ->
            let open Typed.Infix in
            L.info (fun g ->
                g "Switch if/else %a/%a for %a" UllbcAst.pp_block_id if_block
                  UllbcAst.pp_block_id else_block Typed.ppa discr);
            let* block =
              if%sat discr ==@ Typed.zero then return else_block
              else return if_block
            in
            let block = UllbcAst.BlockId.nth body.body block in
            exec_block ~crate ~body store state block
        | SwitchInt (_, options, default) -> (
            let* block =
              match_on options ~constr:(fun (v, _) ->
                  discr ==@ value_of_scalar v)
            in
            match block with
            | None ->
                let block = UllbcAst.BlockId.nth body.body default in
                exec_block ~crate ~body store state block
            | Some (_, block) ->
                let block = UllbcAst.BlockId.nth body.body block in
                exec_block ~crate ~body store state block))
    | Abort kind -> (
        match kind with
        | UndefinedBehavior -> Heap.error `UBAbort state
        | Panic name ->
            let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
            let name_str = PrintTypes.name_to_string ctx name in
            Heap.error (`Panic name_str) state)

  and exec_fun ~crate ~args ~state (fundef : UllbcAst.fun_decl) =
    (* Put arguments in store *)
    let GAst.{ item_meta = { span = loc; name; _ }; body; _ } = fundef in
    let** body =
      match body with
      | None ->
          let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
          Fmt.kstr not_impl "Function %s is opaque"
            (PrintTypes.name_to_string ctx name)
      | Some body -> Result.ok body
    in
    let@ () = with_loc ~loc in
    L.info (fun m ->
        let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
        m "Calling %s with [@[%a@]]"
          (PrintTypes.name_to_string ctx name)
          Fmt.(list ~sep:comma pp_rust_val)
          args);
    let** store, state = alloc_stack body.locals args state in
    (* TODO: local optimisation to put values in store directly when no address is taken. *)
    let starting_block = List.hd body.body in
    let** value, store, state =
      exec_block ~crate ~body store state starting_block
    in
    let protected_address =
      match (fundef.signature.output, value) with
      | TRef (RStatic, _, RShared), Ptr addr
      | TRef (RStatic, _, RShared), FatPtr (addr, _) ->
          Some addr
      | _ -> None
    in
    let++ state = dealloc_store ?protected_address store state in
    (* We model void as zero, it should never be used anyway *)
    (value, state)
end
