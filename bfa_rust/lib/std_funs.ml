open Charon
module NameMatcherMap = Charon.NameMatcher.NameMatcherMap
open Rustsymex
open Rustsymex.Syntax
open Typed.Syntax
open Typed.Infix
open Charon_util
module T = Typed.T

module M (Heap : Heap_intf.S) = struct
  module Sptr = Heap.Sptr

  type nonrec rust_val = Sptr.t rust_val

  let pp_rust_val = pp_rust_val Sptr.pp

  type std_op = Add | Sub | Mul | Div | Rem
  type std_bool = Id | Neg
  type type_loc = GenArg | Input

  let op_of = function
    | Add -> fun x y _ -> Result.ok (x +@ y)
    | Sub -> fun x y _ -> Result.ok (x -@ y)
    | Mul -> fun x y _ -> Result.ok (x *@ y)
    | Div ->
        fun x y st ->
          if%sat y ==@ 0s then Heap.error `DivisionByZero st
          else Result.ok (x /@ Typed.cast y)
    | Rem ->
        fun x y st ->
          if%sat y ==@ 0s then Heap.error `DivisionByZero st
          else Result.ok (Typed.rem x (Typed.cast y))

  let assert_ _ ~crate:_ ~(args : rust_val list) ~state =
    let open Typed.Infix in
    let* to_assert =
      match args with
      | [ Base t ] ->
          of_opt_not_impl ~msg:"not an integer"
            (Typed.cast_checked t Typed.t_int)
      | _ -> not_impl "to_assert with non-one arguments"
    in
    if%sat to_assert ==@ 0s then Heap.error `FailedAssert state
    else Result.ok (Charon_util.unit_, state)

  let assume _ ~crate:_ ~args ~state =
    let* to_assume =
      match args with
      | [ Base t ] ->
          of_opt_not_impl ~msg:"not an integer"
            (Typed.cast_checked t Typed.t_int)
      | _ -> not_impl "assume with non-one arguments"
    in
    L.debug (fun g -> g "Assuming: %a\n" Typed.ppa to_assume);
    let* () = assume [ Typed.bool_of_int to_assume ] in
    Result.ok (Charon_util.unit_, state)

  let nondet (fun_sig : UllbcAst.fun_sig) ~crate:_ ~args:_ ~state =
    let ty = fun_sig.output in
    let* value = Layout.nondet ty in
    Result.ok (value, state)

  let cast_to_int v st =
    match Typed.cast_checked v Typed.t_int with
    | Some v -> Result.ok v
    | None -> Heap.error `UBPointerArithmetic st

  let safe_binop (bop : std_op) l r ty st =
    let* constrs =
      of_opt_not_impl ~msg:"constraints not implemented" (Layout.constraints ty)
    in
    let** l = cast_to_int l st in
    let** r = cast_to_int r st in
    let** res = (op_of bop) l r st in
    let** () =
      (* additional check for rem, since the result doesn't directly overflow *)
      if bop = Rem then
        let* min =
          match ty with
          | TInteger inty -> return (Layout.min_value inty)
          | _ -> not_impl "Unexpected type in rem"
        in
        if%sat l ==@ min &&@ (r ==@ -1s) then Heap.error `Overflow st
        else Result.ok ()
      else Result.ok ()
    in
    if%sat Typed.conj @@ constrs res then Result.ok res
    else Heap.error `Overflow st

  let unchecked_op op (fun_sig : UllbcAst.fun_sig) ~crate:_ ~args ~state =
    let* ty =
      match fun_sig.inputs with
      | TLiteral ty :: _ -> return ty
      | _ -> not_impl "unchecked_op wrong inputs"
    in
    let* left, right =
      match args with
      | [ Base left; Base right ] -> return (left, right)
      | _ -> not_impl "unchecked_op with not two arguments"
    in
    let++ res = safe_binop op left right ty state in
    (Base res, state)

  let checked_op op (fun_sig : UllbcAst.fun_sig) ~crate ~args ~state =
    let+ res = unchecked_op op fun_sig ~crate ~args ~state in
    match res with
    | Ok (res, state) -> Bfa_symex.Compo_res.Ok (Enum (1s, [ res ]), state)
    | _ -> Bfa_symex.Compo_res.Ok (Enum (0s, []), state)

  let wrapping_op op (fun_sig : UllbcAst.fun_sig) ~crate:_ ~args ~state =
    let* ty =
      match fun_sig.inputs with
      | TLiteral (TInteger ty) :: _ -> return ty
      | ty :: _ ->
          Fmt.kstr not_impl "wrapping_op with non integer: %a" Types.pp_ty ty
      | [] -> not_impl "wrapping_op with no inputs"
    in
    let* left, right =
      match args with
      | [ Base left; Base right ] ->
          let* left =
            of_opt_not_impl ~msg:"not an integer"
              (Typed.cast_checked left Typed.t_int)
          in
          let+ right =
            of_opt_not_impl ~msg:"not an integer"
              (Typed.cast_checked right Typed.t_int)
          in
          (left, right)
      | _ -> not_impl "wrapping_op with not two arguments"
    in
    let size = Layout.size_of_int_ty ty in
    (* 2^N *)
    let unsigned_max = Typed.nonzero_z (Z.shift_left Z.one (8 * size)) in
    let max : T.nonzero Typed.t = Typed.cast @@ Layout.max_value ty in
    let signed = Layout.is_signed ty in
    let** res = op left right state in
    let res = res %@ unsigned_max in
    if not signed then Result.ok (Base res, state)
    else
      if%sat res <=@ max then Result.ok (Base res, state)
      else Result.ok (Base (res -@ unsigned_max), state)

  let is_some (fun_sig : UllbcAst.fun_sig) ~crate:_ ~args ~state =
    let val_ptr, opt_ty =
      match (args, fun_sig.inputs) with
      | [ Ptr ptr ], [ Types.TRef (_, ty, _) ] -> (ptr, ty)
      | _ ->
          failwith
            "is_some expects a single ptr argument and a single tref input type"
    in
    let** enum_value, state = Heap.load val_ptr opt_ty state in
    let* discr =
      match enum_value with
      | Enum (discr, _) -> return discr
      | _ ->
          Fmt.kstr not_impl
            "expected value pointed to in is_some to be an enum, got %a"
            pp_rust_val enum_value
    in
    Result.ok (Base (Typed.int_of_bool (discr ==@ 1s)), state)

  let is_none (fun_sig : UllbcAst.fun_sig) ~crate:_ ~args ~state =
    let val_ptr, opt_ty =
      match (args, fun_sig.inputs) with
      | [ Ptr ptr ], [ Types.TRef (_, ty, _) ] -> (ptr, ty)
      | _ ->
          failwith
            "is_none expects a single ptr argument and a single tref input type"
    in
    let** enum_value, state = Heap.load val_ptr opt_ty state in
    let* discr =
      match enum_value with
      | Enum (discr, _) -> return discr
      | _ ->
          Fmt.kstr not_impl
            "expected value pointed to in is_none to be an enum, got %a"
            pp_rust_val enum_value
    in
    Result.ok (Base (Typed.int_of_bool (discr ==@ 0s)), state)

  let unwrap_opt _ ~crate:_ ~args ~state =
    let* discr, value =
      match args with
      | [ Enum (disc, value) ] -> return (disc, value)
      | _ -> not_impl "is_some expects a single option argument"
    in
    if%sat discr ==@ 0s then Heap.error (`StdErr "Unwrapped none") state
    else
      match value with
      | [ value ] -> Result.ok (value, state)
      | _ -> not_impl "option is some, but doesn't have one value"

  let unwrap_res _ ~crate:_ ~args ~state =
    let* discr, value =
      match args with
      | [ Enum (disc, value) ] -> return (disc, value)
      | _ -> not_impl "is_some expects a single option argument"
    in
    if%sat discr ==@ 1s then Heap.error (`StdErr "Unwrapped Err") state
    else
      match value with
      | [ value ] -> Result.ok (value, state)
      | _ -> not_impl "Result is Ok, but doesn't have one value"

  let eq_values ~neg (fun_sig : UllbcAst.fun_sig) ~crate:_ ~args ~state =
    let rec aux state left right =
      match (left, right) with
      | Base left, Base right -> Result.ok (left ==@ right, state)
      | Struct lefts, Struct rights
      | Array lefts, Array rights
      | Tuple lefts, Tuple rights ->
          if List.compare_lengths lefts rights <> 0 then
            Result.ok (Typed.v_false, state)
          else aux_list Typed.v_true state lefts rights
      | Enum (l_d, l_vs), Enum (r_d, r_vs) ->
          if List.compare_lengths l_vs r_vs <> 0 then
            Result.ok (Typed.v_false, state)
          else aux_list (l_d ==@ r_d) state l_vs r_vs
      | _ ->
          Fmt.kstr not_impl "Unexpected eq_values pair: %a / %a" pp_rust_val
            left pp_rust_val right
    and aux_list init state lefts rights =
      if init = Typed.v_false then Result.ok (Typed.v_false, state)
      else
        match (lefts, rights) with
        | [], [] -> Result.ok (init, state)
        | l :: lefts, r :: rights ->
            let** b_val, state = aux state l r in
            aux_list (b_val &&@ init) state lefts rights
        | [], _ | _, [] -> Result.ok (Typed.v_false, state)
    in
    let left_ptr, right_ptr =
      match args with
      | [ Ptr left; Ptr right ] -> (left, right)
      | _ -> failwith "eq_values expects two arguments"
    in
    let** left, right, state =
      match fun_sig.inputs with
      | Types.TRef (_, (TRef (_, ty, _) as outer_ty), _) :: _ ->
          (* STD provides an implementation of eq for references (&T), where the arguments are
             thus &&T -- we handle this here by adding an indirection. *)
          let** left, state = Heap.load left_ptr outer_ty state in
          let** right, state = Heap.load right_ptr outer_ty state in
          let left_ptr = as_ptr left in
          let right_ptr = as_ptr right in
          let** left, state = Heap.load left_ptr ty state in
          let++ right, state = Heap.load right_ptr ty state in
          (left, right, state)
      | Types.TRef (_, ty, _) :: _ ->
          let** left, state = Heap.load left_ptr ty state in
          let++ right, state = Heap.load right_ptr ty state in
          (left, right, state)
      | ty :: _ ->
          Fmt.kstr not_impl "Unexpected type for eq_values: %a" Types.pp_ty ty
      | _ -> not_impl "Error: eq_values received no arguments?"
    in
    let++ b_val, state = aux state left right in
    let b_val = if neg then Typed.not b_val else b_val in
    let res = Typed.int_of_bool b_val in
    (Base res, state)

  let bool_not _ ~crate:_ ~args ~state =
    let b_ptr =
      match args with
      | [ Ptr b ] -> b
      | _ -> failwith "bool_not expects one Ptr argument"
    in
    let** b_rval, state = Heap.load b_ptr (Types.TLiteral TBool) state in
    let b_int = as_base_of ~ty:Typed.t_int b_rval in
    let b_int' = Typed.not_int_bool b_int in
    Result.ok (Base b_int', state)

  let zeroed (fun_sig : UllbcAst.fun_sig) ~(crate : UllbcAst.crate) ~args:_
      ~state =
    let rec aux : Types.ty -> rust_val = function
      | TLiteral _ -> Base Typed.zero
      | TRawPtr _ | TRef _ -> Ptr Sptr.null_ptr
      | TAdt (TAdtId t_id, _) -> (
          let adt = Types.TypeDeclId.Map.find t_id crate.type_decls in
          match adt.kind with
          | Struct fields ->
              Struct (List.map (fun (f : Types.field) -> aux f.field_ty) fields)
          | Enum [] -> failwith "zeroed does not handle empty enums!"
          | k ->
              Fmt.failwith "Unhandled zeroed ADT kind: %a"
                Types.pp_type_decl_kind k)
      | ty -> Fmt.failwith "Unhandled zeroed type: %a" Types.pp_ty ty
    in
    try Result.ok (aux fun_sig.output, state) with Failure f -> not_impl f

  let array_repeat (gen_args : Types.generic_args) ~crate:_ ~args ~state =
    let rust_val, size =
      match (args, gen_args.const_generics) with
      | [ rust_val ], [ size ] ->
          (rust_val, Charon_util.int_of_const_generic size)
      | args, cgens ->
          Fmt.failwith
            "array_repeat: unexpected params / generic constants: %a / %a"
            Fmt.(list pp_rust_val)
            args
            Fmt.(list Types.pp_const_generic)
            cgens
    in
    Result.ok (Array (List.init size (fun _ -> rust_val)), state)

  let array_index (idx_op : Expressions.builtin_index_op)
      (gen_args : Types.generic_args) ~crate:_ ~args ~state =
    let ptr = as_ptr @@ List.hd args in
    let size =
      match (idx_op.is_array, Sptr.meta ptr, gen_args.const_generics) with
      (* Array with static size *)
      | true, _, [ size ] -> Typed.int @@ Charon_util.int_of_const_generic size
      | false, Some size, [] -> Typed.cast size
      | _ ->
          Fmt.failwith "array_index: unexpected arguments: %a / %a"
            Fmt.(list pp_rust_val)
            args
            Fmt.(list Types.pp_const_generic)
            gen_args.const_generics
    in
    (* TODO: take into account idx.mutability *)
    let idx = as_base_of ~ty:Typed.t_int (List.nth args 1) in
    if%sat 0s <=@ idx &&@ (idx <@ size) then
      let ty = List.hd gen_args.types in
      let ptr' = Sptr.offset ~ty ptr idx in
      if not idx_op.is_range then Result.ok (Ptr ptr', state)
      else
        let range_end = as_base_of ~ty:Typed.t_int (List.nth args 2) in
        (* range_end is exclusive *)
        if%sat idx <=@ range_end &&@ (range_end <=@ size) then
          let size = range_end -@ idx in
          let ptr' = Sptr.with_meta ~meta:size ptr' in
          Result.ok (Ptr ptr', state)
        else
          (* not sure this is the right diagnostic *)
          Heap.error `OutOfBounds state
    else Heap.error `OutOfBounds state

  (* Some array accesses are ran on functions, so we handle those here and redirect them.
     Eventually, it would be good to maybe make a Charon pass that gets rid of these before. *)
  let array_index_fn (fun_sig : UllbcAst.fun_sig) ~crate ~args ~state =
    let mode, gargs, range_ty_id =
      match fun_sig.inputs with
      (* Unfortunate, but right now i don't have a better way to handle this... *)
      | [
       TRef (_, TAdt (TBuiltin ((TArray | TSlice) as mode), gargs), _);
       TAdt (TAdtId range_ty_id, _);
      ] ->
          (mode, gargs, range_ty_id)
      | _ -> failwith "Unexpected input type"
    in
    let range_adt =
      Types.TypeDeclId.Map.find range_ty_id UllbcAst.(crate.type_decls)
    in
    let range_name =
      match List.rev range_adt.item_meta.name with
      | PeIdent (name, _) :: _ -> name
      | _ -> failwith "Unexpected range name"
    in
    let size =
      match (args, gargs.const_generics) with
      (* Array with static size *)
      | _, [ size ] -> Typed.int @@ Charon_util.int_of_const_generic size
      | Ptr ptr :: _, [] -> Typed.cast @@ Option.get @@ Sptr.meta ptr
      | _ -> failwith "array_index (fn): couldn't calculate size"
    in
    let ptr, idx_from, idx_to =
      match (args, range_name) with
      | [ ptr; Struct [] ], "RangeFull" -> (ptr, Base 0s, Base size)
      | [ ptr; Struct [ idx_from ] ], "RangeFrom" -> (ptr, idx_from, Base size)
      | [ ptr; Struct [ idx_to ] ], "RangeTo" -> (ptr, Base 0s, idx_to)
      | [ ptr; Struct [ idx_from; idx_to ] ], "Range" -> (ptr, idx_from, idx_to)
      | [ ptr; Struct [ idx_from; idx_to ] ], "RangeInclusive" ->
          let idx_to = as_base_of ~ty:Typed.t_int idx_to in
          (ptr, idx_from, Base (idx_to +@ 1s))
      | [ ptr; Struct [ idx_to ] ], "RangeToInclusive" ->
          let idx_to = as_base_of ~ty:Typed.t_int idx_to in
          (ptr, Base 0s, Base (idx_to +@ 1s))
      | _ -> Fmt.failwith "array_index (fn): unexpected range %s" range_name
    in
    let idx_op : Expressions.builtin_index_op =
      { is_array = mode = TArray; mutability = RShared; is_range = true }
    in
    array_index idx_op gargs ~crate ~args:[ ptr; idx_from; idx_to ] ~state

  let array_slice ~mut:_ (gen_args : Types.generic_args) ~crate:_ ~args ~state =
    let size =
      match gen_args.const_generics with
      | [ size ] -> Charon_util.int_of_const_generic size
      | _ -> failwith "array_slice: unexpected generic constants"
    in
    match args with
    | [ Ptr arr_ptr ] ->
        let ptr' = Sptr.with_meta ~meta:(Typed.int size) arr_ptr in
        Result.ok (Ptr ptr', state)
    | _ -> failwith "array_index: unexpected arguments"

  let slice_len _ ~crate:_ ~args ~state =
    match args with
    | [ Ptr ptr ] -> Result.ok (Base (Option.get @@ Sptr.meta ptr), state)
    | _ -> failwith "slice_len: unexpected arguments"

  let discriminant_value (funsig : GAst.fun_sig) ~crate:_ ~args ~state =
    let value_ptr =
      match args with
      | [ Ptr value_ptr ] -> value_ptr
      | _ -> failwith "discriminant_value: unexpected arguments"
    in
    let value_ty =
      match funsig.inputs with
      | [ TRef (_, value_ty, _) ] -> value_ty
      | _ -> failwith "discriminant_value: unexpected arguments"
    in
    let++ value, state = Heap.load value_ptr value_ty state in
    match value with
    | Enum (discr, _) -> (Base discr, state)
    | _ -> failwith "discriminant_value: unexpected value"

  let to_string _ ~crate:_ ~args ~state =
    match args with
    | [ (Ptr ptr as slice) ] ->
        Result.ok (Struct [ slice; Base (Option.get @@ Sptr.meta ptr) ], state)
    | _ -> failwith "to_string: unexpected value"

  let str_chars _ ~crate:_ ~args ~state =
    let ptr =
      match args with
      | [ Ptr ptr ] -> ptr
      | _ -> failwith "str_chars: unexpected value"
    in
    let len = Option.get @@ Sptr.meta ptr in
    let len = Typed.cast len in
    let ty = Types.TLiteral TChar in
    let arr_end = Sptr.offset ~ty ptr len in
    Result.ok (Struct [ Ptr ptr; Ptr arr_end ], state)

  let iter_nth (fun_sig : GAst.fun_sig) ~crate ~args ~state =
    let iter_ptr, idx =
      match args with
      | [ Ptr iter_ptr; Base idx ] -> (iter_ptr, idx)
      | _ -> failwith "iter_nth: unexpected value"
    in
    let* idx =
      of_opt_not_impl ~msg:"iter_nth: expected int idx"
        (Typed.cast_checked idx Typed.t_int)
    in
    let iter_ty, sub_ty =
      match fun_sig.inputs with
      | TRef (_, (TAdt (TAdtId adt_id, _) as iter_ty), _) :: _ -> (
          let adt = Std_types.get_adt ~crate adt_id in
          match adt.kind with
          | Struct (_ :: { field_ty = TRawPtr (sub_ty, _); _ } :: _) ->
              (iter_ty, sub_ty)
          | _ -> failwith "iter_nth: unexpected signature")
      | _ -> failwith "iter_nth: unexpected signature"
    in
    let** iter, state = Heap.load iter_ptr iter_ty state in
    let start_ptr, end_ptr =
      match iter with
      | Struct [ Ptr start_ptr; Ptr end_ptr ] -> (start_ptr, end_ptr)
      | _ -> failwith "iter_nth: unexpected iter structure"
    in
    if%sat Sptr.is_same_loc start_ptr end_ptr then
      let ptr = Sptr.offset ~ty:sub_ty start_ptr idx in
      let dist = Sptr.distance ptr end_ptr in
      if%sat dist <@ 0s then
        let iter' = Struct [ Ptr ptr; Ptr end_ptr ] in
        let** value, state = Heap.load ptr sub_ty state in
        let++ (), state = Heap.store iter_ptr iter_ty iter' state in
        (Enum (1s, [ value ]), state)
      else
        (* return None *)
        Result.ok (Enum (0s, []), state)
    else Heap.error `UBPointerArithmetic state

  let deref (funsig : GAst.fun_sig) ~crate:_ ~args ~state =
    (* This works for string deref -- don't know about the rest *)
    let ptr =
      match args with
      | [ Ptr ptr ] -> ptr
      | _ -> failwith "deref: unexpected argument"
    in
    let ty =
      match funsig.inputs with
      | TRef (_, ty, _) :: _ -> ty
      | _ -> failwith "deref: unexpected signature"
    in
    let++ v, state = Heap.load ptr ty state in
    match v with
    | Struct [ v; _ ] -> (v, state)
    | _ -> Fmt.failwith "deref: unexpected value: %a" pp_rust_val v

  let str_len (fun_sig : GAst.fun_sig) ~crate:_ ~args ~state =
    let str_ptr =
      match args with
      | [ Ptr ptr ] -> ptr
      | _ -> failwith "str_len: unexpected argument"
    in
    let str_ty =
      match fun_sig.inputs with
      | [ TRef (_, str_ty, _) ] -> str_ty
      | _ -> failwith "str_len: unexpected input"
    in
    let++ str_obj, state = Heap.load str_ptr str_ty state in
    match str_obj with
    | Struct [ Ptr ptr; _ ] -> (Base (Option.get @@ Sptr.meta ptr), state)
    | _ -> failwith "str_len: unexpected string type"

  let assert_zero_is_valid (fun_sig : GAst.fun_sig) ~crate:_ ~args:_ ~state =
    let ty =
      (List.hd fun_sig.generics.trait_clauses).trait.binder_value.decl_generics
        .types
      |> List.hd
    in
    match ty with
    | TRef _ -> Heap.error (`Panic "core::intrinsics::assert_zero_valid") state
    | _ -> Result.ok (Tuple [], state)

  let size_of (fun_sig : GAst.fun_sig) ~crate:_ ~args:_ ~state =
    let ty =
      (List.hd fun_sig.generics.trait_clauses).trait.binder_value.decl_generics
        .types
      |> List.hd
    in
    let+ size = Layout.size_of_s ty in
    Bfa_symex.Compo_res.Ok (Base size, state)

  let min_align_of ~in_input (fun_sig : GAst.fun_sig) ~crate:_ ~args:_ ~state =
    let ty =
      if in_input then
        match fun_sig.inputs with
        | [ TRawPtr (ty, _) ] -> ty
        | _ -> failwith "min_align_of: invalid input type"
      else
        (List.hd fun_sig.generics.trait_clauses).trait.binder_value
          .decl_generics
          .types
        |> List.hd
    in
    let layout = Layout.layout_of ty in
    let align = Typed.int layout.align in
    Result.ok (Base align, state)

  let box_new (gen_args : Types.generic_args) ~crate:_ ~args ~state =
    let ty, v =
      match (gen_args, args) with
      | { types = [ ty ]; _ }, [ v ] -> (ty, v)
      | _ -> failwith "box new: invalid arguments"
    in
    let** ptr, state = Heap.alloc_ty ty state in
    let++ (), state = Heap.store ptr ty v state in
    (Ptr ptr, state)

  let ptr_op op (funsig : GAst.fun_sig) ~crate:_ ~args ~state =
    let ptr, v =
      match args with
      | [ Ptr ptr; Base v ] -> (ptr, v)
      | _ -> failwith "ptr_add: invalid arguments"
    in
    let v =
      match Typed.cast_checked v Typed.t_int with
      | Some v -> v
      | None -> failwith "ptr_add: invalid offset type"
    in
    let ty =
      match funsig.inputs with
      | TRawPtr (ty, _) :: _ -> ty
      | _ -> failwith "ptr_offset_from: invalid arguments"
    in
    let v = if op = Add then v else ~-v in
    let ptr' = Sptr.offset ~ty ptr v in
    if%sat Sptr.constraints ptr' then Result.ok (Ptr ptr', state)
    else Heap.error `Overflow state

  let box_into_raw _ ~crate:_ ~args ~state =
    (* internally a box is exactly a pointer so nothing to do *)
    let box_ptr = List.hd args in
    Result.ok (box_ptr, state)

  let ptr_offset_from (funsig : GAst.fun_sig) ~crate:_ ~args ~state =
    let ptr1, ptr2 =
      match args with
      | [ Ptr ptr1; Ptr ptr2 ] -> (ptr1, ptr2)
      | _ -> failwith "ptr_offset_from: invalid arguments"
    in
    let ty =
      match funsig.inputs with
      | TRawPtr (ty, _) :: _ -> ty
      | _ -> failwith "ptr_offset_from: invalid arguments"
    in
    let* size = Layout.size_of_s ty in
    if%sat Sptr.is_same_loc ptr1 ptr2 &&@ (size >@ 0s) then
      let size = Typed.cast size in
      let off = Sptr.distance ptr1 ptr2 in
      if%sat off %@ size ==@ 0s then Result.ok (Base (off /@ size), state)
      else Heap.error `UBPointerComparison state
    else Heap.error `UBPointerComparison state

  let black_box _ ~crate:_ ~args ~state =
    match args with
    | [ v ] -> Result.ok (v, state)
    | _ -> failwith "black_box: invalid arguments"

  let transmute (funsig : GAst.fun_sig) ~crate ~args ~state =
    let from_ty = List.hd funsig.inputs in
    let to_ty = funsig.output in
    let v = List.hd args in
    match (from_ty, to_ty, v) with
    | TLiteral _, TLiteral to_ty, Base v ->
        let* constrs =
          of_opt_not_impl ~msg:"Constraints missing" (Layout.constraints to_ty)
        in
        if%sat Typed.conj (constrs v) then Result.ok (Base v, state)
        else Heap.error `UBTransmute state
    | _ ->
        let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
        Fmt.failwith "Unhandled transmute of %a: %s -> %s" pp_rust_val v
          (PrintTypes.ty_to_string ctx from_ty)
          (PrintTypes.ty_to_string ctx to_ty)

  type std_fun =
    | Any
    | Assert
    | AssertZeroValid
    | Assume
    | BlackBox
    | BoolNot
    | BoxIntoRaw
    | Checked of std_op
    | Deref
    | DiscriminantValue
    | Eq of std_bool
    | Index
    | IsNone
    | IsSome
    | IterNth
    | MinAlignOf of type_loc
    | OptUnwrap
    | PtrOp of std_op
    | PtrOffsetFrom
    | ResUnwrap
    | SizeOf
    | SliceLen
    | StrChars
    | StrLen
    | ToString
    | Transmute
    | Unchecked of std_op
    | Wrapping of std_op
    | Zeroed

  let std_fun_map =
    [
      (* Kani *)
      ("kani::assert", Assert);
      ("kani::assume", Assume);
      ("kani::any", Any);
      (* Core *)
      ("alloc::boxed::{alloc::boxed::Box}::into_raw", BoxIntoRaw);
      ("alloc::string::{alloc::string::String}::len", StrLen);
      ("alloc::string::{alloc::string::ToString}::to_string", ToString);
      ("alloc::string::{core::ops::deref::Deref}::deref", Deref);
      ("core::array::{core::ops::index::Index}::index", Index);
      ("core::cmp::impls::{core::cmp::PartialEq}::eq", Eq Id);
      ("core::cmp::impls::{core::cmp::PartialEq}::ne", Eq Neg);
      ("core::hint::black_box", BlackBox);
      ("core::intrinsics::assert_zero_valid", AssertZeroValid);
      ("core::intrinsics::black_box", BlackBox);
      ("core::intrinsics::discriminant_value", DiscriminantValue);
      ("core::intrinsics::min_align_of", MinAlignOf GenArg);
      ("core::intrinsics::min_align_of_val", MinAlignOf Input);
      ("core::intrinsics::pref_align_of", MinAlignOf GenArg);
      ("core::intrinsics::ptr_offset_from", PtrOffsetFrom);
      ("core::intrinsics::size_of", SizeOf);
      ("core::intrinsics::transmute", Transmute);
      ("core::intrinsics::unchecked_add", Unchecked Add);
      ("core::intrinsics::unchecked_div", Unchecked Div);
      ("core::intrinsics::unchecked_mul", Unchecked Mul);
      ("core::intrinsics::unchecked_rem", Unchecked Rem);
      ("core::intrinsics::unchecked_sub", Unchecked Sub);
      ("core::intrinsics::wrapping_add", Wrapping Add);
      ("core::intrinsics::wrapping_div", Wrapping Div);
      ("core::intrinsics::wrapping_mul", Wrapping Mul);
      ("core::intrinsics::wrapping_rem", Wrapping Rem);
      ("core::intrinsics::wrapping_sub", Wrapping Sub);
      ("core::mem::zeroed", Zeroed);
      ("core::num::{@N}::checked_add", Checked Add);
      ("core::num::{@N}::checked_div", Checked Div);
      ("core::num::{@N}::checked_mul", Checked Mul);
      ("core::num::{@N}::checked_sub", Checked Sub);
      ("core::num::{@N}::unchecked_add", Unchecked Add);
      ("core::num::{@N}::unchecked_div", Unchecked Div);
      ("core::num::{@N}::unchecked_mul", Unchecked Mul);
      ("core::num::{@N}::unchecked_rem", Unchecked Rem);
      ("core::num::{@N}::unchecked_sub", Unchecked Sub);
      ("core::num::{@N}::wrapping_add", Wrapping Add);
      ("core::num::{@N}::wrapping_div", Wrapping Div);
      ("core::num::{@N}::wrapping_mul", Wrapping Mul);
      ("core::num::{@N}::wrapping_rem", Wrapping Rem);
      ("core::num::{@N}::wrapping_sub", Wrapping Sub);
      ("core::option::{core::cmp::PartialEq}::eq", Eq Id);
      ("core::option::{@T}::is_none", IsNone);
      ("core::option::{@T}::is_some", IsSome);
      ("core::option::{@T}::unwrap", OptUnwrap);
      ("core::ptr::const_ptr::{@T}::add", PtrOp Add);
      ("core::ptr::const_ptr::{@T}::offset", PtrOp Add);
      ("core::ptr::const_ptr::{@T}::sub", PtrOp Sub);
      ("core::result::{@T}::unwrap", ResUnwrap);
      ("core::result::{core::cmp::PartialEq}::eq", Eq Id);
      ("core::result::{core::cmp::PartialEq}::ne", Eq Neg);
      ("core::slice::index::{core::ops::index::Index}::index", Index);
      ("core::str::iter::{core::iter::traits::iterator::Iterator}::nth", IterNth);
      ("core::str::{str}::chars", StrChars);
      ("core::option::{core::cmp::PartialEq}::ne", Eq Neg);
      ("core::ops::bit::{core::ops::bit::Not}::not", BoolNot);
      ("core::slice::{@T}::len", SliceLen);
    ]
    |> List.map (fun (p, v) -> (NameMatcher.parse_pattern p, v))
    |> NameMatcherMap.of_list

  let match_config =
    NameMatcher.{ map_vars_to_vars = false; match_with_trait_decl_refs = false }

  let std_fun_eval ~crate (f : UllbcAst.fun_decl) =
    let ctx = NameMatcher.ctx_from_crate crate in
    NameMatcherMap.find_opt ctx match_config f.item_meta.name std_fun_map
    |> Option.map @@ function
       | Any -> nondet f.signature
       | Assert -> assert_ f.signature
       | AssertZeroValid -> assert_zero_is_valid f.signature
       | Assume -> assume f.signature
       | BlackBox -> black_box f.signature
       | BoolNot -> bool_not f.signature
       | BoxIntoRaw -> box_into_raw f.signature
       | Checked op -> checked_op op f.signature
       | Deref -> deref f.signature
       | DiscriminantValue -> discriminant_value f.signature
       | Eq b -> eq_values ~neg:(b = Neg) f.signature
       | Index -> array_index_fn f.signature
       | IsNone -> is_none f.signature
       | IsSome -> is_some f.signature
       | IterNth -> iter_nth f.signature
       | MinAlignOf t -> min_align_of ~in_input:(t = Input) f.signature
       | OptUnwrap -> unwrap_opt f.signature
       | PtrOp op -> ptr_op op f.signature
       | PtrOffsetFrom -> ptr_offset_from f.signature
       | ResUnwrap -> unwrap_res f.signature
       | SizeOf -> size_of f.signature
       | SliceLen -> slice_len f.signature
       | StrChars -> str_chars f.signature
       | StrLen -> str_len f.signature
       | ToString -> to_string f.signature
       | Transmute -> transmute f.signature
       | Unchecked op -> unchecked_op op f.signature
       | Wrapping op -> wrapping_op (op_of op) f.signature
       | Zeroed -> zeroed f.signature

  let builtin_fun_eval ~crate:_ (f : Expressions.builtin_fun_id) generics =
    match f with
    | ArrayRepeat -> array_repeat generics
    | ArrayToSliceMut -> array_slice ~mut:true generics
    | ArrayToSliceShared -> array_slice ~mut:false generics
    | Index idx -> array_index idx generics
    | BoxNew -> box_new generics
end
