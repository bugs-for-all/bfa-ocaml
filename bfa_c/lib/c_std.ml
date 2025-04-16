module Ctype = Cerb_frontend.Ctype
open Csymex
open Csymex.Syntax
open Typed.Syntax
module T = Typed.T

module M (State : State_intf.S) = struct
  let malloc ~prog:_ ~(args : T.cval Typed.t list) ~state =
    let* sz =
      match args with
      | [ sz ] -> return sz
      | _ -> not_impl "malloc with non-one arguments"
    in
    let sz = Typed.cast sz in
    Csymex.branches
      [
        (fun () -> State.alloc sz state);
        (fun () -> Result.ok (Typed.Ptr.null, state));
      ]

  let free ~prog:_ ~(args : T.cval Typed.t list) ~state =
    let* ptr =
      match args with
      | [ ptr ] -> return ptr
      | _ -> not_impl "free with non-one arguments"
    in
    match Typed.get_ty ptr with
    | TPointer ->
        let++ (), state =
          if%sat Typed.Ptr.is_null (Typed.cast ptr) then Result.ok ((), state)
          else State.free (Typed.cast ptr) state
        in
        (0s, state)
    | TInt -> Fmt.kstr not_impl "free with int argument: %a" Typed.ppa ptr
    | _ -> Fmt.kstr not_impl "free with non-pointer argument: %a" Typed.ppa ptr

  let memcpy ~prog:_ ~(args : T.cval Typed.t list) ~state =
    let* dst, src, size =
      match args with
      | [ dst; src; size ] -> return (dst, src, size)
      | _ -> not_impl "memcpy with non-three arguments"
    in
    let dst = Typed.cast dst in
    let src = Typed.cast src in
    let size = Typed.cast size in
    let++ (), state = State.copy_nonoverlapping ~dst ~src ~size state in
    (dst, state)

  let assert_ ~prog:_ ~(args : T.cval Typed.t list) ~state =
    let open Typed.Infix in
    let* to_assert =
      match args with
      | [ t ] ->
          Csymex.of_opt_not_impl ~msg:"not an integer"
            (Typed.cast_checked t Typed.t_int)
      | _ -> not_impl "to_assert with non-one arguments"
    in
    if%sat to_assert ==@ 0s then State.error `FailedAssert state
    else Result.ok (0s, state)

  let nondet_int_fun ~prog:_ ~args:_ ~state =
    let constrs = Layout.int_constraints (Ctype.Signed Int_) |> Option.get in
    let* v = Csymex.nondet ~constrs Typed.t_int in
    let v = (v :> T.cval Typed.t) in
    Result.ok (v, state)
end
