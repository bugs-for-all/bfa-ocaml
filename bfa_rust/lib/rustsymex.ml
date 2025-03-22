module SYMEX =
  Bfa_symex.Symex.Make_iter
    (struct
      let steps = 500
      let branching = 10
    end)
    (Z3solver)

include SYMEX

let check_nonzero (t : Typed.T.sint Typed.t) :
    (Typed.T.nonzero Typed.t, [> `NonZeroIsZero ], 'fix) Result.t =
  let open Syntax in
  let open Typed.Infix in
  if%sat t ==@ Typed.zero then Result.error `NonZeroIsZero
  else Result.ok (Typed.cast t)

let match_on ~(constr : 'a -> Typed.sbool Typed.t) (elements : 'a list) :
    'a option t =
  let open Syntax in
  let rec aux = function
    | e :: rest -> if%sat constr e then return (Some e) else aux rest
    | [] -> return None
  in
  aux elements

let ( let@ ) = ( @@ )

let push_give_up, flush_give_up =
  let give_up_reasons = Dynarray.create () in
  let push_give_up r = Dynarray.add_last give_up_reasons r in
  let flush_give_up () =
    let reasons = Dynarray.to_list give_up_reasons in
    Dynarray.clear give_up_reasons;
    reasons
  in
  (push_give_up, flush_give_up)

let current_loc = ref Charon_util.empty_span
let get_loc () = !current_loc

let with_loc ~(loc : Charon.Meta.span) f =
  let open Syntax in
  let old_loc = !current_loc in
  current_loc := loc;
  let* res = f () in
  current_loc := old_loc;
  return res

let with_loc_immediate ~loc f =
  let old_loc = !current_loc in
  current_loc := loc;
  let res = f () in
  current_loc := old_loc;
  res

let not_impl_happened = ref None

let not_impl msg =
  if !not_impl_happened = None then not_impl_happened := Some msg;
  let msg = "MISSING FEATURE, VANISHING: " ^ msg in
  L.info (fun m -> m "%s" msg);
  print_endline msg;
  push_give_up (msg, get_loc ());
  vanish ()

let[@inline] with_loc_err () f =
  let loc = get_loc () in
  Result.map_error (f ()) (fun e -> (e, loc))

let error e = Result.error (e, get_loc ())
let of_opt = function Some x -> return x | None -> vanish ()
let of_opt_not_impl ~msg = function Some x -> return x | None -> not_impl msg

module Freeable = Bfa_symex.Freeable.Make (SYMEX)
module Pmap_direct_access = Bfa_symex.Pmap.Direct_access (SYMEX)
module Pmap = Bfa_symex.Pmap.Make (SYMEX)
module Bi = Bfa_symex.Bi_abd.Make (SYMEX)
