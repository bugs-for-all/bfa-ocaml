module SYMEX =
  Soteria_symex.Symex.Make_iter
    (struct
      let fuel : Soteria_symex.Fuel_gauge.t = { steps = 150; branching = 4 }

      module Logger = Soteria_symex.Logging.Fmt_log_reporter
    end)
    (Z3solver)

include SYMEX

let check_nonzero (t : Typed.T.sint Typed.t) :
    ([> Typed.T.nonzero ] Typed.t, [> `NonZeroIsZero ], 'fix) Result.t =
  let open Syntax in
  let open Typed.Infix in
  if%sat t ==@ Typed.zero then Result.error `NonZeroIsZero
  else Result.ok (Typed.cast t)

(* sint t -> ([> nonzero ] t, [> `NonZeroIsZero ], 'fix) Csymex.Result.t *)

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

let unsupported_file = ref None

let dump_unsupported () =
  match !unsupported_file with
  | None -> ()
  | Some file ->
      let reasons = flush_give_up () in
      let module M = Map.Make (String) in
      let map =
        List.fold_left
          (fun map (reason, _loc) ->
            M.update reason
              (function None -> Some 1 | Some k -> Some (k + 1))
              map)
          M.empty reasons
      in
      let oc = open_out file in
      let json : Yojson.Safe.t =
        `Assoc (List.map (fun (k, v) -> (k, `Int v)) (M.bindings map))
      in
      Yojson.Safe.to_channel oc json;
      close_out oc

let current_loc = ref Cerb_location.unknown
let get_loc () = !current_loc

let with_loc ~(loc : Cerb_location.t) f =
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

let not_impl msg =
  let msg = "MISSING FEATURE, VANISHING: " ^ msg in
  L.info (fun m -> m "%s" msg);
  push_give_up (msg, get_loc ());
  vanish ()

let[@inline] with_loc_err () f =
  let loc = get_loc () in
  Result.map_error (f ()) (fun e -> (e, loc))

let error e = Result.error (e, get_loc ())
let of_opt = function Some x -> return x | None -> vanish ()
let of_opt_not_impl ~msg = function Some x -> return x | None -> not_impl msg

module Freeable = Soteria_symex.Freeable.Make (SYMEX)
module Pmap_direct_access = Soteria_symex.Pmap.Direct_access (SYMEX)
module Pmap = Soteria_symex.Pmap.Make (SYMEX)
module Concrete_map = Soteria_symex.Pmap.Concrete (SYMEX)
module Bi = Soteria_symex.Bi_abd.Make (SYMEX)
