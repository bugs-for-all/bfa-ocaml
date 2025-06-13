include Stdlib.List

(** [take_count n l] takes the prefix [l'] of at most [n] from the beginning of
    the list [l], or the entire list if the length of [l] is smaller than [n].
    It returns the pair [(k, l')], where [k] is the length of [l']. *)
let take_count n l =
  let taken = ref 0 in
  let[@tail_mod_cons] rec aux n l =
    match (n, l) with
    | 0, _ | _, [] -> []
    | n, x :: l ->
        incr taken;
        x :: aux (n - 1) l
  in
  if n < 0 then invalid_arg "List.take_count";
  let result = aux n l in
  (result, !taken)

let%test "take_count" =
  let l = [ 1; 2; 3; 4; 5 ] in
  take_count 3 l = ([ 1; 2; 3 ], 3)
  && take_count 0 l = ([], 0)
  && take_count 5 l = (l, 5)
  && take_count 6 l = (l, 5)
  && take_count 10 l = (l, 5)

let combine_opt l1 l2 =
  try Some (combine l1 l2) with Invalid_argument _ -> None

let join_results outcomes =
  let oks, errors =
    partition_map
      (function Ok v -> Either.Left v | Error e -> Either.Right e)
      outcomes
  in
  if is_empty errors then Ok oks else Error errors

let rec combine3 l1 l2 l3 =
  match (l1, l2, l3) with
  | a :: l1, b :: l2, c :: l3 -> (a, b, c) :: combine3 l1 l2 l3
  | [], [], [] -> []
  | _ -> raise (Invalid_argument "combine3")

let combine3_opt l1 l2 l3 =
  try Some (combine3 l1 l2 l3) with Invalid_argument _ -> None

let[@tail_mod_cons] rec map2i i f l1 l2 =
  match (l1, l2) with
  | [], [] -> []
  | [ a1 ], [ b1 ] ->
      let r1 = f i a1 b1 in
      [ r1 ]
  | a1 :: a2 :: l1, b1 :: b2 :: l2 ->
      let r1 = f i a1 b1 in
      let r2 = f (i + 1) a2 b2 in
      r1 :: r2 :: map2i (i + 2) f l1 l2
  | _, _ -> invalid_arg "List_ex.map2i"

let map2i f l1 l2 = map2i 0 f l1 l2

let map_changed f l =
  let changed = ref false in
  let res =
    map
      (fun x ->
        let r = f x in
        if r != x then changed := true;
        r)
      l
  in
  (res, !changed)

let rec last l =
  match l with [] -> invalid_arg "List.last" | [ x ] -> x | _ :: l -> last l

let rec last_opt l =
  match l with [] -> None | [ x ] -> Some x | _ :: l -> last_opt l

let[@tail_mod_cons] rec concat_map2 f l1 l2 =
  match (l1, l2) with
  | [], [] -> []
  | x :: xs, y :: ys -> prepend_concat_map2 (f x y) f xs ys
  | _, _ -> invalid_arg "List_ex.concat_map2"

and[@tail_mod_cons] prepend_concat_map2 zs f xs ys =
  match zs with
  | [] -> concat_map2 f xs ys
  | z :: zs -> z :: prepend_concat_map2 zs f xs ys

let findi f =
  let rec aux i = function
    | [] -> raise Not_found
    | x :: l -> if f x then (i, x) else aux (i + 1) l
  in
  aux 0
