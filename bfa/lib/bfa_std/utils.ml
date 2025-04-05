module Hint = Hashtbl.Make (Int)

module List_ex = struct
  let combine_opt l1 l2 =
    try Some (List.combine l1 l2) with Invalid_argument _ -> None

  let join_results outcomes =
    let oks, errors =
      List.partition_map
        (function Ok v -> Either.Left v | Error e -> Either.Right e)
        outcomes
    in
    if List.is_empty errors then Ok oks else Error errors

  let join_options fn opts =
    let[@tailrec] rec aux acc = function
      | [] -> Some acc
      | x :: xs -> (
          match fn x with Some x -> aux (x :: acc) xs | None -> None)
    in
    aux [] opts

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
end

module Seq_ex = struct
  (** An iteration over the cross-product of [seq] with itself, excluding pairs
      (x, x) of the same element. For instance, [self_cross_product [1; 2; 3]]
      will iterate over [(1, 2), (1, 3), (2, 3)] Note that the sequence needs to
      be persistent, otherwise result is unspecified. *)
  let self_cross_product seq =
    let rec aux seq =
      match seq () with
      | Seq.Nil -> Seq.empty
      | Seq.Cons (x, seq) ->
          Seq.append (Seq.map (fun y -> (x, y)) seq) (aux seq)
    in
    aux seq
end

module Syntax = struct
  let ( << ) f g x = f (g x)
end
