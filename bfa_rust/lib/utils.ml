module Hint = Hashtbl.Make (Int)

module List_ex = struct
  let combine_opt l1 l2 =
    try Some (List.combine l1 l2) with Invalid_argument _ -> None

  let partition3_map p l =
    let rec part l1 l2 l3 = function
      | [] -> (List.rev l1, List.rev l2, List.rev l3)
      | x :: l -> (
          match p x with
          | Either.Left v -> part (v :: l1) l2 l3 l
          | Either.Right (Either.Left v) -> part l1 (v :: l2) l3 l
          | Either.Right (Either.Right v) -> part l1 l2 (v :: l3) l)
    in
    part [] [] [] l

  let rec combine3 l1 l2 l3 =
    match (l1, l2, l3) with
    | a :: l1, b :: l2, c :: l3 -> (a, b, c) :: combine3 l1 l2 l3
    | [], [], [] -> []
    | _ -> raise (Invalid_argument "combine3")

  let combine3_opt l1 l2 l3 =
    try Some (combine3 l1 l2 l3) with Invalid_argument _ -> None

  (* An iteration over the cross-product of l with itself,
      excluding pairs (x, x) of the same element.
     For instance, [self_cross_product [1; 2; 3]] will iterate over [(1, 2), (1, 2), (2, 3)] *)
  let iter_self_cross_product l f =
    let rec aux = function
      | [] -> ()
      | x :: l ->
          List.iter (fun y -> f (x, y)) l;
          aux l
    in
    aux l

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

module Syntax = struct
  let ( << ) f g x = f (g x)
end

module ExecResult = struct
  type ('ok, 'err, 'fatal) exresult =
    | Ok of 'ok
    | Error of 'err
    | Fatal of 'fatal

  type ('ok, 'err, 'fatal) t = ('ok, 'err, 'fatal) exresult

  let ok x = Ok x
  let error e = Error e
  let fatal e = Fatal e
  let is_ok = function Ok _ -> true | _ -> false
  let is_error = function Error _ -> true | _ -> false
  let is_fatal = function Fatal _ -> true | _ -> false

  let bind x f =
    match x with Ok x -> f x | Error e -> Error e | Fatal e -> Fatal e

  let map f = function
    | Ok x -> Ok (f x)
    | Error e -> Error e
    | Fatal e -> Fatal e

  let map_full f_ok f_err f_fatal = function
    | Ok x -> Ok (f_ok x)
    | Error e -> Error (f_err e)
    | Fatal e -> Fatal (f_fatal e)

  let res_of_code = function
    | Ok 0 -> Ok ()
    | Ok code -> Fmt.kstr fatal "Error code: %d" code
    | Error e -> Error e
    | Fatal e -> Fatal e

  let combine list =
    let ok_list, error_list, fatal_list =
      List_ex.partition3_map
        (function
          | Ok x -> Left x
          | Error x -> Right (Left x)
          | Fatal x -> Right (Right x))
        list
    in
    if not @@ List.is_empty fatal_list then Fatal fatal_list
    else if not @@ List.is_empty error_list then Error error_list
    else Ok ok_list

  let of_result = function Result.Ok x -> Ok x | Error e -> Error e
  let of_result_fatal = function Result.Ok x -> Ok x | Error e -> Fatal e
  let ( let* ) = bind
  let ( let+ ) x f = map f x
  let ( let*> ) x f = bind (res_of_code x) f
  let ( let*! ) x f = bind (of_result_fatal x) f
end
