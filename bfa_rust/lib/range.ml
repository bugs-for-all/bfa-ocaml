open Typed
open Typed.Infix
open Rustsymex.Syntax

type t = T.sint Typed.t * T.sint Typed.t

let pp ft (l, u) = Fmt.pf ft "[%a, %a[" Typed.ppa l Typed.ppa u
let sem_eq (a, c) (b, d) = a ==@ b &&@ (c ==@ d)
let is_inside (l1, u1) (l2, u2) = l2 <=@ l1 &&@ (u1 <=@ u2)
let strictly_inside x (l, u) = l <@ x &&@ (x <@ u)
let size (l, u) = u -@ l
let split_at (l, h) x = ((l, x), (x, h))
let of_low_and_size low size : t = (low, low +@ size)

let of_low_and_type low ty =
  let+ size = Layout.size_of_s ty in
  (low, low +@ size)
