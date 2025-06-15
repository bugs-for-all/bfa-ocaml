(*** More standard interface to [Symbol], making it nicer for 
   using with Stdlib functors *)

open Cerb_frontend.Symbol

module SELF = struct
  type t = Cerb_frontend.Symbol.sym

  let equal = equal_sym
  let compare = compare_sym
  let hash = Hashtbl.hash
  let pp = Fmt_ail.pp_sym
end

include SELF
module Set = Set.Make (SELF)
module Map = Map.Make (SELF)
