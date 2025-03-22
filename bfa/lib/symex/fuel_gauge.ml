module type Config = sig
  val steps : int
  val branching : int
end

module Make (C : Config) = struct
  type exhaust = Exhausted | Not_exhausted

  type t = {
    steps : int;  (** The number of steps that can be taken per branch. *)
    branching : int;
        (** Number of times branching happens. Careful, the total number of
            branches is potentially exponential in the number of branchings *)
        (* TODO:
          Realised that we could just have the symex monad do a
          [Iter.take branches] and we wouldn't ever have to keep track of
          branching inside execution since everything is lazily executed *)
  }
  [@@deriving show { with_path = false }]

  let default = { steps = C.steps; branching = C.branching }

  let consume_fuel_steps n gauge =
    if gauge.steps >= n then
      (Not_exhausted, { gauge with steps = gauge.steps - n })
    else (Exhausted, { gauge with steps = 0 })

  let consume_branching n gauge =
    if gauge.branching >= n then
      (Not_exhausted, { gauge with branching = gauge.branching - n })
    else (Exhausted, { gauge with branching = 0 })

  let branching_left gauge = gauge.branching
end
