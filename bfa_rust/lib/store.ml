open Charon

include Stdlib.Map.Make (struct
  type t = Expressions.VarId.id

  let compare = Expressions.VarId.compare_id
end)

let find_type sym store = Option.map snd (find_opt sym store)
let find_value sym store = Option.bind (find_opt sym store) fst
