type location = (string[@opaque])
and tag = (int[@opaque])
and access = Read | Write
and locality = Local | Foreign
and event = locality * access
and state = Reserved of bool | Unique | Frozen | ReservedIM | Disabled | UB

and node = {
  location : location;
  tag : tag;
  tag_protected : bool;
  state : state;
  parent : node option;
  children : node list;
}

let equal n1 n2 = n1.location == n2.location && n1.tag == n2.tag

let transition st e =
  match (st, e) with
  | Reserved b, (_, Read) -> Reserved b
  | Reserved _, (Local, Write) -> Unique
  | Reserved _, (Foreign, Write) -> Disabled
  | Unique, (Local, _) -> Unique
  | Unique, (Foreign, Read) -> Frozen
  | Unique, (Foreign, Write) -> Disabled
  | Frozen, (_, Read) -> Frozen
  | Frozen, (Local, Write) -> UB
  | Frozen, (Foreign, Write) -> Disabled
  | ReservedIM, (_, Read | Foreign, _) -> ReservedIM
  | ReservedIM, (Local, Write) -> Unique
  | Disabled, (Foreign, _) -> Disabled
  | Disabled, (Local, _) -> UB
  | UB, _ -> UB

let transition_protected st e =
  match (st, e) with
  | Reserved b, (Local, Read) -> Reserved b
  | Reserved _, (Foreign, Read) -> Reserved true
  | Reserved true, (_, Write) -> UB
  | Unique, (Foreign, Read) -> UB
  | _ ->
      let st' = transition st e in
      if st' = Disabled then UB else st'

(** Relation of n2, with regards to n1; if n1 == n2 or if n2 is a descendant of
    n1, then it is Local, otherwise it is Foreign. *)
let relation n1 n2 =
  let[@tailrec] rec aux = function
    | [] -> false
    | n :: _ when n.tag == n2.tag -> true
    | n :: ns -> aux (n.children @ ns)
  in
  if equal n1 n2 then Local else if aux n1.children then Local else Foreign
