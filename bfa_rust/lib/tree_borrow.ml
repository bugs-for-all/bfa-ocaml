type tag = int
and access = Read | Write
and locality = Local | Foreign
and state = Reserved of bool | Unique | Frozen | ReservedIM | Disabled | UB

and t = {
  tag : tag;
  protected : bool;
  children : t list;
  initial_state : state;
}
[@@deriving show { with_path = false }]

let tag_counter = ref 0

let fresh_tag () =
  incr tag_counter;
  !tag_counter

let zero = 0

let rec pp fmt t =
  if List.is_empty t.children then
    Fmt.pf fmt (if t.protected then "{%d}" else "[%d]") t.tag
  else
    Fmt.pf fmt
      (if t.protected then "{%d}(%a)" else "[%d](%a)")
      t.tag
      Fmt.(list ~sep:comma pp)
      t.children

let pp_tag : tag Fmt.t = Fmt.int

let init ?(protected = false) ~state () =
  {
    tag = fresh_tag ();
    protected;
    (* parent = None; *)
    children = [];
    initial_state = state;
  }

let equal n1 n2 = n1.tag == n2.tag

let meet st1 st2 =
  match (st1, st2) with
  | UB, _ | _, UB -> UB
  | Disabled, _ | _, Disabled -> Disabled
  | Frozen, _ | _, Frozen -> Frozen
  | Unique, _ | _, Unique -> Unique
  | Reserved b1, Reserved b2 -> Reserved (b1 || b2)
  | ReservedIM, ReservedIM -> ReservedIM
  | Reserved _, ReservedIM | ReservedIM, Reserved _ ->
      raise @@ Failure "Can't compare Reserved and ReservedIM"

let transition =
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
  in

  let transition_protected st e =
    match (st, e) with
    | Reserved false, (Foreign, Read) -> Reserved true
    | Reserved true, (_, Write) -> UB
    | Unique, (Foreign, Read) -> UB
    | _ ->
        let st' = transition st e in
        if st' = Disabled then UB else st'
  in
  fun ~protected -> if protected then transition_protected else transition

(* let[@tailrec] rec root n = match n.parent with None -> n | Some p -> root p *)

(** [find n t] Looks for the node with tag [t] in the tree rooted at [n] *)
let find n t =
  let[@tailrec] rec aux = function
    | [] -> raise Not_found
    | n :: _ when n.tag = t -> n
    | n :: ns -> aux (n.children @ ns)
  in
  aux [ n ]

(** [is_derived n t]: Returns true if [t] is derived from [n], i.e. [t] is a
    descendant of [n] *)
let is_derived n t =
  try
    let _ = find n t in
    Local
  with Not_found -> Foreign

let add_child ~parent ~root child =
  let found = ref false in
  let rec aux node =
    if node.tag = parent then (
      found := true;
      { node with children = child :: node.children })
    else { node with children = List.map aux node.children }
  in
  let root' = aux root in
  if !found then root' else raise Not_found

let all_nodes root =
  let rec aux acc = function
    | [] -> acc
    | n :: ns -> aux (n :: acc) (n.children @ ns)
  in
  aux [] [ root ]

let update root f tag =
  let found = ref false in
  let rec aux node =
    if node.tag = tag then (
      found := true;
      f node)
    else { node with children = List.map aux node.children }
  in
  let root' = aux root in
  if !found then root' else raise Not_found

module TagMap = Map.Make (struct
  type t = tag

  let compare = compare
end)

type tb_state = state TagMap.t

let empty_state = TagMap.empty
let set_state = TagMap.add

(** [access root accessed e state]: Update all nodes in the mapping [state] for
    the tree rooted at [root] with an event [e], that happened at [accessed].
    Returns the new state and a boolean indicating whether an undefined behavior
    was encountered *)
let access (root : t) accessed e st : tb_state * bool =
  let ub_happened = ref false in
  let st =
    List.fold_left
      (fun st node ->
        TagMap.update node.tag
          (function None -> Some node.initial_state | st -> st)
          st)
      st
    @@ all_nodes root
  in
  L.debug (fun m ->
      let pp_binding fmt (tag, st) = Fmt.pf fmt "%d->%a" tag pp_state st in
      m "TB: %a at %d, for state (%a) and tree %a" pp_access e accessed
        Fmt.(iter_bindings ~sep:comma TagMap.iter pp_binding)
        st pp root);
  let st' =
    TagMap.mapi
      (fun tag st ->
        let node = find root tag in
        let rel = is_derived node accessed in
        let st' = transition ~protected:node.protected st (rel, e) in
        if st' = UB then (
          ub_happened := true;
          L.debug (fun m ->
              m
                "TB: Undefined behavior encountered for %d, %a %a (protected? \
                 %b): %a->%a"
                tag pp_locality rel pp_access e node.protected pp_state st
                pp_state st'));
        st')
      st
  in
  (st', !ub_happened)

let merge =
  TagMap.merge @@ fun _ l r ->
  match (l, r) with
  | None, None -> None
  | Some v, None | None, Some v -> Some v
  | Some l, Some r -> Some (meet l r)
