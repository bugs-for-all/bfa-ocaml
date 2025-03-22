(* This could be parametric on an integer type and Node type. But is there realy a reason ? *)

open Bfa_symex.Compo_res
open Rustsymex.Syntax
open Typed
open Typed.Infix
open Typed.Syntax
open Rustsymex
open Charon

let miss_no_fix_immediate ?(msg = "") () =
  let msg = "MISSING WITH NO FIX " ^ msg in
  L.info (fun m -> m "%s" msg);
  Rustsymex.push_give_up (msg, get_loc ());
  Bfa_symex.Compo_res.Missing []

let miss_no_fix ?msg () = Rustsymex.return (miss_no_fix_immediate ?msg ())

module MemVal = struct
  type t = { value : T.cval Typed.t; ty : Values.literal_type }
  [@@deriving make]

  let pp ft t =
    let open Fmt in
    pf ft "%a : %s" Typed.ppa t.value (Charon_util.lit_to_string t.ty)
end

module Node = struct
  type qty = Partially | Totally

  let pp_qty ft = function
    | Partially -> Fmt.pf ft "Part."
    | Totally -> Fmt.pf ft "Tot."

  let merge_qty left right =
    match (left, right) with Totally, Totally -> Totally | _, _ -> Partially

  type mem_val = Zeros | Uninit of qty | Lazy | Init of MemVal.t | Any

  let pp_mem_val ft =
    let open Fmt in
    function
    | Zeros -> pf ft "Zeros"
    | Uninit qty -> pf ft "Uninit %a" pp_qty qty
    | Lazy -> pf ft "Lazy"
    | Init mv -> MemVal.pp ft mv
    | Any -> pf ft "Any"

  (* Later we could add arbitrary annotations to the mem_val variant.
     For example, we can keep track of read-only or tainted bytes.
     i.e. MemVal { v: mem_val; annot: 'a }.
     Quite probably, the Node module can be parametrized by a module
     capturing the annotations behavior in analysis. *)
  type t = NotOwned of qty | Owned of mem_val

  let pp ft = function
    | NotOwned qty -> Fmt.pf ft "NotOwned %a" pp_qty qty
    | Owned mv -> pp_mem_val ft mv

  let is_fully_owned = function NotOwned _ -> false | Owned _ -> true

  let merge ~left ~right =
    match (left, right) with
    | NotOwned Totally, NotOwned Totally -> return (NotOwned Totally)
    | NotOwned _, _ | _, NotOwned _ -> return (NotOwned Partially)
    | Owned Zeros, Owned Zeros -> return (Owned Zeros)
    | Owned (Uninit lq), Owned (Uninit rq) ->
        let qty = merge_qty lq rq in
        return (Owned (Uninit qty))
    | Owned _, Owned _ -> return (Owned Lazy)

  let split ~range:(_, _) ~at node =
    match node with
    | NotOwned Totally -> return (NotOwned Totally, NotOwned Totally)
    | Owned (Uninit Totally) ->
        return (Owned (Uninit Totally), Owned (Uninit Totally))
    | Owned Zeros -> return (Owned Zeros, Owned Zeros)
    | Owned Any -> return (Owned Any, Owned Any)
    | Owned (Init _) ->
        Fmt.kstr not_impl "Splitting %a at %a" pp node Typed.ppa at
    | NotOwned Partially | Owned (Uninit Partially) | Owned Lazy ->
        failwith "Should never split an intermediate node"

  let uninit = Owned (Uninit Totally)

  let decode ~ty t : ([> T.sint ] Typed.t, 'err, 'fix) Rustsymex.Result.t =
    match t with
    | NotOwned _ -> miss_no_fix ~msg:"decode" ()
    | Owned (Uninit _) -> Result.error `UninitializedMemoryAccess
    | Owned Zeros -> Result.ok @@ Layout.to_zeros ty
    | Owned Lazy ->
        Fmt.kstr not_impl "Lazy memory access, cannot decode %a" pp t
    | Owned (Init { value; ty = tyw }) ->
        if Values.equal_literal_type ty tyw then Result.ok value
        else
          Fmt.kstr not_impl "Type mismatch when decoding value: %a vs %a"
            Types.pp_literal_type ty Types.pp_literal_type tyw
    | Owned Any ->
        (* We don't know if this read is valid, as memory could be uninitialised.
           We have to approximate and vanish. *)
        L.info (fun m -> m "Reading from Any memory, vanishing.");
        Rustsymex.vanish ()
end

module Tree = struct
  type t = { node : Node.t; range : Range.t; children : (t * t) option }
  [@@deriving show { with_path = false }, make]

  let rec pp ft { node; range; children } =
    let open Fmt in
    pf ft "@[<v 2>{%a %a%a}@]" Node.pp node Range.pp range
      (option ~none:nop (fun fmt (l, r) -> pf fmt " -->@,@[%a@,%a@]" pp l pp r))
      children

  let is_empty { node; _ } =
    match node with NotOwned Totally -> true | _ -> false

  let uninit range = make ~node:Node.uninit ~range ?children:None ()
  let zeros range = make ~node:(Node.Owned Node.Zeros) ~range ?children:None ()
  let not_owned range = make ~node:(NotOwned Totally) ~range ?children:None ()

  let sval_leaf ~range ~value ~ty =
    make ~node:(Owned (Init { value; ty })) ~range ?children:None ()

  let any range = make ~node:(Owned Node.Any) ~range ?children:None ()

  let rec iter_leaves_rev t f =
    match t.children with
    | None -> f t
    | Some (l, r) ->
        iter_leaves_rev r f;
        iter_leaves_rev l f

  let of_children_s ~left ~right =
    let range = (fst left.range, snd right.range) in
    let+ node = Node.merge ~left:left.node ~right:right.node in
    let children =
      match node with
      | Node.NotOwned Totally | Owned (Zeros | Uninit Totally) -> None
      | _ -> Some (left, right)
    in
    { range; children; node }

  let of_children _ ~left ~right = of_children_s ~left ~right

  let with_children t ~left ~right =
    return { t with children = Some (left, right) }

  let rec offset ~by tree =
    let low, high = tree.range in
    let range = (low +@ by, high +@ by) in
    let children =
      Option.map (fun (l, r) -> (offset ~by l, offset ~by r)) tree.children
    in
    make ~node:tree.node ~range ?children ()

  let rec split ~range t : (Node.t * t * t) Rustsymex.t =
    (* this function splits a tree and returns the node in the given range *)
    (* We're assuming that range is inside old_span *)
    let old_span = t.range in
    let ol, oh = old_span in
    let nl, nh = range in
    if%sat ol ==@ nl then
      let at = nh in
      let+ left_node, right_node = Node.split ~range:old_span ~at t.node in
      let left_span, right_span = Range.split_at old_span at in
      let left = make ~node:left_node ~range:left_span () in
      let right = make ~node:right_node ~range:right_span () in
      (left_node, left, right)
    else
      if%sat oh ==@ nh then
        let+ left_node, right_node = Node.split ~range:old_span ~at:nl t.node in
        let left_span, right_span = Range.split_at old_span nl in
        let left = make ~node:left_node ~range:left_span () in
        let right = make ~node:right_node ~range:right_span () in
        (right_node, left, right)
      else
        (* We're first splitting on the left then splitting again on the right *)
        let* left_node, right_node = Node.split ~range:old_span ~at:nl t.node in
        let left_span, right_span = Range.split_at old_span nl in
        let left = make ~node:left_node ~range:left_span () in
        let full_right = make ~node:right_node ~range:right_span () in
        let* node, right_left, right_right = split ~range full_right in
        let+ right =
          with_children full_right ~left:right_left ~right:right_right
        in
        (node, left, right)

  let extend_if_needed t range =
    let rl, rh = range in
    let sl, sh = t.range in
    let* t_with_left =
      if%sat rl <@ sl then
        let new_left_tree = make ~node:(NotOwned Totally) ~range:(rl, sl) () in
        let children = (new_left_tree, t) in
        let qty = if is_empty t then Node.Totally else Partially in
        return (make ~node:(NotOwned qty) ~range:(rl, sh) ~children ())
      else return t
    in
    let sl, _ = t_with_left.range in
    let* result =
      if%sat rh >@ sh then
        let new_right_tree = make ~node:(NotOwned Totally) ~range:(sh, rh) () in
        let children = (t_with_left, new_right_tree) in
        let qty = if is_empty t_with_left then Node.Totally else Partially in
        return (make ~node:(NotOwned qty) ~range:(sl, rh) ~children ())
      else return t_with_left
    in
    return result

  let rec extract (t : t) (range : Range.t) : (t * t option) Rustsymex.t =
    (* First result is the extracted tree, second is the remain *)
    if%sat Range.sem_eq range t.range then return (t, None)
    else
      let left, right = Option.get t.children in
      if%sat Range.is_inside range left.range then
        let* extracted, new_left = extract left range in
        let+ new_self =
          match new_left with
          | Some left -> of_children_s ~right ~left
          | None -> return right
        in
        (extracted, Some new_self)
      else
        let* extracted, new_right = extract right range in
        let+ new_self =
          match new_right with
          | Some right -> of_children_s ~right ~left
          | None -> return left
        in
        (extracted, Some new_self)

  let rec add_to_the_right t addition : t Rustsymex.t =
    match t.children with
    | None -> of_children_s ~left:t ~right:addition
    | Some (left, right) ->
        let* new_right = add_to_the_right right addition in
        of_children_s ~left ~right:new_right

  let rec add_to_the_left t addition : t Rustsymex.t =
    match t.children with
    | None -> of_children_s ~left:addition ~right:t
    | Some (left, right) ->
        let* new_left = add_to_the_left left addition in
        of_children_s ~left:new_left ~right

  let frame_range (t : t) ~(replace_node : t -> t) ~rebuild_parent
      (range : Range.t) : (t * t) Rustsymex.t =
    let rec frame_inside ~(replace_node : t -> t) ~rebuild_parent (t : t)
        (range : Range.t) : (t * t) Rustsymex.t =
      if%sat Range.sem_eq range t.range then
        let new_tree = replace_node t in
        return (t, new_tree)
      else
        match t.children with
        | Some (left, right) ->
            let _, mid = left.range in
            if%sat Range.strictly_inside mid range then
              let l, h = range in
              let upper_range = (mid, h) in
              let dont_replace_node t = t in
              if%sat
                (* High-range already good *)
                Range.sem_eq upper_range right.range
              then
                (* Rearrange left*)
                let lower_range = (l, mid) in
                let* _, left =
                  frame_inside ~replace_node:dont_replace_node
                    ~rebuild_parent:with_children left lower_range
                in
                let* extracted, left_opt = extract left lower_range in
                let* right = add_to_the_left right extracted in
                let* new_self =
                  of_children_s ~left:(Option.get left_opt) ~right
                in
                frame_inside ~replace_node ~rebuild_parent new_self range
              else
                let* _, right =
                  frame_inside ~replace_node:dont_replace_node
                    ~rebuild_parent:with_children right upper_range
                in
                let* extracted, right_opt = extract right upper_range in
                let* left = add_to_the_right left extracted in
                let* new_self =
                  of_children_s ~left ~right:(Option.get right_opt)
                in
                frame_inside ~replace_node ~rebuild_parent new_self range
            else
              if%sat Range.is_inside range left.range then
                let* node, left =
                  frame_inside ~replace_node ~rebuild_parent left range
                in
                let+ new_parent = rebuild_parent t ~left ~right in
                (node, new_parent)
              else
                (* Range is necessarily inside of right *)
                let* node, right =
                  frame_inside ~replace_node ~rebuild_parent right range
                in
                let+ new_parent = rebuild_parent t ~left ~right in
                (node, new_parent)
        | None ->
            let* _, left, right = split ~range t in
            let* new_self = with_children t ~left ~right in
            frame_inside ~replace_node ~rebuild_parent new_self range
    in
    let* root = extend_if_needed t range in
    frame_inside ~replace_node ~rebuild_parent root range

  let load (ofs : [< T.sint ] Typed.t) (size : [< T.sint ] Typed.t)
      (ty : Values.literal_type) (t : t) :
      (T.cval Typed.t * t, 'err, 'fix) Result.t =
    let range = Range.of_low_and_size ofs size in
    let replace_node node = node in
    let rebuild_parent = with_children in
    let* framed, tree = frame_range t ~replace_node ~rebuild_parent range in
    let++ sval = Node.decode ~ty framed.node in
    ((sval :> T.cval Typed.t), tree)

  let store (low : [< T.sint ] Typed.t) (size : [< T.sint ] Typed.t)
      (ty : Types.literal_type) (sval : [< T.cval ] Typed.t) (t : t) :
      (unit * t, 'err, 'fix) Result.t =
    let range = Range.of_low_and_size low size in
    let replace_node _ = sval_leaf ~range ~value:sval ~ty in
    let rebuild_parent = of_children in
    let* node, tree = frame_range t ~replace_node ~rebuild_parent range in
    let++ () =
      match node.node with
      | NotOwned _ -> miss_no_fix ~msg:"store" ()
      | _ -> Result.ok ()
    in
    ((), tree)

  let get_raw ofs size t =
    let range = (ofs, ofs +@ size) in
    let replace_node node = node in
    let rebuild_parent = with_children in
    frame_range t ~replace_node ~rebuild_parent range

  let put_raw tree t =
    let replace_node _ = tree in
    let rebuild_parent = of_children in
    let* old_node, new_tree =
      frame_range t ~replace_node ~rebuild_parent tree.range
    in
    let++ () =
      match old_node.node with
      | NotOwned _ -> miss_no_fix ~msg:"put_raw" ()
      | _ -> Result.ok ()
    in
    ((), new_tree)

  (** Cons/prod *)

  let consume_typed_val (low : [< T.sint ] Typed.t) (size : [< T.sint ] Typed.t)
      (ty : Types.literal_type) (v : [< T.cval ] Typed.t) (t : t) :
      (t, 'err, 'fix list) Result.t =
    let range = Range.of_low_and_size low size in
    let replace_node _ = not_owned range in
    let rebuild_parent = of_children in
    let* framed, tree = frame_range t ~replace_node ~rebuild_parent range in
    let* sval_res = Node.decode ~ty framed.node in
    match sval_res with
    | Ok sv ->
        let+ () = Rustsymex.assume [ sv ==?@ v ] in
        Ok tree
    | Missing fixes -> Rustsymex.Result.miss fixes
    | Error `UninitializedMemoryAccess -> Rustsymex.vanish ()

  let produce_typed_val (low : [< T.sint ] Typed.t) (size : [< T.sint ] Typed.t)
      (ty : Types.literal_type) (value : T.cval Typed.t) (t : t) : t Rustsymex.t
      =
    let range = Range.of_low_and_size low size in
    let* constrs =
      Rustsymex.of_opt_not_impl ~msg:"Produce typed_val constraints"
        (Layout.constraints ty)
    in
    let* () = Rustsymex.assume (constrs value) in
    let replace_node _ = sval_leaf ~range ~value ~ty in
    let rebuild_parent = of_children in
    let* framed, tree = frame_range t ~replace_node ~rebuild_parent range in
    match framed.node with
    | NotOwned Totally -> return tree
    | _ -> Rustsymex.vanish ()

  let consume_any (low : [< T.sint ] Typed.t) (len : [< T.sint ] Typed.t)
      (t : t) : (t, 'err, 'fix list) Result.t =
    let range = (low, low +@ len) in
    let replace_node _ = not_owned range in
    let rebuild_parent = of_children in
    let* framed, tree = frame_range t ~replace_node ~rebuild_parent range in
    let++ () =
      match framed.node with
      | NotOwned _ -> miss_no_fix ~msg:"consume_any" ()
      | _ -> Result.ok ()
    in
    tree

  let produce_any (low : [< T.sint ] Typed.t) (len : [< T.sint ] Typed.t)
      (t : t) : t Rustsymex.t =
    let range = (low, low +@ len) in
    let replace_node _ = any range in
    let rebuild_parent = of_children in
    let* framed, tree = frame_range t ~replace_node ~rebuild_parent range in
    match framed.node with
    | NotOwned Totally -> return tree
    | _ -> Rustsymex.vanish ()

  let consume_uninit (low : [< T.sint ] Typed.t) (len : [< T.sint ] Typed.t)
      (t : t) : (t, 'err, 'fix list) Result.t =
    let range = (low, low +@ len) in
    let replace_node _ = not_owned range in
    let rebuild_parent = of_children in
    let* framed, tree = frame_range t ~replace_node ~rebuild_parent range in
    let++ () =
      match framed.node with
      | NotOwned _ -> miss_no_fix ~msg:"consume_uninit" ()
      | Owned (Uninit Totally) -> Result.ok ()
      | _ ->
          L.info (fun m -> m "Consuming uninit but no uninit, vanishing");
          Rustsymex.vanish ()
    in
    tree

  let produce_uninit (low : [< T.sint ] Typed.t) (len : [< T.sint ] Typed.t)
      (t : t) : t Rustsymex.t =
    let range = (low, low +@ len) in
    let replace_node _ = uninit range in
    let rebuild_parent = of_children in
    let* framed, tree = frame_range t ~replace_node ~rebuild_parent range in
    match framed.node with
    | NotOwned Totally -> return tree
    | _ -> Rustsymex.vanish ()

  let consume_zeros (low : [< T.sint ] Typed.t) (len : [< T.sint ] Typed.t)
      (t : t) : (t, 'err, 'fix list) Result.t =
    let range = (low, low +@ len) in
    let replace_node _ = not_owned range in
    let rebuild_parent = of_children in
    let* framed, tree = frame_range t ~replace_node ~rebuild_parent range in
    match framed.node with
    | NotOwned _ -> miss_no_fix ~msg:"consume_zeros" ()
    | Owned Zeros -> Result.ok tree
    | Owned (Init { ty = _; value }) ->
        let+ () = Rustsymex.assume [ value ==?@ 0s ] in
        Ok tree
    | _ ->
        L.info (fun m -> m "Consuming zero but not zero, vanishing");
        Rustsymex.vanish ()

  let produce_zeros (low : [< T.sint ] Typed.t) (len : [< T.sint ] Typed.t)
      (t : t) : t Rustsymex.t =
    let range = (low, low +@ len) in
    let replace_node _ = zeros range in
    let rebuild_parent = of_children in
    let* framed, tree = frame_range t ~replace_node ~rebuild_parent range in
    match framed.node with
    | NotOwned Totally -> return tree
    | _ -> Rustsymex.vanish ()
end

type t = { root : Tree.t; bound : T.sint Typed.t option }
[@@deriving show { with_path = false }]

let is_empty t = Option.is_none t.bound && Tree.is_empty t.root

let pp_pretty ft t =
  let open PrintBox in
  let r = ref [] in
  let () =
    Option.iter
      (fun b ->
        let range_str = Fmt.str "[%a; ∞[" Typed.ppa b in
        r := [ (range_str, text "OOB") ])
      t.bound
  in
  let () =
    Tree.iter_leaves_rev t.root (fun leaf ->
        let range_str = (Fmt.to_to_string Range.pp) leaf.range in
        let node_str = Fmt.kstr text "%a" Node.pp leaf.node in
        r := (range_str, node_str) :: !r)
  in
  PrintBox_text.pp ft (frame @@ record !r)

(** Logic *)

type serialized_atom =
  | TypedVal of {
      offset : T.sint Typed.t;
      ty : Types.literal_type; [@printer Types.pp_literal_type]
      v : T.cval Typed.t;
    }
  | Bound of T.sint Typed.t
  | Uninit of { offset : T.sint Typed.t; len : T.sint Typed.t }
  | Zeros of { offset : T.sint Typed.t; len : T.sint Typed.t }
  | Any of { offset : T.sint Typed.t; len : T.sint Typed.t }
[@@deriving show { with_path = false }]

type serialized = serialized_atom list

let iter_values_serialized serialized f =
  List.iter (function TypedVal { v; _ } -> f v | _ -> ()) serialized

let mk_fix_typed offset ty () =
  let* v = Layout.nondet_literal_ty ty in
  return [ [ TypedVal { offset; ty; v } ] ]

let with_bound_check (t : t) (ofs : [< T.sint ] Typed.t) f =
  let** () =
    match t.bound with
    | None -> Result.ok ()
    | Some bound ->
        if%sat ofs >@ bound then (
          L.debug (fun m ->
              m "Out of bounds access: %a > %a" Typed.ppa ofs Typed.ppa bound);
          Result.error `OutOfBounds)
        else Result.ok ()
  in
  let++ res, root = f () in
  (res, { t with root })

let of_opt ?(mk_fixes = fun () -> Rustsymex.return []) = function
  | None ->
      let+ fixes = mk_fixes () in
      Bfa_symex.Compo_res.miss fixes
  | Some t -> Result.ok t

let to_opt t = if is_empty t then None else Some t

let assert_exclusively_owned t =
  let** t = of_opt t in
  match t.bound with
  | None -> miss_no_fix ~msg:"assert_exclusively_owned - no bound" ()
  | Some bound ->
      let Tree.{ range = low, high; node; _ } = t.root in
      if Node.is_fully_owned node then
        if%sat low ==@ 0s &&@ (high ==@ bound) then Result.ok ()
        else
          miss_no_fix
            ~msg:"assert_exclusively_owned - tree does not span [0; bound[" ()
      else miss_no_fix ~msg:"assert_exclusively_owned - tree not fully owned" ()

let load ofs size ty t =
  let** t = of_opt ~mk_fixes:(mk_fix_typed ofs ty) t in
  let++ res, tree =
    let@ () = with_bound_check t (ofs +@ size) in
    Tree.load ofs size ty t.root
  in
  (res, to_opt tree)

let store ofs size ty sval t =
  match t with
  | None -> miss_no_fix ~msg:"outer store" ()
  | Some t ->
      let++ (), tree =
        let@ () = with_bound_check t (ofs +@ size) in
        Tree.store ofs size ty sval t.root
      in
      ((), to_opt tree)

let get_raw_tree_owned ofs size t =
  let** t = of_opt t in
  let++ res, tree =
    let@ () = with_bound_check t (ofs +@ size) in
    let+ tree, t = Tree.get_raw ofs size t.root in
    if Node.is_fully_owned tree.node then
      let tree = Tree.offset ~by:~-ofs tree in
      Ok (tree, t)
    else miss_no_fix_immediate ~msg:"get_raw_tree_owned" ()
  in
  (res, to_opt tree)

(* This is used for copy_nonoverapping.
   It is an action on the destination block, and assumes the received tree is at offset 0 *)
let put_raw_tree ofs (tree : Tree.t) t :
    (unit * t option, 'err, 'fix list) Result.t =
  let** t = of_opt t in
  let size = Range.size tree.range in
  let tree = Tree.offset ~by:ofs tree in
  let++ res, t =
    let@ () = with_bound_check t (ofs +@ size) in
    Tree.put_raw tree t.root
  in
  (res, to_opt t)

let alloc size = { root = Tree.uninit (0s, size); bound = Some size }

let uninit t =
  match t with
  | None -> miss_no_fix ~msg:"uninit on none" ()
  | Some t ->
      let root = Tree.uninit t.root.range in
      Result.ok ((), Some { t with root })

(** Logic *)

let subst_serialized subst_var (serialized : serialized) =
  let v_subst v = Typed.subst subst_var v in
  let subst_atom = function
    | TypedVal { offset; ty; v } ->
        TypedVal { offset = v_subst offset; ty; v = v_subst v }
    | Bound v -> Bound (v_subst v)
    | Uninit { offset; len } ->
        Uninit { offset = v_subst offset; len = v_subst len }
    | Zeros { offset; len } ->
        Zeros { offset = v_subst offset; len = v_subst len }
    | Any { offset; len } -> Any { offset = v_subst offset; len = v_subst len }
  in
  List.map subst_atom serialized

let iter_vars_serialized serialized (f : Svalue.Var.t * [< T.cval ] ty -> unit)
    =
  List.iter
    (function
      | TypedVal { offset; v; _ } ->
          Typed.iter_vars offset f;
          Typed.iter_vars v f
      | Bound v -> Typed.iter_vars v f
      | Uninit { offset; len } ->
          Typed.iter_vars offset f;
          Typed.iter_vars len f
      | Zeros { offset; len } ->
          Typed.iter_vars offset f;
          Typed.iter_vars len f
      | Any { offset; len } ->
          Typed.iter_vars offset f;
          Typed.iter_vars len f)
    serialized

let pp_serialized = Fmt.Dump.list pp_serialized_atom

let serialize t =
  let bound =
    match t.bound with
    | None -> Seq.empty
    | Some bound -> Seq.return (Bound bound)
  in
  let rec serialize_tree (tree : Tree.t) =
    match tree.node with
    | Node.NotOwned _ -> Seq.empty
    | Owned Zeros ->
        Seq.return
          (Zeros { offset = fst tree.range; len = Range.size tree.range })
    | Owned (Uninit Totally) ->
        Seq.return
          (Uninit { offset = fst tree.range; len = Range.size tree.range })
    | Owned (Init { value = v; ty }) ->
        Seq.return (TypedVal { offset = fst tree.range; ty; v })
    | Owned Any ->
        Seq.return
          (Any { offset = fst tree.range; len = Range.size tree.range })
    | Owned (Lazy | Uninit Partially) ->
        let children = Option.get tree.children in
        Seq.append
          (serialize_tree (fst children))
          (serialize_tree (snd children))
  in
  Seq.append (serialize_tree t.root) bound |> List.of_seq

let assume_bound_check_res (t : t) (ofs : [< T.sint ] Typed.t) f =
  let* () =
    match t.bound with
    | None -> Rustsymex.return ()
    | Some bound -> Rustsymex.assume [ ofs <=@ bound ]
  in
  let++ root = f () in
  { t with root }

let assume_bound_check (t : t) (ofs : [< T.sint ] Typed.t) f =
  let* () =
    match t.bound with
    | None -> Rustsymex.return ()
    | Some bound -> Rustsymex.assume [ ofs <=@ bound ]
  in
  let+ root = f () in
  { t with root }

let abstract_cons bound cons_tree t =
  let** t = of_opt t in
  let++ tree =
    let@ () = assume_bound_check_res t bound in
    cons_tree t.root
  in
  to_opt tree

let abstract_prod bound mk_root_from_empty prod_tree t =
  match t with
  | None ->
      let+ root = mk_root_from_empty () in
      Some { bound = None; root }
  | Some t ->
      let+ tree =
        let@ () = assume_bound_check t bound in
        prod_tree t.root
      in
      to_opt tree

let consume_typed_val ofs size ty v t =
  let cons_tree root = Tree.consume_typed_val ofs size ty v root in
  abstract_cons (ofs +@ size) cons_tree t

let produce_typed_val ofs size ty v t =
  let prod_tree root = Tree.produce_typed_val ofs size ty v root in
  let mk_root_from_empty () =
    let range = Range.of_low_and_size ofs size in
    return @@ Tree.sval_leaf ~range ~value:v ~ty
  in
  abstract_prod (ofs +@ size) mk_root_from_empty prod_tree t

let consume_any ofs len t =
  let cons_tree root = Tree.consume_any ofs len root in
  abstract_cons (ofs +@ len) cons_tree t

let produce_any ofs len t =
  let prod_tree root = Tree.produce_any ofs len root in
  let mk_root_from_empty () =
    let range = (ofs, ofs +@ len) in
    Rustsymex.return (Tree.any range)
  in
  abstract_prod (ofs +@ len) mk_root_from_empty prod_tree t

let consume_uninit ofs len t =
  let cons_tree root = Tree.consume_uninit ofs len root in
  abstract_cons (ofs +@ len) cons_tree t

let produce_uninit ofs len t =
  let bound = ofs +@ len in
  let prod_tree root = Tree.produce_uninit ofs len root in
  let mk_root_from_empty () =
    let range = (ofs, bound) in
    Rustsymex.return (Tree.uninit range)
  in
  abstract_prod bound mk_root_from_empty prod_tree t

let consume_zeros ofs len t =
  let cons_tree root = Tree.consume_zeros ofs len root in
  abstract_cons (ofs +@ len) cons_tree t

let produce_zeros ofs len t =
  let bound = ofs +@ len in
  let prod_tree root = Tree.produce_zeros ofs len root in
  let mk_root_from_empty () =
    let range = (ofs, bound) in
    Rustsymex.return (Tree.zeros range)
  in
  abstract_prod bound mk_root_from_empty prod_tree t

let consume_bound bound t =
  match t with
  | None | Some { bound = None; _ } -> miss_no_fix ~msg:"consume_bound" ()
  | Some { bound = Some v; root } ->
      let+ () = Rustsymex.assume [ v ==?@ bound ] in
      Ok (to_opt { bound = None; root })

let produce_bound bound t =
  match t with
  | None ->
      Rustsymex.return
        (Some { bound = Some bound; root = Tree.not_owned (0s, bound) })
  | Some { bound = None; root } ->
      Rustsymex.return (Some { bound = Some bound; root })
  | Some { bound = Some _; _ } -> Rustsymex.vanish ()

let consume_atom atom t =
  match atom with
  | Bound bound -> consume_bound bound t
  | TypedVal { offset; ty; v } ->
      let size = Typed.int @@ Layout.size_of_literal_ty ty in
      consume_typed_val offset size ty v t
  | Uninit { offset; len } -> consume_uninit offset len t
  | Any { offset; len } -> consume_any offset len t
  | Zeros { offset; len } -> consume_zeros offset len t

let produce_atom atom t =
  match atom with
  | Bound bound -> produce_bound bound t
  | TypedVal { offset; ty; v } ->
      let size = Typed.int @@ Layout.size_of_literal_ty ty in
      produce_typed_val offset size ty v t
  | Uninit { offset; len } -> produce_uninit offset len t
  | Any { offset; len } -> produce_any offset len t
  | Zeros { offset; len } -> produce_zeros offset len t

let consume (list : serialized) (t : t option) =
  Rustsymex.Result.fold_list ~f:(fun acc st -> consume_atom st acc) ~init:t list

let produce (list : serialized) (t : t option) =
  Rustsymex.fold_list ~f:(fun acc st -> produce_atom st acc) ~init:t list
