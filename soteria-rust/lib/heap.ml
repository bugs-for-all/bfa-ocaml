open Rustsymex.Syntax
open Typed.Infix
open Typed.Syntax
module T = Typed.T
open Rustsymex
module Sptr = Sptr.ArithPtr
module Encoder = Encoder.Make (Sptr)

type 'a err = 'a * Charon.Meta.span Soteria_terminal.Call_trace.t

let add_to_call_trace (err, trace_elem) trace_elem' =
  (err, trace_elem' :: trace_elem)

let with_error_loc_as_call_trace ?(msg = "Triggering memory operation") st f =
  let open Rustsymex.Syntax in
  let+- err, loc = f () in
  ((err, Soteria_terminal.Call_trace.singleton ~loc ~msg ()), st)

module HeapKey = struct
  include Typed
  module Symex = Rustsymex.SYMEX
  (* FIXME: Rustsymex.SYMEX instead of just Rustsymex because Rustsymex overrides the L module right now. *)

  type t = T.sloc Typed.t

  let pp = ppa

  (* let fresh ?constrs () = Rustsymex.nondet ?constrs Typed.t_loc *)
  let indices = ref 0

  (* We know all keys are distinct, so we avoid the extra assertion *)
  let distinct _ = Typed.v_true

  (* This *only* works in WPST!!! *)
  let fresh ?constrs () =
    incr indices;
    let idx = !indices in
    let loc = Ptr.loc_of_int idx in
    match constrs with
    | Some constrs ->
        let+ () = Rustsymex.assume (constrs loc) in
        loc
    | None -> return loc
end

module SPmap = Pmap_direct_access (HeapKey)

type global = String of string | Global of Charon.Types.global_decl_id
[@@deriving show { with_path = false }, ord]

module GlobMap = struct
  include Map.Make (struct
    type t = global

    let compare = compare_global
  end)

  let pp pp_v fmt m =
    let pp_pair = Fmt.pair ~sep:(Fmt.any " -> ") pp_global pp_v in
    Fmt.pf fmt "%a" (Fmt.iter_bindings ~sep:Fmt.comma iter pp_pair) m
end

module FunBiMap = struct
  module LocMap = Map.Make (struct
    type t = T.sloc Typed.t

    let compare = Typed.compare
  end)

  module FunMap = Map.Make (struct
    type t = Charon.Expressions.fn_ptr

    let compare = Charon.Expressions.compare_fn_ptr
  end)

  type t = Charon.Expressions.fn_ptr LocMap.t * T.sloc Typed.t FunMap.t

  let empty = (LocMap.empty, FunMap.empty)

  let add loc fn_ptr (lmap, fmap) =
    let lmap' = LocMap.add loc fn_ptr lmap in
    let fmap' = FunMap.add fn_ptr loc fmap in
    (lmap', fmap')

  let get_fn loc (lmap, _) = LocMap.find_opt loc lmap
  let get_loc fn (_, fmap) = FunMap.find_opt fn fmap

  let pp fmt (lmap, _) =
    let pp_pair =
      Fmt.pair ~sep:(Fmt.any " -> ") Typed.ppa Charon.Expressions.pp_fn_ptr
    in
    Fmt.pf fmt "%a" (Fmt.iter_bindings ~sep:Fmt.comma LocMap.iter pp_pair) lmap
end

type sub = Tree_block.t * Tree_borrow.t

and t = {
  heap : sub Freeable.t SPmap.t option;
  functions : FunBiMap.t;
  globals : Sptr.t Charon_util.full_ptr GlobMap.t;
  errors : Error.t err list; [@printer Fmt.list Error.pp_err_and_call_trace]
}
[@@deriving show { with_path = false }]

type serialized = Tree_block.serialized Freeable.serialized SPmap.serialized
[@@deriving show { with_path = false }]

let pp_pretty ~ignore_freed ft { heap; _ } =
  let ignore =
    if ignore_freed then function _, Freeable.Freed -> true | _ -> false
    else fun _ -> false
  in
  match heap with
  | None -> Fmt.pf ft "Empty Heap"
  | Some st ->
      SPmap.pp ~ignore
        (Freeable.pp (fun fmt (tb, _) -> Tree_block.pp_pretty fmt tb))
        ft st

let empty =
  {
    heap = None;
    functions = FunBiMap.empty;
    globals = GlobMap.empty;
    errors = [];
  }

let log action ptr st =
  L.trace (fun m ->
      m "About to execute action: %s at %a (%a)@\n@[<2>HEAP:@ %a@]" action
        Sptr.pp ptr Charon_util.pp_span (get_loc ())
        (pp_pretty ~ignore_freed:true)
        st)

let with_heap st f =
  let+ res = f st.heap in
  match res with
  | Soteria_symex.Compo_res.Ok (v, h) ->
      Soteria_symex.Compo_res.Ok (v, { st with heap = h })
  | Missing fixes -> Missing fixes
  | Error e -> Error e

let with_tbs b f =
  let block, tree_borrow =
    match b with
    | None -> (None, Tree_borrow.init ~state:UB ())
    | Some (block, tb) -> (Some block, tb)
  in
  let+ res = f (block, tree_borrow) in
  match res with
  | Soteria_symex.Compo_res.Ok (v, block) ->
      let block = Option.map (fun b -> (b, tree_borrow)) block in
      Soteria_symex.Compo_res.Ok (v, block)
  | Missing fixes -> Missing fixes
  | Error e -> Error e

let check_ptr_align (ptr : Sptr.t) ty st =
  let@ () = with_error_loc_as_call_trace st in
  let* expected_align = Layout.align_of_s ty in
  let ofs = Typed.Ptr.ofs ptr.ptr in
  let align = ptr.align in
  L.debug (fun m ->
      m "Checking pointer alignment of %a: ofs %a mod %d / expect %a for %a"
        Sptr.pp ptr Typed.ppa ofs align Typed.ppa expected_align
        Charon_util.pp_ty ty);
  if%sat
    ofs %@ expected_align ==@ 0s &&@ (Typed.int align %@ expected_align ==@ 0s)
  then Result.ok ()
  else error `MisalignedPointer

let with_ptr (ptr : Sptr.t) (st : t)
    (f :
      [< T.sint ] Typed.t * sub option ->
      ('a * sub option, 'err, 'fix list) Result.t) :
    ('a * t, 'err, serialized list) Result.t =
  if%sat Sptr.is_at_null_loc ptr then Result.error `NullDereference
  else
    let loc, ofs = Typed.Ptr.decompose ptr.ptr in
    let@ heap = with_heap st in
    let++ v, heap =
      (SPmap.wrap (Freeable.wrap (fun st -> f (ofs, st)))) loc heap
    in
    (v, heap)

let uninit (ptr, _) ty st =
  let@ () = with_error_loc_as_call_trace st in
  let@ () = with_loc_err () in
  log "uninit" ptr st;
  let* size = Layout.size_of_s ty in
  let@ ofs, block = with_ptr ptr st in
  let@ block, _ = with_tbs block in
  Tree_block.uninit_range ofs size block

let load ?(is_move = false) ?(ignore_borrow = false) (ptr, meta) ty st =
  let** () = check_ptr_align ptr ty st in
  let@ () = with_error_loc_as_call_trace st in
  let@ () = with_loc_err () in
  log "load" ptr st;
  let@ ofs, block = with_ptr ptr st in
  let@ block, tb = with_tbs block in
  L.debug (fun f ->
      f "Recursively reading %a from block tree at %a:@.%a" Charon_util.pp_ty ty
        Sptr.pp ptr
        Fmt.(option ~none:(any "None") Tree_block.pp)
        block);
  let handler (ty, ofs) block =
    L.debug (fun f ->
        f "Loading blocks %a:%a" Typed.ppa ofs Charon_util.pp_ty ty);
    Tree_block.load ~is_move ~ignore_borrow ofs ty ptr.tag tb block
  in
  let parser = Encoder.rust_of_cvals ~offset:ofs ?meta ty in
  let++ value, block = Encoder.ParserMonad.parse ~init:block ~handler parser in
  L.debug (fun f ->
      f "Finished reading rust value %a" (Charon_util.pp_rust_val Sptr.pp) value);
  (value, block)

(** Performs a load at the tree borrow level, by updating the borrow state,
    without attempting to validate the values or checking uninitialised memory
    accesses; all of these are ignored. *)
let tb_load (ptr, _) ty st =
  let* size = Layout.size_of_s ty in
  if%sat size ==@ 0s then Result.ok ((), st)
  else
    let@ () = with_error_loc_as_call_trace st in
    let@ () = with_loc_err () in
    log "tb_load" ptr st;
    let@ ofs, block = with_ptr ptr st in
    let@ block, tb = with_tbs block in
    Tree_block.tb_access ofs size ptr.tag tb block

(** Performs a side-effect free ghost read -- this does not modify the state or
    the tree-borrow state. Returns true if the value was read successfully,
    false otherwise. *)
let is_valid_ptr st ptr ty =
  L.debug (fun m -> m "The following read is a GHOST read");
  let+ res = load ~ignore_borrow:true ptr ty st in
  match res with Ok _ -> true | _ -> false

let store (ptr, _) ty sval st =
  let parts = Encoder.rust_to_cvals sval ty in
  if List.is_empty parts then Result.ok ((), st)
  else
    let** () = check_ptr_align ptr ty st in
    let@ () = with_error_loc_as_call_trace st in
    let@ () = with_loc_err () in
    L.debug (fun f ->
        f "Parsed to parts [%a]"
          Fmt.(list ~sep:comma Encoder.pp_cval_info)
          parts);
    log "store" ptr st;
    let@ ofs, block = with_ptr ptr st in
    let@ block, tb = with_tbs block in
    let* size = Layout.size_of_s ty in
    (* We uninitialise the whole range before writing, to ensure padding bytes are copied if
           there are any. *)
    let** (), block = Tree_block.uninit_range ofs size block in
    Result.fold_list parts ~init:((), block)
      ~f:(fun ((), block) { value; ty; offset } ->
        Tree_block.store (offset +@ ofs) ty value ptr.tag tb block)

let copy_nonoverlapping ~dst:(dst, _) ~src:(src, _) ~size st =
  let@ () = with_error_loc_as_call_trace st in
  let@ () = with_loc_err () in
  let** tree_to_write, st =
    let@ ofs, block = with_ptr src st in
    let@ block, _ = with_tbs block in
    let++ tree, _ = Tree_block.get_raw_tree_owned ofs size block in
    (tree, block)
  in
  let@ ofs, block = with_ptr dst st in
  let@ block, _ = with_tbs block in
  Tree_block.put_raw_tree ofs tree_to_write block

let alloc size align st =
  (* Commenting this out as alloc cannot fail *)
  (* let@ () = with_loc_err () in*)
  let@ heap = with_heap st in
  let@ () = with_error_loc_as_call_trace st in
  let tb = Tree_borrow.init ~state:Unique () in
  let block = Freeable.Alive (Tree_block.alloc (Typed.int size), tb) in
  let** loc, heap = SPmap.alloc ~new_codom:block heap in
  let ptr = Typed.Ptr.mk loc 0s in
  let ptr : Sptr.t Charon_util.full_ptr =
    ({ ptr; tag = tb.tag; align; size }, None)
  in
  (* The pointer is necessarily not null *)
  let+ () = assume [ Typed.(not (loc ==@ Ptr.null_loc)) ] in
  Soteria_symex.Compo_res.ok (ptr, heap)

let alloc_ty ty st =
  let* layout = Layout.layout_of_s ty in
  alloc layout.size layout.align st

let alloc_tys tys st =
  let@ heap = with_heap st in
  let@ () = with_error_loc_as_call_trace st in
  SPmap.allocs heap ~els:tys ~fn:(fun ty loc ->
      (* make treeblock *)
      let* layout = Layout.layout_of_s ty in
      let size = Typed.int layout.size in
      let tb = Tree_borrow.init ~state:Unique () in
      let block = Freeable.Alive (Tree_block.alloc size, tb) in
      (* create pointer *)
      let+ () = assume [ Typed.(not (loc ==@ Ptr.null_loc)) ] in
      let ptr = Typed.Ptr.mk loc 0s in
      let ptr : Sptr.t =
        { ptr; tag = tb.tag; align = layout.align; size = layout.size }
      in
      (block, (ptr, None)))

let free (({ ptr; _ } : Sptr.t), _) ({ heap; _ } as st) =
  let@ () = with_error_loc_as_call_trace st in
  if%sat Typed.Ptr.ofs ptr ==@ 0s then
    let@ () = with_loc_err () in
    (* TODO: does the tag not play a role in freeing? *)
    let++ (), heap =
      SPmap.wrap
        (Freeable.free ~assert_exclusively_owned:(fun t ->
             Tree_block.assert_exclusively_owned @@ Option.map fst t))
        (Typed.Ptr.loc ptr) heap
    in
    ((), { st with heap })
  else error `InvalidFree

let zeros (ptr, _) size st =
  let@ () = with_error_loc_as_call_trace st in
  let@ () = with_loc_err () in
  log "zeroes" ptr st;
  let@ ofs, block = with_ptr ptr st in
  let@ block, _ = with_tbs block in
  Tree_block.zero_range ofs size block

let error err st =
  let@ () = with_error_loc_as_call_trace st in
  L.info (fun m -> m "Heap errored !");
  error err

let lift_err st (symex : ('a, 'e, 'f) Result.t) =
  let* res = symex in
  match res with
  | Error e -> error e st
  | Ok ok -> Result.ok ok
  | Missing fix -> Result.miss fix

let store_str_global str ptr ({ globals; _ } as st) =
  let globals = GlobMap.add (String str) ptr globals in
  Result.ok ((), { st with globals })

let store_global g ptr ({ globals; _ } as st) =
  let globals = GlobMap.add (Global g) ptr globals in
  Result.ok ((), { st with globals })

let load_str_global str ({ globals; _ } as st) =
  let ptr = GlobMap.find_opt (String str) globals in
  Result.ok (ptr, st)

let load_global g ({ globals; _ } as st) =
  let ptr = GlobMap.find_opt (Global g) globals in
  Result.ok (ptr, st)

let borrow (ptr, meta) (ty : Charon.Types.ty)
    (kind : Charon.Expressions.borrow_kind) st =
  (* &UnsafeCell<T> are treated as raw pointers, and reuse parent's tag! *)
  if (kind = BShared && Layout.is_unsafe_cell ty) || Tree_borrow.is_disabled ()
  then Result.ok ((ptr, meta), st)
  else
    let@ () = with_error_loc_as_call_trace st in
    let@ () = with_loc_err () in
    let@ _, block = with_ptr ptr st in
    let* tag_st =
      match kind with
      | BShared -> return Tree_borrow.Frozen
      | (BTwoPhaseMut | BMut) when Layout.is_unsafe_cell ty ->
          return Tree_borrow.ReservedIM
      | BTwoPhaseMut | BMut -> return @@ Tree_borrow.Reserved false
      | _ ->
          Fmt.kstr not_impl "Unhandled borrow kind: %a"
            Charon.Expressions.pp_borrow_kind kind
    in
    let node = Tree_borrow.init ~state:tag_st () in
    let block, tb = Option.get block in
    let tb' = Tree_borrow.add_child ~root:tb ~parent:ptr.tag node in
    let block = Some (block, tb') in
    let ptr' = { ptr with tag = node.tag } in
    L.debug (fun m ->
        m "Borrowed pointer %a -> %a (%a)" Sptr.pp ptr Sptr.pp ptr'
          Tree_borrow.pp_state tag_st);
    Result.ok ((ptr', meta), block)

let protect (ptr, meta) (ty : Charon.Types.ty) (mut : Charon.Types.ref_kind) st
    =
  if Layout.is_unsafe_cell ty || Tree_borrow.is_disabled () then
    Result.ok ((ptr, meta), st)
  else
    let@ () = with_error_loc_as_call_trace st in
    let@ () = with_loc_err () in
    let@ ofs, block = with_ptr ptr st in
    let block, tb = Option.get block in
    let tag_st =
      match mut with RMut -> Tree_borrow.Reserved false | RShared -> Frozen
    in
    let node = Tree_borrow.init ~state:tag_st ~protector:true () in
    let tb' = Tree_borrow.add_child ~root:tb ~parent:ptr.tag node in
    let ptr' = { ptr with tag = node.tag } in
    let* size = Layout.size_of_s ty in
    L.debug (fun m ->
        m "Protecting pointer %a -> %a, on [%a;%a]" Sptr.pp ptr Sptr.pp ptr'
          Typed.ppa ofs Typed.ppa size);
    let++ (), block' =
      (* nothing to protect *)
      if%sat size ==@ 0s then Result.ok ((), Some block)
      else Tree_block.protect ofs size node.tag tb' (Some block)
    in
    let block = Option.map (fun b' -> (b', tb')) block' in
    ((ptr', meta), block)

let unprotect (ptr, _) (ty : Charon.Types.ty) st =
  let@ () = with_error_loc_as_call_trace st in
  let@ () = with_loc_err () in
  let@ ofs, block = with_ptr ptr st in
  let* size = Layout.size_of_s ty in
  let block, tb = Option.get block in
  let tb' =
    Tree_borrow.update tb (fun n -> { n with protector = false }) ptr.tag
  in
  let++ (), block' =
    if%sat size ==@ 0s then Result.ok ((), Some block)
    else Tree_block.unprotect ofs size ptr.tag tb' (Some block)
  in
  let block' = Option.map (fun b' -> (b', tb')) block' in
  L.debug (fun m -> m "Unprotected pointer %a" Sptr.pp ptr);
  ((), block')

let leak_check st =
  let@ () = with_error_loc_as_call_trace ~msg:"Leaking function" st in
  let@ () = with_loc_err () in
  let global_addresses =
    GlobMap.bindings st.globals
    |> List.map (fun (_, ((ptr : Sptr.t), _)) -> Typed.Ptr.loc ptr.ptr)
  in
  let@ heap = with_heap st in
  let** leaks =
    SPmap.fold
      (fun leaks (k, v) ->
        (* FIXME: This only works because our addresses are concrete *)
        if Freeable.Freed <> v && not (List.mem k global_addresses) then
          Result.ok (k :: leaks)
        else Result.ok leaks)
      [] heap
  in
  if List.is_empty leaks then Result.ok ((), heap) else Result.error `MemoryLeak

let add_error e ({ errors; _ } as st) =
  Result.ok ((), { st with errors = (e :> Error.t err) :: errors })

let pop_error ({ errors; _ } as st) =
  match errors with
  | e :: rest -> Result.error (e, { st with errors = rest })
  | _ -> failwith "pop_error with no errors?"

let unwind_with ~f ~fe symex =
  Result.bind_2 symex ~f ~fe:(fun (((err_ty, _) as err), state) ->
      if Error.is_unwindable err_ty then fe (err, state)
      else Result.error (err, state))

let declare_fn fn_ptr ({ functions; _ } as st) =
  match FunBiMap.get_loc fn_ptr functions with
  | Some loc ->
      let ptr = Typed.Ptr.mk loc 0s in
      (* FIXME: what is the size and align of a fn pointer? *)
      let ptr : Sptr.t = { ptr; tag = Tree_borrow.zero; align = 1; size = 1 } in
      Result.ok ((ptr, None), st)
  | None ->
      (* FIXME: once we stop having concrete locations, we'll need to add a distinct
       constraint here. *)
      let* loc = HeapKey.fresh () in
      let ptr = Typed.Ptr.mk loc 0s in
      (* FIXME: what is the size and align of a fn pointer? *)
      let ptr : Sptr.t = { ptr; tag = Tree_borrow.zero; align = 1; size = 1 } in
      let functions = FunBiMap.add loc fn_ptr functions in
      Result.ok ((ptr, None), { st with functions })

let lookup_fn (({ ptr; _ } : Sptr.t), _) ({ functions; _ } as st) =
  let@ () = with_error_loc_as_call_trace st in
  let@ () = with_loc_err () in
  if%sat Typed.Ptr.ofs ptr ==@ 0s then
    let loc = Typed.Ptr.loc ptr in
    let fn = FunBiMap.get_fn loc functions in
    let* fn = of_opt_not_impl ~msg:"Could not resolve function" fn in
    Result.ok (fn, st)
  else Result.error `MisalignedFnPointer
