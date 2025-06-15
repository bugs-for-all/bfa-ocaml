module CF = Cerb_frontend
open CF.Ctype
open Typed.Syntax

type bv_info = { bv_size : int; signed : bool }

type layout = {
  size : int;
  align : int;
  members_ofs : (CF.Symbol.identifier * int) Array.t;
}

module Tag_defs = struct
  type def = Cerb_location.t * Cerb_frontend.Ctype.tag_definition

  type _ Effect.t +=
    | Find_tag : CF.Symbol.sym -> def option Effect.t
    | Find_layout_cache : CF.Ctype.ctype -> layout option Effect.t
    | Add_layout_cache : CF.Ctype.ctype * layout -> unit Effect.t

  let find_opt id tbl =
    match Hashtbl.find_opt tbl id with
    | Some x -> Some x
    | None ->
        L.debug (fun m -> m "Cannot find definition for %a" Fmt_ail.pp_sym id);
        None

  let add_defs defs tbl =
    List.iter (fun (id, (loc, _, def)) -> Hashtbl.add tbl id (loc, def)) defs

  let get_or_compute_cached_layout id f =
    match Effect.perform (Find_layout_cache id) with
    | Some layout -> Some layout
    | None ->
        let open Syntaxes.Option in
        let* layout = f () in
        Effect.perform (Add_layout_cache (id, layout));
        Some layout

  let run_with_prog (sigma : Ail_tys.sigma) f =
    let open Effect.Deep in
    let tag_defs = Hashtbl.create 1020 in
    let layouts = Hashtbl.create 1020 in
    add_defs sigma.tag_definitions tag_defs;
    try f () with
    | effect Find_tag id, k -> continue k (find_opt id tag_defs)
    | effect Find_layout_cache ty, k -> continue k (Hashtbl.find_opt layouts ty)
    | effect Add_layout_cache (ty, l), k ->
        continue k (Hashtbl.replace layouts ty l)

  let find_opt id = Effect.perform (Find_tag id)
end

let is_int (Ctype (_, ty)) =
  match ty with Basic (Integer _) -> true | _ -> false

let normalise_int_ty int_ty =
  Cerb_frontend.Ocaml_implementation.(normalise_integerType DefaultImpl.impl)
    int_ty

let size_of_int_ty (int_ty : integerType) =
  (* DefaultImpl has a size for everything *)
  CF.Ocaml_implementation.DefaultImpl.impl.sizeof_ity (normalise_int_ty int_ty)

let align_of_int_ty (int_ty : integerType) =
  CF.Ocaml_implementation.DefaultImpl.impl.alignof_ity (normalise_int_ty int_ty)

let size_of_float_ty (fty : floatingType) =
  CF.Ocaml_implementation.DefaultImpl.impl.sizeof_fty fty

let align_of_float_ty (fty : floatingType) =
  CF.Ocaml_implementation.DefaultImpl.impl.alignof_fty fty

(* TODO: unsupported things need to be signaled a bit better here. *)

let rec layout_of ty =
  let open Syntaxes.Option in
  (* Get cache, if not found, compute and update cache. *)
  Tag_defs.get_or_compute_cached_layout ty @@ fun () ->
  let (Ctype (_, ty)) = ty in
  match ty with
  | Basic (Integer inty) ->
      let* size = size_of_int_ty inty in
      let+ align = align_of_int_ty inty in
      { size; align; members_ofs = [||] }
  | Basic (Floating fty) ->
      let* size = size_of_float_ty fty in
      let+ align = align_of_float_ty fty in
      { size; align; members_ofs = [||] }
  | Pointer _ -> layout_of (Ctype ([], Basic (Integer Size_t)))
  | Struct tag ->
      let* loc, def = Tag_defs.find_opt tag in
      let* members, flexible_array_member =
        match def with
        | StructDef (m, fam) -> Some (m, fam)
        | _ ->
            L.debug (fun m -> m "Don't have a definition of structure");
            None
      in
      let* () =
        (* TODO: flexible array members *)
        if Option.is_some flexible_array_member then (
          Csymex.push_give_up ("Unsupported flexible array member", loc);
          None)
        else Some ()
      in
      layout_of_members members
  | _ ->
      L.debug (fun m -> m "Cannot compute layout of %a" Fmt_ail.pp_ty_ ty);
      None

(** From:
    https://www.gnu.org/software/c-intro-and-ref/manual/html_node/Structure-Layout.html
    The structure’s fields appear in the structure layout in the order they are
    declared. When possible, consecutive fields occupy consecutive bytes within
    the structure. However, if a field’s type demands more alignment than it
    would get that way, C gives it the alignment it requires by leaving a gap
    after the previous field.

    Once all the fields have been laid out, it is possible to determine the
    structure’s alignment and size. The structure’s alignment is the maximum
    alignment of any of the fields in it. Then the structure’s size is rounded
    up to a multiple of its alignment. That may require leaving a gap at the end
    of the structure. *)
and layout_of_members members =
  let open Syntaxes.Option in
  let rec aux members_ofs (layout : layout) = function
    | [] -> Some (List.rev members_ofs, layout)
    | (field_name, (_attrs, _align, _quals, ty)) :: rest ->
        let { size = curr_size; align = curr_align; members_ofs = _ } =
          layout
        in
        let* { size; align; _ } = layout_of ty in
        let mem_ofs = curr_size + (curr_size mod align) in
        let new_size = mem_ofs + size in
        let new_align = Int.max align curr_align in
        aux
          ((field_name, mem_ofs) :: members_ofs)
          { size = new_size; align = new_align; members_ofs = [||] }
          rest
  in
  let+ members_ofs, { size; align; members_ofs = _ } =
    aux [] { size = 0; align = 1; members_ofs = [||] } members
  in
  {
    size = size + (size mod align);
    align;
    members_ofs = Array.of_list members_ofs;
  }

let size_of_s ty =
  match layout_of ty with
  | Some { size; _ } -> Csymex.return (Typed.int size)
  | None ->
      Fmt.kstr Csymex.not_impl "Cannot yet compute size of type %a"
        Fmt_ail.pp_ty ty

let member_ofs id ty =
  match layout_of ty with
  | Some { members_ofs; _ } -> (
      let res =
        Array.find_opt (fun (id', _) -> CF.Symbol.idEqual id id') members_ofs
      in
      match res with
      | Some (_, ofs) -> Csymex.return (Typed.int ofs)
      | None ->
          Fmt.kstr Csymex.not_impl "Cannot find member %a in type %a"
            Fmt_ail.pp_id id Fmt_ail.pp_ty ty)
  | None ->
      Fmt.kstr Csymex.not_impl "Cannot yet compute layout of type %a"
        Fmt_ail.pp_ty ty

let int_bv_info (int_ty : integerType) =
  let open Syntaxes.Option in
  let int_ty = normalise_int_ty int_ty in
  match int_ty with
  | Char | Bool -> Some { bv_size = 8; signed = false }
  | Signed _ ->
      let+ size = size_of_int_ty int_ty in
      { bv_size = size * 8; signed = true }
  | Unsigned _ ->
      let+ size = size_of_int_ty int_ty in
      { bv_size = size * 8; signed = false }
  | _ ->
      L.debug (fun m ->
          m "Did not derive bv_info for %a" Fmt_ail.pp_int_ty int_ty);
      None

let bv_info (ty : ctype) =
  match proj_ctype_ ty with Basic (Integer ity) -> int_bv_info ity | _ -> None

let int_constraints (int_ty : integerType) =
  let open Typed.Infix in
  let open Syntaxes.Option in
  let int_ty = normalise_int_ty int_ty in
  match int_ty with
  | Char -> Some (fun x -> [ 0s <=@ x; x <@ 256s ])
  | Bool -> Some (fun x -> [ 0s <=@ x; x <@ 2s ])
  | Signed _ ->
      let+ size = size_of_int_ty int_ty in
      let min = Z.neg (Z.shift_left Z.one ((size * 8) - 1)) in
      let max = Z.pred (Z.shift_left Z.one ((size * 8) - 1)) in
      fun x -> [ Typed.int_z min <=@ x; x <=@ Typed.int_z max ]
  | Unsigned _ ->
      let+ size = size_of_int_ty int_ty in
      let max = Z.pred (Z.shift_left Z.one (size * 8)) in
      fun x -> [ 0s <=@ x; x <=@ Typed.int_z max ]
  | _ ->
      L.debug (fun m -> m "No int constraints for %a" Fmt_ail.pp_int_ty int_ty);
      None

let constraints (ty : ctype) :
    (Typed.T.cval Typed.t -> Typed.T.sbool Typed.t list) option =
  let open Typed.Infix in
  match proj_ctype_ ty with
  | Void -> Some (fun x -> [ x ==@ 0s ])
  | Pointer _ -> Some (fun _ -> [])
  | Basic (Integer ity) -> (
      match int_constraints ity with
      | None -> None
      | Some constrs ->
          Some
            (fun x ->
              match Typed.cast_checked x Typed.t_int with
              | None -> [ Typed.v_false ]
              | Some x -> constrs x))
  | _ ->
      L.info (fun m ->
          m "No constraints implemented for type %a" Fmt_ail.pp_ty ty);
      None

let nondet_c_ty (ty : ctype) : Typed.T.cval Typed.t Csymex.t =
  let open Csymex.Syntax in
  match proj_ctype_ ty with
  | Void -> Csymex.return 0s
  | Pointer _ ->
      let* loc = Csymex.nondet Typed.t_loc in
      let* ofs = Csymex.nondet Typed.t_int in
      Csymex.return (Typed.Ptr.mk loc ofs)
  | Basic (Integer ity) ->
      let constrs = int_constraints ity |> Option.get in
      let+ res = Csymex.nondet ~constrs Typed.t_int in
      (res :> Typed.T.cval Typed.t)
  | Basic (Floating _) -> Csymex.not_impl "nondet_c_ty: floating"
  | Array _ | Function _ | FunctionNoParams _ | Struct _ | Union _ | Atomic _ ->
      Csymex.not_impl "nondet_c_ty: unsupported type"
