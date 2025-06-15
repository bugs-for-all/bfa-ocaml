open Cerb_frontend
open AilSyntax
module Sym_set = Symbol_std.Set
module Sym_map = Symbol_std.Map

type extern_idmap = (Symbol.identifier, sigma_extern_id) Pmap.map
type redundant_globs = (Symbol.sym * Symbol.sym) list

exception LinkError of string

(* Ext2 should be the accumulator! *)
let link_extern (ext_cur : extern_idmap) (ext_oth : extern_idmap)
    (symmap : Ail_tys.extern_symmap) :
    (extern_idmap * redundant_globs * Ail_tys.extern_symmap, string) result =
  Pmap.fold
    (fun k (in_d, in_lk) acc ->
      let open Syntaxes.Result in
      let* acc_ext, acc_tent, symmap = acc in
      let symmap =
        match in_lk with
        | IK_declaration -> symmap
        | IK_definition | IK_tentative -> Pmap.add in_d in_d symmap
      in
      match Pmap.lookup k ext_cur with
      | None -> Ok (Pmap.add k (in_d, in_lk) acc_ext, acc_tent, symmap)
      | Some (cur_def, IK_declaration) ->
          Ok
            ( Pmap.add k (in_d, in_lk) acc_ext,
              acc_tent,
              Pmap.add cur_def in_d symmap )
      | Some (cur_tent, IK_tentative) -> (
          match in_lk with
          | IK_definition | IK_tentative ->
              Ok
                ( Pmap.add k (in_d, in_lk) acc_ext,
                  (cur_tent, in_d) :: acc_tent,
                  Pmap.add cur_tent in_d symmap )
          | IK_declaration ->
              Ok
                ( Pmap.add k (cur_tent, IK_tentative) acc_ext,
                  acc_tent,
                  Pmap.add in_d cur_tent symmap ))
      | Some (cur_def, IK_definition) -> (
          match in_lk with
          | IK_definition ->
              if !Config.current.no_ignore_duplicate_symbols then
                Error (Fmt.str "Duplicate external name %a" Fmt_ail.pp_id k)
              else
                (* There's no clear semantics for duplicate symbols, since it's not legal in C.
                   We only accept in case the build we inferred is somewhat invalid.
                   We make one choice, which is to treat the definition as a declaration. *)
                Ok
                  ( Pmap.add k (cur_def, IK_definition) acc_ext,
                    acc_tent,
                    Pmap.add in_d cur_def symmap )
          | IK_tentative ->
              Ok
                ( Pmap.add k (cur_def, IK_definition) acc_ext,
                  (in_d, cur_def) :: acc_tent,
                  Pmap.add in_d cur_def symmap )
          | IK_declaration ->
              Ok
                ( Pmap.add k (cur_def, IK_definition) acc_ext,
                  acc_tent,
                  Pmap.add in_d cur_def symmap )))
    ext_oth
    (Ok (ext_cur, [], symmap))

let set_of_bindings (bindings : bindings) =
  List.fold_left
    (fun acc (sym, _) -> Sym_set.add sym acc)
    Sym_set.empty bindings

let rec free_syms_expr acc expr =
  let (AnnotatedExpression (_, _, _, expr)) = expr in
  match expr with
  | AilEident sym -> Sym_set.add sym acc
  | AilEva_start (e, id) -> free_syms_expr (Sym_set.add id acc) e
  | AilEva_arg (e, _)
  | AilEva_end e
  | AilEunary (_, e)
  | AilEcast (_, _, e)
  | AilEassert e
  | AilEcompound (_, _, e)
  | AilEannot (_, e)
  | AilEsizeof_expr e
  | AilEprint_type e
  | AilErvalue e
  | AilEarray_decay e
  | AilEfunction_decay e
  | AilEatomic e
  | AilEmemberof (e, _)
  | AilEmemberofptr (e, _)
  | AilEbmc_assume e ->
      free_syms_expr acc e
  | AilEbinary (e1, _, e2)
  | AilEassign (e1, e2)
  | AilEcompoundAssign (e1, _, e2)
  | AilEva_copy (e1, e2) ->
      free_syms_expr (free_syms_expr acc e1) e2
  | AilEcond (e1, e2_opt, e3) ->
      let acc = free_syms_expr (free_syms_expr acc e1) e3 in
      Option.fold ~none:acc ~some:(free_syms_expr acc) e2_opt
  | AilEcall (e1, es) -> List.fold_left free_syms_expr acc (e1 :: es)
  | AilEstruct (_ty_sym, members) ->
      List.fold_left
        (fun acc (_, e) -> Option.fold ~none:acc ~some:(free_syms_expr acc) e)
        acc members
  | AilEarray (_, _, eopts) ->
      List.fold_left
        (fun acc e_opt ->
          Option.fold ~none:acc ~some:(free_syms_expr acc) e_opt)
        acc eopts
  | AilEunion (_ty_sym, _field, e_opt) ->
      Option.fold ~none:acc ~some:(free_syms_expr acc) e_opt
  | AilEgeneric (e, assocs) ->
      let acc = free_syms_expr acc e in
      List.fold_left
        (fun acc assoc ->
          match assoc with
          | AilGAtype (_, e) | AilGAdefault e -> free_syms_expr acc e)
        acc assocs
  | AilEgcc_statement (bindings, stmts) ->
      let exclude = set_of_bindings bindings in
      let res = List.fold_left free_syms_stmt acc stmts in
      Sym_set.diff res exclude
  | AilEbuiltin _ | AilEstr _ | AilEconst _ | AilEsizeof _ | AilEalignof _
  | AilEreg_load _ | AilEoffsetof _ | AilEinvalid _ ->
      acc

and free_syms_stmt acc stmt =
  match stmt.node with
  | AilSexpr e | AilSreturn e | AilSreg_store (_, e) -> free_syms_expr acc e
  | AilSwhile (e, stmt, _) | AilSdo (stmt, e, _) | AilSswitch (e, stmt) ->
      free_syms_stmt (free_syms_expr acc e) stmt
  | AilScase (_, stmt)
  | AilScase_rangeGNU (_, _, stmt)
  | AilSdefault stmt
  | AilSmarker (_, stmt) (* CN stuff *)
  | AilSlabel (_, stmt, _) ->
      free_syms_stmt acc stmt
  | AilSif (e, s1, s2) ->
      let acc = free_syms_expr acc e in
      let acc = free_syms_stmt acc s1 in
      free_syms_stmt acc s2
  | AilSskip | AilSbreak | AilScontinue | AilSreturnVoid | AilSgoto _ -> acc
  | AilSdeclaration l ->
      List.fold_left
        (fun acc (_, e_opt) ->
          Option.fold ~none:acc ~some:(free_syms_expr acc) e_opt)
        acc l
  | AilSblock (bindings, stmtl) ->
      let exclude = set_of_bindings bindings in
      let res = List.fold_left free_syms_stmt acc stmtl in
      Sym_set.diff res exclude
  | AilSpar stmtl -> List.fold_left free_syms_stmt acc stmtl

let free_syms_object_definition acc expr = free_syms_expr acc expr

let merge_globs (globs_1 : 'a sigma_object_definition list)
    (globs_2 : 'a sigma_object_definition list)
    (redundant_globs : (Symbol.sym * Symbol.sym) list) =
  let catch f = try Ok (f ()) with LinkError s -> Error s in
  catch @@ fun () ->
  (* globs_1 is the accumulator, so it is potentially much bigger than globs_2. *)
  let globs = List.rev_append globs_2 globs_1 in
  (* Create an associate list of the dependencies for each global *)
  let dep_map =
    List.map
      (fun (sym, glob) ->
        let acc =
          List.filter_map
            (fun (def, k) -> if Symbol.equal_sym def sym then Some k else None)
            redundant_globs
        in
        ( sym,
          Sym_set.to_list
            (free_syms_object_definition (Sym_set.of_list acc) glob) ))
      globs
  in
  (* We order the symbols in topological order! *)
  let ordered_syms =
    match Tsort.sort dep_map with
    | ErrorCycle _ -> raise (LinkError "cycle in global dependencies!")
    | Sorted l -> l
  in
  (* We remove the redundant globs from the list of globs *)
  let gs_map =
    List.fold_left (fun acc (s, g) -> Sym_map.add s g acc) Sym_map.empty globs
  in
  let ordered_gs =
    List.filter_map
      (fun k ->
        (* TODO: should the None case be allowed to happen? *)
        Option.map (fun g -> (k, g)) (Sym_map.find_opt k gs_map)
        (* match Sym_map.find_opt k gs_map with
        | None ->
            None
            (* Fmt.kstr
              (fun s -> raise (LinkError s))
              "merge_globs: %a not found" Fmt_ail.pp_sym k *)
        | Some g -> Some (k, g) *))
      ordered_syms
  in
  (* We have now ordered the globs *)
  (* We remove the redundant globs from the list of globs *)
  List.fold_left
    (fun acc g ->
      (* We remove the association for each redundant glob *)
      List.remove_assoc g acc)
    (List.rev ordered_gs)
    (List.map fst redundant_globs)

(* We need to check that there is only one main function. *)
(* If there are multiple main functions, we return an error. *)
(* If there is no main function, we return None. *)
(* If there is one main function, we return Some main function. *)

let link_main opt_m1 opt_m2 =
  match (opt_m1, opt_m2) with
  | (Some _ as m1), Some _ ->
      if !Config.current.no_ignore_duplicate_symbols then
        Error "linking: multiple main functions"
      else (
        L.info (fun m -> m "Detecting several main functions. ");
        Ok m1)
  | (Some _ as m), None | None, (Some _ as m) -> Ok m
  | None, None -> Ok None

let has_cn_stuff (sigma : 'a sigma) =
  match sigma with
  | {
   cn_functions = [];
   cn_lemmata = [];
   cn_predicates = [];
   cn_decl_specs = [];
   cn_idents;
   _;
  }
    when Pmap.is_empty cn_idents ->
      false
  | _ -> true

(* [(m1, f1)] is the accumulator, and [(m2, f2)] is being added to it. *)
let link_aux ((m1, f1) : 'a ail_program) ((m2, f2) : 'a ail_program)
    (symmap : Ail_tys.extern_symmap) : (Ail_tys.linked_program, string) result =
  let open Syntaxes.Result in
  let* () =
    if has_cn_stuff f1 || has_cn_stuff f2 then
      Result.error "linking: CN stuff not supported"
    else Ok ()
  in
  let* m = link_main m1 m2 in
  let* extern_idmap, redundant_globs, symmap =
    link_extern f1.extern_idmap f2.extern_idmap symmap
  in
  let+ object_definitions =
    merge_globs f1.object_definitions f2.object_definitions redundant_globs
  in
  Ail_tys.
    {
      sigma =
        {
          object_definitions;
          function_definitions =
            f1.function_definitions @ f2.function_definitions;
          declarations = f1.declarations @ f2.declarations;
          tag_definitions = f1.tag_definitions @ f2.tag_definitions;
          extern_idmap;
          typedef_attributes =
            Pmap.union f1.typedef_attributes f2.typedef_attributes;
          loop_attributes = Pmap.union f1.loop_attributes f2.loop_attributes;
          static_assertions = f1.static_assertions @ f2.static_assertions;
          cn_idents = f1.cn_idents;
          cn_functions = [];
          cn_predicates = [];
          cn_lemmata = [];
          cn_datatypes = [];
          cn_decl_specs = [];
        };
      entry_point = m;
      symmap;
      label_cache = Hashtbl.create 0;
    }

let link : Ail_tys.program list -> (Ail_tys.linked_program, string) result =
  let open Ail_tys in
  function
  | [] -> Result.error "linking: no ail files"
  | (entry_point, sigma) :: fs ->
      let symmap =
        Pmap.fold
          (fun _ (d, lk) acc ->
            match lk with
            | IK_definition | IK_tentative -> Pmap.add d d acc
            | IK_declaration -> acc)
          sigma.extern_idmap
          (Pmap.empty Symbol.compare_sym)
      in
      let init_linked =
        { entry_point; sigma; symmap; label_cache = Hashtbl.create 0 }
      in
      List.fold_left
        (fun acc f' ->
          Result.bind acc
          @@
          fun {
                entry_point = entry_point_acc;
                sigma = sigma_acc;
                symmap = symmap_acc;
                label_cache = _;
              }
          -> link_aux (entry_point_acc, sigma_acc) f' symmap_acc)
        (Ok init_linked) fs

let compress_union_find (map : Ail_tys.extern_symmap) : Ail_tys.extern_symmap =
  let rec resolve (map : Ail_tys.extern_symmap) (key : Symbol.sym) =
    match Pmap.lookup key map with
    | None -> key
    | Some k when Symbol.equal_sym k key -> key
    | Some k -> resolve map k
  in
  Pmap.fold
    (fun k _ acc -> Pmap.add k (resolve map k) acc)
    map
    (Pmap.empty Symbol.compare_sym)

let link progs =
  match link progs with
  | Ok linked_prog ->
      Ok { linked_prog with symmap = compress_union_find linked_prog.symmap }
  | Error s -> Error (`LinkError s, Soteria_terminal.Call_trace.empty)
