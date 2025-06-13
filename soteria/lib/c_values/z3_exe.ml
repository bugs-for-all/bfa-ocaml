open Simple_smt
module L = Soteria_logs.Logs.L

let initialize_solver : (Simple_smt.solver -> unit) ref = ref (fun _ -> ())

let register_solver_init f =
  let old = !initialize_solver in
  let f' solver =
    old solver;
    f solver
  in
  initialize_solver := f'

let () =
  register_solver_init (fun solver ->
      match !Solver_config.current.solver_timeout with
      | None -> ()
      | Some timeout ->
          ack_command solver (set_option ":timeout" (string_of_int timeout)))

module Encoding = struct
  open Smt_utils

  let ( $$ ) = app
  let ( $ ) f v = f $$ [ v ]
  let t_seq = atom "Seq"
  let seq_singl t = atom "seq.unit" $$ [ t ]
  let seq_concat ts = atom "seq.++" $$ ts

  let t_ptr, mk_ptr, get_loc, get_ofs =
    let ptr = "Ptr" in
    let mk_ptr = "mk-ptr" in
    let loc = "loc" in
    let ofs = "ofs" in
    let cmd =
      Simple_smt.(
        declare_datatype ptr [] [ (mk_ptr, [ (loc, t_int); (ofs, t_int) ]) ])
    in
    register_solver_init (fun solver -> ack_command solver cmd);
    ( atom ptr,
      (fun l o -> atom mk_ptr $$ [ l; o ]),
      (fun p -> atom loc $$ [ p ]),
      fun p -> atom ofs $$ [ p ] )

  let rec sort_of_ty = function
    | Svalue.TBool -> Simple_smt.t_bool
    | TInt -> Simple_smt.t_int
    | TLoc -> Simple_smt.t_int
    | TFloat F16 -> t_f16
    | TFloat F32 -> t_f32
    | TFloat F64 -> t_f64
    | TFloat F128 -> t_f128
    | TSeq ty -> t_seq $ sort_of_ty ty
    | TPointer -> t_ptr
    | TBitVector n -> t_bits n

  let memo_encode_value_tbl : sexp Hashtbl.Hint.t = Hashtbl.Hint.create 1023

  let rec encode_value (v : Svalue.t) =
    match v.node.kind with
    | Var v -> atom (Svalue.Var.to_string v)
    | Int z -> int_zk z
    | Float f -> (
        match Svalue.precision_of_f v.node.ty with
        | F16 -> f16_k @@ Float.of_string f
        | F32 -> f32_k @@ Float.of_string f
        | F64 -> f64_k @@ Float.of_string f
        | F128 -> f128_k @@ Float.of_string f)
    | Bool b -> bool_k b
    | BitVec z ->
        let n = Svalue.size_of_bv v.node.ty in
        bv_k n z
    | Ptr (l, o) -> mk_ptr (encode_value_memo l) (encode_value_memo o)
    | Seq vs -> (
        match vs with
        | [] -> failwith "need type to encode empty lists"
        | _ :: _ ->
            List.map (fun v -> seq_singl (encode_value_memo v)) vs |> seq_concat
        )
    | Ite (c, t, e) ->
        ite (encode_value_memo c) (encode_value_memo t) (encode_value_memo e)
    | Unop (unop, v1_) -> (
        let v1 = encode_value_memo v1_ in
        match unop with
        | Not -> bool_not v1
        | FAbs -> fp_abs v1
        | GetPtrLoc -> get_loc v1
        | GetPtrOfs -> get_ofs v1
        | IntOfBool -> ite v1 (int_k 1) (int_k 0)
        | BvOfInt ->
            let size = Svalue.size_of_bv v.node.ty in
            bv_of_int size v1
        | IntOfBv signed -> int_of_bv signed v1
        | BvOfFloat n -> bv_of_float n v1
        | FloatOfBv -> (
            match Svalue.precision_of_f v.node.ty with
            | F16 -> f16_of_bv v1
            | F32 -> f32_of_bv v1
            | F64 -> f64_of_bv v1
            | F128 -> f128_of_bv v1)
        | BvExtract (from_, to_) -> bv_extract to_ from_ v1
        | BvExtend by -> bv_zero_extend by v1
        | FIs fc -> fp_is fc v1
        | FRound rm -> fp_round rm v1)
    | Binop (binop, v1, v2) -> (
        let v1 = encode_value_memo v1 in
        let v2 = encode_value_memo v2 in
        match binop with
        | Eq -> eq v1 v2
        | Leq -> num_leq v1 v2
        | Lt -> num_lt v1 v2
        | And -> bool_and v1 v2
        | Or -> bool_or v1 v2
        | Plus -> num_add v1 v2
        | Minus -> num_sub v1 v2
        | Times -> num_mul v1 v2
        | Div -> num_div v1 v2
        | Rem -> num_rem v1 v2
        | Mod -> num_mod v1 v2
        | FEq -> fp_eq v1 v2
        | FLeq -> fp_leq v1 v2
        | FLt -> fp_lt v1 v2
        | FPlus -> fp_add v1 v2
        | FMinus -> fp_sub v1 v2
        | FTimes -> fp_mul v1 v2
        | FDiv -> fp_div v1 v2
        | FRem -> fp_rem v1 v2
        | BitAnd -> bv_and v1 v2
        | BitOr -> bv_or v1 v2
        | BitXor -> bv_xor v1 v2
        | BitShl -> bv_shl v1 v2
        | BitShr -> bv_lshr v1 v2
        | BvPlus -> bv_add v1 v2
        | BvMinus -> bv_sub v1 v2)
    | Nop (Distinct, vs) ->
        let vs = List.map encode_value_memo vs in
        distinct vs

  and encode_value_memo v =
    match Hashtbl.Hint.find_opt memo_encode_value_tbl v.Hashcons.tag with
    | Some k -> k
    | None ->
        let k = encode_value v in
        Hashtbl.Hint.add memo_encode_value_tbl v.Hashcons.tag k;
        k

  let encode_value v = encode_value_memo v
end

let solver_log =
  let debug_str ~prefix s = L.smt (fun m -> m "%s: %s" prefix s) in
  {
    send = debug_str ~prefix:"-> ";
    receive = debug_str ~prefix:"<- ";
    stop = Fun.id;
  }

module Dump = struct
  let current_channel = ref None

  let close_channel () =
    match !current_channel with
    | None -> ()
    | Some (oc, _) ->
        close_out oc;
        current_channel := None

  let () = at_exit close_channel

  let open_channel f =
    let oc = open_out f in
    current_channel := Some (oc, f);
    Some oc

  let channel () =
    (* We only open if current file is not None and its different from current config *)
    match (!Solver_config.current.dump_smt_file, !current_channel) with
    | None, None -> None
    | Some f, None -> open_channel f
    | Some f, Some (oc, f') ->
        if f == f' then Some oc
        else (
          close_channel ();
          open_channel f)
    | None, Some _ ->
        close_channel ();
        None

  let log_sexp sexp =
    match channel () with
    | None -> ()
    | Some oc ->
        Sexplib.Sexp.output_hum oc sexp;
        flush oc

  let log_response response elapsed =
    match channel () with
    | None -> ()
    | Some oc ->
        output_string oc " ; -> ";
        Sexplib.Sexp.output_hum oc response;
        if elapsed > 0 && not !Solver_config.current.hide_response_times then (
          output_string oc " (";
          output_string oc (string_of_int elapsed);
          output_string oc "ms)");
        output_char oc '\n';
        flush oc
end

type t = Simple_smt.solver

let solver_config () =
  { z3 with log = solver_log; exe = !Solver_config.current.z3_path }

let init () =
  let solver = new_solver (solver_config ()) in
  let command sexp =
    Dump.log_sexp sexp;
    let now = Unix.gettimeofday () in
    let res = solver.command sexp in
    let elapsed = Int.of_float ((Unix.gettimeofday () -. now) *. 1000.) in
    Dump.log_response res elapsed;
    res
  in
  let solver = { solver with command } in
  !initialize_solver solver;
  solver

let declare_var solver name ty =
  let name = Soteria_symex.Var.to_string name in
  let ty = Encoding.sort_of_ty ty in
  let sexp = declare name ty in
  ack_command solver sexp

let add_constraint solver v =
  let v = Encoding.encode_value v in
  let sexp = Simple_smt.assume v in
  ack_command solver sexp

let check_sat solver : Soteria_symex.Solver.result =
  let smt_res =
    try check solver
    with Simple_smt.UnexpectedSolverResponse s ->
      L.error (fun m ->
          m "Unexpected solver response: %s" (Sexplib.Sexp.to_string_hum s));
      Unknown
  in
  match smt_res with
  | Sat -> Sat
  | Unsat -> Unsat
  | Unknown ->
      L.info (fun m -> m "Solver returned unknown");
      Unknown

let push solver n = ack_command solver (Simple_smt.push n)
let pop solver n = ack_command solver (Simple_smt.pop n)
