(** {2 Phantom types} *)

module T : sig
  type sint = [ `NonZero | `MaybeZero ]
  type nonzero = [ `NonZero ]
  type sbool = [ `Bool ]
  type sptr = [ `Ptr ]
  type sloc = [ `Loc ]
  type 'a sseq = [ `List of 'a ]
  type cval = [ sint | sptr ]
  type any = [ `Bool | `Ptr | `Loc | `List of any | `NonZero | `MaybeZero ]

  val pp_sint : sint Fmt.t
  val pp_nonzero : nonzero Fmt.t
  val pp_sbool : sbool Fmt.t
  val pp_sptr : sptr Fmt.t
  val pp_sloc : sloc Fmt.t
  val pp_cval : cval Fmt.t
  val pp_sseq : 'a Fmt.t -> 'a sseq Fmt.t
  val pp_any : any Fmt.t
end

open T

(** {2 Types} *)
type +'a ty

val pp_ty : 'a ty Fmt.t -> 'a ty Fmt.t
val ppa_ty : 'a ty Fmt.t
val t_bool : [> sbool ] ty
val t_int : [> sint ] ty
val t_ptr : [> sptr ] ty
val t_loc : [> sloc ] ty
val t_seq : ([< any ] as 'a) ty -> [> 'a sseq ] ty

(** {2 Typed svalues} *)

type +'a t
type sbool = T.sbool

(** Basic value operations *)

val get_ty : 'a t -> Svalue.ty
val untype_type : 'a ty -> Svalue.ty
val kind : 'a t -> Svalue.t_kind
val mk_var : Svalue.Var.t -> 'a ty -> 'a t
val iter_vars : 'a t -> (Svalue.Var.t * 'b ty -> unit) -> unit
val subst : (Svalue.Var.t -> Svalue.Var.t) -> 'a t -> 'a t
val type_ : Svalue.t -> 'a t
val type_checked : Svalue.t -> 'a ty -> 'a t option
val cast : 'a t -> 'b t
val cast_checked : 'a t -> 'b ty -> 'b t option
val untyped : 'a t -> Svalue.t
val untyped_list : 'a t list -> Svalue.t list
val pp : 'a Fmt.t -> 'a t Fmt.t
val ppa : 'a t Fmt.t
val equal : ([< any ] as 'a) t -> 'a t -> bool
val compare : ([< any ] as 'a) t -> 'a t -> int

(** Typed constructors *)

val sem_eq : 'a t -> 'a t -> sbool t
val sem_eq_untyped : 'a t -> 'a t -> [> sbool ] t
val v_true : [> sbool ] t
val v_false : [> sbool ] t
val bool : bool -> [> sbool ] t
val as_bool : 'a t -> bool option
val and_ : [< sbool ] t -> [< sbool ] t -> [> sbool ] t
val conj : [< sbool ] t list -> [> sbool ] t
val split_ands : [< sbool ] t -> ([> sbool ] t -> unit) -> unit
val or_ : [< sbool ] t -> [< sbool ] t -> [> sbool ] t
val not : sbool t -> sbool t
val not_int_bool : [< sint ] t -> [> sint ] t
val distinct : 'a t list -> [> sbool ] t
val int_z : Z.t -> [> sint ] t
val int : int -> [> sint ] t
val nonzero_z : Z.t -> [> nonzero ] t
val nonzero : int -> [> nonzero ] t
val int_of_bool : [< sbool ] t -> [> sint ] t
val bool_of_int : [< sint ] t -> [> sbool ] t
val zero : [> sint ] t
val one : [> nonzero ] t
val geq : [< sint ] t -> [< sint ] t -> [> sbool ] t
val gt : [< sint ] t -> [< sint ] t -> [> sbool ] t
val leq : [< sint ] t -> [< sint ] t -> [> sbool ] t
val lt : [< sint ] t -> [< sint ] t -> [> sbool ] t
val plus : [< sint ] t -> [< sint ] t -> [> sint ] t
val minus : [< sint ] t -> [< sint ] t -> [> sint ] t
val times : [< sint ] t -> [< sint ] t -> [> sint ] t
val div : [< sint ] t -> nonzero t -> [> sint ] t
val rem : [< sint ] t -> nonzero t -> [> sint ] t

module Ptr : sig
  val mk : [< sloc ] t -> [< sint ] t -> [> sptr ] t
  val loc : [< sptr ] t -> [> sloc ] t
  val ofs : [< sptr ] t -> [> sint ] t
  val decompose : [< sptr ] t -> [> sloc ] t * [> sint ] t
  val add_ofs : [< sptr ] t -> [< sint ] t -> [> sptr ] t
  val loc_of_int : int -> [> sloc ] t
  val null : [> sptr ] t
  val null_loc : [> sloc ] t
  val is_null : [< sptr ] t -> [> sbool ] t
  val is_at_null_loc : [< sptr ] t -> [> sbool ] t
end

module SSeq : sig
  val mk : inner_ty:([< any ] as 'a) ty -> 'a t list -> [> 'a sseq ] t
end

module Infix : sig
  val ( ==@ ) : ([< any ] as 'a) t -> 'a t -> [> sbool ] t
  val ( ==?@ ) : 'a t -> 'a t -> [> sbool ] t
  val ( >@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( >=@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( <@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( <=@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( &&@ ) : [< sbool ] t -> [< sbool ] t -> [< sbool ] t
  val ( ||@ ) : [< sbool ] t -> [< sbool ] t -> [< sbool ] t
  val ( +@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( -@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( ~- ) : [< sint ] t -> [> sint ] t
  val ( *@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( /@ ) : [< sint ] t -> [< nonzero ] t -> [> sint ] t
  val ( %@ ) : [< sint ] t -> [< nonzero ] t -> [> sint ] t
end

module Syntax : sig
  module Sym_int_syntax : sig
    val mk_int : int -> [> sint ] t
    val zero : [> sint ] t
    val one : [> sint ] t
  end
end
