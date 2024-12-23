module type S = sig
  type t
  type ty

  val not : t -> t
  val sem_eq : t -> t -> t
  val pp : t Fmt.t
  val iter_vars : t -> ty Var.iter_vars
  val subst : (Var.t -> Var.t) -> t -> t
  val mk_var : Var.t -> ty -> t
  val as_bool : t -> bool option
end
