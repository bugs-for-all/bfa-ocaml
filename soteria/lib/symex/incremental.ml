module type Immutable = sig
  type init_data
  type t

  (** Initialisation can be side-effectful *)
  val init : init_data -> t

  val backtrack_n : t -> int -> t
  val save : t -> t
  val reset : t -> t
end

module type Mutable = sig
  type init_data
  type t

  val init : init_data -> t
  val backtrack_n : t -> int -> unit
  val save : t -> unit
  val reset : t -> unit
end

module Make_mutable (M : sig
  type t

  val default : t
end) : sig
  include Mutable with type t = M.t Dynarray.t with type init_data = unit

  val wrap : (M.t -> 'a * M.t) -> t -> 'a
  val wrap_read : (M.t -> 'a) -> t -> 'a
end = struct
  type init_data = unit
  type t = M.t Dynarray.t

  let init () =
    let d = Dynarray.create () in
    Dynarray.add_last d M.default;
    d

  let backtrack_n d n =
    let len = Dynarray.length d in
    Dynarray.truncate d (len - n)

  let save d = Dynarray.add_last d (Dynarray.get_last d)

  let reset d =
    Dynarray.clear d;
    Dynarray.add_last d M.default

  let wrap (f : M.t -> 'a * M.t) d =
    let e = Dynarray.pop_last d in
    let a, e' = f e in
    Dynarray.add_last d e';
    a

  let wrap_read (f : M.t -> 'a) d =
    let e = Dynarray.get_last d in
    f e
end

module type In_place = sig
  val backtrack_n : int -> unit
  val save : unit -> unit
  val reset : unit -> unit
end

module Mutable_to_in_place (M : Mutable with type init_data = unit) = struct
  let state = lazy (M.init ())
  let save () = M.save (Lazy.force state)
  let backtrack_n n = M.backtrack_n (Lazy.force state) n
  let reset () = M.reset (Lazy.force state)
  let wrap (f : M.t -> 'a) () : 'a = f (Lazy.force state)
end

module Make_in_place (M : sig
  type t

  val default : t
end) =
struct
  module Mutable = Make_mutable (M)
  include Mutable_to_in_place (Mutable)

  let wrap_read f () = wrap (Mutable.wrap_read f) ()
  let wrap f () = wrap (Mutable.wrap f) ()
end
