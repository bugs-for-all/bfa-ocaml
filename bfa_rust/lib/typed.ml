open Hashcons
include Svalue

module T = struct
  type sint = [ `NonZero | `MaybeZero ]
  type nonzero = [ `NonZero ]
  type sbool = [ `Bool ]
  type sptr = [ `Ptr ]
  type sloc = [ `Loc ]
  type 'a sseq = [ `List of 'a ]
  type cval = [ sint | sptr ]
  type any = [ `Bool | `Ptr | `Loc | `List of any | `NonZero | `MaybeZero ]

  let pp_sint _ _ = ()
  let pp_nonzero _ _ = ()
  let pp_sbool _ _ = ()
  let pp_sptr _ _ = ()
  let pp_sloc _ _ = ()
  let pp_sseq _ _ _ = ()
  let pp_any _ _ = ()
  let pp_cval _ _ = ()
end

type nonrec +'a t = t
type nonrec +'a ty = ty
type sbool = T.sbool

let[@inline] get_ty x = x.node.ty
let[@inline] untype_type x = x
let ppa = pp
let pp _ = pp
let ppa_ty = pp_ty
let pp_ty _ = pp_ty
let[@inline] cast x = x
let[@inline] untyped x = x
let[@inline] untyped_list l = l
let[@inline] type_ x = x
let type_checked x ty = if equal_ty x.node.ty ty then Some x else None
let cast_checked = type_checked

let nonzero_z z =
  if Z.equal Z.zero z then raise (Invalid_argument "nonzero_z")
  else Svalue.int_z z

let nonzero x =
  if x = 0 then raise (Invalid_argument "nonzero_z") else Svalue.int x
