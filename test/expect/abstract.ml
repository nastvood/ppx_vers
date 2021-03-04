module V0_t = struct type t = int end
type t = V0_t.t
type tn_novers = int[@@novers ]
module V0_tn = struct type tn = tn_novers[@@novers ] end
module V1_tn =
  struct
    type tn = int[@@migrate p][@@novers ]
    module Prev = V0_tn
    let upgrade p = p
  end
type tn = V1_tn.tn
let tn_from_novers (x : tn_novers) = V1_tn.upgrade x
