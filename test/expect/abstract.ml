module V0_t = struct type t = int end
type t = V0_t.t
type ti_novers = int
module V0_ti = struct type ti = ti_novers end
module V1_ti = struct type ti = int
                      module Prev = V0_ti
                      let upgrade p = p end
type ti = V1_ti.ti
let ti_from_novers (x : ti_novers) = V1_ti.upgrade x
type tn_novers = int
module V0_tn = struct type tn = tn_novers end
module V1_tn = struct type tn = int
                      module Prev = V0_tn
                      let upgrade p = p end
module V2_tn =
  struct type tn = float
         module Prev = V1_tn
         let upgrade p = float_of_int p end
type tn = V2_tn.tn
let tn_from_novers (x : tn_novers) = V2_tn.upgrade (V1_tn.upgrade x)
