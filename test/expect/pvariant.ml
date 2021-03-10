module V0_tp = struct type tp = [ `A1  | `A2 of int ] end
type tp = V0_tp.tp
type tpn_novers = [ `B1  | `B2 of tp ]
module V0_tpn = struct type tpn = tpn_novers end
module V1_tpn =
  struct
    type tpn = [ `B1  | `B2 of tp ]
    module Prev = V0_tpn
    let upgrade p = match p with | `B1 -> `B1 | `B2 x -> `B2 x
  end
type tpn = V1_tpn.tpn
let tpn_from_novers (x : tpn_novers) = V1_tpn.upgrade x
type tpn1_novers = [ `C1 of int  | `C2 of tp ]
module V0_tpn1 = struct type tpn1 = tpn1_novers end
module V1_tpn1 =
  struct
    type tpn1 = [ `C1 of int  | `C2 of tp ]
    module Prev = V0_tpn1
    let upgrade p = match p with | `C1 x -> `C1 x | `C2 x -> `C2 x
  end
module V2_tpn1 =
  struct
    type tpn1 = [ `C1 of float  | `C2 of tp ]
    module Prev = V1_tpn1
    let upgrade p =
      match p with | `C1 p -> `C1 (float_of_int p) | `C2 x -> `C2 x
  end
type tpn1 = V2_tpn1.tpn1
let tpn1_from_novers (x : tpn1_novers) = V2_tpn1.upgrade (V1_tpn1.upgrade x)
