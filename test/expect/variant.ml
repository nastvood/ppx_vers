module V0_ta = struct type ta =
                        | A1 
                        | A2  end
type ta = V0_ta.ta =
  | A1 
  | A2 
type tn_novers =
  | B1 
  | B2 of ta 
module V0_tn = struct type tn = tn_novers end
module V1_tn =
  struct
    type tn =
      | B1 
      | B2 of ta 
    module Prev = V0_tn
    let upgrade p =
      match p with | (B1 : Prev.tn) -> B1 | (B2 x : Prev.tn) -> B2 x
  end
type tn = V1_tn.tn =
  | B1 
  | B2 of ta 
let tn_from_novers (x : tn_novers) = V1_tn.upgrade x
type tn1_novers =
  | C1 
  | C2 of tn_novers 
module V0_tn1 = struct type tn1 = tn1_novers end
module V1_tn1 =
  struct
    type tn1 =
      | C1 
      | C2 of tn 
    module Prev = V0_tn1
    let upgrade p =
      match p with
      | (C1 : Prev.tn1) -> C1
      | (C2 x : Prev.tn1) -> C2 (tn_from_novers x)
  end
module V2_tn1 =
  struct
    type tn1 =
      | C1 of int 
      | C2 of tn 
    module Prev = V1_tn1
    let upgrade p = match p with | Prev.C1 -> C1 0 | Prev.C2 x -> C2 x
  end
type tn1 = V2_tn1.tn1 =
  | C1 of int 
  | C2 of tn 
let tn1_from_novers (x : tn1_novers) = V2_tn1.upgrade (V1_tn1.upgrade x)
