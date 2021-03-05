module Module1 =
  struct
    type t_novers = {
      f1: int ;
      f2: string }[@@novers ]
    module V0_t = struct type t = {
                           _novers_field: t_novers }[@@novers ] end
    module V1_t =
      struct
        type t = {
          f1: int ;
          f2: string }[@@novers ]
        module Prev = V0_t
        let upgrade p =
          {
            f1 = ((p.Prev._novers_field).f1);
            f2 = ((p.Prev._novers_field).f2)
          }
      end
    module V2_t =
      struct
        type t = {
          f1: int ;
          f2: string ;
          f4: float }[@@novers ]
        module Prev = V1_t
        let upgrade p = { f1 = (p.Prev.f1); f2 = (p.Prev.f2); f4 = 0.0 }
      end
    type t = V2_t.t = {
      f1: int ;
      f2: string ;
      f4: float }
    let t_from_novers (x : t_novers) =
      V2_t.upgrade (V1_t.upgrade { _novers_field = x })
  end
module ModuleFake = struct type t = int end
module Module2 =
  struct
    type t_novers =
      | A1 
      | A2 of Module1.t_novers [@@novers ]
    module V0_t = struct type t = t_novers[@@novers ] end
    module V1_t =
      struct
        type t =
          | A1 
          | A2 of Module1.t [@@novers ]
        module Prev = V0_t
        let upgrade p =
          match p with
          | (A1 : Prev.t) -> A1
          | (A2 x : Prev.t) -> A2 (Module1.t_from_novers x)
      end
    type t = V1_t.t =
      | A1 
      | A2 of Module1.t 
    let t_from_novers (x : t_novers) = V1_t.upgrade x
  end
