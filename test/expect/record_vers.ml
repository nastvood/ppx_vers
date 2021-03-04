module V0_tr = struct type tr = {
                        f1: int ;
                        f2: string } end
type tr = V0_tr.tr = {
  f1: int ;
  f2: string }
module V0_trv = struct type trv = {
                         f1: int ;
                         f2: string } end
module V1_trv =
  struct
    type trv = {
      f1: int ;
      f2: string ;
      f3: float }
    module Prev = V0_trv
    let upgrade p = { f1 = (p.Prev.f1); f2 = (p.Prev.f2); f3 = 0.0 }
  end
module V2_trv =
  struct
    type trv = {
      f1: int ;
      f2: int ;
      f3: float }
    module Prev = V1_trv
    let upgrade p =
      { f1 = (p.Prev.f1); f2 = (int_of_string p.Prev.f2); f3 = (p.Prev.f3) }
  end
type trv = V2_trv.trv = {
  f1: int ;
  f2: int ;
  f3: float }
module V0_trv_v = struct type trv_v = {
                           fv1: int ;
                           fv2: string } end
module V1_trv_v =
  struct
    type trv_v = {
      fv1: int ;
      fv2: string ;
      fv3: trv }
    module Prev = V0_trv_v
    let upgrade p =
      {
        fv1 = (p.Prev.fv1);
        fv2 = (p.Prev.fv2);
        fv3 = { f1 = 0; f2 = 0; f3 = 0.0 }
      }
  end
type trv_v = V1_trv_v.trv_v = {
  fv1: int ;
  fv2: string ;
  fv3: trv }
