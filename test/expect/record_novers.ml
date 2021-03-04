type tr_novers = {
  ftr1: int ;
  ftr2: string }[@@novers ]
module V0_tr = struct type tr = {
                        _novers_field: tr_novers }[@@novers ] end
module V1_tr =
  struct
    type tr = {
      ftr1: int ;
      ftr2: string }[@@novers ]
    module Prev = V0_tr
    let upgrade p =
      {
        ftr1 = ((p.Prev._novers_field).ftr1);
        ftr2 = ((p.Prev._novers_field).ftr2)
      }
  end
type tr = V1_tr.tr = {
  ftr1: int ;
  ftr2: string }
let tr_from_novers (x : tr_novers) = V1_tr.upgrade { _novers_field = x }
type trv_novers = {
  f1: int ;
  f2: string }[@@novers ]
module V0_trv = struct type trv = {
                         _novers_field: trv_novers }[@@novers ] end
module V1_trv =
  struct
    type trv = {
      f1: int ;
      f2: string }[@@novers ]
    module Prev = V0_trv
    let upgrade p =
      { f1 = ((p.Prev._novers_field).f1); f2 = ((p.Prev._novers_field).f2) }
  end
module V2_trv =
  struct
    type trv = {
      f1: int ;
      f2: string ;
      f3: float }[@@novers ]
    module Prev = V1_trv
    let upgrade p = { f1 = (p.Prev.f1); f2 = (p.Prev.f2); f3 = 0.0 }
  end
module V3_trv =
  struct
    type trv = {
      f1: int ;
      f2: int ;
      f3: float }[@@novers ]
    module Prev = V2_trv
    let upgrade p =
      { f1 = (p.Prev.f1); f2 = (int_of_string p.Prev.f2); f3 = (p.Prev.f3) }
  end
type trv = V3_trv.trv = {
  f1: int ;
  f2: int ;
  f3: float }
let trv_from_novers (x : trv_novers) =
  V3_trv.upgrade (V2_trv.upgrade (V1_trv.upgrade { _novers_field = x }))
type fake = {
  fake: char }
type trv_v_novers = {
  fv1: tr_novers ;
  fv2: string ;
  fv3: trv_novers list }[@@novers ]
module V0_trv_v =
  struct type trv_v = {
           _novers_field: trv_v_novers }[@@novers ] end
module V1_trv_v =
  struct
    type trv_v = {
      fv1: tr ;
      fv2: string ;
      fv3: trv list }[@@novers ]
    module Prev = V0_trv_v
    let upgrade p =
      {
        fv1 = (tr_from_novers (p.Prev._novers_field).fv1);
        fv2 = ((p.Prev._novers_field).fv2);
        fv3 = (List.map trv_from_novers (p.Prev._novers_field).fv3)
      }
  end
module V2_trv_v =
  struct
    type trv_v = {
      fv1: tr ;
      fv2: string list ;
      fv3: trv list }[@@novers ]
    module Prev = V1_trv_v
    let upgrade p =
      { fv1 = (p.Prev.fv1); fv2 = [p.Prev.fv2]; fv3 = (p.Prev.fv3) }
  end
type trv_v = V2_trv_v.trv_v = {
  fv1: tr ;
  fv2: string list ;
  fv3: trv list }
let trv_v_from_novers (x : trv_v_novers) =
  V2_trv_v.upgrade (V1_trv_v.upgrade { _novers_field = x })
