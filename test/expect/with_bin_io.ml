open Bin_prot.Std
type trv_novers = {
  f1: int ;
  f2: string }[@@deriving (bin_write, bin_read)]
include
  struct
    let _ = fun (_ : trv_novers) -> ()
    let (bin_size_trv_novers : trv_novers Bin_prot.Size.sizer) =
      function
      | { f1 = v1; f2 = v2 } ->
          let size = 0 in
          let size = Bin_prot.Common.(+) size (bin_size_int v1) in
          Bin_prot.Common.(+) size (bin_size_string v2)
    let _ = bin_size_trv_novers
    let (bin_write_trv_novers : trv_novers Bin_prot.Write.writer) =
      fun buf ->
        fun ~pos ->
          function
          | { f1 = v1; f2 = v2 } ->
              let pos = bin_write_int buf ~pos v1 in
              bin_write_string buf ~pos v2
    let _ = bin_write_trv_novers
    let bin_writer_trv_novers =
      {
        Bin_prot.Type_class.size = bin_size_trv_novers;
        write = bin_write_trv_novers
      }
    let _ = bin_writer_trv_novers
    let (__bin_read_trv_novers__ : (int -> trv_novers) Bin_prot.Read.reader)
      =
      fun _buf ->
        fun ~pos_ref ->
          fun _vint ->
            Bin_prot.Common.raise_variant_wrong_type
              "test/with_bin_io.ml.trv_novers" (!pos_ref)
    let _ = __bin_read_trv_novers__
    let (bin_read_trv_novers : trv_novers Bin_prot.Read.reader) =
      fun buf ->
        fun ~pos_ref ->
          let v_f1 = bin_read_int buf ~pos_ref in
          let v_f2 = bin_read_string buf ~pos_ref in { f1 = v_f1; f2 = v_f2 }
    let _ = bin_read_trv_novers
    let bin_reader_trv_novers =
      {
        Bin_prot.Type_class.read = bin_read_trv_novers;
        vtag_read = __bin_read_trv_novers__
      }
    let _ = bin_reader_trv_novers
  end
module V0_trv =
  struct
    type trv = {
      _novers_field: trv_novers }[@@deriving (bin_write, bin_read)]
    include
      struct
        let _ = fun (_ : trv) -> ()
        let (bin_size_trv : trv Bin_prot.Size.sizer) =
          function
          | { _novers_field = v1 } ->
              let size = 0 in
              Bin_prot.Common.(+) size (bin_size_trv_novers v1)
        let _ = bin_size_trv
        let (bin_write_trv : trv Bin_prot.Write.writer) =
          fun buf ->
            fun ~pos ->
              function
              | { _novers_field = v1 } -> bin_write_trv_novers buf ~pos v1
        let _ = bin_write_trv
        let bin_writer_trv =
          { Bin_prot.Type_class.size = bin_size_trv; write = bin_write_trv }
        let _ = bin_writer_trv
        let (__bin_read_trv__ : (int -> trv) Bin_prot.Read.reader) =
          fun _buf ->
            fun ~pos_ref ->
              fun _vint ->
                Bin_prot.Common.raise_variant_wrong_type
                  "test/with_bin_io.ml.V0_trv.trv" (!pos_ref)
        let _ = __bin_read_trv__
        let (bin_read_trv : trv Bin_prot.Read.reader) =
          fun buf ->
            fun ~pos_ref ->
              let v__novers_field = bin_read_trv_novers buf ~pos_ref in
              { _novers_field = v__novers_field }
        let _ = bin_read_trv
        let bin_reader_trv =
          {
            Bin_prot.Type_class.read = bin_read_trv;
            vtag_read = __bin_read_trv__
          }
        let _ = bin_reader_trv
      end
  end
module V1_trv =
  struct
    type trv = {
      f1: int ;
      f2: string }[@@deriving (bin_write, bin_read)]
    module Prev = V0_trv
    let upgrade p =
      { f1 = ((p.Prev._novers_field).f1); f2 = ((p.Prev._novers_field).f2) }
    include
      struct
        let _ = fun (_ : trv) -> ()
        let (bin_size_trv : trv Bin_prot.Size.sizer) =
          function
          | { f1 = v1; f2 = v2 } ->
              let size = 0 in
              let size = Bin_prot.Common.(+) size (bin_size_int v1) in
              Bin_prot.Common.(+) size (bin_size_string v2)
        let _ = bin_size_trv
        let (bin_write_trv : trv Bin_prot.Write.writer) =
          fun buf ->
            fun ~pos ->
              function
              | { f1 = v1; f2 = v2 } ->
                  let pos = bin_write_int buf ~pos v1 in
                  bin_write_string buf ~pos v2
        let _ = bin_write_trv
        let bin_writer_trv =
          { Bin_prot.Type_class.size = bin_size_trv; write = bin_write_trv }
        let _ = bin_writer_trv
        let (__bin_read_trv__ : (int -> trv) Bin_prot.Read.reader) =
          fun _buf ->
            fun ~pos_ref ->
              fun _vint ->
                Bin_prot.Common.raise_variant_wrong_type
                  "test/with_bin_io.ml.V1_trv.trv" (!pos_ref)
        let _ = __bin_read_trv__
        let (bin_read_trv : trv Bin_prot.Read.reader) =
          fun buf ->
            fun ~pos_ref ->
              let v_f1 = bin_read_int buf ~pos_ref in
              let v_f2 = bin_read_string buf ~pos_ref in
              { f1 = v_f1; f2 = v_f2 }
        let _ = bin_read_trv
        let bin_reader_trv =
          {
            Bin_prot.Type_class.read = bin_read_trv;
            vtag_read = __bin_read_trv__
          }
        let _ = bin_reader_trv
      end
  end
module V2_trv =
  struct
    type trv = {
      f1: int ;
      f2: string ;
      f3: float }[@@deriving (bin_write, bin_read)]
    module Prev = V1_trv
    let upgrade p = { f1 = (p.Prev.f1); f2 = (p.Prev.f2); f3 = 0.0 }
    include
      struct
        let _ = fun (_ : trv) -> ()
        let (bin_size_trv : trv Bin_prot.Size.sizer) =
          function
          | { f1 = v1; f2 = v2; f3 = v3 } ->
              let size = 0 in
              let size = Bin_prot.Common.(+) size (bin_size_int v1) in
              let size = Bin_prot.Common.(+) size (bin_size_string v2) in
              Bin_prot.Common.(+) size (bin_size_float v3)
        let _ = bin_size_trv
        let (bin_write_trv : trv Bin_prot.Write.writer) =
          fun buf ->
            fun ~pos ->
              function
              | { f1 = v1; f2 = v2; f3 = v3 } ->
                  let pos = bin_write_int buf ~pos v1 in
                  let pos = bin_write_string buf ~pos v2 in
                  bin_write_float buf ~pos v3
        let _ = bin_write_trv
        let bin_writer_trv =
          { Bin_prot.Type_class.size = bin_size_trv; write = bin_write_trv }
        let _ = bin_writer_trv
        let (__bin_read_trv__ : (int -> trv) Bin_prot.Read.reader) =
          fun _buf ->
            fun ~pos_ref ->
              fun _vint ->
                Bin_prot.Common.raise_variant_wrong_type
                  "test/with_bin_io.ml.V2_trv.trv" (!pos_ref)
        let _ = __bin_read_trv__
        let (bin_read_trv : trv Bin_prot.Read.reader) =
          fun buf ->
            fun ~pos_ref ->
              let v_f1 = bin_read_int buf ~pos_ref in
              let v_f2 = bin_read_string buf ~pos_ref in
              let v_f3 = bin_read_float buf ~pos_ref in
              { f1 = v_f1; f2 = v_f2; f3 = v_f3 }
        let _ = bin_read_trv
        let bin_reader_trv =
          {
            Bin_prot.Type_class.read = bin_read_trv;
            vtag_read = __bin_read_trv__
          }
        let _ = bin_reader_trv
      end
  end
module V3_trv =
  struct
    type trv = {
      f1: int ;
      f2: int ;
      f3: float }[@@deriving (bin_write, bin_read)]
    module Prev = V2_trv
    let upgrade p =
      { f1 = (p.Prev.f1); f2 = (int_of_string p.Prev.f2); f3 = (p.Prev.f3) }
    include
      struct
        let _ = fun (_ : trv) -> ()
        let (bin_size_trv : trv Bin_prot.Size.sizer) =
          function
          | { f1 = v1; f2 = v2; f3 = v3 } ->
              let size = 0 in
              let size = Bin_prot.Common.(+) size (bin_size_int v1) in
              let size = Bin_prot.Common.(+) size (bin_size_int v2) in
              Bin_prot.Common.(+) size (bin_size_float v3)
        let bin_size_trv t = (bin_size_trv t) + 1
        let _ = bin_size_trv
        let (bin_write_trv : trv Bin_prot.Write.writer) =
          fun buf ->
            fun ~pos ->
              function
              | { f1 = v1; f2 = v2; f3 = v3 } ->
                  let pos = bin_write_int buf ~pos v1 in
                  let pos = bin_write_int buf ~pos v2 in
                  bin_write_float buf ~pos v3
        let _ = bin_write_trv
        let bin_write_trv buf ~pos  v =
          let pos = Bin_prot.Write.bin_write_int_8bit buf ~pos 3 in
          bin_write_trv buf ~pos v
        let bin_writer_trv =
          { Bin_prot.Type_class.size = bin_size_trv; write = bin_write_trv }
        let _ = bin_writer_trv
        let (__bin_read_trv__ : (int -> trv) Bin_prot.Read.reader) =
          fun _buf ->
            fun ~pos_ref ->
              fun _vint ->
                Bin_prot.Common.raise_variant_wrong_type
                  "test/with_bin_io.ml.V3_trv.trv" (!pos_ref)
        let _ = __bin_read_trv__
        let (bin_read_trv : trv Bin_prot.Read.reader) =
          fun buf ->
            fun ~pos_ref ->
              let v_f1 = bin_read_int buf ~pos_ref in
              let v_f2 = bin_read_int buf ~pos_ref in
              let v_f3 = bin_read_float buf ~pos_ref in
              { f1 = v_f1; f2 = v_f2; f3 = v_f3 }
        let _ = bin_read_trv
        let bin_read_trv buf ~pos_ref  =
          match Bin_prot.Read.bin_read_int_8bit buf ~pos_ref with
          | 3 -> bin_read_trv buf ~pos_ref
          | 2 -> upgrade (V2_trv.bin_read_trv buf ~pos_ref)
          | 1 -> upgrade (V2_trv.upgrade (V1_trv.bin_read_trv buf ~pos_ref))
          | 0 ->
              upgrade
                (V2_trv.upgrade
                   (V1_trv.upgrade (V0_trv.bin_read_trv buf ~pos_ref)))
          | v ->
              failwith
                (Printf.sprintf "Unknown VERS %s.%s:%d" "test/with_bin_io.ml"
                   "trv" v)
        let bin_reader_trv =
          {
            Bin_prot.Type_class.read = bin_read_trv;
            vtag_read = __bin_read_trv__
          }
        let _ = bin_reader_trv
      end
  end
let bin_reader_trv = V3_trv.bin_reader_trv
let bin_read_trv = V3_trv.bin_read_trv
let bin_writer_trv = V3_trv.bin_writer_trv
let bin_write_trv = V3_trv.bin_write_trv
let bin_size_trv = V3_trv.bin_size_trv
type trv = V3_trv.trv = {
  f1: int ;
  f2: int ;
  f3: float }
let trv_from_novers (x : trv_novers) =
  V3_trv.upgrade (V2_trv.upgrade (V1_trv.upgrade { _novers_field = x }))
type fake = {
  fake: char }
type trv_v_novers = {
  fv1: int ;
  fv2: string ;
  fv3: trv_novers list }[@@deriving (bin_write, bin_read)]
include
  struct
    let _ = fun (_ : trv_v_novers) -> ()
    let (bin_size_trv_v_novers : trv_v_novers Bin_prot.Size.sizer) =
      function
      | { fv1 = v1; fv2 = v2; fv3 = v3 } ->
          let size = 0 in
          let size = Bin_prot.Common.(+) size (bin_size_int v1) in
          let size = Bin_prot.Common.(+) size (bin_size_string v2) in
          Bin_prot.Common.(+) size (bin_size_list bin_size_trv_novers v3)
    let _ = bin_size_trv_v_novers
    let (bin_write_trv_v_novers : trv_v_novers Bin_prot.Write.writer) =
      fun buf ->
        fun ~pos ->
          function
          | { fv1 = v1; fv2 = v2; fv3 = v3 } ->
              let pos = bin_write_int buf ~pos v1 in
              let pos = bin_write_string buf ~pos v2 in
              (bin_write_list bin_write_trv_novers) buf ~pos v3
    let _ = bin_write_trv_v_novers
    let bin_writer_trv_v_novers =
      {
        Bin_prot.Type_class.size = bin_size_trv_v_novers;
        write = bin_write_trv_v_novers
      }
    let _ = bin_writer_trv_v_novers
    let (__bin_read_trv_v_novers__ :
      (int -> trv_v_novers) Bin_prot.Read.reader) =
      fun _buf ->
        fun ~pos_ref ->
          fun _vint ->
            Bin_prot.Common.raise_variant_wrong_type
              "test/with_bin_io.ml.trv_v_novers" (!pos_ref)
    let _ = __bin_read_trv_v_novers__
    let (bin_read_trv_v_novers : trv_v_novers Bin_prot.Read.reader) =
      fun buf ->
        fun ~pos_ref ->
          let v_fv1 = bin_read_int buf ~pos_ref in
          let v_fv2 = bin_read_string buf ~pos_ref in
          let v_fv3 = (bin_read_list bin_read_trv_novers) buf ~pos_ref in
          { fv1 = v_fv1; fv2 = v_fv2; fv3 = v_fv3 }
    let _ = bin_read_trv_v_novers
    let bin_reader_trv_v_novers =
      {
        Bin_prot.Type_class.read = bin_read_trv_v_novers;
        vtag_read = __bin_read_trv_v_novers__
      }
    let _ = bin_reader_trv_v_novers
  end
module V0_trv_v =
  struct
    type trv_v = {
      _novers_field: trv_v_novers }[@@deriving (bin_write, bin_read)]
    include
      struct
        let _ = fun (_ : trv_v) -> ()
        let (bin_size_trv_v : trv_v Bin_prot.Size.sizer) =
          function
          | { _novers_field = v1 } ->
              let size = 0 in
              Bin_prot.Common.(+) size (bin_size_trv_v_novers v1)
        let _ = bin_size_trv_v
        let (bin_write_trv_v : trv_v Bin_prot.Write.writer) =
          fun buf ->
            fun ~pos ->
              function
              | { _novers_field = v1 } -> bin_write_trv_v_novers buf ~pos v1
        let _ = bin_write_trv_v
        let bin_writer_trv_v =
          {
            Bin_prot.Type_class.size = bin_size_trv_v;
            write = bin_write_trv_v
          }
        let _ = bin_writer_trv_v
        let (__bin_read_trv_v__ : (int -> trv_v) Bin_prot.Read.reader) =
          fun _buf ->
            fun ~pos_ref ->
              fun _vint ->
                Bin_prot.Common.raise_variant_wrong_type
                  "test/with_bin_io.ml.V0_trv_v.trv_v" (!pos_ref)
        let _ = __bin_read_trv_v__
        let (bin_read_trv_v : trv_v Bin_prot.Read.reader) =
          fun buf ->
            fun ~pos_ref ->
              let v__novers_field = bin_read_trv_v_novers buf ~pos_ref in
              { _novers_field = v__novers_field }
        let _ = bin_read_trv_v
        let bin_reader_trv_v =
          {
            Bin_prot.Type_class.read = bin_read_trv_v;
            vtag_read = __bin_read_trv_v__
          }
        let _ = bin_reader_trv_v
      end
  end
module V1_trv_v =
  struct
    type trv_v = {
      fv1: int ;
      fv2: string ;
      fv3: trv list }[@@deriving (bin_write, bin_read)]
    module Prev = V0_trv_v
    let upgrade p =
      {
        fv1 = ((p.Prev._novers_field).fv1);
        fv2 = ((p.Prev._novers_field).fv2);
        fv3 = (List.map trv_from_novers (p.Prev._novers_field).fv3)
      }
    include
      struct
        let _ = fun (_ : trv_v) -> ()
        let (bin_size_trv_v : trv_v Bin_prot.Size.sizer) =
          function
          | { fv1 = v1; fv2 = v2; fv3 = v3 } ->
              let size = 0 in
              let size = Bin_prot.Common.(+) size (bin_size_int v1) in
              let size = Bin_prot.Common.(+) size (bin_size_string v2) in
              Bin_prot.Common.(+) size (bin_size_list bin_size_trv v3)
        let _ = bin_size_trv_v
        let (bin_write_trv_v : trv_v Bin_prot.Write.writer) =
          fun buf ->
            fun ~pos ->
              function
              | { fv1 = v1; fv2 = v2; fv3 = v3 } ->
                  let pos = bin_write_int buf ~pos v1 in
                  let pos = bin_write_string buf ~pos v2 in
                  (bin_write_list bin_write_trv) buf ~pos v3
        let _ = bin_write_trv_v
        let bin_writer_trv_v =
          {
            Bin_prot.Type_class.size = bin_size_trv_v;
            write = bin_write_trv_v
          }
        let _ = bin_writer_trv_v
        let (__bin_read_trv_v__ : (int -> trv_v) Bin_prot.Read.reader) =
          fun _buf ->
            fun ~pos_ref ->
              fun _vint ->
                Bin_prot.Common.raise_variant_wrong_type
                  "test/with_bin_io.ml.V1_trv_v.trv_v" (!pos_ref)
        let _ = __bin_read_trv_v__
        let (bin_read_trv_v : trv_v Bin_prot.Read.reader) =
          fun buf ->
            fun ~pos_ref ->
              let v_fv1 = bin_read_int buf ~pos_ref in
              let v_fv2 = bin_read_string buf ~pos_ref in
              let v_fv3 = (bin_read_list bin_read_trv) buf ~pos_ref in
              { fv1 = v_fv1; fv2 = v_fv2; fv3 = v_fv3 }
        let _ = bin_read_trv_v
        let bin_reader_trv_v =
          {
            Bin_prot.Type_class.read = bin_read_trv_v;
            vtag_read = __bin_read_trv_v__
          }
        let _ = bin_reader_trv_v
      end
  end
module V2_trv_v =
  struct
    type trv_v = {
      fv1: int ;
      fv2: string list ;
      fv3: trv list }[@@deriving (bin_write, bin_read)]
    module Prev = V1_trv_v
    let upgrade p =
      { fv1 = (p.Prev.fv1); fv2 = [p.Prev.fv2]; fv3 = (p.Prev.fv3) }
    include
      struct
        let _ = fun (_ : trv_v) -> ()
        let (bin_size_trv_v : trv_v Bin_prot.Size.sizer) =
          function
          | { fv1 = v1; fv2 = v2; fv3 = v3 } ->
              let size = 0 in
              let size = Bin_prot.Common.(+) size (bin_size_int v1) in
              let size =
                Bin_prot.Common.(+) size (bin_size_list bin_size_string v2) in
              Bin_prot.Common.(+) size (bin_size_list bin_size_trv v3)
        let bin_size_trv_v t = (bin_size_trv_v t) + 1
        let _ = bin_size_trv_v
        let (bin_write_trv_v : trv_v Bin_prot.Write.writer) =
          fun buf ->
            fun ~pos ->
              function
              | { fv1 = v1; fv2 = v2; fv3 = v3 } ->
                  let pos = bin_write_int buf ~pos v1 in
                  let pos = (bin_write_list bin_write_string) buf ~pos v2 in
                  (bin_write_list bin_write_trv) buf ~pos v3
        let _ = bin_write_trv_v
        let bin_write_trv_v buf ~pos  v =
          let pos = Bin_prot.Write.bin_write_int_8bit buf ~pos 2 in
          bin_write_trv_v buf ~pos v
        let bin_writer_trv_v =
          {
            Bin_prot.Type_class.size = bin_size_trv_v;
            write = bin_write_trv_v
          }
        let _ = bin_writer_trv_v
        let (__bin_read_trv_v__ : (int -> trv_v) Bin_prot.Read.reader) =
          fun _buf ->
            fun ~pos_ref ->
              fun _vint ->
                Bin_prot.Common.raise_variant_wrong_type
                  "test/with_bin_io.ml.V2_trv_v.trv_v" (!pos_ref)
        let _ = __bin_read_trv_v__
        let (bin_read_trv_v : trv_v Bin_prot.Read.reader) =
          fun buf ->
            fun ~pos_ref ->
              let v_fv1 = bin_read_int buf ~pos_ref in
              let v_fv2 = (bin_read_list bin_read_string) buf ~pos_ref in
              let v_fv3 = (bin_read_list bin_read_trv) buf ~pos_ref in
              { fv1 = v_fv1; fv2 = v_fv2; fv3 = v_fv3 }
        let _ = bin_read_trv_v
        let bin_read_trv_v buf ~pos_ref  =
          match Bin_prot.Read.bin_read_int_8bit buf ~pos_ref with
          | 2 -> bin_read_trv_v buf ~pos_ref
          | 1 -> upgrade (V1_trv_v.bin_read_trv_v buf ~pos_ref)
          | 0 ->
              upgrade
                (V1_trv_v.upgrade (V0_trv_v.bin_read_trv_v buf ~pos_ref))
          | v ->
              failwith
                (Printf.sprintf "Unknown VERS %s.%s:%d" "test/with_bin_io.ml"
                   "trv_v" v)
        let bin_reader_trv_v =
          {
            Bin_prot.Type_class.read = bin_read_trv_v;
            vtag_read = __bin_read_trv_v__
          }
        let _ = bin_reader_trv_v
      end
  end
let bin_reader_trv_v = V2_trv_v.bin_reader_trv_v
let bin_read_trv_v = V2_trv_v.bin_read_trv_v
let bin_writer_trv_v = V2_trv_v.bin_writer_trv_v
let bin_write_trv_v = V2_trv_v.bin_write_trv_v
let bin_size_trv_v = V2_trv_v.bin_size_trv_v
type trv_v = V2_trv_v.trv_v = {
  fv1: int ;
  fv2: string list ;
  fv3: trv list }
let trv_v_from_novers (x : trv_v_novers) =
  V2_trv_v.upgrade (V1_trv_v.upgrade { _novers_field = x })
