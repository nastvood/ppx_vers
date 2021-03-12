open Bin_prot.Std
type t1_novers =
  | A1 of int 
  | A2 
  | A3 of string [@@deriving (bin_write, bin_read)]
include
  struct
    let _ = fun (_ : t1_novers) -> ()
    let (bin_size_t1_novers : t1_novers Bin_prot.Size.sizer) =
      function
      | A1 v1 -> let size = 1 in Bin_prot.Common.(+) size (bin_size_int v1)
      | A3 v1 ->
          let size = 1 in Bin_prot.Common.(+) size (bin_size_string v1)
      | A2 -> 1
    let _ = bin_size_t1_novers
    let (bin_write_t1_novers : t1_novers Bin_prot.Write.writer) =
      fun buf ->
        fun ~pos ->
          function
          | A1 v1 ->
              let pos = Bin_prot.Write.bin_write_int_8bit buf ~pos 0 in
              bin_write_int buf ~pos v1
          | A2 -> Bin_prot.Write.bin_write_int_8bit buf ~pos 1
          | A3 v1 ->
              let pos = Bin_prot.Write.bin_write_int_8bit buf ~pos 2 in
              bin_write_string buf ~pos v1
    let _ = bin_write_t1_novers
    let bin_writer_t1_novers =
      {
        Bin_prot.Type_class.size = bin_size_t1_novers;
        write = bin_write_t1_novers
      }
    let _ = bin_writer_t1_novers
    let (__bin_read_t1_novers__ : (int -> t1_novers) Bin_prot.Read.reader) =
      fun _buf ->
        fun ~pos_ref ->
          fun _vint ->
            Bin_prot.Common.raise_variant_wrong_type
              "test/ptag_bin_io.ml.t1_novers" (!pos_ref)
    let _ = __bin_read_t1_novers__
    let (bin_read_t1_novers : t1_novers Bin_prot.Read.reader) =
      fun buf ->
        fun ~pos_ref ->
          match Bin_prot.Read.bin_read_int_8bit buf ~pos_ref with
          | 0 -> let arg_1 = bin_read_int buf ~pos_ref in A1 arg_1
          | 1 -> A2
          | 2 -> let arg_1 = bin_read_string buf ~pos_ref in A3 arg_1
          | _ ->
              Bin_prot.Common.raise_read_error
                (Bin_prot.Common.ReadError.Sum_tag
                   "test/ptag_bin_io.ml.t1_novers") (!pos_ref)
    let _ = bin_read_t1_novers
    let bin_reader_t1_novers =
      {
        Bin_prot.Type_class.read = bin_read_t1_novers;
        vtag_read = __bin_read_t1_novers__
      }
    let _ = bin_reader_t1_novers
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
module V0_t1 =
  struct
    type t1 = t1_novers[@@deriving (bin_write, bin_read)]
    include
      struct
        let _ = fun (_ : t1) -> ()
        let (bin_size_t1 : t1 Bin_prot.Size.sizer) = bin_size_t1_novers
        let _ = bin_size_t1
        let (bin_write_t1 : t1 Bin_prot.Write.writer) = bin_write_t1_novers
        let _ = bin_write_t1
        let bin_writer_t1 =
          { Bin_prot.Type_class.size = bin_size_t1; write = bin_write_t1 }
        let _ = bin_writer_t1
        let (__bin_read_t1__ : (int -> t1) Bin_prot.Read.reader) =
          __bin_read_t1_novers__
        let _ = __bin_read_t1__
        let (bin_read_t1 : t1 Bin_prot.Read.reader) = bin_read_t1_novers
        let _ = bin_read_t1
        let bin_reader_t1 =
          {
            Bin_prot.Type_class.read = bin_read_t1;
            vtag_read = __bin_read_t1__
          }
        let _ = bin_reader_t1
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end
module V1_t1 =
  struct
    type t1 =
      | A1 of int 
      | A2 
      | A3 of string [@@deriving (bin_write, bin_read)]
    module Prev = V0_t1
    let upgrade p =
      match p with
      | (A1 x : Prev.t1) -> A1 x
      | (A2 : Prev.t1) -> A2
      | (A3 x : Prev.t1) -> A3 x
    include
      struct
        let _ = fun (_ : t1) -> ()
        let (bin_size_t1 : t1 Bin_prot.Size.sizer) =
          function
          | A1 v1 ->
              let size = 4 in Bin_prot.Common.(+) size (bin_size_int v1)
          | A3 v1 ->
              let size = 4 in Bin_prot.Common.(+) size (bin_size_string v1)
          | A2 -> 4
        let _ = bin_size_t1
        let (bin_write_t1 : t1 Bin_prot.Write.writer) =
          fun buf ->
            fun ~pos ->
              function
              | A1 v1 ->
                  let pos =
                    Bin_prot.Write.bin_write_variant_int buf ~pos 14544 in
                  bin_write_int buf ~pos v1
              | A2 -> Bin_prot.Write.bin_write_variant_int buf ~pos 14545
              | A3 v1 ->
                  let pos =
                    Bin_prot.Write.bin_write_variant_int buf ~pos 14546 in
                  bin_write_string buf ~pos v1
        let _ = bin_write_t1
        let bin_writer_t1 =
          { Bin_prot.Type_class.size = bin_size_t1; write = bin_write_t1 }
        let _ = bin_writer_t1
        let (__bin_read_t1__ : (int -> t1) Bin_prot.Read.reader) =
          fun _buf ->
            fun ~pos_ref ->
              fun _vint ->
                Bin_prot.Common.raise_variant_wrong_type
                  "test/ptag_bin_io.ml.V1_t1.t1" (!pos_ref)
        let _ = __bin_read_t1__
        let (bin_read_t1 : t1 Bin_prot.Read.reader) =
          fun buf ->
            fun ~pos_ref ->
              match Bin_prot.Read.bin_read_int_8bit buf ~pos_ref with
              | 14544 -> let arg_1 = bin_read_int buf ~pos_ref in A1 arg_1
              | 14545 -> A2
              | 14546 -> let arg_1 = bin_read_string buf ~pos_ref in A3 arg_1
              | _ ->
                  Bin_prot.Common.raise_read_error
                    (Bin_prot.Common.ReadError.Sum_tag
                       "test/ptag_bin_io.ml.V1_t1.t1") (!pos_ref)
        let _ = bin_read_t1
        let bin_reader_t1 =
          {
            Bin_prot.Type_class.read = bin_read_t1;
            vtag_read = __bin_read_t1__
          }
        let _ = bin_reader_t1
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end
module V2_t1 =
  struct
    type t1 =
      | A1 of int 
      | A2 
      | A3 of string 
      | A4 of float [@@deriving (bin_write, bin_read)]
    module Prev = V1_t1
    let upgrade p =
      match p with | Prev.A1 x -> A1 x | Prev.A2 -> A2 | Prev.A3 x -> A3 x
    include
      struct
        let _ = fun (_ : t1) -> ()
        let (bin_size_t1 : t1 Bin_prot.Size.sizer) =
          function
          | A1 v1 ->
              let size = 4 in Bin_prot.Common.(+) size (bin_size_int v1)
          | A3 v1 ->
              let size = 4 in Bin_prot.Common.(+) size (bin_size_string v1)
          | A4 v1 ->
              let size = 4 in Bin_prot.Common.(+) size (bin_size_float v1)
          | A2 -> 4
        let _ = bin_size_t1
        let (bin_write_t1 : t1 Bin_prot.Write.writer) =
          fun buf ->
            fun ~pos ->
              function
              | A1 v1 ->
                  let pos =
                    Bin_prot.Write.bin_write_variant_int buf ~pos 14544 in
                  bin_write_int buf ~pos v1
              | A2 -> Bin_prot.Write.bin_write_variant_int buf ~pos 14545
              | A3 v1 ->
                  let pos =
                    Bin_prot.Write.bin_write_variant_int buf ~pos 14546 in
                  bin_write_string buf ~pos v1
              | A4 v1 ->
                  let pos =
                    Bin_prot.Write.bin_write_variant_int buf ~pos 14547 in
                  bin_write_float buf ~pos v1
        let _ = bin_write_t1
        let bin_write_t1 buf ~pos  v =
          let pos = Bin_prot.Write.bin_write_int_8bit buf ~pos 2 in
          bin_write_t1 buf ~pos v
        let bin_writer_t1 =
          { Bin_prot.Type_class.size = bin_size_t1; write = bin_write_t1 }
        let _ = bin_writer_t1
        let (__bin_read_t1__ : (int -> t1) Bin_prot.Read.reader) =
          fun _buf ->
            fun ~pos_ref ->
              fun _vint ->
                Bin_prot.Common.raise_variant_wrong_type
                  "test/ptag_bin_io.ml.V2_t1.t1" (!pos_ref)
        let _ = __bin_read_t1__
        let (bin_read_t1 : t1 Bin_prot.Read.reader) =
          fun buf ->
            fun ~pos_ref ->
              match Bin_prot.Read.bin_read_int_8bit buf ~pos_ref with
              | 14544 -> let arg_1 = bin_read_int buf ~pos_ref in A1 arg_1
              | 14545 -> A2
              | 14546 -> let arg_1 = bin_read_string buf ~pos_ref in A3 arg_1
              | 14547 -> let arg_1 = bin_read_float buf ~pos_ref in A4 arg_1
              | _ ->
                  Bin_prot.Common.raise_read_error
                    (Bin_prot.Common.ReadError.Sum_tag
                       "test/ptag_bin_io.ml.V2_t1.t1") (!pos_ref)
        let _ = bin_read_t1
        let bin_read_t1 buf ~pos_ref  =
          match Bin_prot.Read.bin_read_int_8bit buf ~pos_ref with
          | 2 -> bin_read_t1 buf ~pos_ref
          | 1 -> upgrade (V1_t1.bin_read_t1 buf ~pos_ref)
          | 0 -> upgrade (V1_t1.upgrade (V0_t1.bin_read_t1 buf ~pos_ref))
          | v ->
              failwith
                (Printf.sprintf "Unknown VERS %s.%s:%d" "test/ptag_bin_io.ml"
                   "t1" v)
        let bin_reader_t1 =
          {
            Bin_prot.Type_class.read = bin_read_t1;
            vtag_read = __bin_read_t1__
          }
        let _ = bin_reader_t1
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end
let bin_reader_t1 = V2_t1.bin_reader_t1
let bin_read_t1 = V2_t1.bin_read_t1
let bin_writer_t1 = V2_t1.bin_writer_t1
let bin_write_t1 = V2_t1.bin_write_t1
let bin_size_t1 = V2_t1.bin_size_t1
type t1 = V2_t1.t1 =
  | A1 of int 
  | A2 
  | A3 of string 
  | A4 of float 
let t1_from_novers (x : t1_novers) = V2_t1.upgrade (V1_t1.upgrade x)
