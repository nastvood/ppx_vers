open Bin_prot.Std

type%vers[@novers] trv = {
  f1: int;
  f2: string
} and trv = {
  f1: int;
  f2: string;
  f3: float [@migrate 0.0]
} and trv = {
  f1: int;
  f2: int; [@migrate int_of_string p.Prev.f2]
  f3: float
}[@@deriving (bin_read, bin_write)]

type fake = {
  fake: char;
}

type%vers[@novers] trv_v = {
  fv1: int;
  fv2: string;
  fv3: trv list [@from_novers List.map trv_from_novers p.Prev._novers_field.fv3]; 
} and trv_v = {
  fv1: int;
  fv2: string list; [@migrate [p.Prev.fv2] ]
  fv3: trv list;
}[@@deriving (bin_read, bin_write)]
