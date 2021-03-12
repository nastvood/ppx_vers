open Bin_prot.Std

type%vers[@novers] t1 =
  | A1 of int
  | A2 
  | A3 of string
    and t1 = 
  | A1 of int
  | A2 
  | A3 of string
  | A4 of float
  [@@deriving (bin_read, bin_write)] [@@ptag]
