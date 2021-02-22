(* dune build tests.exe *)

open Bin_prot.Std

(*type enmp1i25 = 
  | E1
  | E2 of int
  | E3 of (string * int)*)

type%vers[@num 3] user = 
{
  name: string;
  lx : int list;
  opt : string option;
} and user =
{
  name: string;
  lx : int list;
  opt : string option;
  age: int [@migrate 0]
}  and user = 
{
  name: string;
  lx : int list;
  opt : string option;
  age: int;  
  surname: string [@migrate (try BatString.split "" p.Prev.name |> snd with | Not_found -> "")]
} [@@deriving bin_io]

 
(*type%vers address =
{
  city: string;
  street: string;
  number: int;
} [@@deriving bin_io]*)


(*type%vers tli = int list

type%vers tli = float list [@@migrate (List.map float p)]*)


type%vers enmp = 
  [ `First 
  ] and enmp =
  [ `First
  | `Second of int
  ] and enmp =
  [ `First
  | `Second of int
  | `Third of string
  ] and enmp =
  [ `First
  | `Second of int
  | `Third of (string * int) [@migrate ? `Third s when `Third (s, 0) ]
  ] [@@deriving bin_io]

type%vers enm = 
  | First
  and enm =
  | First
  | Second of int
  and enm =
  | First
  | Second of int
  | Third of string
  and enm =
  | First
  | Second of int
  | Third of (string * string) [@migrate ? Prev.Third p when Third (p, p) ]
  [@@deriving bin_io]

let (bin_read_enm : enm Bin_prot.Read.reader) =
  fun buf ->
    fun ~pos_ref ->
      match Bin_prot.Read.bin_read_int_8bit buf ~pos_ref with
      | 0 -> First
      | 1 -> let arg_1 = bin_read_int buf ~pos_ref in Second arg_1
      | 2 ->
          let arg_1 =
            let v1 = bin_read_string buf ~pos_ref in
            let v2 = bin_read_string buf ~pos_ref in (v1, v2) in
          Third arg_1
      | _ ->
          Bin_prot.Common.raise_read_error
            (Bin_prot.Common.ReadError.Sum_tag "tests.ml.V3_enm.enm")
            (!pos_ref)

