(* dune build tests.exe *)

open Bin_prot.Std
open Sexplib.Std


(*type enmp1i25 = 
  | E1
  | E2 of int
  | E3 of (string * int)*)

(*type%vers[@num 3] user = 
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
} [@@deriving bin_io]*)

type%vers[@novers] user = 
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
  and tli =
float list [@@migrate (List.map float p)]
[@@deriving bin_io]*)

(*type%vers enmp = 
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
  ] [@@deriving bin_io]*)

(*type%vers enm = 
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
  [@@deriving (bin_io, sexp, yojson)] [@@ptag]*)
