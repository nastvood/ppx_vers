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

 
type%vers address =
{
  city: string;
  street: string;
  number: int;
} [@@deriving bin_io]


(*type%vers tli = int list

type%vers tli = float list [@@vers_set (List.map float p)]

type%vers enmp = 
  [ `First ]

type%vers enmp = 
  [ `First
  | `Second of int
  ]

type%vers enmp = 
  [ `First
  | `Second of int
  | `Third of string
  ]

type%vers enmp = 
  [ `First
  | `Second of int
  | `Third of (string * int) [@vers_set ? `Third s when `Third (s, 0) ]
  ]

type%vers enm = 
  | First

type%vers enm = 
  | First
  | Second of int

type%vers enm = 
  | First
  | Second of int
  | Third of string

type%vers enm = 
  | First
  | Second of int
  | Third of (string * string) [@vers_set ? Prev.Third p when Third (p, p) ]*)
