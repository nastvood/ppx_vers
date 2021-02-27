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

(*type%vers[@novers] user = 
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

(*type%vers[@novers] user = 
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
} (*[@@deriving bin_io]*)*)

 
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

(*type%vers[@novers] tli = int list
  and tli =
float list [@@migrate (List.map float p)]
[@@deriving (bin_io, sexp, yojson)]*)

(*type%vers[@novers] tli = int list
  and tli =
float list [@@migrate (List.map float p)]
[@@deriving (bin_io)]*)

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

(*type%vers[@novers] enmp = 
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
  ] (*[@@deriving bin_io]*)*)

(*type%vers[@novers] tenmp = {
    field1: enmp [@from_novers];
    field2: enmp;
    field3: string
  } (*[@@deriving bin_io]*)*)

(*type%vers[@novers] enm = 
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
  (*[@@deriving (bin_io, sexp, yojson)] [@@ptag]*)*)

(*type%vers[@novers] t = 
| Enm_novers of enm  
| Enm of enm  
| Nothing *)

(*type%vers[@novers] tenm = 
[ `Enm_novers of enm [@from_novers]
| `Enm of enm  
| `Nothing 
]*)

type%vers[@novers] kind = string [@@deriving (sexp,bin_io)]

type%vers[@novers] cost = 
  (kind_novers * int) [@@deriving (sexp,bin_read,bin_write)]
and cost = 
  (kind * int) [@@migrate (kind_from_kind_novers (fst p), snd p)]
  
type%vers[@novers] arena_competition_reward =
  {
    position : int;
    new_league : int option;
    reward : cost_novers list;
  } [@@deriving (sexp,bin_read,bin_write)]
and arena_competition_reward =
  {
    position : int;
    new_league : int option;
    reward : cost list [@migrate (List.map cost_from_cost_novers p.Prev.reward)]
  }
