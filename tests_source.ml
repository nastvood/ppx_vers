(* ocamlc -dparsetree tests_source.ml *)
(* ocamlfind ppx_tools/dumpast -e "{ name = p.Prev.name }" *)

(*type enmv = 
  [ `Firs
  | `Second
  ]
  

type enm = Firs | Second*)

(*type enmp1 = 
  [`First
  | `Second
  | `Third of (string * int) [@vers_set  ? `Third s when `Third (s, 0) ]
  ]*)
  

(*type user = 
{
  name: string;
  age: int [@vers_set 0]
}

type tli = int list*)

(*type enm = 
  | First
  | Second of int*)

(*type enmp1 = 
  | First 
  | Second of int
  | Third of (string * int) [@vers_set  ? `Third s when `Third (s, 0) ]
  [@@vers_num 1]*)


(*type user = 
{
  name: string;
  lx : int list;
  opt : string option;
} and 
user =
{
  name: string;
  lx : int list;
  opt : string option;
} *)

(*type enm_novers =
  [ `First [@migrate ? `First when `First]
  | `Second of int [@migrate ? `Second x when `Second x]
  ]

type enm = enm_novers*)

(*type tli_novers = int list*)

type tli = int list [@@migrate]

(*type user =
{
  name: string;
  lx : int list;
  opt : string option;
  pa : a
}

let f (x:t) = V4_enmp.upgrade (V3_enmp.upgrade (V2_enmp.upgrade (V1_enmp.upgrade x)))*)
