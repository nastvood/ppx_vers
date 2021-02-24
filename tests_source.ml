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
  | First [@migrate ? First when First]
  | Second of int [@migrate ? Second x when Second x]

type enm = enm_novers*)

type user_novers = 
{
  name: string;
  age: int 
}

type user =
  {
    novers_field : user_novers
  }

type user =
  {
    name: string; [@migrate p.Prev.novers_field.name]
    age: int [@migrate p.Prev.novers_field.age]
  }  


