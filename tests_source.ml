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

type enmp1 = 
  | First
  | Second of int
  | Third of (string * int) [@vers_set  ? `Third s when `Third (s, 0) ]

let f x = x + 1  

