(* dune build tests.exe *)


type fake_user = 
{
  name: string
}


type%vers user = 
{
  name: string
}

type%vers user = 
{
  name: string;
  age: int [@vers_set 0]
}

type%vers user = 
{
  name: string;
  age: int;  
  surname: string [@vers_set (try BatString.split "" p.Prev.name |> snd with | Not_found -> "")]
}

type%vers enm = 
  [`Firs]

type%vers enm = 
  [`Firs
  | `Second
  ]

type%vers tli = int list

type%vers tli = float list [@@vers_set (List.map float p)]


type %vers address = 
{
  city: string
}

type %vers address = 
{
  city: string;
  street: string [@vers_set ""];
}
