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

type%vers enmp = 
  [`Firs]

type%vers enmp = 
  [`Firs
  | `Second of int
  ]

type%vers enmp = 
  [`Firs
  | `Second of int
  | `Third of string
  ]

type%vers enmp = 
  [`Firs
  | `Second of int
  | `Third of (string * int) [@vers_set ? `Third s when `Third (s, 0) ]
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
