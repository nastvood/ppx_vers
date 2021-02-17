(* dune build tests.exe *)

type %vers user = 
{
  name: string
}

type %vers user = 
{
  name: string;
  age: int
}

(*type %vers user = 
{
  name: string
}*)

(*type %vers t0 = int
type %vers t0 = float*)

(*type [%vers "foo"] user = 
{
  name: string
};;*)

(*print_endline "Hello, world!" *)
(*
[
  structure_item (_build/default/tests.ml[1,0+0]..[4,30+1])
    Pstr_type Rec
    [
      type_declaration "user" (_build/default/tests.ml[1,0+5]..[1,0+9]) (_build/default/tests.ml[1,0+0]..[4,30+1])
        ptype_params =
          []
        ptype_cstrs =
          []
        ptype_kind =
          Ptype_record
            [
              (_build/default/tests.ml[3,15+2]..[3,15+14])
                Immutable
                "name" (_build/default/tests.ml[3,15+2]..[3,15+6])                core_type (_build/default/tests.ml[3,15+8]..[3,15+14])
                  Ptyp_constr "string" (_build/default/tests.ml[3,15+8]..[3,15+14])
                  []
            ]
        ptype_private = Public
        ptype_manifest =
          None
    ]
]*)

