(* ocamlc -dsource _build/default/tests.ml *)
open Ppxlib


let expand ~ctxt vers = 
  (*let () = print_endline _vers in*)
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match vers with (* Ppxlib_ast__.Import.Parsetree.payload*)
  | PStr s -> List.hd s 
  | _ -> [%stri let i = 6]

let vers_extension =
  Extension.V3.declare
    "vers"
    Extension.Context.structure_item
    Ast_pattern.(__)
    expand

let rule = Ppxlib.Context_free.Rule.extension vers_extension

let () =
  Driver.register_transformation
    ~rules:[rule]
    "vers"
