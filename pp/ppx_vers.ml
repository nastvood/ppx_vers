(* ocamlc -dsource _build/default/tests.ml *)
open Ppxlib

let cnt = ref 0

let mod_name_by_cnt name cnt =
  Printf.sprintf "V%d_%s" cnt name


let expand_ver ~ctxt vers = 
  (*let () = print_endline _vers in*)
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match vers with (* Ppxlib_ast__.Import.Parsetree.payload*)
  | PStr s ->
    let first_struct = List.hd s in 
    let first_struct =
      match first_struct.pstr_desc with
      | Pstr_type (_, td) when List.length td = 1 ->  
        let type_name = (List.hd td).ptype_name.txt in
        let mod_name = mod_name_by_cnt type_name !cnt in
        let sts = [first_struct] in
        let sts =
          if !cnt > 0
          then 
            let prev_mod_name = mod_name_by_cnt type_name !cnt in
            let lm = Ast_builder.Default.pmod_ident ~loc {txt = Lident prev_mod_name; loc} in
            let m = [%stri module Prev = [%m lm] ]  in
            m :: sts
          else sts             
        in
        let m = Ast_builder.Default.pmod_structure ~loc sts in
        let mb =  Ast_builder.Default.module_binding ~loc ~name:{txt = Some mod_name; loc} ~expr:m in
        let () = incr cnt in
        Ast_builder.Default.pstr_module ~loc mb
      | _ -> assert false
    in
    first_struct
  | _ -> [%stri let i = 6]

let vers_extension =
  Extension.V3.declare
    "vers"
    Extension.Context.structure_item
    Ast_pattern.(__)
    expand_ver

let rule_vers = Ppxlib.Context_free.Rule.extension vers_extension

(*let expand_next (e : Parsetree.expression) =
  let loc = e.pexp_loc in
  Some [%expr 1]

let rule_next = Ppxlib.Context_free.Rule.special_function "=>" expand_next*)

let () =
  Driver.register_transformation
    ~rules:[rule_vers(*; rule_next*)]
    "vers"
