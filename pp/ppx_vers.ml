(* ocamlc -dsource _build/default/tests.ml *)
open Ppxlib

type str_descr = 
  {
    type_name: string;
    cnt: int ref;
    last_loc: location;
  }

let h_strs: (string, str_descr) Hashtbl.t   = Hashtbl.create 0

let init_str ~loc type_name =
  try    
    let _ = Hashtbl.find h_strs type_name in
    assert false
  with | Not_found -> { type_name; cnt = ref 0; last_loc = loc } 

let get_vers_num_by_name ~loc type_name =
  try    
    let descr = Hashtbl.find h_strs type_name in
    !(descr.cnt)
  with | Not_found -> 
    Hashtbl.add h_strs type_name (init_str ~loc type_name);
    0

let exists_name = Hashtbl.mem h_strs

let incr_vers_by_name ~loc type_name =
  try    
    let descr = Hashtbl.find h_strs type_name in
    let descr = {(descr) with last_loc = loc} in
    Hashtbl.replace h_strs type_name descr;
    incr descr.cnt 
  with | Not_found -> assert false      

let mod_name_by_cnt name cnt =
  Printf.sprintf "V%d_%s" cnt name

let data_by_mod_name mod_name =
  try
    let (ver, name) = Scanf.sscanf mod_name "V%d%s" (fun v t -> (v, t))  in
    Some (ver, name)  
  with | _ -> None    

let expand_ver ~ctxt vers = 
  (*let () = print_endline _vers in*)
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match vers with (* Ppxlib_ast__.Import.Parsetree.payload*)
  PStr s ->
    let first_struct = List.hd s in 
    let first_struct =
      match first_struct.pstr_desc with
      | Pstr_type (_, td) when List.length td = 1 ->  
        let type_name = (List.hd td).ptype_name.txt in
        let cnt = get_vers_num_by_name ~loc type_name in
        let mod_name = mod_name_by_cnt type_name cnt in
        let sts = [first_struct] in
        let sts =
          if cnt > 0
          then 
            let prev_mod_name = mod_name_by_cnt type_name (cnt - 1) in
            let lm = Ast_builder.Default.pmod_ident ~loc {txt = Lident prev_mod_name; loc} in
            let m = [%stri module Prev = [%m lm] ]  in
            m :: sts |> List.rev
          else sts             
        in
        let m = Ast_builder.Default.pmod_structure ~loc sts in
        let mb =  Ast_builder.Default.module_binding ~loc ~name:{txt = Some mod_name; loc} ~expr:m in
        let () = incr_vers_by_name ~loc type_name in
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

  (*Hashtbl.fold (fun _k str s ->
    let loc = str.last_loc  in
    let pe = Ast_builder.Default.pexp_constant ~loc (Pconst_integer ("6", None)) in
    let pv = Ast_builder.Default.ppat_var ~loc {txt = "i"; loc} in
    let vb = Ast_builder.Default.value_binding ~loc ~pat:pv ~expr:pe in
    let sv = Ast_builder.Default.pstr_value ~loc Nonrecursive [vb] in
    (*[%stri let i = 6] :: s*)
    sv::s
  ) h_strs s*)

(*Printf.printf "%s\n\n" (Pprintast.string_of_structure [s]);*)

let type_kind_by_mod_expr pe =
  match pe.pmod_desc with
  | Pmod_structure s -> 
    let si = List.hd s in
    (match si.pstr_desc with
    | Pstr_type (_, td) -> 
      (List.hd td).ptype_kind
      (*Printf.printf "%s\n\n" (Pprintast.string_of_structure s)*)
    | _ -> assert false)
  | _ -> assert false

let impl s =
  List.fold_right (fun s strs ->
    let loc = s.pstr_loc in
    match s.pstr_desc with
    | Pstr_module {pmb_name = {txt = Some(mod_name); _}; pmb_expr = pe; _} ->      
      (match data_by_mod_name mod_name with
      | Some (ver, type_name) when exists_name type_name ->
        let cur_ver = get_vers_num_by_name ~loc type_name - 1 in
        if cur_ver = ver
        then 
          let ct = {
            ptyp_desc = Ptyp_constr ({txt = Lident (mod_name ^ "." ^ type_name); loc}, []);
            ptyp_loc = loc;
            ptyp_loc_stack = [];
            ptyp_attributes = [];
          } 
          in
          let td = {
            ptype_name = {txt = type_name; loc};
            ptype_params = [];
            ptype_cstrs = [];
            ptype_kind = type_kind_by_mod_expr pe;
            ptype_private = Public;
            ptype_manifest = Some ct;
            ptype_attributes = [];
            ptype_loc = loc
          } 
          in
          let pt = Ast_builder.Default.pstr_type ~loc Recursive [td] in
          s :: pt :: strs
        else s :: strs
      | _ -> s :: strs)
    | _ -> s :: strs
  ) s []

let () =
  Driver.register_transformation
    ~rules:[rule_vers]
    ~impl
    "vers"
