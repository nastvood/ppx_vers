(* ocamlc -dsource _build/default/tests.ml *)
(*ignore(Location.raise_errorf ~loc "this is an %s message" "error")*)
(*Printf.printf "%s\n\n" (Pprintast.string_of_structure [s]);*)

open Ppxlib

module AD = Ast_builder.Default

type str_descr = 
  {
    type_name: string;
    cnt: int ref;
    last_loc: location;
    prev_td: type_declaration
  }

let h_strs: (string, str_descr) Hashtbl.t   = Hashtbl.create 0

let init_str ~loc type_name prev_td =
  try    
    let _ = Hashtbl.find h_strs type_name in
    assert false
  with | Not_found -> { type_name; cnt = ref 0; last_loc = loc; prev_td } 

let init_vers_num ~loc type_name last_td =
  try    
    let descr = Hashtbl.find h_strs type_name in
    !(descr.cnt)
  with | Not_found -> 
    Hashtbl.add h_strs type_name (init_str ~loc type_name last_td);
    0

let get_vers_num_by_name type_name =
  let descr = Hashtbl.find h_strs type_name in
  !(descr.cnt)

let exists_name = Hashtbl.mem h_strs

let incr_vers_by_name ~loc type_name prev_td =
  try    
    let descr = Hashtbl.find h_strs type_name in
    let descr = {(descr) with last_loc = loc; prev_td} in
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

let vers_set = "vers_set"  

let vers_set_payload attrs =
  if attrs = []
  then None
  else
    try
      let attr = List.find (fun a -> a.attr_name.txt = vers_set) attrs in
      Some attr.attr_payload
    with Not_found -> None 

let expr_by_payload payload =    
  (match payload with
  | PStr s -> 
    let hd_s = List.hd s in
    (match hd_s.pstr_desc with
    | Pstr_eval (e, _) -> e
    | _ -> assert false)
  | _ -> assert false)

let pvariant_by_type_declaration td =
  match td.ptype_manifest with
  | Some ({ptyp_desc = Ptyp_variant (rfx, cf, lx); _})  -> Some (rfx, cf, lx)
  | _ -> None

let vers_set_payload_row_fields rsx =
  List.fold_left (fun acc rs ->
    match (rs.prf_desc, vers_set_payload rs.prf_attributes) with
    | (Rtag (loc, _, _), Some pl) ->          
      let (pat, exp) =
        match pl with
        | PPat (pat, Some exp) -> (pat, exp)
        | _ -> assert false
      in
      (loc.txt, (pat, exp)) :: acc
    | (Rtag (_, _, _), None) -> acc
    | (Rinherit _, _) -> assert false
  ) [] rsx   

let vers_set_payload_by_constructor_declaration cdx =
  List.fold_left (fun acc cd ->
    match vers_set_payload cd.pcd_attributes with
    | Some pl -> 
      let (pat, exp) =
        match pl with
        | PPat (pat, Some exp) -> (pat, exp)
        | _ -> assert false
      in
      (cd.pcd_name.txt, (pat, exp)) :: acc
    | _ -> acc
  ) [] cdx

let get_upgrade_fun ~loc type_name td =
  match td.ptype_kind with
  | Ptype_record lds ->
    let lexps =
      List.map (fun ld ->
        let lid = {txt = Lident ld.pld_name.txt; loc} in
        let efield = 
          match vers_set_payload ld.pld_attributes with
          | Some payload -> expr_by_payload payload
          | _ -> AD.pexp_field ~loc [%expr p] {txt = Ldot (Lident "Prev", ld.pld_name.txt); loc}
        in  
        (lid, efield)
      ) lds
    in
    let eres = AD.pexp_record ~loc lexps None in
    [%stri let upgrade p = [%e eres] ]
  | Ptype_abstract -> 
    (match pvariant_by_type_declaration td with
    | Some (rfx, _cf, _l) ->
      (match vers_set_payload_row_fields rfx with
      | [] -> 
        [%stri let upgrade p = (p :> [%t (AD.ptyp_constr ~loc {txt = Lident td.ptype_name.txt; loc} [])]) ]
      | pls ->        
        let cases =
          List.map (fun rf ->
            let (tag_name, exists_constr) = match rf.prf_desc with Rtag (l, _, ctlx) -> (l.txt, ctlx <> []) | Rinherit _ -> assert false in
            let (lhs, rhs) =
              match List.assoc tag_name pls with
              | (pat, exp) -> (pat, exp)
              | exception Not_found ->                  
                (AD.ppat_variant ~loc tag_name (if exists_constr then Some(AD.ppat_var ~loc {txt = "x"; loc}) else None),
                AD.pexp_variant ~loc tag_name (if exists_constr then Some(AD.pexp_ident ~loc {txt = Lident "x"; loc}) else None))
            in  
            AD.case lhs None rhs
          ) rfx
        in
        let m = AD.pexp_match ~loc [%expr p] cases in
        [%stri let upgrade p = [%e m] ]
      )        
    | None ->
      (match vers_set_payload td.ptype_attributes with
      | Some payload -> [%stri let upgrade p = [%e (expr_by_payload payload)] ]  
      | None ->  assert false)
    )      
  | Ptype_variant cdx -> 
    let pls = vers_set_payload_by_constructor_declaration cdx in
    let parsed_cdx = 
      if pls = [] 
      then match (Hashtbl.find h_strs type_name).prev_td.ptype_kind with Ptype_variant cdx -> cdx | _ -> assert false
      else cdx 
    in
    let cases =
      List.map (fun cd ->
        let tag_name = cd.pcd_name.txt in
        let exists_constr = match cd.pcd_args with Pcstr_tuple ctx -> ctx <> [] | Pcstr_record lbdx -> lbdx <> [] in
        let (lhs, rhs) =
          (match List.assoc tag_name pls with
          | (pat, exp) -> (pat, exp)
          | exception Not_found ->
            (AD.ppat_construct ~loc {txt = Ldot (Lident "Prev", tag_name); loc} (if exists_constr then Some(AD.ppat_var ~loc {txt = "x"; loc}) else None),
            AD.pexp_construct ~loc {txt = Lident tag_name; loc} (if exists_constr then Some(AD.pexp_ident ~loc {txt = Lident "x"; loc}) else None))
          )
        in  
        AD.case lhs None rhs
      ) parsed_cdx
    in     
    let m = AD.pexp_match ~loc [%expr p] cases in
    [%stri let upgrade p = [%e m] ]
  | Ptype_open -> assert false

let expand_ver ~ctxt payload = 
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match payload with
  PStr s ->
    let first_struct = List.hd s in 
    let first_struct =
      match first_struct.pstr_desc with
      | Pstr_type (_, td) when List.length td = 1 ->  
        let hd_td = List.hd td in
        let type_name = hd_td.ptype_name.txt in
        let cnt = init_vers_num ~loc type_name hd_td in
        let mod_name = mod_name_by_cnt type_name cnt in
        let sts = [first_struct] in
        let sts =
          if cnt > 0
          then 
            let prev_mod_name = mod_name_by_cnt type_name (cnt - 1) in
            let lm = AD.pmod_ident ~loc {txt = Lident prev_mod_name; loc} in
            let mod_prev = [%stri module Prev = [%m lm] ]  in
            let upgrade_fun = get_upgrade_fun ~loc type_name hd_td in
            upgrade_fun :: mod_prev :: sts |> List.rev
          else sts             
        in
        let m = AD.pmod_structure ~loc sts in
        let mb =  AD.module_binding ~loc ~name:{txt = Some mod_name; loc} ~expr:m in
        let () = incr_vers_by_name ~loc type_name hd_td in
        AD.pstr_module ~loc mb
      | _ -> assert false
    in
    first_struct
  | _ -> [%stri let () = ()]

let vers_extension =
  Extension.V3.declare
    "vers"
    Extension.Context.structure_item
    Ast_pattern.(__)
    expand_ver

let rule_vers = Ppxlib.Context_free.Rule.extension vers_extension

let type_kind_by_mod_expr pe =
  match pe.pmod_desc with
  | Pmod_structure s -> 
    let si = List.hd s in
    (match si.pstr_desc with
    | Pstr_type (_, td) -> 
      (List.hd td).ptype_kind
    | _ -> assert false)
  | _ -> assert false

let impl s =
  List.fold_right (fun s strs ->
    let loc = s.pstr_loc in
    match s.pstr_desc with
    | Pstr_module {pmb_name = {txt = Some(mod_name); _}; pmb_expr = pe; _} ->      
      (match data_by_mod_name mod_name with
      | Some (ver, type_name) when exists_name type_name ->
        let cur_ver = get_vers_num_by_name type_name - 1 in
        if cur_ver = ver
        then 
          let ct = AD.ptyp_constr ~loc {txt = Ldot (Lident mod_name, type_name); loc} [] in
          let td = AD.type_declaration ~loc ~name:{txt = type_name; loc}
            ~params:[] ~cstrs:[] ~kind:(type_kind_by_mod_expr pe) ~private_:Public ~manifest:(Some ct)
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
