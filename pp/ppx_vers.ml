(* ocamlc -dsource _build/default/tests.ml *)
(*ignore(Location.raise_errorf ~loc "this is an %s messapge" "error")*)
(*Printf.printf "%s\n\n" (Pprintast.string_of_structure [s]);*)
(*ocamlfind ppx_tools/dumpast -e '(match p with `First x -> `First x)'*)

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

let vers_set = "migrate"  

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
            get_upgrade_fun ~loc type_name hd_td ::
            [%stri module Prev = [%m lm] ] :: 
            sts |> List.rev
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

let gen_bin_funcs ~loc type_name mod_name =  
  List.map (fun s ->
    let f_name = "bin_" ^ s  ^  "_" ^ type_name in
    let f_name_l = AD.pvar ~loc f_name in
    let e_r = AD.pexp_ident ~loc {txt = Ldot (Lident mod_name, f_name); loc} in
    [%stri let [%p f_name_l] = [%e e_r]]
  ) ["shape"; "reader"; "size"; "write"; "read"]

let gen_last_write_fun ~loc type_name ver =  
  let f_name = "bin_write_" ^ type_name in
  let f_name_l = AD.pvar ~loc f_name in
  let ever = AD.eint ~loc ver in
  let e_f_name = AD.pexp_ident ~loc {txt = Lident f_name; loc} in
  [%stri let [%p f_name_l] = fun buf -> fun ~pos -> fun v ->
    let pos = Bin_prot.Write.bin_write_int_8bit buf ~pos [%e ever] in  
    [%e e_f_name] buf ~pos v
  ] 

let gen_last_read_fun ~loc type_name ver =    
  let f_name = "bin_read_" ^ type_name in
  let f_name_l = AD.pvar ~loc f_name in
  let ever = AD.eint ~loc ver in
  let e_f_name = AD.pexp_ident ~loc {txt = Lident f_name; loc} in
  [%stri let [%p f_name_l] = fun buf -> fun ~pos_ref ->
    assert false
    (*let pos = Bin_prot.Write.bin_write_int_8bit buf ~pos [%e ever] in  
    [%e e_f_name] buf ~pos v*)
  ] 

let patch_module_expr ~loc type_name ver pe =
  match pe.pmod_desc with
  | Pmod_structure sx -> 
    let sx =
      List.fold_right (fun s sx ->
        match s.pstr_desc with
        | Pstr_include i -> 
          let pm = i.pincl_mod in
          (match pm.pmod_desc with
          | Pmod_structure isx -> 
            let isx =
              List.fold_right (fun s isx ->
                (match s.pstr_desc with
                | Pstr_value (_, vbx) when vbx <> [] ->
                  let hd_vbx = List.hd vbx in
                  (match hd_vbx.pvb_pat.ppat_desc with
                  | Ppat_var l when l.txt = "bin_writer_" ^ type_name -> 
                    gen_last_write_fun ~loc type_name ver :: s :: isx
                  | Ppat_var l when l.txt = "bin_reader_" ^ type_name -> 
                    gen_last_read_fun ~loc type_name ver :: s :: isx
                  | _ -> s :: isx)
                | _ -> s :: isx)
              ) isx []
            in
            let pm = {(pm) with pmod_desc = Pmod_structure isx} in
            let i = {(i) with pincl_mod = pm} in
            {(s) with pstr_desc = Pstr_include i} :: sx
          | _ -> s :: sx)
        | _ -> s :: sx
      ) sx []
    in
    {(pe) with pmod_desc = Pmod_structure sx}
  | _ -> pe  

let impl s =
  List.fold_right (fun s strs ->
    let loc = s.pstr_loc in
    match s.pstr_desc with
    | Pstr_module ({pmb_name = {txt = Some(mod_name); _}; pmb_expr = pe; _} as pm) ->      
      (match data_by_mod_name mod_name with
      | Some (ver, type_name) when exists_name type_name ->
        let cur_ver = get_vers_num_by_name type_name - 1 in
        if cur_ver = ver
        then 
          let pe = patch_module_expr ~loc type_name ver pe in
          let s = {(s) with pstr_desc = Pstr_module {(pm) with pmb_expr = pe}} in
          let ct = AD.ptyp_constr ~loc {txt = Ldot (Lident mod_name, type_name); loc} [] in
          let td = AD.type_declaration ~loc ~name:{txt = type_name; loc}
            ~params:[] ~cstrs:[] ~kind:(type_kind_by_mod_expr pe) ~private_:Public ~manifest:(Some ct)
          in
          let pt = Ast_builder.Default.pstr_type ~loc Recursive [td] in
          s :: 
          pt :: 
          (List.append (gen_bin_funcs ~loc type_name mod_name) strs)
        else s :: strs
      | _ -> s :: strs)
    | _ -> s :: strs
  ) s []

let () =
  Driver.register_transformation
    ~rules:[rule_vers]
    ~impl
    "vers"
