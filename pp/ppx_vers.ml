(* ocamlc -dsource _build/default/tests.ml *)
(*ignore(Location.raise_errorf ~loc "this is an %s messapge" "error")*)
(*Printf.printf "%s\n\n" (Pprintast.string_of_structure [s]);*)
(*ocamlfind ppx_tools/dumpast -e '(match p with `First x -> `First x)'*)

open Ppxlib

module AD = Ast_builder.Default

let vers_set = "migrate"  
let vers = "vers"
let vers_num = "num"
let vers_ptag = "ptag"
let vers_novers = "novers"
let variant_int_size = 4
let bin_io = "bin_io"
let from_novers = "from_novers"
let novers_field_name = "novers_field"
let allowed_inner_attrs = [
  ("deriving", bin_io); 
  ("deriving", "sexp"); 
  ("deriving", "sexp_of");
  ("deriving", "of_sexp");
  (vers_ptag,""); 
  (vers_num, "");
  (vers_novers, "");
  (vers_set, "")
] 

module SD = struct (* str_descr*)

  type str_attrs = ((string * string) * attribute) list

  type str_descr = 
    {
      type_name: string;
      first_ver: int;
      cnt: int ref;
      prev_td: type_declaration;
    }
  
  let h_strs: (string, str_descr) Hashtbl.t = Hashtbl.create 0
  let h_attrs: (string, str_attrs) Hashtbl.t = Hashtbl.create 0 
  
  let __init ~loc type_name prev_td =
    try    
      let _ = Hashtbl.find h_strs type_name in
      assert false
    with | Not_found -> 
      let cnt =
        try
          let attr = List.find (fun a -> a.attr_name.txt = vers_num) prev_td.ptype_attributes in   
          (match attr.attr_payload with
          | PStr [s] -> 
            (match s.pstr_desc with
            | Pstr_eval ({pexp_desc = Pexp_constant (Pconst_integer (s, None)); _}, _) -> int_of_string s            
            | _ -> assert false)  
          | _ -> assert false)  
        with Not_found -> 0    
      in  
      { type_name; cnt = ref cnt; first_ver = cnt; prev_td }   

  let init ~loc type_name last_td =
    try    
      let descr = Hashtbl.find h_strs type_name in
      (!(descr.cnt), descr.first_ver)
    with | Not_found -> 
      let descr = __init ~loc type_name last_td in 
      Hashtbl.add h_strs type_name descr;
      (!(descr.cnt), descr.first_ver)  

  let get type_name =
    Hashtbl.find h_strs type_name 

  let exists = Hashtbl.mem h_strs

  let incr type_name prev_td =
    try    
      let descr = Hashtbl.find h_strs type_name in
      let descr = {(descr) with prev_td} in
      Hashtbl.replace h_strs type_name descr;
      incr descr.cnt 
    with | Not_found -> assert false      

  let add_attr a type_name =
    let a_name = a.attr_name.txt in
    let append new_desc a attrs = 
      if List.mem_assoc new_desc attrs
      then attrs      
      else (new_desc, a) :: attrs
    in
    let add_descr_attr a attrs = 
      match a.attr_payload with
      | PStr s ->
        if s = []
        then append (a_name, "") a attrs
        else 
          let hd_s = List.hd s in
          (match hd_s.pstr_desc with
          | Pstr_eval (e, p) -> 
            (match e.pexp_desc with
            | Pexp_ident i ->
              append (a_name, Longident.name i.txt) a attrs
            | Pexp_tuple ex when a_name = "deriving" ->
              List.fold_right (fun e attrs ->
                (match e.pexp_desc with
                | Pexp_ident i ->                   
                  let e = {(e) with pexp_desc = Pexp_ident i} in
                  let hd_s = {(hd_s) with pstr_desc = Pstr_eval (e, p)} in
                  let s = hd_s :: List.tl s in
                  let a = {(a) with attr_payload = PStr s} in
                  append (a_name, Longident.name i.txt) a attrs
                | _ -> assert false)
              ) ex attrs
            | _ -> append (a_name, "") a attrs)
          | _ -> append (a_name, "") a attrs)
      | _ -> append (a_name, "") a attrs
    in    
   (*let () = Printf.printf "%s %s %s \n" type_name a.attr_name.txt descr in*)
    try
      let attrs = Hashtbl.find h_attrs type_name in
      Hashtbl.replace h_attrs type_name (add_descr_attr a attrs)
    with Not_found -> 
      Hashtbl.add h_attrs type_name (add_descr_attr a [])

  let add_attrs attrs type_name =
    List.iter (fun a ->
      add_attr a type_name
    ) attrs   

  let group_deriving attrs =
    let rec loop attrs lx dx dlx =
      match attrs with
      | [] -> 
        if dx = []
        then lx
        else
          let loc = (List.hd dlx).attr_loc in
          let ex = List.map (fun d -> AD.pexp_ident ~loc {txt = Longident.parse d; loc}) dx in 
          let ex = AD.pexp_tuple ~loc ex  in
          let s = AD.pstr_eval ~loc ex [] in
          let d_a = AD.attribute ~loc ~name: {txt = "deriving"; loc} ~payload:(PStr [s]) in
          d_a :: lx 
      | ((name, descr), a) :: tl -> 
        if name = "deriving"
        then loop tl lx (descr :: dx) (a :: dlx)
        else loop tl (a :: lx) dx dlx
    in
    loop attrs [] [] [] 

  let exists_attr name descr type_name =
    try
      let attrs = Hashtbl.find h_attrs type_name in
      List.exists (fun ((aname, adescr), _) ->
        aname = name && adescr = descr
      ) attrs
    with Not_found -> false

  let get_all_attrs ~excl type_name =
    try 
      List.fold_right (fun (descr, a) attrs -> 
        if List.mem descr excl
        then attrs
        else (descr, a) :: attrs
      ) (Hashtbl.find h_attrs type_name) [] |> group_deriving
    with Not_found -> []      

  let get_attrs ~is_inner type_name =
    try 
      List.fold_right (fun (descr, a) attrs -> 
        if is_inner 
        then 
          (if List.mem descr allowed_inner_attrs 
          then 
            (descr, a) :: attrs
          else attrs)
        else 
          (if not (List.mem descr allowed_inner_attrs)          
          then (descr, a) :: attrs
          else attrs)
      ) (Hashtbl.find h_attrs type_name) [] |> group_deriving
    with Not_found -> []      

end

let mod_name_by_cnt name cnt =
  Printf.sprintf "V%d_%s" cnt name

let data_by_mod_name mod_name =
  try
    let (ver, name) = Scanf.sscanf mod_name "V%d%s" (fun v t -> (v, t))  in
    Some (ver, name)  
  with | _ -> None    

let remove_attr name attrs = 
  List.fold_right (fun a attrs ->
    if a.attr_name.txt = name
    then attrs
    else a :: attrs  
  ) attrs []

let attr_by_name name attrs =
  if attrs = []
  then None
  else     
    try
      let attr = List.find (fun a -> a.attr_name.txt = name) attrs in
      Some attr
    with Not_found -> None 

let exists_attr name attrs =
  attr_by_name name attrs <> None    

let vers_set_payload attrs =
  match attr_by_name vers_set attrs with
  | Some attr -> Some attr.attr_payload
  | None -> None

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
        let lid = {txt = Longident.parse ld.pld_name.txt; loc} in
        let efield = 
          match vers_set_payload ld.pld_attributes with
          | Some payload -> expr_by_payload payload
          | _ -> AD.pexp_field ~loc [%expr p] {txt = Longident.parse ("Prev." ^ ld.pld_name.txt); loc}
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
      then match (SD.get type_name).prev_td.ptype_kind with Ptype_variant cdx -> cdx | _ -> assert false
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
            (AD.ppat_construct ~loc {txt = Longident.parse ("Prev." ^ tag_name); loc} (if exists_constr then Some(AD.ppat_var ~loc {txt = "x"; loc}) else None),
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
  | PStr s ->
    let first_struct = List.hd s in 
    let first_struct =
      match first_struct.pstr_desc with
      | Pstr_type (_, td) when List.length td = 1 ->  
        let hd_td = List.hd td in
        let type_name = hd_td.ptype_name.txt in
        let (cnt, first_ver) = SD.init ~loc type_name hd_td in
        let mod_name = mod_name_by_cnt type_name cnt in
        let sts = [first_struct] in
        let sts =
          if cnt > first_ver
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
        let () = SD.incr type_name hd_td in
        AD.pstr_module ~loc mb
      | Pstr_type _ -> failwith "Only one typedef in type definition"  
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

let gen_app_funcs ~loc type_name mod_name pe =
  (*let () = Pprintast.module_expr Format.std_formatter pe in*)
  match pe.pmod_desc with
  | Pmod_structure sx ->
    (*let () = Printf.printf "%s\n\n" (Pprintast.string_of_structure sx) in*)
    List.fold_right (fun s sx ->
      match s.pstr_desc with
      | Pstr_include i -> 
        let pm = i.pincl_mod in
        (match pm.pmod_desc with
        | Pmod_structure isx ->
          let isx = 
            List.fold_right (fun s isx ->
              (match s.pstr_desc with
              | Pstr_value (_, vbx) ->
                List.fold_right (fun vb isx ->
                  (match vb.pvb_pat.ppat_desc with
                  | Ppat_var l ->
                    let f_name = l.txt in
                    let f_name_l = AD.pvar ~loc f_name in
                    let e_r = AD.pexp_ident ~loc {txt = Longident.parse (mod_name ^ "." ^ f_name); loc} in
                    [%stri let [%p f_name_l] = [%e e_r]] ::
                    isx
                  | _ -> isx)
                ) vbx isx                      
              | _ -> isx)
            ) isx []
          in
          List.append isx sx
        | _ -> sx)
      | Pstr_value _ ->
        sx  
      | _ -> sx
    ) sx []
  | _ -> [] 

let gen_last_write_fun ~loc type_name ver =  
  let f_name = "bin_write_" ^ type_name in
  let f_name_l = AD.pvar ~loc f_name in
  let ever = AD.eint ~loc ver in
  let e_f_name = AD.pexp_ident ~loc {txt = Lident f_name; loc} in
  [%stri let [%p f_name_l] = fun buf -> fun ~pos -> fun v ->
    let pos = Bin_prot.Write.bin_write_int_8bit buf ~pos [%e ever] in  
    [%e e_f_name] buf ~pos v
  ] 

let gen_last_size_fun ~loc type_name =  
  let f_name = "bin_size_" ^ type_name in
  let f_name_l = AD.pvar ~loc f_name in
  let e_f_name = AD.pexp_ident ~loc {txt = Lident f_name; loc} in
  [%stri let [%p f_name_l] = fun t -> [%e e_f_name] t + 1 ] 

let gen_last_read_fun ~loc type_name cur_ver first_ver =    
  let f_name = "bin_read_" ^ type_name in
  let f_name_l = AD.pvar ~loc f_name in
  let ever = AD.eint ~loc cur_ver in
  let e_f_name = AD.pexp_ident ~loc {txt = Lident f_name; loc} in
  let fail_case = 
    let e_file = AD.estring ~loc loc.loc_start.Stdlib.Lexing.pos_fname in
    let e_type = AD.estring ~loc type_name in
    AD.case [%pat? v] None [%expr failwith (Printf.sprintf "Unknown VERS %s.%s:%d" [%e e_file] [%e e_type] v)] 
  in
  let rec loop cases ver ex =
    if ver < first_ver
    then cases 
    else
      let p = AD.pint ~loc ver in
      let e_res =
        let ex = List.rev ex in
        let e_first =
          let txt = if ver = cur_ver then Lident (List.hd ex) else Ldot (Lident (mod_name_by_cnt type_name ver), List.hd ex) in
          let e_f_name = AD.pexp_ident ~loc {txt; loc} in
          [%expr [%e e_f_name]  buf ~pos_ref]  
        in
        let rec eloop cnt ex e =
          match ex with
          | [] -> e
          | h :: tl -> 
            let txt = if tl = [] then Lident "upgrade" else Ldot (Lident (mod_name_by_cnt type_name (cnt + 1)), h) in
            let e_upg = AD.pexp_ident ~loc {txt; loc} in 
            let e = [%expr [%e e_upg] [%e e]] in
            eloop (cnt + 1) tl e
        in
        eloop ver (List.tl ex) e_first
      in  
      let case = AD.case [%pat? [%p p]] None e_res in
      loop (case :: cases) (ver - 1) ("upgrade" :: ex)
  in
  let cases = fail_case :: loop [] cur_ver [f_name] |> List.rev in
  let m = AD.pexp_match ~loc [%expr Bin_prot.Read.bin_read_int_8bit buf ~pos_ref] cases in
  [%stri let [%p f_name_l] = fun buf -> fun ~pos_ref ->
    [%e m] 
  ] 

let include_to_end pe =
  match pe.pmod_desc with
  | Pmod_structure sx -> 
    let (opt_inc, sx) = 
      List.fold_right (fun s (opt_inc, sx) ->
        match s.pstr_desc  with
        | Pstr_include _ -> (Some s, sx)
        | _ -> (opt_inc, s :: sx)
      ) sx (None, [])
    in 
    (match opt_inc with
    | Some inc ->
      let sx = List.append sx [inc] in
      {(pe) with pmod_desc = Pmod_structure sx }
    | None -> pe)
  | _ -> pe

let patch_ptag_bin_size ~loc type_kind s =
  match s.pstr_desc with
  | Pstr_value (Nonrecursive, [vb]) ->
    let se = vb.pvb_expr in
    (match se.pexp_desc with
    | Pexp_function pf_cx ->  
      let pf_cx = 
        List.mapi (fun i c ->
          let new_const = AD.pexp_constant ~loc (Pconst_integer (string_of_int variant_int_size, None)) in 
          match c.pc_rhs.pexp_desc with
          | Pexp_constant (Pconst_integer ("1", None)) -> 
            let c = {(c) with pc_rhs = new_const} in
            c
          | Pexp_let (Nonrecursive, vbx, pl_e) ->  
            let (hd_vb, tl_vb) = (List.hd vbx, List.tl vbx) in
            let hd_vb = {(hd_vb) with pvb_expr = new_const} in
            let c = {(c) with pc_rhs = {(c.pc_rhs) with pexp_desc = Pexp_let (Nonrecursive, hd_vb :: tl_vb, pl_e)}} in
            c
          | _ -> assert false   
        ) pf_cx
      in  
      let se = {(se) with pexp_desc = Pexp_function pf_cx} in
      let vb = {(vb) with pvb_expr = se} in
      {(s) with pstr_desc = Pstr_value (Nonrecursive, [vb])}
    | _ -> assert false)
  | Pstr_value _ -> assert false      
  | _ -> s      

let patch_ptag_bin_write ~loc type_kind s =
  (* Bin_prot.Write.bin_write_variant_int *)  
  match s.pstr_desc with
  | Pstr_value (Nonrecursive, [vb]) ->
    let se = vb.pvb_expr in
    (match se.pexp_desc with
    | Pexp_fun (Nolabel, None, pf_p0, pf_e0) ->  
      (match pf_p0.ppat_desc with
      | Ppat_var l when l.txt = "buf" ->
        (match pf_e0.pexp_desc with
        | Pexp_fun (pf_l1, None, pf_p1, pf_e1) -> 
          (match pf_e1.pexp_desc with
          | Pexp_function pf_cx ->
            let cdx = match type_kind with | Ptype_variant cdx -> cdx | _ -> assert false in 
            let cdx_hv = List.map (fun cd -> Ocaml_common.Btype.hash_variant cd.pcd_name.txt) cdx in
            let cdx_len = List.length cdx in
            (*let () = Printf.printf "%s\n\n" (Pprintast.string_of_expression se) in*)
            let pf_cx = 
              List.mapi (fun i c ->
                (*let () = Printf.printf "%s\n" (Pprintast.string_of_expression c.pc_rhs) in*)
                let new_s = string_of_int (List.nth cdx_hv i) in
                let new_const = AD.pexp_constant ~loc (Pconst_integer (new_s, None)) in 
                let pexp_desc = [%expr Bin_prot.Write.bin_write_variant_int buf ~pos [%e new_const]] in
                match c.pc_rhs.pexp_desc with
                | Pexp_apply _ -> 
                  let c = {(c) with pc_rhs = pexp_desc} in
                  (*let () = Printf.printf "---%s\n" (Pprintast.string_of_expression c.pc_rhs) in*)
                  c
                | Pexp_let (Nonrecursive, vbx, pl_e) ->  
                  let (hd_vb, tl_vb) = (List.hd vbx, List.tl vbx) in
                  let hd_vb = {(hd_vb) with pvb_expr = pexp_desc} in
                  let c = {(c) with pc_rhs = {(c.pc_rhs) with pexp_desc = Pexp_let (Nonrecursive, hd_vb :: tl_vb, pl_e)}} in
                  (*let () = Printf.printf "---%s\n" (Pprintast.string_of_expression c.pc_rhs) in*)
                  c
                | _ -> assert false               
              ) pf_cx
            in  
            let pf_e1 = {(pf_e1) with pexp_desc = Pexp_function pf_cx} in
            let pf_e0 = {(pf_e0) with pexp_desc = Pexp_fun (pf_l1, None, pf_p1, pf_e1)} in
            let se = {(se) with pexp_desc = Pexp_fun (Nolabel, None, pf_p0, pf_e0)} in
            (*let () = Printf.printf "%s\n" (Pprintast.string_of_expression se) in*)
            let vb = {(vb) with pvb_expr = se} in
            {(s) with pstr_desc = Pstr_value (Nonrecursive, [vb])}
          | _ -> assert false)
        | _ -> assert false)            
      | _ -> assert false)          
    | _ -> assert false)
  | Pstr_value _ -> assert false      
  | _ -> s      

let patch_ptag_bin_read ~loc type_kind s =
  match s.pstr_desc with
  | Pstr_value (Nonrecursive, [vb]) ->
    let se = vb.pvb_expr in
    (match se.pexp_desc with
    | Pexp_fun (Nolabel, None, pf_p0, pf_e0) ->  
      (match pf_p0.ppat_desc with
      | Ppat_var l when l.txt = "buf" ->
        (match pf_e0.pexp_desc with
        | Pexp_fun (pf_l1, None, pf_p1, pf_e1) -> 
          (match pf_e1.pexp_desc with
          | Pexp_match (pm_e, pm_cx) ->
            let cdx = match type_kind with | Ptype_variant cdx -> cdx | _ -> assert false in 
            let cdx_hv = List.map (fun cd -> Ocaml_common.Btype.hash_variant cd.pcd_name.txt) cdx in
            let cdx_len = List.length cdx in
            let pm_cx =
              List.mapi (fun i c ->
                if i < cdx_len
                then  
                  match c.pc_lhs.ppat_desc with
                  | Ppat_constant (Pconst_integer (s, co)) -> 
                    let new_s = string_of_int (List.nth cdx_hv i) in
                    {(c) with pc_lhs = {(c.pc_lhs) with ppat_desc = Ppat_constant (Pconst_integer (new_s, co))}}
                  | _ -> assert false                 
                else c                  
              ) pm_cx
            in
            let pf_e1 = {(pf_e1) with pexp_desc = Pexp_match (pm_e, pm_cx)} in
            let pf_e0 = {(pf_e0) with pexp_desc = Pexp_fun (pf_l1, None, pf_p1, pf_e1)} in
            let se = {(se) with pexp_desc = Pexp_fun (Nolabel, None, pf_p0, pf_e0)} in
            (*let () = Printf.printf "%s\n" (Pprintast.string_of_expression se) in*)
            let vb = {(vb) with pvb_expr = se} in
            {(s) with pstr_desc = Pstr_value (Nonrecursive, [vb])}
          | _ -> assert false)
        | _ -> assert false)            
      | _ -> assert false)          
    | _ -> assert false)
  | Pstr_value _ -> assert false      
  | _ -> s      

let is_type_variant type_kind =
  match type_kind with
  | Ptype_variant _ -> true
  | _ -> false 

let patch_ptag ~loc p type_name type_kind s isx =   
  (match p.ppat_desc with
  | Ppat_var l when l.txt = "bin_read_" ^ type_name && is_type_variant type_kind ->
    patch_ptag_bin_read ~loc type_kind s :: (*s ::*) isx  
  | Ppat_var l when l.txt = "bin_write_" ^ type_name && is_type_variant type_kind ->
    patch_ptag_bin_write ~loc type_kind s :: (*s ::*) isx  
  | Ppat_var l when l.txt = "bin_size_" ^ type_name && is_type_variant type_kind ->
    patch_ptag_bin_size ~loc type_kind s :: s :: isx  
  | _ -> s :: isx)  

let patch_bin_io_incl ~loc incl type_name type_kind pm isx s =  
  let isx =
    List.fold_right (fun s isx ->
      (match s.pstr_desc with
      | Pstr_value (_, vbx) when vbx <> [] ->
        let hd_vbx = List.hd vbx in
        (match hd_vbx.pvb_pat.ppat_desc with
        | Ppat_constraint (p, _) -> patch_ptag ~loc p type_name type_kind s isx
        | _ -> s :: isx)
      | _ -> s :: isx)
    ) isx []
  in
  let pm = {(pm) with pmod_desc = Pmod_structure isx} in
  let incl = {(incl) with pincl_mod = pm} in
  {(s) with pstr_desc = Pstr_include incl}

let patch_bin_io_incl_last ~loc ver first_ver incl type_name type_kind pm isx s =  
  let isx =
    List.fold_right (fun s isx ->
      (match s.pstr_desc with
      | Pstr_value (_, vbx) when vbx <> [] ->
        let hd_vbx = List.hd vbx in
        (match hd_vbx.pvb_pat.ppat_desc with
        | Ppat_var l when l.txt = "bin_writer_" ^ type_name -> 
          gen_last_write_fun ~loc type_name ver :: s :: isx
        | Ppat_var l when l.txt = "bin_reader_" ^ type_name -> 
          gen_last_read_fun ~loc type_name ver first_ver :: s :: isx
        | Ppat_var l when l.txt = "bin_size_" ^ type_name -> 
          s :: gen_last_size_fun ~loc type_name  :: isx              
        | Ppat_constraint (p, _) -> 
          if SD.exists_attr "ptag" "" type_name 
          then  patch_ptag ~loc p type_name type_kind s isx
          else
            (match p.ppat_desc with
            | Ppat_var l when l.txt = "bin_size_" ^ type_name ->
              s :: gen_last_size_fun ~loc type_name  :: isx              
            | _ -> s :: isx)
        | _ -> s :: isx)
      | _ -> s :: isx)
    ) isx []
  in
  let pm = {(pm) with pmod_desc = Pmod_structure isx} in
  let incl = {(incl) with pincl_mod = pm} in
  {(s) with pstr_desc = Pstr_include incl}

let patch_module_expr ~loc type_name type_kind pe =
  match pe.pmod_desc with
  | Pmod_structure sx -> 
    let sx =
      List.fold_right (fun s sx ->
        match s.pstr_desc with
        | Pstr_include i -> 
          let pm = i.pincl_mod in
          (match pm.pmod_desc with
          | Pmod_structure isx -> patch_bin_io_incl ~loc i type_name type_kind pm isx s :: sx
          | _ -> s :: sx)
        | _ -> s :: sx
      ) sx []
    in
    {(pe) with pmod_desc = Pmod_structure sx}
  | _ -> pe  

let patch_module_expr_last ~loc type_name ver first_ver type_kind pe =
  match pe.pmod_desc with
  | Pmod_structure sx -> 
    let sx =
      List.fold_right (fun s sx ->
        match s.pstr_desc with
        | Pstr_include i -> 
          let pm = i.pincl_mod in
          (match pm.pmod_desc with
          | Pmod_structure isx -> patch_bin_io_incl_last ~loc ver first_ver i type_name type_kind pm isx s :: sx
          | _ -> s :: sx)
        | _ -> s :: sx
      ) sx []
    in
    {(pe) with pmod_desc = Pmod_structure sx}
  | _ -> pe  

let mkattr_vers_num ~loc num =
  let const = AD.pexp_constant ~loc (Pconst_integer (num, None)) in
  let payload = PStr [AD.pstr_eval ~loc const []] in
  AD.attribute ~loc ~name:{txt = vers_num; loc } ~payload

let mkattr_bin_io ~loc =
  let id = AD.pexp_ident ~loc {txt = Longident.parse bin_io; loc} in
  let payload = PStr [AD.pstr_eval ~loc id []] in
  AD.attribute ~loc ~name:{txt = "deriving"; loc } ~payload

let gen_novers_first_type ~loc type_name td =
  match td.ptype_kind with
  | Ptype_variant _cdx ->
    let ct = AD.ptyp_constr ~loc {txt = Longident.parse (type_name ^ "_" ^ vers_novers); loc} [] in
    AD.type_declaration ~loc ~name:{txt = type_name; loc} ~params:[] ~cstrs:[] ~kind:Ptype_abstract ~private_:Public 
      ~manifest:(Some ct)     
  | Ptype_record ldx ->
    let ct = AD.ptyp_constr ~loc {txt = Longident.parse (type_name ^ "_" ^ vers_novers); loc} [] in 
    let ld = AD.label_declaration ~loc ~name:{txt = vers_novers ^ "_field"; loc} ~mutable_:Immutable ~type_:ct in
    let kind = Ptype_record [ld] in
    AD.type_declaration ~loc ~name:{txt = type_name; loc} ~params:[] ~cstrs:[] ~kind ~private_:Public ~manifest:None     
  | Ptype_abstract -> 
    (match td.ptype_manifest with
    | Some {ptyp_desc = Ptyp_variant (rfx, clf, lx_opt); _} -> 
      let ct = AD.ptyp_constr ~loc {txt = Longident.parse (type_name ^ "_" ^ vers_novers); loc} [] in
      AD.type_declaration ~loc ~name:{txt = type_name; loc} ~params:[] ~cstrs:[] ~kind:Ptype_abstract ~private_:Public 
        ~manifest:(Some ct)           
    | Some _ ->       
      let ct = AD.ptyp_constr ~loc {txt = Longident.parse (type_name ^ "_" ^ vers_novers); loc} [] in
      AD.type_declaration ~loc ~name:{txt = type_name; loc} ~params:[] ~cstrs:[] ~kind:Ptype_abstract ~private_:Public 
        ~manifest:(Some ct)     
    | _ -> assert false)
  | _ -> assert false

let gen_novers_second_type ~loc type_name td =
  match td.ptype_kind with
  | Ptype_variant cdx ->
    let cdx =
      List.map (fun cd ->
        let (p, e) =
          let exists_constr = match cd.pcd_args with Pcstr_tuple ctx -> ctx <> [] | Pcstr_record lbdx -> lbdx <> [] in
          if exists_constr
          then
            let p = AD.ppat_construct ~loc {txt = Longident.parse cd.pcd_name.txt; loc} (Some (AD.ppat_var ~loc {txt = "x"; loc})) in
            let pct = AD.ptyp_constr ~loc {txt = Longident.parse ("Prev." ^ type_name); loc} [] in
            (AD.ppat_constraint ~loc p pct,
            AD.pexp_construct ~loc {txt = Lident cd.pcd_name.txt; loc} (Some (AD.pexp_ident ~loc {txt = Lident "x"; loc})))
          else   
            let p = AD.ppat_construct ~loc {txt = Longident.parse cd.pcd_name.txt; loc} None in
            let pct = AD.ptyp_constr ~loc {txt = Longident.parse ("Prev." ^ type_name); loc} [] in
            (AD.ppat_constraint ~loc p pct,
            AD.pexp_construct ~loc {txt = Lident cd.pcd_name.txt; loc} None)
        in
        let attr = AD.attribute ~loc ~name:{txt = vers_set; loc} ~payload:(PPat (p, Some e)) in
        {(cd) with pcd_attributes = attr :: cd.pcd_attributes}          
      ) cdx
    in 
    {(td) with ptype_kind = Ptype_variant cdx}
  | Ptype_record ldx -> 
    let ldx =
      List.map (fun ld ->
        let p = AD.pexp_ident ~loc {txt = Longident.parse "p"; loc} in
        let prev = AD.pexp_field ~loc p {txt = Longident.parse ("Prev." ^ novers_field_name); loc} in 
        let pe = AD.pexp_field ~loc prev {txt = Longident.parse ld.pld_name.txt; loc} in
        let ps = AD.pexp_apply ~loc (AD.pexp_ident ~loc {txt = Longident.parse "enmp_from_enmp_novers"; loc}) [(Nolabel, pe)] in
        let s = AD.pstr_eval ~loc (if exists_attr from_novers ld.pld_attributes then ps else pe) [] in
        let attr = AD.attribute ~loc ~name:{txt = vers_set; loc} ~payload:(PStr [s]) in
        {(ld) with pld_attributes = attr :: ld.pld_attributes}
      ) ldx 
    in
    let ct = AD.ptyp_constr ~loc {txt = Longident.parse (type_name ^ "_" ^ vers_novers); loc} [] in 
    let lb = AD.label_declaration ~loc ~name:{txt = vers_novers; loc} ~mutable_:Immutable ~type_:ct in
    let kind = Ptype_record ldx in
    AD.type_declaration ~loc ~name:{txt = type_name; loc} ~params:[] ~cstrs:[] ~kind ~private_:Public ~manifest:None     
  | Ptype_abstract -> 
    (match td.ptype_manifest with
    | Some ({ptyp_desc = Ptyp_variant (rfx, clf, lx_opt); _} as ct) ->
      let rfx =
        List.map (fun rf ->
          let (p, e) =
            let (tag_name, exists_constr) = match rf.prf_desc with Rtag (l, _, ctlx) -> (l.txt, ctlx <> []) | Rinherit _ -> assert false in
            (AD.ppat_variant ~loc tag_name (if exists_constr then Some(AD.ppat_var ~loc {txt = "x"; loc}) else None),
              AD.pexp_variant ~loc tag_name (if exists_constr then Some(AD.pexp_ident ~loc {txt = Lident "x"; loc}) else None))
          in
          let attr = AD.attribute ~loc ~name:{txt = vers_set; loc} ~payload:(PPat (p, Some e)) in
          {(rf) with prf_attributes = attr :: rf.prf_attributes}          
        ) rfx 
      in 
      {(td) with ptype_manifest = Some {(ct) with ptyp_desc = Ptyp_variant (rfx, clf, lx_opt)}}      
    | Some ct -> 
      (*let ct = AD.ptyp_constr ~loc {txt = Longident.parse (type_name ^ "_" ^ vers_novers); loc} [] in*)
      let td = AD.type_declaration ~loc ~name:{txt = type_name; loc} ~params:[] ~cstrs:[] ~kind:Ptype_abstract ~private_:Public 
        ~manifest:(Some ct)     
      in
      let s = AD.pstr_eval ~loc [%expr p] [] in
      let attr = AD.attribute ~loc ~name:{txt = vers_set; loc} ~payload:(PStr [s]) in
      {(td) with ptype_attributes = attr :: td.ptype_attributes}     
    | _ -> assert false)
  | _ -> assert false

let ct_to_novers ct =
  let longident_to_novers l =
    let txt =
      match l.txt with
      | Lident type_name -> Lident (type_name ^ "_" ^ vers_novers)
      | Ldot (l, type_name) -> Ldot (l, type_name ^ "_" ^ vers_novers)
      | Lapply _ -> assert false
    in 
    {(l) with txt}      
  in   
  let rec loop ct =
    let ptyp_desc =
      match ct.ptyp_desc with
      | Ptyp_constr (l, cx) -> Ptyp_constr (longident_to_novers l, List.map loop cx)
      | _ -> assert false
    in
    {(ct) with ptyp_desc}  
  in        
  loop ct

let gen_novers ~loc type_name rf td attrs =     
  let td_novers = {(td) with ptype_name = {txt = type_name ^ "_" ^ vers_novers; loc}; ptype_attributes = attrs} in
  let td_novers =
    match td_novers.ptype_kind with    
    | Ptype_record ldx ->
      let ldx =
        List.map (fun ld -> 
          if exists_attr from_novers ld.pld_attributes
          then {(ld) with pld_attributes = remove_attr from_novers ld.pld_attributes; pld_type = ct_to_novers ld.pld_type}
          else ld   
        ) ldx 
      in
      {(td_novers) with ptype_kind = Ptype_record ldx}
    | _ -> td_novers
  in  
  let novers_type = AD.pstr_type ~loc rf [td_novers] in
  let first_type = gen_novers_first_type ~loc type_name td in
  let second_type = gen_novers_second_type ~loc type_name td in
  ([novers_type], [first_type;  second_type])

let preprocess_impl sx =
  List.fold_right (fun s strs ->
    match s.pstr_desc with
    | Pstr_extension ((e, payload), ax) when e.txt = vers ->
      let loc = s.pstr_loc in
      (match payload with
      | PStr [sf] ->  
        (match sf.pstr_desc with
        | Pstr_type (rf, td) when td <> [] ->
          let hd_td = List.hd td in
          let type_name = hd_td.ptype_name.txt in
          (*let () = Printf.printf "%s\n" (Pprintast.string_of_structure [sf]) in*)
          let last_i = List.length td - 1 in
          let all_attrs = List.append hd_td.ptype_attributes (List.nth td last_i).ptype_attributes  in
          let () = SD.add_attrs all_attrs type_name in
          let ins_attrs = SD.get_all_attrs ~excl:[(vers_set, "")] type_name in
          let is_novers = List.exists (fun a -> a.attr_name.txt = vers_novers) hd_td.ptype_attributes in
          let (novers_type, first_type) = if is_novers then gen_novers ~loc type_name rf hd_td ins_attrs else ([], [] ) in
          let pex =
            List.mapi (fun i t ->
              let ins_attrs =
                match attr_by_name vers_set t.ptype_attributes with
                | Some attr -> attr :: ins_attrs
                | None -> ins_attrs
              in
              let t = {(t) with ptype_attributes = ins_attrs} in
              let pstr_desc = Pstr_type (rf, [t]) in
              let payload = PStr [{(sf) with pstr_desc}] in
              let pstr_desc = Pstr_extension ((e, payload), ax) in
              (*let () = Printf.printf "%s\n" (Pprintast.string_of_structure [{(s) with pstr_desc}]) in*)
              {(s) with pstr_desc} 
            ) (if is_novers then (List.append first_type (List.tl td)) else td) 
          in
          List.append novers_type (List.append pex strs)
        | _ -> assert false)  
      | _ -> s :: strs)
    | _ -> s :: strs
  ) sx []

let impl sx =
  let novers_upg = ref None in
  List.fold_left (fun strs s ->
    let loc = s.pstr_loc in
    match s.pstr_desc with
    | Pstr_module ({pmb_name = {txt = Some(mod_name); _}; pmb_expr = pe; _} as pm) ->      
      (match data_by_mod_name mod_name with
      | Some (ver, type_name) when SD.exists type_name ->
        let pe = include_to_end pe in
        let kind = type_kind_by_mod_expr pe in
        let descr = SD.get type_name in
        let cur_ver = !(descr.cnt) - 1 in
        if cur_ver = ver
        then 
          let pe = patch_module_expr_last ~loc type_name ver descr.first_ver kind pe in
          let s = {(s) with pstr_desc = Pstr_module {(pm) with pmb_expr = pe}} in
          let ct = AD.ptyp_constr ~loc {txt = Longident.parse (mod_name ^ "." ^ type_name); loc} [] in
          let td = AD.type_declaration ~loc ~name:{txt = type_name; loc}
            ~params:[] ~cstrs:[] ~kind ~private_:Public ~manifest:(Some ct)
          in
          let td = {(td) with ptype_attributes = SD.get_attrs false type_name} in
          let app_funcs = gen_app_funcs ~loc type_name mod_name pe in                            
          let novers_upg_fun =
            match !novers_upg with
            | Some (tn, sx) -> 
              [sx]
            | _ -> []
          in
          List.append novers_upg_fun
          (Ast_builder.Default.pstr_type ~loc Recursive [td] :: 
          (List.append app_funcs (s :: strs)))
        else 
          let pe = patch_module_expr ~loc type_name kind pe in
          {(s) with pstr_desc = Pstr_module {(pm) with pmb_expr = pe}} :: strs
      | _ -> s :: strs)
    | Pstr_type (rc, tdx) when tdx <> [] -> (* генерим <type>_from_<type>_novers ф-цию *)
      let h_td = List.hd tdx in
      if BatString.ends_with h_td.ptype_name.txt ("_" ^ vers_novers)
      then 
        let type_name = BatString.left h_td.ptype_name.txt (BatString.rfind h_td.ptype_name.txt "_") in
        let type_name_novers = type_name ^ "_" ^ vers_novers in
        let descr = SD.get type_name in
        let rec expr_upgrade cnt e =
          if cnt = 0
          then e
          else  
            let pe = AD.pexp_ident ~loc {txt = Longident.parse (Printf.sprintf "V%d_%s.upgrade" (!(descr.cnt) - cnt) type_name); loc} in
            let pfe = AD.pexp_apply ~loc pe [(Nolabel, e)] in
            expr_upgrade (cnt - 1) pfe
        in
        let gen_from_fun fe =
          let fun_name = AD.pvar ~loc (type_name ^ "_from_" ^ type_name ^ "_novers") in
          let arg = AD.ppat_constraint ~loc  (AD.pvar ~loc "x") (AD.ptyp_constr ~loc {txt = Lident type_name_novers; loc} []) in
          let pfe = AD.pexp_fun ~loc Nolabel None arg fe in
          let vb = AD.value_binding ~loc ~pat:fun_name ~expr:pfe in
          AD.pstr_value ~loc Nonrecursive [vb]
        in 
        match h_td.ptype_kind with
        | Ptype_record _ ->
          let first_expr = AD.pexp_record ~loc [({txt = Longident.parse novers_field_name; loc}, AD.evar ~loc "x")] None in
          let fe = expr_upgrade (!(descr.cnt) - 1) first_expr in
          let sx = gen_from_fun fe in
          let () = novers_upg :=  Some (type_name, sx) in
          s :: strs                    
        | Ptype_abstract when pvariant_by_type_declaration h_td <> None -> 
          let fe = expr_upgrade (!(descr.cnt) - 1) (AD.evar ~loc "x") in
          let sx = gen_from_fun fe in
          let () = novers_upg :=  Some (type_name, sx) in
          s :: strs          
        | Ptype_abstract ->  
          let fe = expr_upgrade (!(descr.cnt) - 1) (AD.evar ~loc "x") in
          let sx = gen_from_fun fe in
          let () = novers_upg :=  Some (type_name, sx) in
          s :: strs          
        | Ptype_variant _ ->  
          let fe = expr_upgrade (!(descr.cnt) - 1) (AD.evar ~loc "x") in
          let sx = gen_from_fun fe in
          let () = novers_upg :=  Some (type_name, sx) in
          s :: strs
        | _ -> s :: strs          
      else s :: strs
    | _ -> s :: strs
  ) [] sx |> List.rev

let () =
  Driver.register_transformation
    ~preprocess_impl
    ~rules:[rule_vers]
    ~impl
    vers
