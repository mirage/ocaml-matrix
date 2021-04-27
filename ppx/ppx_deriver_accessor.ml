open Ppxlib
open Ast_builder.Default

let errorf ~loc fmt =
  Printf.ksprintf (Location.raise_errorf ~loc "ppx_accessor: %s") fmt

let get_fun ~loc type_name pld_name =
  value_binding ~loc
    ~pat:(ppat_var ~loc (Loc.make ~loc ("get_" ^ Loc.txt pld_name)))
    ~expr:
      (pexp_fun ~loc Nolabel None
         (ppat_var ~loc (Loc.make ~loc type_name))
         (pexp_field ~loc
            (pexp_ident ~loc (Loc.make ~loc (Lident type_name)))
            (Loc.make ~loc (Lident (Loc.txt pld_name)))))

let set_fun ~loc type_name pld_name =
  value_binding ~loc
    ~pat:(ppat_var ~loc (Loc.make ~loc ("set_" ^ Loc.txt pld_name)))
    ~expr:
      (pexp_fun ~loc Nolabel None
         (ppat_var ~loc (Loc.make ~loc type_name))
         (pexp_fun ~loc Nolabel None
            (ppat_var ~loc (Loc.make ~loc "v"))
            (pexp_record ~loc
               [
                 ( Loc.make ~loc (Lident (Loc.txt pld_name))
                 , pexp_ident ~loc (Loc.make ~loc (Lident "v")) )
               ]
               (Some (pexp_ident ~loc (Loc.make ~loc (Lident type_name)))))))

let set_fun_single ~loc pld_name =
  value_binding ~loc
    ~pat:(ppat_var ~loc (Loc.make ~loc ("set_" ^ Loc.txt pld_name)))
    ~expr:
      (pexp_fun ~loc Nolabel None (ppat_any ~loc)
         (pexp_fun ~loc Nolabel None
            (ppat_var ~loc (Loc.make ~loc "v"))
            (pexp_record ~loc
               [
                 ( Loc.make ~loc (Lident (Loc.txt pld_name))
                 , pexp_ident ~loc (Loc.make ~loc (Lident "v")) )
               ]
               None)))

let get_longident = function Lident s -> s | _ -> "%%ERROR%%"

let to_label_or_opt pld_name {ptyp_desc; _} =
  match ptyp_desc with
  | Ptyp_constr ({txt= Lident "option"; _}, _) -> Optional (Loc.txt pld_name)
  | _ -> Labelled (Loc.txt pld_name)

let create_fun ~loc labels =
  value_binding ~loc
    ~pat:(ppat_var ~loc (Loc.make ~loc "make"))
    ~expr:
      (List.fold_right
         (fun {pld_name; pld_type; _} exp ->
           pexp_fun ~loc
             (to_label_or_opt pld_name pld_type)
             None
             (ppat_var ~loc (Loc.make ~loc (Loc.txt pld_name)))
             exp)
         labels
         (pexp_fun ~loc Nolabel None
            (ppat_construct ~loc (Loc.make ~loc (Lident "()")) None)
            (pexp_record ~loc
               (List.map
                  (fun {pld_name; _} ->
                    ( Loc.make ~loc (Lident (Loc.txt pld_name))
                    , pexp_ident ~loc
                        (Loc.make ~loc (Lident (Loc.txt pld_name))) ))
                  labels)
               None)))

let from_label ~loc ~single type_name {pld_name; _} =
  if single then [get_fun ~loc type_name pld_name; set_fun_single ~loc pld_name]
  else [get_fun ~loc type_name pld_name; set_fun ~loc type_name pld_name]

let from_abstract ~loc ptype_manifest =
  match ptype_manifest with
  | Some {ptyp_desc= Ptyp_constr ({txt= Lident "unit"; _}, []); _} ->
    value_binding ~loc
      ~pat:(ppat_var ~loc (Loc.make ~loc "make"))
      ~expr:
        (pexp_fun ~loc Nolabel None
           (ppat_construct ~loc (Loc.make ~loc (Lident "()")) None)
           (pexp_construct ~loc (Loc.make ~loc (Lident "()")) None))
  | _ -> errorf ~loc "unhandled type kind"

let from_td ~loc {ptype_name; ptype_kind; ptype_loc; ptype_manifest; _} =
  let type_name = Loc.txt ptype_name in
  let values =
    match ptype_kind with
    | Ptype_record labels ->
      create_fun ~loc labels
      ::
      List.flatten
        (List.map
           (from_label ~loc ~single:(List.length labels = 1) type_name)
           labels)
    | Ptype_abstract -> [from_abstract ~loc ptype_manifest]
    | _ -> errorf ~loc:ptype_loc "unhandled type kind" in
  pstr_value ~loc Nonrecursive values

let expand ~loc ~path:_ (_, tds) = List.map (from_td ~loc) tds
let str_type_decl_generator = Deriving.Generator.make_noarg expand

let accessor_deriver =
  Deriving.add ~str_type_decl:str_type_decl_generator "accessor"
