open Ppxlib
open Ast_builder.Default

let errorf ~loc fmt =
  Printf.ksprintf (Location.raise_errorf ~loc "ppx_accessor: %s") fmt

let get_fun ~loc type_name pld_name pld_type params =
  value_description ~loc
    ~name:
      (Loc.make ~loc ("get_" ^ Loc.txt pld_name))
    ~type_:
      (ptyp_arrow ~loc Nolabel
        (ptyp_constr ~loc
          (Loc.make ~loc (Lident type_name))
          params)
        pld_type)
    ~prim:
      []

let set_fun ~loc type_name pld_name pld_type params =
  value_description ~loc
    ~name:
      (Loc.make ~loc ("set_" ^ Loc.txt pld_name))
    ~type_:
      (ptyp_arrow ~loc Nolabel
        (ptyp_constr ~loc
          (Loc.make ~loc (Lident type_name))
          params)
        (ptyp_arrow ~loc Nolabel
          pld_type
          (ptyp_constr ~loc
            (Loc.make ~loc (Lident type_name))
            params)))
    ~prim:
      []

let to_label_or_opt pld_name pld_type exp =
  match pld_type.ptyp_desc with
    | Ptyp_constr ({txt=Lident "option"; _}, n) -> Optional (Loc.txt pld_name), List.hd n, exp
    | _ -> Labelled (Loc.txt pld_name), pld_type, exp

let create_fun ~loc type_name labels params =
  value_description ~loc
    ~name:
      (Loc.make ~loc "make")
    ~type_:
      (List.fold_right
        (fun {pld_name; pld_type; _} exp ->
          let label, type_, exp =
            to_label_or_opt pld_name pld_type exp
          in
          (ptyp_arrow ~loc
            label
            type_
            exp))
        labels
        (ptyp_arrow ~loc
          Nolabel
          (ptyp_constr ~loc
            (Loc.make ~loc (Lident "unit"))
            [])
          (ptyp_constr ~loc
            (Loc.make ~loc (Lident type_name))
            params)))
    ~prim:
      []

let create_abstract_type ~loc type_name params =
  type_declaration ~loc
    ~name:
      type_name
    ~params:
      params
    ~cstrs:
      []
    ~kind:
      Ptype_abstract
    ~private_:
      Public
    ~manifest:
      None

let from_label ~loc type_name params {pld_name; pld_type; _} =
  [get_fun ~loc type_name pld_name pld_type params; set_fun ~loc type_name pld_name pld_type params]

let from_abstract ~loc type_name params ptype_manifest =
match ptype_manifest with
  | Some
    { ptyp_desc = Ptyp_constr
      ({ txt = Lident "unit" ; _ }, [])
    ; _ } ->
    value_description ~loc
      ~name:
        (Loc.make ~loc "make")
      ~type_:
        (ptyp_arrow ~loc
          Nolabel
          (ptyp_constr ~loc
            (Loc.make ~loc (Lident "unit"))
            [])
          (ptyp_constr ~loc
            (Loc.make ~loc (Lident type_name))
            params))
      ~prim:
        []
  | _ -> errorf ~loc "unhandled type kind"

let from_td ~loc {ptype_name; ptype_kind; ptype_loc; ptype_params; ptype_manifest; _} =
  let type_name = Loc.txt ptype_name in
  let params, _ = List.split ptype_params in
  let type_, values =
    match ptype_kind with
    | Ptype_record labels -> create_abstract_type ~loc ptype_name ptype_params, (create_fun ~loc type_name labels params)::(List.flatten (List.map (from_label ~loc type_name params) labels))
    | Ptype_abstract -> create_abstract_type ~loc ptype_name ptype_params, [from_abstract ~loc type_name params ptype_manifest]
    | _ -> errorf ~loc:ptype_loc "unhandled type kind"
  in
  (psig_type ~loc Recursive [type_])::(List.map (psig_value ~loc) values)

let expand ~ctxt =
  from_td ~loc:(Expansion_context.Extension.extension_point_loc ctxt)

let extension =
  Extension.V3.declare_inline
    "accessor"
    Extension.Context.signature_item
    Ast_pattern.(psig ((psig_type recursive (__^::nil))^::nil))
    expand

let rule = Context_free.Rule.extension extension

let () =
  Driver.register_transformation ~rules:[rule] "accessor"
