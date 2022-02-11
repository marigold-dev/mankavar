module P = Ppxlib
module A = P.Ast_builder.Default
module W = Woo_types

let get_default_value_opt : P.attribute -> P.expression option = fun attr ->
  let P.{ attr_payload = pl ; attr_name = name ; _ } = attr in
  let (>>?) = Option.bind in
  (if name.txt = "default" then Some () else None) >>? fun () ->
  (match pl with
    | P.PStr items -> Some items
    | _ -> None
  ) >>? fun items ->
  (match items with
  | [ default ] -> Some (default)
  | _ -> None) >>? fun (default) ->
  (match default.pstr_desc with
    | Pstr_eval (expr , _) -> (
        Some expr
      )
    | _ -> None)

let rec get_parameters : P.core_type -> W.type_parameters = fun ct ->
  match ct.ptyp_desc with
  | Ptyp_any -> []
  | Ptyp_var x -> [ x ]
  | Ptyp_arrow (_ , a , b) -> get_parameters a @ get_parameters b
  | Ptyp_tuple lst -> lst |> List.map get_parameters |> List.concat
  | Ptyp_constr (_ , lst) -> lst |> List.map get_parameters |> List.concat
  | Ptyp_object (lst , _) ->
    lst
    |> List.map (fun x -> x.P.pof_desc |> (function P.Otag (_ , x) -> x | P.Oinherit x -> x))
    |> List.map get_parameters |> List.concat
  | Ptyp_class (_ , lst) -> lst |> List.map get_parameters |> List.concat
  | Ptyp_alias (t , x) -> x :: get_parameters t
  | Ptyp_variant (lst , _ , _) -> 
    lst
    |> List.map (fun x -> x.P.prf_desc |> (function P.Rtag (_ , _ , x) -> x | P.Rinherit x -> [ x ]))
    |> List.concat
    |> List.map get_parameters |> List.concat
  | Ptyp_package _ -> []
  | Ptyp_extension _ -> failwith "ppx woo does not support extensions"
  | Ptyp_poly _ -> failwith "ppx woo does not support ptyp poly"

let remove_duplicates eq =
  let rec aux lst =
    match lst with
  | [] -> []
  | hd :: tl when List.exists (fun x -> eq hd x) tl -> aux tl
  | hd :: tl -> hd :: (aux tl)
  in
  aux

let labelled_record_field index : P.label_declaration  -> W.labelled_record_field = fun ld ->
  let label = ld.pld_name.txt in
  let ty = ld.pld_type in
  let parameters = get_parameters ty in
  let attributes = ld.pld_attributes in
  let default_value =
    attributes
    |> List.filter_map get_default_value_opt
    |> (function [] -> None | hd :: _ -> Some hd)
  in
  (label , W.record_field ?default_value ~parameters index (W.T_core ty))

let record : P.label_declaration list -> W.type_expression = fun lds ->
  let record_fields = List.mapi labelled_record_field lds in
  T_record ( W.r_decls record_fields )


let type_declaration ?non_recursive : P.type_declaration -> W.type_declaration = fun td ->
  let label = td.ptype_name.txt in
  let body =
    match td.ptype_kind with
    | P.Ptype_variant cds ->  (
        let aux i : P.constructor_declaration -> W.constructor_declaration = fun cd ->
          let constructor = cd.pcd_name.txt in
          let (params , args) =
            match cd.pcd_args with
            | P.Pcstr_tuple tys -> (
              let params =
                tys
                |> List.map get_parameters
                |> List.concat
                |> remove_duplicates (=)
              in
              let args =
                tys
                |> List.map (fun ty -> W.T_core ty)
              in
              (params , args)
            )
            | P.Pcstr_record _lds -> failwith "ppx woo doesn't support inline records" (* [ record lds ] *)
          in
          (constructor , (i , params , args))
        in
        let polymorphic = false in
        let constructor_declarations = W.c_decls @@ List.mapi aux cds in
        W.T_variant { polymorphic ; constructor_declarations }
      )
    | P.Ptype_record lds -> record lds
    | P.Ptype_abstract -> (
        match td.ptype_manifest with
        | Some ct -> W.T_core ct
        | None -> failwith "parse_type_declaration: unknown case PType_abstract and no manifest"
      )
    | P.Ptype_open -> failwith "parse_type_declaration: unknown case PType_open"
  in
  let parameters =
    let paramer : P.core_type * (P.variance * P.injectivity) -> _ = fun (ct , _) ->
      match ct.ptyp_desc with
      | Ptyp_var x -> x
      | _ -> failwith "parse_type_param: only simple type params are supported"
    in
    td.ptype_params
    |> List.map paramer
  in
  W.type_declaration ?non_recursive ~parameters label body

let type_declarations ?non_recursive : P.type_declaration list -> W.type_declarations = fun tds ->
  List.map (type_declaration ?non_recursive) tds

let non_recursive : P.rec_flag -> W.non_recursive = fun rf ->
  match rf with
  | P.Nonrecursive -> true
  | P.Recursive -> false
