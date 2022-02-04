module P = Ppxlib
module A = P.Ast_builder.Default

let ($$) f x = f x

let rec get_default_name : P.core_type -> string = fun ty ->
  let self = get_default_name in
  let rec from_ident : P.longident -> string = fun ident ->
    let self = from_ident in
    match ident with
    | Lident x -> x
    | Lapply (a , b) -> self b ^ "_" ^ self a
    | Ldot (a , b) -> b ^ "_" ^ self a
  in
  match ty.ptyp_desc with
  | Ptyp_arrow _ -> "fun"
  | Ptyp_tuple lst when List.length lst = 2 -> "pair"
  | Ptyp_tuple _ -> "tpl"
  | Ptyp_constr (ident , []) -> from_ident @@ ident.Location.txt
  | Ptyp_constr (ident , [single]) ->
    (self single) ^ "_" ^ (from_ident ident.Location.txt)
  | Ptyp_constr _ -> failwith "no default name for type app >2 args"
  | Ptyp_any -> failwith "no default name for wildcard"
  | Ptyp_var _ -> failwith "no default name for type var"
  | Ptyp_object _ -> failwith "no default name for objects"
  | Ptyp_class _ -> failwith "no default name for class"
  | Ptyp_alias _ -> failwith "no default name for alias"
  | Ptyp_variant _ -> failwith "no default name for variant"
  | Ptyp_poly _ -> failwith "no default name for poly"
  | Ptyp_package _ -> failwith "no default name for module type"
  | Ptyp_extension _ -> failwith "no default name for extension"


module Make(Params : Ppx_helpers.PARAMS) = struct
module H = Ppx_helpers.Generate(Params)
let loc = Params.location

let abstract_impl ?name : P.core_type -> P.module_expr = fun ty ->
  let open H in
  let name = Option.value name ~default:(get_default_name ty) in
  mod_structure [
    m_type name ty ;
    m_value ("of_" ^ name) @@ e_fun "x" @@ e_var "x" ;
    m_value ("to_" ^ name) @@ e_fun "x" @@ e_var "x" ;
  ]

let abstract_sig ?name : P.core_type -> P.module_type = fun ty ->
  let open H in
  let name = Option.value name ~default:(get_default_name ty) in
  modt_sig [
    s_type_abstract name ;
    s_value ("of_" ^ name) (t_arrow $$ t_construct name [] $$ ty) ;
    s_value ("to_" ^ name) (t_arrow $$ ty $$ t_construct name []) ;
  ]

let abstract_full ?name : P.core_type -> P.module_expr = fun ty ->
  H.mod_ascription $$ abstract_impl ?name ty $$ abstract_sig ?name ty

end

let () =
  let open P in
  let abstract_impl_default ~loc ~path =
    let module M = Make(struct let location = loc end) in
    ignore path ;
    M.abstract_impl ?name:None
  in
  let abstract_impl_name ~loc ~path =
    let module M = Make(struct let location = loc end) in
    ignore path ;
    fun (name , ty) ->
    M.abstract_impl ~name ty
  in
  let abstract_sig_default ~loc ~path =
    let module M = Make(struct let location = loc end) in
    ignore path ;
    M.abstract_sig ?name:None
  in
  let abstract_sig_name ~loc ~path =
    let module M = Make(struct let location = loc end) in
    ignore path ;
    fun (name , ty) ->
    M.abstract_sig ~name ty
  in
  let abstract_full_default ~loc ~path =
    let module M = Make(struct let location = loc end) in
    ignore path ;
    M.abstract_full ?name:None
  in
  let abstract_full_name ~loc ~path =
    let module M = Make(struct let location = loc end) in
    ignore path ;
    fun (name , ty) ->
    M.abstract_full ~name ty
  in
  [
    Extension.declare
      "abstract_impl'"
      Extension.Context.module_expr
      Ast_pattern.(ptyp __)
      abstract_impl_default ;
    Extension.declare
      "abstract_sig'"
      Extension.Context.module_type
      Ast_pattern.(ptyp __)
      abstract_sig_default ;
    Extension.declare
      "abstract'"
      Extension.Context.module_expr
      Ast_pattern.(ptyp __)
      abstract_full_default ;
    Extension.declare
      "abstract_impl"
      Extension.Context.module_expr
      Ast_pattern.(single_expr_payload @@ pack2 @@ pexp_constraint (estring __) __)
      abstract_impl_name ;
    Extension.declare
      "abstract_sig"
      Extension.Context.module_type
      Ast_pattern.(single_expr_payload @@ pack2 @@ pexp_constraint (estring __) __)
      abstract_sig_name ;
    Extension.declare
      "abstract"
      Extension.Context.module_expr
      Ast_pattern.(single_expr_payload @@ pack2 @@ pexp_constraint (estring __) __)
      abstract_full_name ;
  ]
  |> List.map P.Context_free.Rule.extension
  |> fun rules -> P.Driver.register_transformation ~rules "ppx_abstract"
