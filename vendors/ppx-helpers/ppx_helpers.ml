module P = Ppxlib

module type PARAMS = sig
  val location : P.location
end

let make_PARAMS loc = (module struct let location = loc end : PARAMS)

module Generate(Params : PARAMS) = struct
  module A = (val (P.Ast_builder.make Params.location))
  let failwith str = P.Location.raise_errorf ~loc:Params.location str
  let locate x = Location.mkloc x Params.location
  let lident label = A.Located.lident label
  let e_constructor ~polymorphic ?body label =
  if polymorphic
  then A.pexp_variant label body
  else A.pexp_construct (A.Located.lident label) body
  let e_true = e_constructor ~polymorphic:false "true"
  let e_false = e_constructor ~polymorphic:false "false"
  let e_bool b = if b then e_true else e_false
  let e_unit = e_constructor ~polymorphic:false "()"
  let e_var x = A.evar x
  let var_names prefix n =
    let rec range i n =
      if i < n
      then i :: (range (i + 1) n)
      else []
    in
    n
    |> range 0
    |> List.map (fun i -> prefix ^ (string_of_int i))
  let e_vars prefix n = List.map e_var @@ var_names prefix n
  let e_string str = A.estring str
  let e_var_n n = e_var (Int.to_string n)
  let e_tuple lst =
    match lst with
    | [] -> e_unit
    | [ single ] -> single
    | lst -> A.pexp_tuple lst
  let p_var x = A.pvar x
  let p_tuple lst = A.ppat_tuple lst
  let p_record_pun lst =
    A.ppat_record
      (List.map (fun (x : string) -> (lident x , p_var x)) lst)
      Closed
  let e_record lst = A.pexp_record (List.map (fun (var , expr) -> (lident var , expr)) lst) None
  let e_with record property content = A.pexp_record [lident property , content] (Some (e_var record))
  let e_fun ?(label = P.Nolabel) ?default var body =
    A.pexp_fun label default (p_var var) body
  let e_funs vars body =
    List.fold_right e_fun vars body
  let e_funs' ?(label = P.Nolabel) ?default vars body =
    A.pexp_fun label default (p_tuple @@ List.map p_var vars) body
  let e_fun_pat ?(label = P.Nolabel) ?default pat body =
    A.pexp_fun label default pat body
  let e_named_fun var body =
    let label = P.Labelled var in
    e_fun ~label var body
  let e_named_funs vars body = List.fold_right e_named_fun vars body
  let e_option_fun var ~default body =
    let label = P.Optional var in
    e_fun ~label ~default var body
  let t_arrow ?(arglabel = P.Nolabel) a b = A.ptyp_arrow arglabel a b
  let t_var name = A.ptyp_var name
  let t_construct name lst = A.ptyp_constr (lident name) lst
  let t_unit = t_construct "unit" []
  let t_tuple lst = A.ptyp_tuple lst
  let e_property ~record ~(label:string) = A.pexp_field record (lident label)
  let mod_structure lst = A.pmod_structure lst
  let modt_sig = A.pmty_signature
  let mod_ascription a b = A.pmod_constraint a b
  let name_to_variable : string -> string = fun str ->
    let s = String.lowercase_ascii str in
    if P.Keyword.is_keyword s
    then s ^ "_"
    else s
  let m_value x expr =
    let pat = p_var @@ name_to_variable x in
    let declaration = A.value_binding ~expr ~pat in
    A.pstr_value Nonrecursive [ declaration ]

  let s_value x type_ =
    let name = locate x in
    A.value_description ~name ~type_ ~prim:[]
    |> A.psig_value

  let td_type ?(parameters = []) ?(private_ = P.Public) name body =
    let manifest = Some body in
    let name = Location.mkloc name Params.location in
    let params =
      parameters
      |> List.map (fun x -> (t_var x , (P.NoVariance , P.NoInjectivity)))
    in
    let kind = P.Ptype_abstract in
    A.type_declaration ~params ~name ~cstrs:[] ~kind ~private_ ~manifest
  let td_type_abstract name =
    let manifest = None in
    let name = Location.mkloc name Params.location in
    let kind = P.Ptype_abstract in
    A.type_declaration ~params:[] ~name ~cstrs:[] ~kind ~private_:P.Public ~manifest
  let m_type ?parameters ?private_ name body =
    let td = td_type ?parameters ?private_ name body in
    A.pstr_type Nonrecursive [ td ]
  let s_type ?parameters ?private_ name body =
    let td = td_type ?parameters ?private_ name body in
    A.psig_type Nonrecursive [ td ]
  let s_type_abstract name =
    let td = td_type_abstract name in
    A.psig_type Nonrecursive [ td ]
       

  let declaration ~name ~body =
    let pat = p_var name in
    let expr = body in
    let declaration = A.value_binding ~expr ~pat in
    let declarations = A.pstr_value Nonrecursive [ declaration ] in
    declarations

  let p_constructor ~polymorphic ?pattern label =
    if polymorphic
    then A.ppat_variant label pattern
    else A.ppat_construct (A.Located.lident label) pattern
  let p_alias p s = A.ppat_alias p @@ A.Located.mk s

  let e_constructor ~polymorphic ?body label =
    if polymorphic
    then A.pexp_variant label body
    else A.pexp_construct (A.Located.lident label) body

  let e_apply f x =
    A.pexp_apply f [(Nolabel , x)]

  let e_constructors ?(unit = false) ~polymorphic label lst =
    let apply body =
      if polymorphic
      then A.pexp_variant label body
      else A.pexp_construct (A.Located.lident label) body
    in
    match List.length lst = 0 , unit with
    | false , _ -> apply @@ Some (e_tuple lst)
    | true , true -> apply @@ Some e_unit
    | true , false -> apply None

  (* e_applies f [ x1 ; x2 ; x3 ] = f (x1 , x2 , x3) *)
  (* e_applies ~unit f [] = f () *)
  (* e_applies f [] = f *)
  let e_applies ?(unit = false) f lst =
    match List.length lst = 0 , unit with
    | false , _ -> e_apply f @@ e_tuple lst
    | true , true -> e_apply f @@ e_unit
    | true , false -> f

  let rec e_applies_curry f lst =
    match lst with
    | [] -> f
    | hd :: tl -> e_applies_curry (e_apply f hd) tl


  let e_match_pattern : polymorphic:bool -> P.expression -> (string * string list * (P.pattern -> P.pattern) * P.expression) list -> P.expression =
    fun ~polymorphic matchee lst ->
    let case : (string * string list * (P.pattern -> P.pattern) * P.expression) -> P.case = fun (constructor , params , f , rhs) ->
      let lhs =
        if List.length params = 0 then (
          p_constructor ~polymorphic constructor
        ) else (
          let pattern =
            if polymorphic
            then p_var @@ List.hd params
            else (
              let patterns = List.map (fun name -> A.pvar name) params in
              A.ppat_tuple patterns
            )
          in
          p_constructor ~polymorphic ~pattern constructor
        )
      in
      let lhs = f lhs in
      A.case ~lhs ~rhs ~guard:None
    in
    A.pexp_match matchee @@ List.map case lst

  let e_match ~polymorphic matchee lst =
    let lst = List.map (fun (a, b , d) -> (a, b , (fun x -> x) , d)) lst in
    e_match_pattern ~polymorphic matchee lst

  let is_tauto_type_declaration : P.structure_item -> bool = fun si ->
    match si.pstr_desc with
    | Pstr_type (_ , [ single ]) -> (
      match single.ptype_params , single.ptype_cstrs , single.ptype_manifest with
      | [] , [] , Some ty -> (
        let name = single.ptype_name.txt in
        match ty.ptyp_desc with
        | Ptyp_constr (ident , _) -> (
          match ident.txt with
          | Lident var -> name = var
          | _ -> false
        )
        | _ -> false
      )
      | _ -> false
    )
    | _ -> false
end

