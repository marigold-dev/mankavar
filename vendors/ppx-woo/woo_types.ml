module P = Ppxlib
module A = P.Ast_builder.Default

module SMap = struct
  include Map.Make(String)
  let to_kv_list_rev : 'a t -> (string * 'a) list = fun m -> fold (fun k v prev -> (k , v) :: prev) m []
  let to_kv_list : 'a t -> (string * 'a) list = fun m -> List.rev (to_kv_list_rev m)
end

type type_parameter = string
type type_parameters = type_parameter list

type variant = {
  constructor_declarations : constructor_declaration_content SMap.t ;
  polymorphic : bool ;
}

and constructor_declaration_content = (int * type_parameters * type_expression list)
and constructor_declaration = string * constructor_declaration_content

and record_field = {
  type_expression : type_expression ;
  default_value : P.expression option ;
  index : int ;
  parameters : type_parameters ;
}

and record = record_field SMap.t

and type_expression =
  | T_variant of variant
  | T_record of record
  | T_core of P.core_type

let get_t_core_opt = function
  | T_core ct -> Some ct
  | _ -> None

let record_field ?default_value ~parameters index type_expression = { type_expression ; default_value ; index ; parameters }

type label = string
type labelled_record_field = label * record_field
type labelled_types = label * type_expression list
type variant_map = type_expression list SMap.t
type non_recursive = bool


type type_declaration = {
  label : label ;
  ty : type_expression ;
  parameters : type_parameters ;
  non_recursive : non_recursive ;
}

let type_declaration ?(non_recursive = false) ?(parameters = []) label ty =
  { non_recursive ; parameters ; ty ; label }

type type_declarations = type_declaration list

let c_decls : constructor_declaration list -> constructor_declaration_content SMap.t = fun cds ->
  List.fold_left (fun old (k , v) -> SMap.add k v old) SMap.empty cds

let r_decls : (labelled_record_field) list -> record = fun rds ->
  List.fold_left (fun old (k , v) -> SMap.add k v old) SMap.empty rds