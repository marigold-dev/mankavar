module P = Ppxlib
module A = P.Ast_builder.Default
module W = Woo_types
module H = Ppx_helpers

module type PARAMS = H.PARAMS

let make_PARAMS loc = (module struct let location = loc end : PARAMS)

module Generate(Params : PARAMS) = struct
  module Ppx_helpers = Ppx_helpers.Generate(Params)
  include Ppx_helpers
  let label_to_variable : W.label -> string = name_to_variable

  let extract_core_type = function
    | W.T_variant _ -> failwith "ez doesn't support inline variants"
    | W.T_record _ -> failwith "ez doesn't support inline records"
    | W.T_core ct -> ct

  let d_value = m_value
  let abstract_type_declaration
    ?(parameters : W.type_parameters = [])
    ?(private_ = P.Public) name body
  = m_type ~parameters ~private_ name body
end