module P = Ppxlib
module A = P.Ast_builder.Default
module W = Woo_types
module SMap = W.SMap
module Make(Params : Woo_helpers.PARAMS) = struct
module Helpers = Woo_helpers.Generate(Params)
open Helpers

let mappers ?wrap_map:_ : W.variant -> P.structure_item list = fun variant ->
  let W.{ polymorphic ; _ } = variant in
  let case (label_main , _) (label , (_params , tes)) =
    let names = var_names "_p" @@ List.length tes in
    let vars = List.map e_var names in
    let (lhs , body) =
      if label <> label_main then (
        (fun p -> p_alias p "x") , e_var "x"
      ) else (
        let f_vars = e_vars "f" @@ List.length tes in
        let app (a , b) = e_apply a b in
        let lst =
          List.map app @@
          List.combine f_vars vars
        in
        (fun x -> x) , e_constructors ~polymorphic label lst
      )
    in
    (label , names , lhs , body)
  in
  let lst = SMap.to_kv_list variant.constructor_declarations in
  let single : W.constructor_declaration -> _ = fun ((label , (_params , tes)) as main) ->
    let names = var_names "f" @@ List.length tes in
    d_value ("map_" ^ label) @@
    e_funs names @@ e_fun "x" @@ e_match_pattern ~polymorphic (e_var "x") @@
    List.map (case main) lst
  in
  let all = List.map single lst in
  all

let setters ?wrap_map:_ : W.variant -> P.structure_item list = fun variant ->
  let lst = SMap.to_kv_list variant.constructor_declarations in
  let W.{ polymorphic ; _ } = variant in
  let case (label_main , _) (label , (_params , tes)) =
    let names = var_names "_" @@ List.length tes in
    let (lhs , body) =
      if label <> label_main then (
        (fun p -> p_alias p "x") , e_var "x"
      ) else (
        let lst = e_vars "s" @@ List.length tes in
        (fun x -> x) , e_constructors ~polymorphic label lst
      )
    in
    (label , names , lhs , body)
  in
  let single : W.constructor_declaration -> _ = fun ((label , (_params , tes)) as main) ->
    let names = var_names "s" @@ List.length tes in     
    d_value ("set_" ^ label) @@
    e_fun "x" @@ e_funs names @@ e_match_pattern ~polymorphic (e_var "x") @@
    List.map (case main) lst
  in
  let all = List.map single lst in
  all

  end