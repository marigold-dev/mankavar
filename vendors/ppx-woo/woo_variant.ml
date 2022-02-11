module P = Ppxlib
module A = P.Ast_builder.Default
module W = Woo_types
module SMap = W.SMap
module Make(Params : Woo_helpers.PARAMS) = struct
module Helpers = Woo_helpers.Generate(Params)
open Helpers

let destruct prefix : W.variant -> P.structure_item list = fun variant ->
  let W.{ polymorphic ; constructor_declarations } = variant in
  let lts =
    constructor_declarations
    |> SMap.to_kv_list
    |> List.sort (fun (_ , (i , _ , _)) (_ , (i' , _ , _)) -> Int.compare i i')
  in
  let vars =
    lts
    |> List.map (fun (l , _tes) -> l)
    |> List.map String.lowercase_ascii
  in
  let single_case : W.constructor_declaration -> (string * string list * P.expression)
  = fun (lbl , (_index , _params , tes)) ->
    let l = List.length tes in
    let body = e_applies_curry (e_var @@ String.lowercase_ascii lbl) @@ e_vars "x" l in
    (lbl , var_names "x" l , body)
  in
  let body =
    lts
    |> List.map single_case
    |> e_match ~polymorphic (e_var "_matchee")
    |> e_fun "_matchee"
    |> e_named_funs vars
  in
  let name = prefix ^ "destruct" in
  [ declaration ~name ~body ]

let destruct_tpl prefix : W.variant -> P.structure_item list = fun variant ->
  let W.{ polymorphic ; constructor_declarations } = variant in
  let lts =
    constructor_declarations
    |> SMap.to_kv_list
    |> List.sort (fun (_ , (i , _ , _)) (_ , (i' , _ , _)) -> Int.compare i i')
  in
  let vars =
    lts
    |> List.map (fun (l , _tes) -> l)
    |> List.map String.lowercase_ascii
  in
  (* List.iter (fun v -> Format.printf "Var : %s\n%!" v) vars ;  *)
  let single_case : W.constructor_declaration -> (string * string list * P.expression)
  = fun (lbl , (_index , _params , tes)) ->
    let l = List.length tes in
    let body = e_applies_curry (e_var @@ String.lowercase_ascii lbl) @@ e_vars "x" l in
    (lbl , var_names "x" l , body)
  in
  let body =
    lts
    |> List.map single_case
    |> e_match ~polymorphic (e_var "_matchee")
    |> e_fun "_matchee"
    |> e_funs vars
  in
  let name = prefix ^ "destruct_tpl" in
  [ declaration ~name ~body ]

end