let output_stanzas filename =
  let base = Filename.remove_extension filename in
  Printf.printf
    {|
(test
  (name %s)
  (modules %s)
  (preprocess (pps ppx_abstract))
)

(rule
  (targets %s.actual.ml)
  (deps (:pp pp.exe) (:input %s.ml))
  (action (run ./%%{pp} --impl %%{input} -o %%{targets}))
)

(rule
  (alias runtest)
  (action (diff %s.expected.ml %s.actual.ml))
)
|}
    base
    base
    base
    base
    base
    base

let is_positive_test = function
  | "pp.ml" -> false
  | "gen_dune_rules.ml" -> false
  | filename -> Filename.check_suffix filename "_test.ml"

let () =
  Sys.readdir "."
  |> Array.to_list
  |> List.sort String.compare
  |> List.filter is_positive_test
  |> List.iter output_stanzas