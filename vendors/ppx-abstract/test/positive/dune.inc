
(test
  (name a_test)
  (modules a_test)
  (preprocess (pps ppx_abstract))
)

(rule
  (targets a_test.actual.ml)
  (deps (:pp pp.exe) (:input a_test.ml))
  (action (run ./%{pp} --impl %{input} -o %{targets}))
)

(rule
  (alias runtest)
  (action (diff a_test.expected.ml a_test.actual.ml))
)
