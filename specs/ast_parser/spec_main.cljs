(ns ast-parser.spec-main
  (:require-macros
    [untangled-spec.reporters.suite :as ts])
  (:require
    untangled-spec.reporters.impl.suite
    ast-parser.tests-to-run))

(enable-console-print!)

(ts/deftest-all-suite specs #".*-spec")

(specs)
