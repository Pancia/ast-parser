(ns symbolizer.macros-spec
  (:require [untangled-spec.core :refer
             [specification behavior assertions when-mocking]]
            [symbolizer.macros :as src]
            [symbolizer.parser :as syn]
            symbolizer.funk))

(def funk-syms
  (map first (ns-interns 'symbolizer.funk)))

(specification "import-vars<"
  (assertions
    (syn/parse-all src/import-vars<
      '[some.ns])
    => '[[some.ns :all]]
    (syn/parse-all src/import-vars<
      '[some.ns another.ns])
    => '[[some.ns :all] [another.ns :all]]
    (syn/parse-all src/import-vars<
      '[ns1 [ns2 [asdf]]])
    => '[[ns1 :all] [ns2 [asdf]]]))

(specification "import-vars"
  (behavior "in clj interns vars found by ns-interns"
    (let [syms (atom [])]
      (when-mocking
        (intern _ k _) => (swap! syms conj k)
        (assertions
          (do (src/import-vars symbolizer.funk)
              @syms)
          => funk-syms))))
  (behavior "in cljs defs symbols found in the analyzers' ns-interns"
    (let [syms (atom [])]
      (when-mocking
        (src/cljs-env? _) => true
        (cljs.analyzer.api/ns-interns -ns-) => (->> (ns-interns -ns-)
                                                 (mapv #(update % 1 meta)))
        (assertions
          (rest (macroexpand '(symbolizer.macros/import-vars symbolizer.funk)))
          => (mapv #(vector 'def % (symbol "symbolizer.funk" (name %)))
                   funk-syms))))))
