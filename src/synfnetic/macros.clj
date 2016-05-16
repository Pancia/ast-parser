(ns synfnetic.macros
  (:require [synfnetic.parser :as syn]))

(defn cljs-env?
  "https://github.com/Prismatic/schema/blob/master/src/clj/schema/macros.clj"
  [env] (boolean (:ns env)))

(defn import* [k vr ns-sym cljs?]
  (if cljs?
    (when-not (:macro vr)
      `(def ~(symbol k)
         ~(symbol (name ns-sym)
                  (name k))))
    `(when (.hasRoot ~vr)
       (intern *ns* (with-meta '~k
                      (merge (meta ~vr)
                             (meta '~k)))
               @~vr))))

(def ^{:doc "(... ns1 [ns2 [asdf]] ...) => [[ns1 :all] [ns2 [asdf]]]"}
  import-vars<
  (syn/plus<
    (syn/<|> (syn/fmap #(vector % :all)
                       (syn/when< symbol?))
             (syn/when< (every-pred vector?
                                    #(every? symbol? (second %)))))))

(syn/defsyntax import-vars "imports vars from the specified nss"
  [nss <- import-vars<]
  (let [cljs? (cljs-env? &env)
        -ns-interns- (if cljs?
                       cljs.analyzer.api/ns-interns
                       ns-interns)]
    `(do ~@(vec (mapcat (fn [[ns-sym _]]
                          (->> (-ns-interns- ns-sym)
                            (map (fn [[k vr]] (import* k vr ns-sym cljs?)))))
                        nss)))))
