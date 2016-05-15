(ns synfnetic.macros)

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

(defmacro import-vars [[_ ns-sym]]
  (let [cljs? (cljs-env? &env)
        -ns-interns- (if cljs?
                       cljs.analyzer.api/ns-interns
                       ns-interns)]
    `(do ~@(->> (-ns-interns- ns-sym)
             (map (fn [[k vr]] (import* k vr ns-sym cljs?)))))))
