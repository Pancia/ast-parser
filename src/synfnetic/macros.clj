(ns synfnetic.macros)

(defn cljs-env?
  "https://github.com/Prismatic/schema/blob/master/src/clj/schema/macros.clj"
  [env] (boolean (:ns env)))

(defmacro import-vars [[_ ns-sym]]
  `(do
     ~@(if (cljs-env? &env)
         (->>
           (cljs.analyzer.api/ns-interns ns-sym)
           (remove (comp :macro second))
           (map (fn [[k# _]]
                  `(def ~(symbol k#) ~(symbol (name ns-sym) (name k#))))))
         (->>
           (ns-interns ns-sym)
           (map (fn [[k# var#]]
                  (if (.hasRoot var#)
                    (intern *ns* (with-meta k#
                                   (merge (meta var#)
                                          (meta k#)))
                            @var#))))))))
