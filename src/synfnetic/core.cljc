(ns synfnetic.core
  #?(:cljs (:require-macros [synfnetic.macros :refer [import-vars]]))
  (:require #?@(:clj ([synfnetic.macros :refer [import-vars]]))
            [clojure.set :as set]
            [synfnetic.parser]
            [synfnetic.funk]
            ))

(def ^:dynamic *dbg* false)
(defn dbg
  ([tag x] (when *dbg* (print ";") (prn tag x)) x)
  ([x] (dbg :dbg x)))

;(println ";" (macroexpand '(import-vars 'synfnetic.parser)))
(import-vars 'synfnetic.parser)
(import-vars 'synfnetic.funk)
