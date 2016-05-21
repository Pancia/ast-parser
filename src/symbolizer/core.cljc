(ns symbolizer.core
  #?(:cljs (:require-macros [symbolizer.macros :refer [import-vars]]))
  (:require #?@(:clj ([symbolizer.macros :refer [import-vars]]))
            [clojure.set :as set]
            [symbolizer.parser]
            [symbolizer.funk]
            ))

#?(:cljs (enable-console-print!))

(def ^:dynamic *dbg* false)
(defn dbg
  ([tag x] (when *dbg* (prn tag x)) x)
  ([x] (dbg :dbg x)))

;(println (macroexpand '(import-vars symbolizer.parser symbolizer.funk)))
(import-vars symbolizer.parser symbolizer.funk)
