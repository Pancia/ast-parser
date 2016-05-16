(ns synfnetic.core
  #?(:cljs (:require-macros [synfnetic.macros :refer [import-vars]]))
  (:require #?@(:clj ([synfnetic.macros :refer [import-vars]]))
            [clojure.set :as set]
            [synfnetic.parser]
            [synfnetic.funk]
            ))

#?(:cljs (enable-console-print!))

(def ^:dynamic *dbg* false)
(defn dbg
  ([tag x] (when *dbg* (prn tag x)) x)
  ([x] (dbg :dbg x)))

;(println (macroexpand '(import-vars synfnetic.parser synfnetic.funk)))
(import-vars synfnetic.parser synfnetic.funk)
