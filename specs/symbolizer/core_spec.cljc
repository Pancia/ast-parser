(ns symbolizer.core-spec
  (:require [untangled-spec.core #?(:clj :refer :cljs :refer-macros)
             [specification component behavior assertions]]
            [symbolizer.core :as src]))

(specification "core helpers"
  (assertions
    true => true))
