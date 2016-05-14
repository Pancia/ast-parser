(ns synfnetic.core-spec
  (:require [untangled-spec.core #?(:clj :refer :cljs :refer-macros)
             [specification component behavior assertions]]
            [synfnetic.core :as src]))

(specification "core helpers"
  (assertions
    true => true))
