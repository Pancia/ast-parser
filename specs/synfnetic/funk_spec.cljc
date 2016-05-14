(ns synfnetic.funk-spec
  (:require [untangled-spec.core #?(:clj :refer :cljs :refer-macros)
             [specification component behavior assertions]]
            [synfnetic.funk :refer
             [varargs p ID T F flip juxt-> truthify map-vals]]))

(specification "helper functions!"
  (let [coll [1 2 3 4 5 6 7 8]]
    (assertions
      "varargs"
      (apply varargs coll) => coll
      "flip"
      (apply (juxt (flip varargs) varargs) coll)
      => [(reverse coll) coll]
      "p => partial"
      ((p apply str) coll) => (apply str coll)
      "ID"
      (ID :w/e) => :w/e
      "juxt->"
      ((juxt-> ID inc (p * 2)) 1) => [1 2 4]
      "truthify"
      ((truthify vector?) coll) =fn=> vector?
      ((truthify keyword?) :kw) => :kw
      ((truthify nil?) nil) => true
      ((truthify false?) false) => true
      "T and F"
      (T :w/e) => true
      (F :w/e) => false
      "map-vals"
      (map-vals inc {:a 0 :b 2})
      => {:a 1 :b 3})))
