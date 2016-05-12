(ns synfnetic.core-spec
  #?(:cljs (:require-macros
             [synfnetic.core-spec :refer [asserts complex]]))
  (:require [untangled-spec.core #?(:clj :refer :cljs :refer-macros)
             [specification component behavior assertions]]
            [synfnetic.core :as syn
             #?(:clj :refer :cljs :refer-macros)
             [m-do defsyntax defsynfn synfn]])
  #?(:clj (:import (clojure.lang ExceptionInfo))))

(specification "ast parsing baby!"
  (let [input [0 1 2 3]]
    (assertions
      "fail"
      (into {} (syn/fail :c :xs))
      => {:cause :c :input :xs}

      "ok"
      (into {} (syn/ok :v :xs))
      => {:value :v :input :xs}

      "parse-one & any<"
      (mapv #(into {} %) (syn/parse-one syn/any< input))
      => [{:value 0 :input (rest input)}]

      "=<"
      (mapv #(into {} %) (syn/parse-one (syn/=< 0) input))
      => [{:value 0 :input (rest input)}]
      (first (syn/parse-one (syn/=< 1) input))
      =fn=> syn/fail?

      "<&>"
      (syn/parse-one (syn/<&> (syn/=< 0) (syn/=< 1)) input)
      =fn=> (comp #(= % (take 2 input)) :value first)

      "<|>"
      (syn/parse-one (syn/<|> (syn/=< 0) (syn/=< 1)) input)
      =fn=> (comp #(= % 0) :value first)

      "optional<"
      (mapv #(into {} %) (syn/parse-one (syn/optional< (syn/=< 0)) input))
      => [{:value 0 :input (rest input)} {:value nil :input input}]

      "parse-all & star<"
      (syn/parse-all (syn/star< syn/any<) input)
      => [0 1 2 3]

      "when< & star<"
      (syn/parse-all (syn/star< (syn/when< #(<= 0 % 3))) input)
      => [0 1 2 3]
      (binding [syn/*dbg* true]
        (syn/parse-all (syn/star< (syn/when< #(<= 0 % 2))) input))
      =throws=> (ExceptionInfo #"Parser failure"
                  #(-> % ex-data
                     (= {:cause [:when< 3]
                         :input ()})))

      "not=<"
      (syn/parse-all (syn/not=< 13) [0])
      => 0
      (syn/parse-all (syn/not=< 0) [0])
      =throws=> (ExceptionInfo #""
                  #(-> % ex-data
                     (= {:cause [:when< 0]
                         :input ()})))

      "plus<"
      (syn/parse-all (syn/plus< (syn/=< 0)) [0 0 0])
      => [0 0 0]
      (syn/parse-all (syn/plus< (syn/when< vector?)) [[0] [1]])
      => [[0] [1]])))

(def arrow<
  (syn/when< #{'->}))

(def triple<
  (m-do
    (act <- syn/any<)
    (arr <- arrow<)
    (exp <- syn/any<)
    (syn/return
      [act arr exp])))

(def anyString<
  (syn/optional< (syn/when< string?)))

(def blocks<
  (syn/plus<
    (syn/m-do
      (beh <- anyString<)
      (syn/fmap #(if-not beh %
                   (vec (cons beh %)))
                (syn/plus< triple<)))))

(specification "blocks<"
  (assertions
    (syn/parse-all blocks< '[1 -> 2])
    => '[[[1 -> 2]]]
    (syn/parse-all blocks< '["foo" 3 -> 4])
    => '[["foo" [3 -> 4]]]
    (syn/parse-all blocks< '[5 -> 6 7 -> 8])
    => '[[[5 -> 6] [7 -> 8]]]
    ))

(defsyntax asserts
  [blocks <- blocks<]
  `(do '~blocks))

(defsyntax complex
  [lucky  <- (syn/when< #{7 13 23 42})
   blocks <- blocks<
   thing  <- (syn/=< :thing)]
  `(fn [pred#]
     [(pred# ~lucky) ~(count blocks) ~thing]))

(specification "defsyntax"
  (assertions
    (asserts "foo" 1 -> 2 3 -> 4)
    => '[["foo" [1 -> 2] [3 -> 4]]]

    ((complex 42    1 -> 2 "divider" 3 -> 4    :thing)
     #{42})
    => [42 2 :thing]
    ))

(defsynfn maybe-fn-over-numbers
  [f <- (syn/<|> (syn/when< fn?)
                 (syn/return -))
   numbers <- (syn/plus< syn/number<)]
  (apply f numbers))

(specification "(def)synfn"
  (assertions
    "defsynfn"
    (maybe-fn-over-numbers 2 3) => -1
    (maybe-fn-over-numbers + (+ 1 1) 3) => 5

    "synfn"
    ((synfn [x <- (syn/plus< syn/number<)]
       (reduce - x)) 0 1 2 3 4)
    => -10))
