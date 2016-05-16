(ns synfnetic.parser-spec
  #?(:cljs (:require-macros
             [synfnetic.parser-spec :refer [asserts complex]]))
  (:require [untangled-spec.core #?(:clj :refer :cljs :refer-macros)
             [specification component behavior assertions]]
            [synfnetic.parser :as src
             #?(:clj :refer :cljs :refer-macros)
             [m-do defsyntax defsynfn synfn]])
  #?(:clj (:import (clojure.lang ExceptionInfo))))

(specification "ast parsing baby!"
  (let [input [0 1 2 3]]
    (assertions
      "parse-one & any<"
      (mapv #(into {} %) (src/parse-one src/any< input))
      => [{:value 0 :input (rest input) :state (src/make-state [0])}]

      "=<"
      (mapv #(into {} %) (src/parse-one (src/=< 0) input))
      => [{:value 0 :input (rest input) :state (src/make-state [0])}]
      (into {} (first (src/parse-one (src/=< 1) input)))
      => {:cause [:when< 0 := 1] :input (rest input) :state (src/make-state [0])}

      "<&>"
      (src/parse-one (src/<&> (src/=< 0) (src/=< 1)) input)
      =fn=> (comp #(= % (take 2 input)) :value first)

      "<|>"
      (src/parse-one (src/<|> (src/=< 0) (src/=< 1)) input)
      =fn=> (comp #(= % 0) :value first)

      "optional<"
      (mapv #(into {} %) (src/parse-one (src/optional< (src/=< 0)) input))
      => [{:value 0 :input (rest input) :state (src/make-state [0])}
          {:value nil :input input :state (src/make-state)}]

      "parse-all & star<"
      (src/parse-all (src/star< src/any<) input)
      => input

      "when< & star<"
      (src/parse-all (src/star< (src/when< #(<= 0 % 3))) input)
      => input
      (src/parse-all (src/star< (src/when< #(<= 0 % 2))) input)
      =throws=> (ExceptionInfo #"Parser Failure"
                  #(-> % ex-data
                     (= {:cause [:when< 3]
                         :input ()
                         :state (src/make-state input)})))

      "not=<"
      (src/parse-all (src/not=< 13) [0])
      => 0
      (src/parse-all (src/not=< 0) [0])
      =throws=> (ExceptionInfo #""
                  #(-> % ex-data
                     (= {:cause [:when< 0 :not= 0]
                         :input ()
                         :state (src/make-state [0])})))

      "plus<"
      (src/parse-all (src/plus< (src/=< 0)) [0 0 0])
      => [0 0 0]
      (src/parse-all (src/plus< (src/when< vector?)) [[0] [1]])
      => [[0] [1]]

      "parse a string just to show"
      (src/parse-all (src/seq< "foobar") "foobar")
      => [\f \o \o \b \a \r]
      (src/parse-all (src/seq< "foo") "foobar")
      =throws=> (ExceptionInfo #"Parser Error"
                  #(-> % ex-data
                     (= {:input [\b \a \r]
                         :state (src/make-state [\f \o \o])
                         :last-saw [\f \o \o]})))

      "unparsable input"
      (into {} (first (src/parse-one (src/plus< src/number<) [:x :y :z])))
      => {:cause [:when< :x] :input [:y :z] :state (src/make-state [:x])}
      (src/parse-all (src/plus< src/number<) [:x :y :z])
      =throws=> (ExceptionInfo #"Parser Failure"
                  #(-> % ex-data
                     (= {:cause [:when< :x]
                         :input [:y :z]
                         :state (src/make-state [:x])}))))))

(def arrow<
  (src/when< #{'->}))

(def triple<
  (m-do
    (act <- src/any<)
    (arr <- arrow<)
    (exp <- src/any<)
    (src/return
      [act arr exp])))

(def anyString<
  (src/optional< (src/when< string?)))

(def blocks<
  (src/plus<
    (src/m-do
      (beh <- anyString<)
      (src/fmap #(if-not beh %
                   (vec (cons beh %)))
                (src/plus< triple<)))))

(specification "blocks<"
  (assertions
    (src/parse-all blocks< '[1 -> 2])
    => '[[[1 -> 2]]]
    (src/parse-all blocks< '["foo" 3 -> 4])
    => '[["foo" [3 -> 4]]]
    (src/parse-all blocks< '[5 -> 6 7 -> 8])
    => '[[[5 -> 6] [7 -> 8]]]))

(defsyntax asserts
  [blocks <- blocks<]
  `(do '~blocks))

(defsyntax complex
  [lucky  <- (src/when< #{7 13 23 42})
   blocks <- blocks<
   thing  <- (src/=< :thing)]
  `(fn [pred#]
     [(pred# ~lucky) ~(count blocks) ~thing]))

(specification "defsyntax"
  (assertions
    (asserts "foo" 1 -> 2 3 -> 4)
    => '[["foo" [1 -> 2] [3 -> 4]]]

    ((complex 42    1 -> 2 "divider" 3 -> 4    :thing)
     #{42})
    => [42 2 :thing]))

(defsynfn maybe-fn-over-numbers
  [f <- (src/<|> (src/when< fn?)
                 (src/return -))
   numbers <- (src/plus< src/number<)]
  (apply f numbers))

(specification "(def)synfn"
  (assertions
    "defsynfn"
    (maybe-fn-over-numbers 2 3) => -1
    (maybe-fn-over-numbers + (+ 1 1) 3) => 5

    "synfn"
    ((synfn [x <- (src/plus< src/number<)]
       (reduce - x)) 0 1 2 3 4)
    => -10))
