(ns synfnetic.core-spec
  #?(:cljs (:require-macros
             [synfnetic.core-spec :refer [asserts complex]]))
  (:require [untangled-spec.core #?(:clj :refer :cljs :refer-macros)
             [specification component behavior assertions]]
            [synfnetic.core :as syn
             #?(:clj :refer :cljs :refer-macros)
             [m-do defsyntax defsynfn synfn]]))

(def arrow<
  (syn/fmap name (syn/pred< #{'->})))

(def triple<
  (m-do
    (act <- syn/any<)
    (arr <- arrow<)
    (exp <- syn/any<)
    (syn/return
      [act arr exp])))

(def anyString<
  (syn/fmap #(cond->> (seq %) (seq %) (apply str))
            (syn/optional< (syn/pred< string?))))

(def blocks<
  (syn/plus<
    (syn/m-do
      (beh <- anyString<)
      (syn/fmap #(->> % (cons beh) vec)
                (syn/plus< triple<)))))

(defsynfn maybe-fn-over-numbers
  [f <- (syn/<|> (syn/pred< fn?) (syn/return (fn [& args] args)))
   numbers <- (syn/plus< syn/number<)]
  (apply f numbers))

(defsyntax asserts
  [blocks <- blocks<]
  blocks)

(defsyntax complex
  [lucky  <- (syn/pred< #{7 13 23 42})
   blocks <- blocks<
   thing  <- (syn/match< :thing)]
  `(fn [pred#]
     [(pred# ~lucky) ~(count blocks) ~thing]))

(specification "ast parsing baby!"
  (let [input [0 1 2 3]]
    (assertions
      (asserts "foo" 13 -> 42)
      => [["foo" [13 "->" 42]]]

      (asserts "foo" 1 -> 2 3 -> 4)
      => [["foo" [1 "->" 2] [3 "->" 4]]]

      ((complex 42    1 -> 2 "divider" 3 -> 4    :thing)
       #{42})
      => [42 2 :thing])
    (assertions
      (ffirst (syn/parse-one syn/any< input))
      => 0

      (ffirst (syn/parse-one (syn/match< 0) input))
      => 0
      (syn/parse-one (syn/match< 1) input)
      => []

      (ffirst (syn/parse-one (syn/<&> (syn/match< 0) (syn/match< 1)) input))
      => [0 1]

      (ffirst (syn/parse-one (syn/<|> (syn/match< 0) (syn/match< 1)) input))
      => 0

      (syn/parse-one (syn/optional< (syn/match< 0)) input)
      => [[0 [1 2 3]] [[] [0 1 2 3]]]
      (syn/parse-one (syn/optional< (syn/match< 1)) input)
      => [[[] [0 1 2 3]]]

      (syn/parse-all (syn/star< (syn/pred< #(<= 0 % 3))) input)
      => [0 1 2 3]
      (syn/parse-all (syn/star< (syn/pred< #(<= 0 % 2))) input)
      => nil

      (syn/parse-all (syn/noneOf< 13) [0])
      => 0
      (syn/parse-all (syn/noneOf< 0) [0])
      => nil

      (syn/parse-all (syn/plus< (syn/match< 0)) [0 0 0])
      => [0 0 0]

      (syn/parse-all (syn/plus< (syn/pred< vector?)) [[0] [1]])
      => [[0] [1]]

      (syn/parse-all blocks< '[1 -> 2])
      => '[[nil [1 "->" 2]]]
      (syn/parse-all blocks< '["foo" 1 -> 2])
      => '[["foo" [1 "->" 2]]]
      (syn/parse-all blocks< '["foo" 1 -> 2 3 -> 4])
      => '[["foo" [1 "->" 2] [3 "->" 4]]]

      (maybe-fn-over-numbers 2 3) => [2 3]
      (maybe-fn-over-numbers + (+ 1 1) 3) => 5

      ((synfn [x <- (syn/plus< syn/number<)]
         (reduce - x)) 0 1 2 3 4)
      => -10)))
