(ns ast-parser.core-spec
  #?(:cljs (:require-macros
             [ast-parser.core-spec :refer [asserts complex]]))
  (:require [untangled-spec.core #?(:clj :refer :cljs :refer-macros)
             [specification component behavior assertions]]
            [ast-parser.core :as P&
             #?(:clj :refer :cljs :refer-macros)
             [m-do defsyntax defsynfn synfn]]))

(def arrow&
  (P&/fmap name (P&/pred #{'x>})))

(def triple&
  (m-do
    (act <- P&/any)
    (arr <- arrow&)
    (exp <- P&/any)
    (P&/return
      [act arr exp])))

(def anyString&
  (P&/fmap #(cond->> (seq %) (seq %) (apply str))
           (P&/optional (P&/pred string?))))

(def blocks&
  (P&/plus
    (P&/m-do
      (beh <- anyString&)
      (P&/fmap #(->> % (cons beh) vec)
               (P&/plus triple&)))))

(defsynfn maybe-fn-over-digits
  [f <- (P&/<|> (P&/pred fn?) (P&/return (fn [& args] args)))
   digits <- (P&/plus P&/digit)]
  (apply f digits))

(defsyntax asserts
  [blocks <- blocks&]
  blocks)

(defsyntax complex
  [lucky  <- (P&/pred #{7 13 23 42})
   blocks <- blocks&
   thing  <- (P&/match :thing)]
  `(fn [pred#]
     [(pred# ~lucky) (count ~blocks) ~thing]))

(specification "ast parsing baby!"
  (let [input [0 1 2 3]]
    (assertions
      (asserts "foo" 13 x> 42)
      => [["foo" [13 "x>" 42]]]

      (asserts "foo" 1 x> 2 3 x> 4)
      => [["foo" [1 "x>" 2] [3 "x>" 4]]]

      ((complex 42    1 x> 2 "divider" 3 x> 4    :thing)
       #{42})
      => [42 2 :thing])
    (assertions
      (ffirst (P&/parse-one P&/any input))
      => 0

      (ffirst (P&/parse-one (P&/match 0) input))
      => 0
      (P&/parse-one (P&/match 1) input)
      => []

      (ffirst (P&/parse-one (P&/<&> (P&/match 0) (P&/match 1)) input))
      => [0 1]

      (ffirst (P&/parse-one (P&/<|> (P&/match 0) (P&/match 1)) input))
      => 0

      (P&/parse-one (P&/optional (P&/match 0)) input)
      => [[0 [1 2 3]] [[] [0 1 2 3]]]
      (P&/parse-one (P&/optional (P&/match 1)) input)
      => [[[] [0 1 2 3]]]

      (P&/parse-all (P&/star (P&/pred #(<= 0 % 3))) input)
      => [0 1 2 3]
      (P&/parse-all (P&/star (P&/pred #(<= 0 % 2))) input)
      => nil

      (P&/parse-all (P&/noneOf 13) [0])
      => 0
      (P&/parse-all (P&/noneOf 0) [0])
      => nil

      (P&/parse-all (P&/plus (P&/match 0)) [0 0 0])
      => [0 0 0]

      (P&/parse-all (P&/plus (P&/pred vector?)) [[0] [1]])
      => [[0] [1]]

      (P&/parse-all blocks& [1 'x> 2])
      => '[[nil [1 "x>" 2]]]
      (P&/parse-all blocks& ["foo" 1 'x> 2])
      => '[["foo" [1 "x>" 2]]]
      (P&/parse-all blocks& '["foo" 1 x> 2 3 x> 4])
      => '[["foo" [1 "x>" 2] [3 "x>" 4]]]

      (maybe-fn-over-digits 2 3) => [2 3]
      (maybe-fn-over-digits + (+ 1 1) 3) => 5

      ((synfn [x <- (P&/plus P&/digit)]
         (reduce - x)) 0 1 2 3 4)
      => -10)))
