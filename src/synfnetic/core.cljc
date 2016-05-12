(ns synfnetic.core
  ;; FOR ACCESS TO MACROS, DO NOT DELETE
  #?(:cljs (:require-macros [synfnetic.core :refer [m-do dbg]]))

  #?(:clj (:import (clojure.lang IDeref))))

(def ^:dynamic *dbg* false)
(defn dbg [x] (when *dbg* (prn :dbg x)) x)

(defrecord Fail [cause input])
(defn fail
  ([cause input] [(->Fail cause input)])
  ([cause] (fn [input] (fail cause input))))
(defn fail? [x] (instance? Fail x))

(defrecord Ok [value input])
(defn ok [value input] [(->Ok value input)])
(defn ok? [x] (instance? Ok x))

(defn parse-one [parser input]
  (parser input))

(def done? (comp empty? :input))
(defn extract [x]
  (cond (ok? x) (:value x)
        :else (throw (ex-info "Parser failure" (into {} x)))))

(defn parse-all [parser input]
  (let [mrf (atom nil)]
    (->> (sequence
           (comp (filter done?)
                 (map #(do (when (fail? %)
                             (reset! mrf %)) %))
                 (filter ok?))
           (parse-one parser input))
      first (#(or % @mrf))
      extract)))

(defn return [v]
  (fn [input]
     (ok v input)))

(defn any< [input]
  (if (empty? input) (fail ::empty [])
    (ok (first input)
        (rest  input))))

(defn >>= [m f]
  (fn [input]
    (sequence
      (mapcat (fn [{:keys [value input] :as x}]
                (if (fail? x) [x]
                  (parse-one (f value) input))))
      (parse-one m input))))

(defn fmap [f m]
  (>>= m (comp return f)))
(def <$> fmap)

(defn do* [body bind]
  (if (and (not (symbol? bind))
           (= 3 (count bind))
           (= '<- (second bind)))
    `(>>= ~(last bind) (fn [~(first bind)] ~body))
    `(>>= ~bind (fn [_#] ~body))))

#?(:clj (defmacro m-do [& forms]
          (reduce do* (last forms) (reverse (butlast forms)))))

;; PARSERS & COMBINATORS

;; (ab)
(defn and< [pa pb]
  (m-do
    (ra <- pa)
    (rb <- pb)
    (return [ra rb])))
(def <&> and<)

;; (a|b)
(defn or< [p1 p2]
  (fn [input]
    (lazy-cat (parse-one p1 input) (parse-one p2 input))))
(def <|> or<)

;; (a?)
(defn optional< [parser]
  (<|> parser (return nil)))
(def ?< optional<)

(declare plus<)

;; (a*)
(defn star< [parser]
  (optional< (plus< parser)))
(def *< star<)

;; (a+) => (aa*)
(defn plus< [parser]
  (m-do
    (a   <- parser)
    (as  <- (star< parser))
    (return (vec (cons a as)))))
(def +< plus<)

(defn when< [p]
  (>>= any<
       (fn [v]
         (if (p v)
           (return v)
           (fail [:when< v])))))

(defn cmp< [p]
  (fn [v] (when< (partial p v))))

(def    =< (cmp< =))
(def not=< (cmp< not=))

(defn seq< [x] (reduce <&> (map #(=< %) x)))

(def string< seq<)
(def number< (when< number?))

;; PUBLIC MACROS

(defn syn* [arglist body]
  `(fn [args#]
     (parse-all
       (m-do ~@(partition 3 arglist)
             (return ((fn [] ~@body))))
       args#)))

#?(:clj (defmacro defsyntax [n arglist & body]
          `(defmacro ~n [& args#]
             (~(syn* arglist body) args#))))

#?(:clj (defmacro defsynfn [n arglist & body]
          `(defn ~n [& args#]
             (~(syn* arglist body) args#))))

#?(:clj (defmacro synfn [arglist & body]
          `(fn [& args#]
             (~(syn* arglist body) args#))))
