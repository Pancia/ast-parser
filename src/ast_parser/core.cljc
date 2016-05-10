(ns ast-parser.core
  ;; FOR ACCESS TO MACROS, DO NOT DELETE
  #?(:cljs (:require-macros [ast-parser.core :refer [m-do]]))

  #?(:cljs (:require [cljs.reader :refer [read-string]])))

(def fail (constantly []))

(defn any [input]
  (if (empty? input) (fail)
    (vector [(first input)
             (rest input)])))

(defn parse-one [parser input]
  (parser input))

(defn parse-all [parser input]
  (->> input
    (parse-one parser)
    (drop-while #(not= [] (second %)))
    ffirst))

(defn return [v]
  (fn [input] [[v input]]))

(defn >>= [m f]
  (fn [input]
    (->> (parse-one m input)
      (mapcat (fn [[v tail]]
                (parse-one (f v) tail))))))

(defn fmap [f m]
  (>>= m (comp return f)))
(def <$> fmap)

(defn do* [body bind]
  (if (and (not (symbol? bind))
           (= 3 (count bind))
           (= '<- (second bind)))
    `(>>= ~(last bind) (fn [~(first bind)] ~body))
    `(>>= ~bind (fn [~'_] ~body))))

#?(:clj (defmacro m-do [& forms]
          (reduce do* (last forms) (reverse (butlast forms)))))

(defn pred [p]
  (>>= any (fn [v] (if (p v) (return v) fail))))

(defn cmp [p]
  (fn [v] (pred (partial p v))))

(def match (cmp =))
(def noneOf (cmp not=))

;; (ab)
(defn <&> [pa pb]
  (m-do
    (ra <- pa)
    (rb <- pb)
    (return [ra rb])))

;; (a|b)
(defn <|> [p1 p2]
  (fn [input]
    (lazy-cat (parse-one p1 input) (parse-one p2 input))))

;; (a?)
(defn optional [parser] (<|> parser (return [])))

(declare plus)

;; (a*)
(defn star [parser] (optional (plus parser)))

;; (a+) => (aa*)
(defn plus [parser]
  (m-do
    (a <- parser)
    (as <- (star parser))
    (return (vec (cons a as)))))

(def digit (<$> (comp read-string str)
                (pred #(re-find #"\d" (str %)))))
(defn seq& [x] (reduce <&> (map #(match %) x)))
(def string seq&)

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
