(ns synfnetic.parser
  ;; FOR ACCESS TO MACROS, DO NOT DELETE
  #?(:cljs (:require-macros [synfnetic.parser :refer [m-do]]))
  (:require [clojure.set :as set]))

(defrecord State [seen])
(defn make-state
  ([] (make-state []))
  ([seen] (->State seen)))

(defrecord Fail [cause input state])
(defn fail
  ([cause input state] {:pre [(sequential? cause)]} [(->Fail cause input state)])
  ([cause] (fn [input state] (fail cause input state))))
(defn fail? [x] (instance? Fail x))

(defrecord Ok [value input state])
(defn ok
  ([value input state] [(->Ok value input state)])
  ([value] (fn [input state] (ok value input state))))
(defn ok? [x] (instance? Ok x))

(defn parse-one [parser input & [state]]
  (parser input (or state (make-state))))

(def done? (comp empty? :input))

(defn parse-all [parser input]
  (let [mrf (atom nil)
        mrk (atom nil)
        track-fails #(do
                       (when (ok? %)   (reset! mrk %))
                       (when (fail? %) (reset! mrf %)) %)
        dref (fn [x]
               (cond (some-> x meta ::end)
                     (throw (ex-info "Parser Error"
                              (set/rename-keys (into {} x) {:value :last-saw})))
                     (ok? x) (:value x)
                     :else (throw (ex-info "Parser Failure"
                                    (into {} x)))))]
    (->> (sequence
           (comp (map track-fails)
                 (filter (every-pred ok? done?)))
           (parse-one parser input (make-state)))
      first (#(or % @mrf (with-meta @mrk {::end true})))
      dref)))

(defn return [v] (ok v))

(defn any< [input state]
  (if (empty? input) (fail [::empty] [] state)
    (ok (first input)
        (rest  input)
        (update state :seen conj (first input)))))

(defn >>= [m f]
  (fn [input state]
    (sequence
      (mapcat (fn [{:keys [value input state] :as x}]
                (if (fail? x) [x]
                  (parse-one (f value) input state))))
      (parse-one m input state))))

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

(defn <?> [p msg]
  (fn [input state]
    (sequence
      (map #(cond-> % (fail? %)
              (update :cause concat msg)))
      (parse-one p input state))))

;; (ab)
(defn and< [pa pb]
  (m-do
    (ra <- pa)
    (rb <- pb)
    (return [ra rb])))
(def <&> and<)

;; (a|b)
(defn or< [p1 p2]
  (fn [input state]
    (lazy-cat (parse-one p1 input state)
              (parse-one p2 input state))))
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

(defn cmp< [p tag]
  (fn [v] (<?> (when< (partial p v))
               [tag v])))

(def    =< (cmp<    =    :=))
(def not=< (cmp< not= :not=))

(defn seq< [x] (fmap flatten (reduce <&> (map #(=< %) x))))

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
