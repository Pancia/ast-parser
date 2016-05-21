(ns symbolizer.funk)

(defn map-vals
  "Generate a new map resulting from the application of `f` on each value of map `m`."
  [f m] (into {} (for [[k v] m] [k (f v)])))

(defn positions
  "Returns a list of the indexes of items in a collection matching the given predicate."
  [pred coll]
  (keep-indexed (fn [idx x] (when (pred x) idx)) coll))

(defn varargs
  "Returns all of the arguments in a list"
  [& args] args)

(def ID identity)
(def F (constantly false))
(def T (constantly true))
(def p partial)

(defn truthify
  "Modifies a predicate to return false or the thing itself
  based on what the predicate returned
  ie: it tries to return a truthy value instead of a boolean
  NOTE: truthifying nil? & false? will return true
  if they get passed nil & false respectively"
  [p] (fn [x] (if (p x) (if x x true) false)))

(defn juxt->
  "Applies each function (left to right) to the argument cumulatively
  eg: ((juxt-> inc inc dec) 0) => [1 2 1]"
  [& fns]
  (->> (reductions conj '() fns)
       (drop 1)
       (map (p apply comp))
       (apply juxt)))

(defn flip [f]
  (fn
    ([] (f))
    ([x] (f x))
    ([x y] (f y x))
    ([x y z] (f z y x))
    ([a b c d] (f d c b a))
    ([a b c d & rst]
     ((comp (p apply f)
            #(concat % [d c b a])
            reverse)
      rst))))
