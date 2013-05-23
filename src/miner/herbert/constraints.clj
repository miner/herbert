(ns miner.herbert.constraints
  (:refer-clojure :exclude [int num float list vec char str empty keys map seq set mod])
  )

(defn- numeric 
  ([pred]
     (fn pnum
       ([x] (pred x))
       ([hi x] (pnum 0 hi x))
       ([lo hi x] (and (pred x) (<= lo x hi)))))

  ([pred pred2]
     (fn pnum
       ([x] (and (pred x) (pred2 x)))
       ([hi x] (pnum 0 hi x))
       ([lo hi x] (and (pred x) (pred2 x) (<= lo x hi))))))

(def int (numeric integer?))

(def num (numeric number?))

(def float (numeric float?))

(defn mod
  ([d x] (and (integer? x) (zero? (clojure.core/mod x d))))
  ([d r x] (and (integer? x) (== r (clojure.core/mod x d)))))

(def list seq?)
(def char char?)
(def sym symbol?)
(def kw keyword?)
(def vec vector?)
(def seq sequential?)
(def coll coll?)
(def keys map?)
(def map map?)
(def set set?)

(defn bool [x] (or (true? x) (false? x)))

(def even (numeric integer? even?))

(def odd (numeric integer? odd?))

(defn literal [x]
  (or (keyword? x) (number? x) (string? x) (false? x) (true? x) (nil? x)))

(defn empty [x]
  (or (nil? x)
      (and (coll? x) (empty? x))))

(def any (constantly true))

(defn str
  ([x] (string? x))
  ([regex x] (and (string? x) 
                  (re-matches (if (string? x) (re-pattern regex) regex) x)
                  true)))
