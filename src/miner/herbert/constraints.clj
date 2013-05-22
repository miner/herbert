(ns miner.herbert.constraints
  (:refer-clojure :exclude [int num float list vec char str empty keys map seq set mod])
  )

(defn int
  ([x] (integer? x))
  ([hi x] (and (integer? x) (<= x hi)))
  ([lo hi x] (and (integer? x) (<= lo x hi))))

(defn num
  ([x] (number? x))
  ([hi x] (and (number? x) (<= x hi)))
  ([lo hi x] (and (number? x) (<= lo x hi))))

(defn float
  ([x] (float? x))
  ([hi x] (and (float? x) (<= x hi)))
  ([lo hi x] (and (float? x) (<= lo x hi))))

(defn mod
  ([d x] (and (integer? x) (zero? (mod x d))))
  ([d r x] (and (integer? x) (== r (mod x d)))))

(def list seq?)
(def char char?)
(def str string?)
(def sym symbol?)
(def kw keyword?)
(def vec vector?)
(def seq sequential?)
(def coll coll?)
(def keys map?)
(def map map?)
(def set set?)

(defn bool [x] (or (true? x) (false? x)))

(defn even 
  ([x] (and (integer? x) (even? x)))
  ([hi x] (and (integer? x) (even? x) (<= x hi)))
  ([lo hi x] (and (integer? x) (even? x) (<= lo x hi))))

(defn odd
  ([x] (and (integer? x) (odd? x)))
  ([hi x] (and (integer? x) (odd? x) (<= x hi)))
  ([lo hi x] (and (integer? x) (odd? x) (<= lo x hi))))

(defn literal [x]
  (or (keyword? x) (number? x) (string? x) (false? x) (true? x) (nil? x)))

(defn empty [x]
  (or (nil? x)
      (and (coll? x) (empty? x))))

(def any (constantly true))

