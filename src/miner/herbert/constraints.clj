(ns miner.herbert.constraints
  (:refer-clojure 
   :exclude [float? list? char? empty? map? seq? set? coll? even? odd?])
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

(def int? (numeric integer?))

(def num? (numeric number?))

(def float? (numeric clojure.core/float?))

(defn mod?
  ([d x] (and (integer? x) (zero? (clojure.core/mod x d))))
  ([d r x] (and (integer? x) (== r (clojure.core/mod x d)))))

(def list? clojure.core/seq?)
(def char? clojure.core/char?)
(def vec? clojure.core/vector?)
(def seq? clojure.core/sequential?)
(def coll? clojure.core/coll?)
(def map? clojure.core/map?)
(def set? clojure.core/set?)

(defn bool? [x] (or (true? x) (false? x)))

(def even? (numeric integer? clojure.core/even?))

(def odd? (numeric integer? clojure.core/odd?))

(defn literal? [x]
  (or (keyword? x) (number? x) (string? x) (false? x) (true? x) (nil? x)))

(defn empty? [x]
  (or (nil? x)
      (and (clojure.core/coll? x) (clojure.core/empty? x))))

(def any? (constantly true))

(defn- regex-match? [regex-or-str x]
  (and (re-matches (if (string? regex-or-str) (re-pattern regex-or-str) regex-or-str) x)
       true))

(defn str?
  ([x] (string? x))
  ([regex x] (regex-match? regex x)))

(defn sym? 
  ([x] (symbol? x))
  ([regex x] (and (symbol? x) (regex-match? regex (pr-str x)))))
       
(defn kw? 
  ([x] (keyword? x))
  ([regex x] (and (keyword? x) (regex-match? regex (pr-str x)))))

