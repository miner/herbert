(ns miner.herbert.predicates
  (:refer-clojure 
   :exclude [float? list? char? empty? map? seq? set? coll? even? odd? pos? neg? zero?])
  (:require [miner.tagged :as tag]))


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
  ([d x] (and (integer? x) (clojure.core/zero? (clojure.core/mod x d))))
  ([d r x] (and (integer? x) (== r (clojure.core/mod x d)))))

(def list? clojure.core/seq?)
(def vec? clojure.core/vector?)
(def seq? clojure.core/sequential?)
(def coll? clojure.core/coll?)
(def map? clojure.core/map?)
(def keys? clojure.core/map?)
(def set? clojure.core/set?)

(defn bool? [x] (or (true? x) (false? x)))

(def even? (numeric integer? clojure.core/even?))
(def odd? (numeric integer? clojure.core/odd?))
(def pos? (numeric number? clojure.core/pos?))
(def neg? (numeric number? clojure.core/neg?))
(def zero? (numeric number? clojure.core/zero?))

(defn literal? [x]
  (or (keyword? x) (number? x) (string? x) (false? x) (true? x) (nil? x) (clojure.core/char? x)))

(defn empty? [x]
  (or (nil? x)
      (and (clojure.core/coll? x) (clojure.core/empty? x))))

(def any? (constantly true))

(defn- regex-match? [regex-or-str x]
  (and (re-matches (if (string? regex-or-str) (re-pattern regex-or-str) regex-or-str) x)
       true))

(defn str?
  ([x] (string? x))
  ([regex x] (and (string? x) (regex-match? regex x))))

(defn char?
  ([x] (clojure.core/char? x))
  ([regex x] (and (clojure.core/char? x) (regex-match? regex (str x)))))

(defn sym? 
  ([x] (symbol? x))
  ([regex x] (and (symbol? x) (regex-match? regex (pr-str x)))))
       
(defn kw? 
  ([x] (keyword? x))
  ([regex x] (and (keyword? x) (regex-match? regex (pr-str x)))))

(defn iter= [iterfn coll]
  (= (seq coll) (when-first [fst coll] 
                  (take (count coll) (iterate iterfn fst)))))

(defn as-fn [f]
  (if (symbol? f)
    (resolve f)
    f))

(defn step?
  ([n coll] (step? + n coll))
  ([f n coll] (and (clojure.core/coll? coll) (iter= (partial (as-fn f) n) coll))))

(defn iter? [f coll] 
  (and (clojure.core/coll? coll) (iter= (as-fn f) coll)))

(defn indexed= [indexfn coll]
  (= coll (map indexfn (range (count coll)))))

(defn indexed? [f coll]
  (and (clojure.core/coll? coll) (indexed= (as-fn f) coll)))

(defn cnt? [n coll]
  (and (or (clojure.core/coll? coll) (string? coll)) (== n (count coll))))

