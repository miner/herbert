(ns miner.herbert.predicates
  (:refer-clojure 
   :exclude [float? list? char? empty? map? seq? set? coll? even? odd? pos? neg? zero?])
  (:require [miner.tagged :as tag]))

;; Notice that we want some common fn vars defined in this ns, not clojure.core.  That
;; allows us to take all the vars ending in "?" as Herbert predicates.

;; predicates should not depend on other files, except maybe util

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

(def zero? (numeric number? clojure.core/zero?))

;; neg? doesn't exactly follow the numeric? pattern
;; the single arg case is lo, not hi
(defn neg? 
  ([x] (and (number? x) (clojure.core/neg? x)))
  ([lo x] (neg? lo 0 x))
  ([lo hi x] (and (number? x) (clojure.core/neg? x) (<= lo x hi))))

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


;; like contains? with maps and sets, that is checks for key presence.
;; linear search with other collections (what most people might expect)
;; notice: for vectors, it is not like contains?
;; For non-collections, it's like = (maybe wrong but possibly convenient)
(defn in? [coll x]
  (cond (not (clojure.core/coll? coll)) (= coll x)
        (or (clojure.core/map? coll) (clojure.core/set? coll)) (contains? coll x)
        ;; x (some #{x} coll)
        ;; x could by falsey so the some doesn't always work
        :else (some #(= % x) coll)))


