(ns miner.test-generator
  (:use miner.herbert clojure.test)
  (:require [miner.herbert.generators :as hg]
            [clojure.test.check :as sc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as ct :refer (defspec)] ))

(defn gen-test 
  ([schema] (gen-test schema 100))
  ([schema num]
     (let [confn (conform schema)]
       (doseq [v (hg/sample schema num)]
         (is (confn v))))))

(def test-schemas
  '(int
    sym
    kw
    float
    {:a int :b sym}
    {:a [int*] :b? [sym*] :c? kw}
    {kw* odd*}
    {kw+ even+}
    [int {:a sym :b? [int*] :c? {:x? sym :y float}} kw]
    (and int pos (not neg) (not odd) (not zero))
    (and float (not 0.0))
    ))

(doseq [schema test-schemas]
  ;;(println " testing" schema)
  (gen-test schema))

(defspec kw-key 100
  (hg/property (fn [m] (every? keyword? (keys m))) '{kw* int*}))

(defspec int-vals 100
  (hg/property (fn [m] (every? integer? (vals m))) '{kw+ int+}))

(defspec confirm-val-types 100
  (hg/property (fn [m] (and (integer? (:int m))
                            (string? (:str m))
                            (keyword? (:kw m))))
               '{:int int :str str :kw kw}))


(defspec confirm-nested-types 100
  (hg/property (fn [m] (and (== (get-in m [:v 2 :int]) 42)
                            (string? (:str m))))
               '{:v (vec kw sym {:int 42} float) :str str}))


(comment
;; some properties that should fail
(def my-bad-prop (hg/property (fn [[a b]] (> a b)) '[int int]))

(defspec my-little-schema-test 100 my-bad-prop)

(defspec another-bad-schema-test 100 
  (hg/property (fn [[a b]] (> (* 100 (inc b)) a)) '[int int]))
;; end comment
)



  
