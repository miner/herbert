(ns miner.test-generator
  (:require [miner.herbert :refer :all]
            [miner.herbert.generators :as hg]
            [clojure.test :refer :all]
            [clojure.test.check :as sc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as ct :refer (defspec)] ))

(def trials 100)

(defn gen-test
  ([schema] (gen-test schema trials))
  ([schema num]
     (let [confn (conform schema)
           result (hg/check num confn schema)]
       (is (:result result) (str "Schema: " schema " failed "
                                 (first (get-in result [:shrunk :smallest])))))))

(def test-schemas
  '(int
    sym
    kw
    float
    {:a int :b sym}
    {:a [int*] :b? [sym*] :c? kw}
    {kw* odd*}
    {kw+ even+}
    {kw odd}
    {int sym}
    [int {:a sym :b? [int*] :c? {:x? sym :y float}} kw]
    (and int pos (not neg) (not odd) (not zero))
    ;; good to try different symbols in first place (for such-that)
    (and pos int (not neg) (not odd) (not zero))
    (and float (not 0.0))
    (tag inst)
    (tag inst #"[12]\d\d\d[-]0[1-9][-]01T01:0\d:00[.]000[-]00:00")
    (tag uuid)
    ;; UUID format x8-x4-x4-x4-x12, prints with lowercase A-F so be careful with regex
    (tag uuid #"[0-9A-Fa-f]2345678[-]1234[-]1234[-]1234[-]1234567890[Aa][Bb]")
    [(:= n int) n n]
    [(:= mmm {kw (int 0 10)}) (+ (in mmm))]
    [(:= mmm {(kw ":[a-z]") (neg -10)}) {(in mmm) (pos 20 30)}]
    [(+ (even 10) (odd -5 -1)) (int 100 200) (& (int 400 440) (int 500 510))
     (* (int 600 601) (int 770 779)) (? (int 990 999))]
    ))

(deftest schemas
  (doseq [schema test-schemas]
    ;;(println " testing" schema)
    (gen-test schema)))

(defspec kw-key trials
  (hg/property (fn [m] (every? keyword? (keys m))) '{kw* int*}))

(defspec int-vals trials
  (hg/property (fn [m] (every? integer? (vals m))) '{kw+ int+}))

(defspec confirm-val-types trials
  (hg/property (fn [m] (and (integer? (:int m))
                            (string? (:str m))
                            (keyword? (:kw m))))
               '{:int int :str str :kw kw}))


(defspec confirm-nested-types trials
  (hg/property (fn [m] (and (== (get-in m [:v 2 :int]) 42)
                            (string? (:str m))))
               '{:v (vec kw sym {:int 42} float) :str str}))

(defspec basic-regexs trials
  (hg/property (fn [s] (re-matches #"f.o" s))
               '(str "f.o")))

(defspec more-regexs trials
  (hg/property (fn [s] (re-matches #"f.*o+" s))
               '(str "f.*o+")))

(defspec basic-kws trials
  (hg/property (fn [k] (and (keyword? k) (re-matches #":k[a-z]o" (str k))))
               '(kw ":k[a-z]o")))

(defspec more-kws trials
  (hg/property (fn [k] (and (keyword? k) (re-matches #":k[a-z]/f\d*o+" (str k))))
               '(kw #":k[a-z]/f\d*o+")))

(defspec basic-syms trials
  (hg/property (fn [s] (and (symbol? s) (re-matches #"s[a-z]o" (str s))))
               '(sym "s[a-z]o")))

(defspec more-syms trials
  (hg/property (fn [s] (and (symbol? s) (re-matches #"s[a-z]/f\d*o+" (str s))))
               '(sym #"s[a-z]/f\d*o+")))

(let [re #"((s[a-z]*)|\d+)(x[a-j]y|y[^-A-Za-z]z|pq|PQ)\w@[^A-Zaz]"]
  (defspec str-regexes 1000
    (hg/property (fn [s] (and (string? s) (re-matches re s))) (list 'str re))))

(let [re #"\.\?\w\[\][-.?]"]
  (defspec escaped-str-regexes 1000
    (hg/property (fn [s] (and (string? s) (re-matches re s))) (list 'str re))))


(def test-recursive-schemas
  '[
    (grammar rint
             rint (or int [rint]))
    
    (grammar [a b]
             a (or :a [:av a])
             b (or :b [:bv b]))
    
    (:= a (or :a [:b (+ a)]))

    (:= a (or :a [:av a])) (:= b (or :b [:bv b]))

    (:= a (or :a [:av a+])) (:= b (or :b [:bv b]))

    ])


(deftest recursive-schemas
  (doseq [schema test-recursive-schemas]
    ;;(println " testing" schema)
    (gen-test schema)))



(comment
;; some properties that should fail
(def my-bad-prop (hg/property (fn [[a b]] (> a b)) '[int int]))

(defspec my-little-schema-test 100 my-bad-prop)

(defspec another-bad-schema-test 100 
  (hg/property (fn [[a b]] (> (* 100 (inc b)) a)) '[int int]))
;; end comment
)



  
