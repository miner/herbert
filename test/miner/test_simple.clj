(ns miner.test-simple
  (:use miner.herbert clojure.test)
  (:require [clojure.test.check :as sc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as ct :refer (defspec)] ))

(def sort-idempotent-prop
  (prop/for-all [v (gen/vector gen/int)]
    (= (sort v) (sort (sort v)))))

#_ (sc/quick-check 100 sort-idempotent-prop)

(defspec first-element-is-min-after-sorting ;; the name of the test
         100 ;; the number of iterations for simple-check to test
         (prop/for-all [v (gen/such-that not-empty (gen/vector gen/int))]
           (= (apply min v)
              (first (sort v)))))

(defspec herbert-vec-int 100
  (let [vint? (conform '(vec int+))]
    (prop/for-all [v (gen/such-that not-empty (gen/vector gen/int))]
                  (is (vint? v)))))

#_ (defspec buggy-herbert-vec-int 100
  (let [my-test? (conform '(vec int+))]
    (prop/for-all [v (gen/vector gen/int)]
                  (is (vint? v)))))

(def gsymbol (gen/elements '[foo.bar/baz foo bar/foo]))

(defspec herbert-nested 100
  (let [my-test? (conform '{:a int :b [sym+] :c? [str*]})]
    (prop/for-all [v (gen/hash-map :a gen/int 
                                   :b (gen/not-empty (gen/list (gen/elements '[foo bar baz])))
                                   :c (gen/vector gen/string))]
                  (is (my-test? v)))
    (prop/for-all [v (gen/hash-map :a gen/int 
                                   :b (gen/not-empty (gen/list (gen/elements '[foo bar baz]))))]
                  (is (my-test? v)))
    (prop/for-all [v (gen/hash-map :a gen/int 
                                   :b (gen/not-empty (gen/list gsymbol)))]
                  (is (my-test? v)))))


