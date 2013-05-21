(ns miner.test-herbert
  (:use clojure.test
        miner.herbert))

(deftest basics []
  (is (conforms? 'int 10))
  (is (conforms? 'str "foo"))
  (is (conforms? 'sym 'foo))
  (is (conforms? 'kw :foo))
  (is (conforms? 'float 1.23))
  (is (not (conforms? 'sym :foo))))

(deftest nested []
  (is (conforms? '[int sym str kw] '(10 foo "foo" :foo)))
  (is (not (conforms? '[int sym str kw] '(10 :a foo "foo" :foo))))
  (is (conforms? '[(* kw int)] '(:a 10 :b 20 :c 30)))
  (is (conforms? '[(+ kw int sym)] '(:a 10 foo :b 20 bar))))

;; SEM FIXME -- hand to use qualified name to access my local fn.  Seems like a bug.
(defn three+ [n]
  (> n 3))

(deftest complicated []
  (are [val result] (= (conforms? '[{:a [int*]} sym (int miner.test-herbert/three+)] val) result)
       '[{:a [1]} foo 4] true
       '[{:a [1 2 3]} foo 4] true
       '[{:a (1 2 3)} foo 4] true
       '[{:a []} foo 4] true
       '[{:a [:b 1 2 3]} foo 4] false
       '[{:b [1 2 3]} foo 4] false
       '[{:a [:b 1 2 3]} 4 foo] false))

(deftest maps []
  (are [val result] (= (conforms? '{:a int :b sym :c? str} val) result)
       {:a 1 :b 'foo :c "foo"}   true
       {:a 1 :b 'foo}   true
       {:a 1 :b 'foo :d 'bar}   true  
       {:a 1 :b 'foo :c 'bar}   false
       {:a :kw :b 'foo}   false
       {:b 'foo :c "foo"} false)
  (are [val result] (= (conforms? '{:a int :b sym :c? nil} val) result)
       ;; :c is essentiall disallowed, technically can only have nil value
       {:a 1 :b 'foo :c "foo"}   false
       {:a 1 :b 'foo}   true
       {:a 1 :b 'foo :d 'bar}   true  
       {:a 1 :b 'foo :c 'bar}   false
       {:a 1 :b 'foo :c nil}  true))

(deftest sets []
  (are [val result] (= (conforms? '#{:a :b :c} val) result)
       #{:a :b :c}   true
       #{:d :c :a :b}   true
       #{:d :c :a}   false)
  (are [val result] (= (conforms? '#{int :a :b :c} val) result)
       #{:a 10 :b :c}   true
       #{:d :c :a :b}   false
       #{10 20}   false
       #{:b 10 :c 20 :a}   true)
  (are [val result] (= (conforms? '#{int sym} val) result)
       #{10 :a :b 'foo}   true
       #{:d :c :a :b 10} false
       #{'foo 10}   true)
  (are [val result] (= (conforms? '#{kw*} val) result)
       #{:a :b :c}   true
       #{:d :c :a :b}   true
       #{:d :c :a}   true
       #{} true
       #{:d 10 :a} false
       #{:a :b :c 'foo} false)
  (are [val result] (= (conforms? '#{kw+} val) result)
       #{:a :b :c}   true
       #{:d :c :a :b}   true
       #{:d :c :a}   true
       #{} false
       #{:d 10 :a} false
       #{:a :b :c 'foo} false)
  (are [val result] (= (conforms? '#{kw?} val) result)
       #{:a :b :c}   false
       #{:d :c :a :b}   false
       #{:a}   true
       #{} true
       #{10} false
       #{:a 'foo} false))

;; Note (or xxx+ yyy+) works but (or xxx* yyy*) can fail since zero xxx matches and yyy doesn't
;; get the chance after that.  Remember, no backtracking.  The + lets it work as expected.
(deftest or-plus []
  (are [val] (conforms? '[int kw (or sym+ str+)] val)
       '[10 :a foo bar baz]
       '[10 :a "foo" "bar" "baz"])
  (are [val] (conforms? '[int kw (or str+ sym+)] val)
       '[10 :a foo bar baz]
       '[10 :a "foo" "bar" "baz"]))

(defn plus2 [n] (+ 2 n))
(defn nodd [n] (if (odd? n) (dec (- n)) (inc n)))

(deftest stepping []
  (is (conforms? '[(int+ even? :step 4)] [2 6 10 14]))
  (is (not (conforms? '[(int+ even? :step 4)] [2  10  14])))
  (is (conforms? '[(int+ :iter miner.test-herbert/plus2)] [11 13  15 17 19]))
  (is (conforms? '[(int+ :indexed miner.test-herbert/nodd)] [1 -2 3 -4 5 -6])))

(deftest binding-with-guard []
  (is (conforms? '[(:n int) (:m int) (guard (= (* 2 (:n %)) (:m %))) ] [2 4]))
  (is (not (conforms? '[(:n int) (:m int) (guard (= (* 3 (:n %)) (:m %))) ] [2 4])))
  (is (conforms? '[(:ns int* :step 3) (guard (== (count (:ns %)) 4))] [2 5 8 11])))

(deftest and-or []
  (is (conforms? '[(or int kw) (and int (int even?))] [:a 4]))
  (is (conforms? '[(or kw sym) (and int (int odd?))] [:a 7])))

#_ (deftest broken []
     (is (conforms? '[(or int kw sym) (not int)] [:a :a]))
     (is (conforms? '[(or int kw sym) (and num (not (int even?)))] ['a 6.1])))
