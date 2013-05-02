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

(deftest complicated []
  (are [val result] (= (conforms? '[{:a [int*]} sym (int x (> x 3))] val) result)
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

