(ns miner.test-herbert
  (:require [miner.herbert :refer :all]
            [clojure.test :refer :all]
            miner.herbert.predicates
            [miner.tagged :as tag]
            clojure.string))

(deftest basics
  (is (conforms? 'int 10))
  (is (conforms? 'str "foo"))
  (is (conforms? 'sym 'foo))
  (is (conforms? 'kw :foo))
  (is (conforms? 'float 1.23))
  (is (not (conforms? 'sym :foo))))

(deftest numbers
  (is (conforms? 'even 10))
  (is (not (conforms? 'even 'foo)))
  (is (conforms? '(and int pos (not neg) (not odd) (not zero)) 10))
  (is (not (conforms? '(or int pos neg odd zero) 'foo)))
  (is (conforms? '[(* (or neg zero))] [-1 0.0 0 -100.0 0])))

(deftest nested 
  (is (conforms? '[int sym str kw] '(10 foo "foo" :foo)))
  (is (not (conforms? '[int sym str kw] '(10 :a foo "foo" :foo))))
  (is (conforms? '[(* kw int)] '(:a 10 :b 20 :c 30)))
  (is (conforms? '[(+ kw int sym)] '(:a 10 foo :b 20 bar))))

(deftest complicated
  (are [val result] (= (conforms? '[{:a [(odd* 20)]} sym (mod 4)] val) result)
       '[{:a [1]} foo 4] true
       '[{:a [1 3 5]} foo 24] true
       '[{:a (1 2 3)} foo 4] false
       '[{:a []} foo 84] true
       '[{:a [:b 1 2 3]} foo 4] false
       '[{:b [1 2 3]} foo 4] false
       '[{:a [11 21 33]} foo 8] false))

(deftest kw-maps
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

(deftest other-keys
  (are [val result] (= (conforms? '{a int b sym "c" str} val) result)
       '{a 1 b foo "c" "foo"}   true
       '{a 1 b foo "c" 'bar}   false
       '{a :kw b foo "c" "foo"}   false))

(deftest quoted-kws
  (are [val result] (= (conforms? '{':a? int ':b sym} val) result)
       {:a? 1 :b 'foo}   true
       {:a 1 :b 'foo}   false
       {:a? 1 :b :foo}   false
       {:a? 1 :b 'foo :d 'bar}   true  
       {:a 'foo :a? 1 :b 'foo :c nil}  true))

(deftest single-optional
  (is (conforms? '{:a? int} {:a 1}))
  (is (conforms? '{:a? int} {:a 1 :b 2}))
  (is (conforms? '{:a? int} {:b 1}))
  (is (conforms? '{:a? int :b? int} {:b 1}))
  (is (not (conforms? '{:a? int} {:a :b}))))

(deftest sets
  (are [val result] (= (conforms? '#{:a :b :c} val) result)
       #{:a :b :c}   true
       #{:d :c :a :b}   true
       [:a :b :c] false
       '(:a :b :c) false
       #{:d :c :a}   false)
  (are [val result] (= (conforms? '#{0 1 2} val) result)
       #{0 1 2}   true
       #{1 2} false
       #{1 2 3 0} true
       [0 1 2] false
       '(0 1 2) false
       {0 :a 1 :b 2 :c} false)
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

(deftest sets2
  (are [val result] (= (conforms? '(set :a :b :c) val) result)
       #{:a :b :c}   true
       #{:d :c :a :b}   true
       #{:d :c :a}   false)
  (are [val result] (= (conforms? '(set int :a :b :c) val) result)
       #{:a 10 :b :c}   true
       #{:d :c :a :b}   false
       #{10 20}   false
       #{:b 10 :c 20 :a}   true)
  (are [val result] (= (conforms? '(set int sym) val) result)
       #{10 :a :b 'foo}   true
       #{:d :c :a :b 10} false
       #{'foo 10}   true)
  (are [val result] (= (conforms? '(set kw*) val) result)
       #{:a :b :c}   true
       #{:d :c :a :b}   true
       #{:d :c :a}   true
       #{} true
       #{:d 10 :a} false
       #{:a :b :c 'foo} false)
  (are [val result] (= (conforms? '(set (* kw)) val) result)
       #{:a :b :c}   true
       #{:d :c :a :b}   true
       #{:d :c :a}   true
       #{} true
       #{:d 10 :a} false
       #{:a :b :c 'foo} false)
  (are [val result] (= (conforms? '(set kw+) val) result)
       #{:a :b :c}   true
       #{:d :c :a :b}   true
       #{:d :c :a}   true
       #{} false
       #{:d 10 :a} false
       #{:a :b :c 'foo} false)
  (are [val result] (= (conforms? '(set (+ kw)) val) result)
       #{:a :b :c}   true
       #{:d :c :a :b}   true
       #{:d :c :a}   true
       #{} false
       #{:d 10 :a} false
       #{:a :b :c 'foo} false)
  (are [val result] (= (conforms? '(set kw?) val) result)
       #{:a :b :c}   false
       #{:d :c :a :b}   false
       #{:a}   true
       #{} true
       #{10} false
       #{:a 'foo} false)
  (are [val result] (= (conforms? '(set (? kw)) val) result)
       #{:a :b :c}   false
       #{:d :c :a :b}   false
       #{:a}   true
       #{} true
       #{10} false
       #{:a 'foo} false))

;; Note (or xxx+ yyy+) works but (or xxx* yyy*) can fail since zero xxx matches and yyy doesn't
;; get the chance after that.  Remember, no backtracking.  The + lets it work as expected.
(deftest or-plus
  (are [val] (conforms? '[int kw (or sym+ str+)] val)
       '[10 :a foo bar baz]
       '[10 :a "foo" "bar" "baz"])
  (are [val] (conforms? '[int kw (or str+ sym+)] val)
       '[10 :a foo bar baz]
       '[10 :a "foo" "bar" "baz"]))

(defn plus2 [n] (+ 2 n))
(defn nodd [n] (if (odd? n) (dec (- n)) (inc n)))

(deftest stepping2
  (is (conforms? '(and [even+] (step 4)) [2 6 10 14]))
  (is (conforms? '(step * 3) [2 6 18 54]))
  (is (not (conforms? '(and [even+] (step 4)) [2  10  14])))
  (is (conforms? '(and [int+] (iter miner.test-herbert/plus2)) [11 13  15 17 19]))
  (is (conforms? '(and [int+] (indexed miner.test-herbert/nodd)) [1 -2 3 -4 5 -6])))

(deftest binding-with-when 
  (is (conforms? '[(:= n int) (:= m int) (when (= (* 2 n) m)) ] [2 4]))
  (is (conforms? '[(:= n int) [(:= ms kw*)] (when (= (count ms) n))] '[3 [:a :b :c]]))
  (is (not (conforms? '[(:= n int) (:= m int) (when (= (* 3 n) m)) ] [2 4])))
  (is (conforms? '(& (:= ns (and [int+] (step 3))) (when (== (count ns) 4))) [2 5 8 11]))
  ;; better equivalent
  (is (conforms? '(and [int+] (step 3) (cnt 4)) [2 5 8 11])))

(deftest binding-with-implied-when 
  (is (conforms? '[(:= n int) (:= m int) (>= (* 2 n) m) ] [2 4]))
  (is (conforms? '[(:= n int) [(:= ms kw*)] (<= (count ms) n)] '[3 [:a :b :c]]))
  (is (not (conforms? '[(:= n int) (:= m int) (== (* 3 n) m) ] [2 4])))
  (is (conforms? '(& (:= ns (and [int+] (step 3))) (== (count ns) 4)) [2 5 8 11])))

(deftest step-count
  (is (conforms? '(& (:= ns [int+]) (when (and (miner.herbert.predicates/step? 3 ns) 
                                              (miner.herbert.predicates/cnt? 4 ns)))) 
                 [2 5 8 11]))
  (is (conforms? '(and [int+] (step 3) (cnt 4)) [2 5 8 11]))
  (is (not (conforms? '(and [int+] (step 3) (cnt 4)) [2 5 8 11 14])))
  (is (not (conforms? '(and [int+] (step 3) (cnt 4)) [2 5 9 11])))
  (is (not (conforms? '(and [int+] (step 3) (cnt 4)) '[2 foo 8 11])))
  (is (not (conforms? '(and [int+] (step 3) (cnt 4)) 'foo))))

(deftest and-or
  (is (conforms? '[(or int kw) (and int even)] [:a 4]))
  (is (conforms? '[(or kw sym) (and int odd)] [:a 7])))

(deftest not-constraints
  (is (conforms? '[(not sym)] [:a]))
  (is (conforms? '[(or int kw sym) (not int)] [:a :a]))
  (is (conforms? '[(or int kw sym) (and num (not even))] ['a 6.1])))

(defn over3? [x] (> x 3))

;; not the best way to handle this case, but imagine a fancier function
(deftest with-constraints
  (is (conforms? '(grammar [over3*] over3 (pred miner.test-herbert/over3?)) [4 5 6 9]))
  (is (not (conforms? '(grammar [over3*] over3 (pred miner.test-herbert/over3?)) [4 5 2 9])))
  (is (conforms? '(grammar [over3*] over3 (pred miner.test-herbert/over3?)) []))
  (is (conforms? '(grammar [over3? int] over3 (pred miner.test-herbert/over3?)) [4 2]))
  (is (conforms? '(grammar [over3? int] over3 (pred miner.test-herbert/over3?)) [2])))

(deftest pred-args
  (is (conforms? '[(+ (even 20)) kw] [4 10 18 :a]))
  (is (conforms? '[(even+ 20) kw] [4 10 18 :a]))
  (is (not (conforms? '[(even+ 20) kw] [4 30 18 :a])))
  (is (conforms? '[(:= lo int) (:= hi int) (even+ lo hi) kw] [4 20 14 10 18 :a]))
  (is (conforms? '[(:= lo int) (:= hi int) (:= es even+ lo hi) 
                   (when (miner.herbert.predicates/step? 4 es)) kw]
                 [4 20 6 10 14 18 :a]))
  (is (not (conforms? '[(:= lo int) (:= hi int) (:= es even+ lo hi) 
                        (when (miner.herbert.predicates/step? 4 es)) kw]
                      [4 20 6 10 16 18 :a]))))

(deftest strings
  (is (conforms? 'str "foobar"))
  (is (conforms? '(str "f.*r") "foobar"))
  (is (not (conforms? '(str "f.*r") "xfoobar"))))

(deftest nested-map-when
  (is (conforms? '(& {:a (:= a int) :b {:bb (:= bb int)}} (when (== a bb))) 
                 {:a 10 :b {:bb 10}}))
  (is (not (conforms? '(& {:a (:= a int) :b {:bb (:= bb int)}} (when (== a bb))) 
                      {:a 11 :b {:bb 10}}))))

(deftest solo-constraints-for-equality
  (is (conforms? '{:a (:= a int) :b {:bb (a)}} {:a 10 :b {:bb 10}}))
  (is (conforms? '{:a (:= a int) :b {:bb a}} {:a 10 :b {:bb 10}}))
  (is (conforms? '{:a (:= a int) :b {:bb [a+]}} {:a 10 :b {:bb [10 10 10]}}))
  (is (not (conforms? '{:a (:= a int) :b {:bb [a+]}} {:a 10 :b {:bb 10}})))
  (is (not (conforms? '{:a (:= a int) :b {:bb a}} {:a 10 :b {:bb 11}}))))

(deftest solo-count
  (is (conforms? '(& {:a (:= a int) :b (:= b sym) :c? (:= c [b+])} (when (= (count c) a))) 
             '{:a 2 :b foo :c [foo foo]})))

(deftest bind-args 
  (is (conforms? '[(:= a int) (:= b int) (:= c int a b)] [3 7 5])))

(deftest underbar-bind-args 
  ;; underbar _ should be ignored as a binding name
  (is (conforms? '[(:= _ int) (:= _ sym "f.*") (:= _ (str ".*r"))] '[42 foo "bar"])))

(deftest as-bind-args 
  (is (conforms? '[(:= a int) (:= b (int 2 15)) (:= c (int a b))] [3 7 5]))
  (is (not (conforms? '[(:= a int) (:= b (int 2 15)) (:= c (int a b))] [3 4 5])))
  (is (conforms? '[(:= _ int) (:= _ (sym "f.*")) (:= _ (str ".*r"))] '[42 foo "bar"]))
  (is (not (conforms? '[(:= _ int) (:= _ (sym "f.*")) (:= _ (str ".*r"))] '[42 "foo" bar]))))

(deftest quoted-syms
  (is (conforms? '[int 'int sym] '[42 int foo]))
  (is (not (conforms? '[int 'int sym] '[42 13 foo]))))

(deftest regex-forms
  (is (conforms? '(kw ":foo/.*") :foo/bar))
  (is (not (conforms? '(kw ":foo/.*") :foo)))
  (is (conforms? '(kw ":miner[.]test-herbert/foo") ::foo))
  (is (conforms? '(str "foo/.*") "foo/bar"))
  (is (conforms? '(str "fo+.ar") "fooooobar"))
  (is (conforms? '(sym "user/.*") 'user/foobar))
  (is (not (conforms? '(sym "user/.*") :user/foobar))))

(defn palindrome? [s]
  (and (string? s)
       (= s (clojure.string/reverse s))))

(deftest grammar
  (is (conforms? '(grammar {:a over3 :b long}
                          over3 (pred miner.test-herbert/over3?)
                          long int)
                 {:a 42 :b 42}))
  (is (conforms? '(grammar [pal+]
                          palindrome (pred miner.test-herbert/palindrome?)
                          pal {:len (:= len int) :palindrome (and palindrome (cnt len))})
                 [{:palindrome "civic" :len 5}
                  {:palindrome "kayak" :len 5} 
                  {:palindrome "level" :len 5}
                  {:palindrome "ere" :len 3}
                  {:palindrome "racecar" :len 7}])))

(deftest recursive
  (let [r? (conform '(:= a (or :a [:b (+ a)])))]
    (is (r? ':a))
    (is (r? [:b :a]))
    (is (r? [:b [:b :a :a] :a]))))

(deftest recursive-grammar
  (let [r? (conform '(grammar [a b]
                         a (or :a [:av a])
                         b (or :b [:bv b])) )]
    (is (r? [:a :b]))
    (is (r? [[:av :a] :b]))
    (is (r? [[:av :a] [:bv :b]]))
    (is (r? [[:av [:av :a]] [:bv [:bv :b]]]))))

(deftest recursive-doubly
  (let [r? (conform '[(:= a (or :a [:av a])) (:= b (or :b [:bv b]))] )]
    (is (r? [:a :b]))
    (is (r? [[:av :a] :b]))
    (is (r? [[:av :a] [:bv :b]]))
    (is (r? [[:av [:av :a]] [:bv [:bv :b]]]))))

(deftest recursive-plus
  (let [r? (conform '[(:= a (or :a [:av a+])) (:= b (or :b [:bv b]))] )]
    (is (r? [:a :b]))
    (is (r? [[:av :a :a] :b]))
    (is (r? [[:av :a [:av :a [:av :a]]] [:bv :b]]))
    (is (r? [[:av [:av :a :a :a] :a] [:bv [:bv :b]]]))))


(deftest quantified-keys-vals
  (is (conforms? '{kw* int*} {:a 42}))
  (is (conforms? '{kw* int*} {}))
  (is (conforms? '{kw+ int+} {:a 42}))
  (is (not (conforms? '{kw+ int+} {})))
  (is (not (conforms? '{kw* int*} {'a 42})))
  (is (not (conforms? '{kw* int*} {:a 'b52})))
  (is (conforms? '{(* (or sym kw)) (* (or sym int))} {:a 'b52}))
  (is (conforms? '(map (+ (or sym kw)) (+ (or sym int))) {'b 'b52}))
  (is (conforms? '(map (* (or sym kw)) (* (or sym int))) {'b 52}))
  (is (conforms? '(map sym* any*) {'b 52}))
  (is (conforms? '(map any* any*) {'b 52}))
  (is (conforms? 'map {'b 52}))
  (is (not (conforms? '(map (* (or sym kw)) (* (or sym int))) {:a :b52}))))
  
(defrecord Foo [a])

(deftest on-records
  (is (conforms? '{:a int} (->Foo 42)))
  (is (conforms? '(& (:= rec {:a int}) (when (instance? miner.test_herbert.Foo rec)))
                 (->Foo 42)))
  (is (not (conforms? '(& (:= rec {:a int}) (when (instance? miner.test_herbert.Foo rec)))
                 {:a 42})))
  (is (not (conforms? '(& (:= rec {:a int}) (when (instance? miner.test_herbert.Foo rec)))
                 {->Foo "bar"}))))

(deftest on-records-by-class
  (is (conforms? '{:a int} (->Foo 42)))
  (is (conforms? '(and {:a int} (class miner.test_herbert.Foo))
                 (->Foo 42)))
  (is (not (conforms? '(and {:a int} (class miner.test_herbert.Foo))
                 {:a 42})))
  (is (not (conforms? '(and {:a int} (class miner.test_herbert.Foo))
                 {->Foo "bar"}))))

(deftest records-by-tag
  (is (conforms? '(tag miner.test-herbert/Foo) (->Foo 42)))
  (is (conforms? '(tag miner.test-herbert/Foo {:a 42}) (->Foo 42)))
  (is (conforms? '(tag "miner[.]test-.*/Foo") (->Foo 42)))
  (is (not (conforms? '(tag "miner/test-.*Foo") (->Foo 42))))
  (is (conforms? '(tag foo.bar/Baz) (tag/read-string "#foo.bar/Baz {:a 42}")))
  (is (not (conforms? '(tag miner.test-herbert/Bad) (->Foo 42))))
  (is (not (conforms? '(tag foo.wrong/Bar) (tag/read-string "#foo.bar/Baz {:a 42}"))))
  (is (conforms? '(tag miner.test-herbert/Foo {:a int}) (->Foo 42)))
  (is (conforms? '(tag "miner[.]test-herbert/F.*" {:a int}) (->Foo 42)))
  (is (not (conforms? '(tag "miner[.]test-herbert/F.*" {:a sym}) (->Foo 42))))
  (is (conforms? '(tag foo.bar/Baz {:a int}) (tag/read-string "#foo.bar/Baz {:a 42}")))
  (is (not (conforms? '(tag miner.test-herbert/Foo {:b any}) (->Foo 42))))
  (is (not (conforms? '(tag foo.wrong/Bar {:a int}) (tag/read-string "#foo.bar/Baz {:a 42}")))))

(deftest dates-and-uuid
  (is (conforms? '(tag inst) (java.util.Date.)))
  (is (conforms? '(tag inst) (java.sql.Timestamp. 0)))
  (is (conforms? '(tag inst) (java.util.Calendar/getInstance)))
  (is (conforms? '(tag inst "1970-01-01T00:00:00.000-00:00") (java.util.Date. 0)))
  (is (conforms? '(tag inst "1970.*") (java.util.Date. 0)))
  (is (conforms? '(tag inst "1970.*") #inst "1970"))
  (is (not (conforms? '(tag inst "1970.*") "1970")))
  (is (not (conforms? '(tag inst "1980.*") #inst "1990"))))

(deftest nested-inst-and-uuid
  (is (conforms? '[[:create-resource
                   [:resource/id (tag uuid "552fc7b3-8905-46fd-b50b-dd613c940504")]
                   {:occurrence/place 281474976711832,
                    :occurrence/start-time (tag inst)}]
                  [:create-resource
                   [:resource/id (tag uuid)]
                   {:occurrence/place 281474976711832,
                    :occurrence/start-time (tag inst "1970-01-01T00:00:00.002-00:00")}]
                  [:create-resource
                   [:resource/id (tag uuid)]
                   {:occurrence/place 281474976711832,
                    :occurrence/start-time (tag inst "1970-01-01T00:00:00.003-00:00")}]]

                 [[:create-resource
                   [:resource/id #uuid "552fc7b3-8905-46fd-b50b-dd613c940504"]
                   {:occurrence/place 281474976711832,
                    :occurrence/start-time #inst "1970-01-01T00:00:00.001-00:00"}]
                  [:create-resource
                   [:resource/id #uuid "552fc7b3-5ffa-4eb8-b3ac-8e3b15bcead0"]
                   {:occurrence/place 281474976711832,
                    :occurrence/start-time #inst "1970-01-01T00:00:00.002-00:00"}]
                  [:create-resource
                   [:resource/id #uuid "552fc7b3-af79-489c-8eaf-09e3845106d0"]
                   {:occurrence/place 281474976711832,
                    :occurrence/start-time #inst "1970-01-01T00:00:00.003-00:00"}]])))

(deftest readme-examples
  (is (= ((conform '[(:= A int) (:= B int) (:= C int+ A B)]) [3 7 4 5 6])
         '{C [4 5 6], B 7, A 3}))
  (is (= ((conform '[(:= MAX int) (:= XS int+ MAX)]) [7 3 5 6 4])
         '{XS [3 5 6 4], MAX 7}))
  (is (conforms? '{:a int :b [sym+] :c str} '{:a 42 :b [foo bar baz] :c "foo"}))
  (is (conforms? '{:a int :b sym :c? [str*]} '{:a 1 :b foo}))
  (is (conforms? '{:a int :b sym :c? [str*]} '{:a 1 :b foo :c ["foo" "bar" "baz"]}))
  (is (not (conforms? '{:a int :b sym :c? [str*]} '{:a foo :b bar})))
  (is (conforms? '{:a (:= A int) :b sym :c? [A+]} '{:a 1 :b foo :c [1 1 1]}))
  (is (conforms? '{:x? sym ':k? int} {:k? 10}))
  (is (not (conforms? '{:x? sym ':k? int} {:k 10}))))

(deftest grammar-with-regex
  (is (conforms? '(grammar [person+] 
                          phone (str #"\d{3}+-\d{3}+-\d{4}+") 
                          person {:name str :phone phone}) 
                 [{:name "Steve" :phone "408-555-1212"}
                  {:name "Jenny" :phone "415-867-5309"}]))
  ;; only difference is a regex above and a string with escape notation below
  (is (conforms? '(grammar [person+] 
                          phone (str "\\d{3}+-\\d{3}+-\\d{4}+") 
                          person {:name str :phone phone}) 
                 [{:name "Steve" :phone "408-555-1212"}
                  {:name "Jenny" :phone "415-867-5309"}])))

(deftest nested-grammar
  (is (not (conforms? '(grammar [short+] short (grammar sh sh (int 255))) '[2 3000 2 4])))
  (is (conforms? '(grammar [short+] short (grammar sh sh (int 255))) '[2 3 2 4])))

(deftest merging-grammar
  (let [s1 '(grammar [iii+] iii (int 3))
        s2 '(grammar [sss+] sss (sym "..."))
        s3 '(grammar [kkk+] kkk (kw ":..."))]
    (is (= (schema-merge '[iii sss kkk] s1 s2 s3)
           '(grammar [iii sss kkk]
                    iii (int 3)
                    sss (sym "...")
                    kkk (kw ":..."))))
    (is (= (schema-merge '(grammar [jjj sss kkk] jjj {:a iii}) s1 s2 s3)
           '(grammar [jjj sss kkk]
                    iii (int 3)
                    sss (sym "...")
                    kkk (kw ":...")
                    jjj {:a iii})))))

(deftest char-lits
  (is (conforms? \k \k))
  (is (conforms? [\f \o \o] (seq "foo")))
  (is (conforms? \f (first "foo")))
  (is (conforms? \o (last "foo")))
  (is (conforms? '[char+] (vec (seq "bar")))))

(deftest char-regex
  (is (conforms? '(char "x") \x))
  (is (conforms? '(char "[a-z]") \x))
  (is (not (conforms? '(char "[a-z]") \X)))
  (is (not (conforms? '(char "[a-z]") 42)))
  (is (not (conforms? '(char "[a-z]x") \x))))

(deftest top-kw
  ;; top-level :k? is still optional; use ':k? if the qmark is part of the literal
  (is (conforms? [:k?] [:k]))
  (is (conforms? [:k?] []))
  (is (conforms? [':k] [:k]))
  (is (conforms? '[(? :k)] [:k]))
  (is (conforms? '[(? :k)] []))
  ;; tricky needs both quotes below to work as literal
  (is (conforms? '[':k?] [:k?]))
  (is (not (conforms? '[':k?] [:k])))
  (is (conforms? '[(? ':k?)] [:k?]))
  (is (not (conforms? '[(? ':k?)] [:k])))
  (is (conforms? '[(? :k)] [:k]))
  (is (not (conforms? '[(? :k)] [:k?])))
  (is (not (conforms? [:k?] [42])))
  (is (not (conforms? [:k?] [:a]))))

(deftest singleton-collections
  (is (conforms? '[int] [1]))
  (is (conforms? '(seq int) [1]))
  (is (conforms? '(vec int) [1]))
  (is (conforms? '(list int) '(1)))
  (is (not (conforms? '[int] [])))
  (is (not (conforms? '(seq int) [])))
  (is (not (conforms? '(list int) ())))
  (is (not (conforms? '(vec int) [])))
  (is (conforms? '[int] [1 2]))
  (is (conforms? '(seq int) [1 2]))
  (is (conforms? '(list int) '(1 2)))
  (is (conforms? '(vec int) [1 2]))
  (is (not (conforms? '[int?] [1 2])))
  (is (not (conforms? '(seq int?) [1 2])))
  (is (not (conforms? '(list int?) '(1 2))))
  (is (not (conforms? '(vec int?) [1 2]))))

(deftest pair-collections
  (is (conforms? '[int int] [1 2]))
  (is (conforms? '(seq int int) [1 2]))
  (is (conforms? '(vec int int) [1 2]))
  (is (conforms? '(list int int) '(1 2)))
  (is (not (conforms? '[int int] [])))
  (is (not (conforms? '(seq int int) [])))
  (is (not (conforms? '(list int int) ())))
  (is (not (conforms? '(vec int int) [])))
  (is (not (conforms? '[int int] [1 2 3])))
  (is (not (conforms? '(seq int int) [1 2 3])))
  (is (not (conforms? '(list int int) '(1 2 3))))
  (is (not (conforms? '(vec int int) [1 2 3]))))

(deftest empty-collections
  (is (not (conforms? '[] [1])))
  (is (not (conforms? '() '(1))))
  (is (conforms? '[] []))
  (is (conforms? '[] ()))
  (is (conforms? '(seq) [1]))
  (is (conforms? '(seq) '(1)))
  (is (conforms? '(list) '(1)))
  (is (conforms? '(vec) [1]))
  (is (not (conforms? {} {:a 1})))
  (is (conforms? '(map) {:a 1}))
  (is (conforms? 'map {:a 1}))
  (is (conforms? 'map {}))
  (is (conforms? '(map) {}))
  (is (conforms? 'list '(1)))
  (is (conforms? 'seq '(1)))
  (is (conforms? 'seq [1]))
  (is (conforms? 'vec [1]))
  (is (conforms? 'list ()))
  (is (conforms? 'seq ()))
  (is (conforms? 'seq []))
  (is (conforms? 'vec []))
  (is (not (conforms? '() [1])))
  (is (not (conforms? '() '(1))))
  (is (not (conforms? '[] [1])))
  (is (not (conforms? '() '(1))))
  (is (conforms? '[] []))
  (is (conforms? '(seq) []))
  (is (conforms? '(list) ()))
  (is (conforms? '(vec) [])))


(deftest disallowed-keys
  (are [schema val] (not (conforms? schema val))
       ;; note: not
       '{:a int :b str} {:a 10}
       '{:a int :b str} {:b "foo"}
       '{(* (or :a :b)) (* any)} {:a 10 :b 20 :c 30}
       '{(or :a :b) int} {:a 10 :b 20 :c 30}))

(deftest single-maps
  (is (conforms? '[(* {:a int})] [{:a 1}]))
  (is (conforms? '[(* (map :a int))] [{:a 1}]))
  (is (conforms? '[(* {:a int})] [{:a 1 :b 2}]))
  (is (conforms? '[(* (map :a int))] [{:a 1 :b 2}]))
  (is (not (conforms? '[(* {:a int})] {:a 1})))
  (is (not (conforms? '[(* (map :a int))] {:a 1})))
  (is (not (conforms? '[(* {:a int})] {:a 1 :b 2})))
  (is (not (conforms? '[(* (map :a int))] {:a 1 :b 2}))))

(deftest many-quantified-maps
  (is (conforms? '{kw int} {:a 1 :b 2}))
  (is (not (conforms? '{kw int} {})))
  (is (conforms? '{kw int} {:a 1 :b 2}))
  (is (conforms? '{kw int} {:a 1}))
  (is (conforms? '{kw? int?} {}))
  (is (conforms? '{kw? int?} {:a 1}))
  (is (not (conforms? '{kw? int?} {:a 1 :b 2})))
  (is (not (conforms? '{kw+ int+} {})))
  (is (conforms? '{kw* int*} {:a 1 :b 2}))
  (is (conforms? '{kw* int*} {}))
  (is (conforms? '{(* kw) (* int)} {:a 1 :b 2}))
  (is (conforms? '(map kw int) {:a 1 :b 2}))
  (is (conforms? '(map kw? int?) {}))
  (is (conforms? '(map kw? int?) {:a 1}))
  (is (not (conforms? '(map kw? int?) {:a 1 :b 2})))
  (is (not (conforms? '(map kw+ int+) {})))
  (is (conforms? '(map kw+ int+) {:a 1 :b 2}))
  (is (conforms? '(map kw* int*) {:a 1 :b 2}))
  (is (conforms? '(map (* kw) (* int)) {:a 1 :b 2}))
  (is (not (conforms? '{kw int} {:a 1 :b 2 :c :xxx})))
  (is (not (conforms? '{kw* int*} {:a 1 'xxx 2})))
  (is (not (conforms? '{(* kw) (* int)} {:a 1 :b "foo"})))
  (is (not (conforms? '(map kw int) [:a 1 :b 2])))
  (is (not (conforms? '(map kw* int*) [{:a 1 :b 2}])))
  (is (not (conforms? '(map (* kw) (* int)) {:a :b})))
  (is (not (conforms? '{kw+ int+} {})))
  (is (not (conforms? '(map kw+ int+) {})))
  (is (conforms? '{int* kw*} {1 :a 2 :b}))
  (is (conforms? '{(int* 10) kw*} {1 :a 2 :b}))
  (is (conforms? '{(int 10) kw} {1 :a 2 :b}))
  (is (not (conforms? '{(int* 10) kw*} {11 :a 2 :b})))
  (is (conforms? '(map int* kw*) {1 :a 2 :b}))
  (is (conforms? '(map (int* 10) kw*) {1 :a 2 :b}))
  (is (not (conforms? '(map (int 10) kw) {11 :a 2 :b})))
  (is (not (conforms? '(map (int* 10) kw*) {11 :a 2 :b}))))

(deftest in-operator
  (is (conforms? '(in #{:a :b :c}) :a))
  (is (conforms? '(or :a :b :c) :a))    ;; better
  (is (conforms? '[(:= mmm map) (* (in mmm))] [{:a 1 :b 2 :c 3} :a :b :c :a]))
  (is (conforms? '[(:= mmm map) (in* mmm)] [{:a 1 :b 2 :c 3} :a :b :c :a :c]))
  ;; old way with "when"
  (is (conforms? '[(:= mmm map) (* (:= k kw) (when (contains? mmm k)))]
                 [{:a 1 :b 2 :c 3} :a :b :c]))
  (is (conforms? '[(:= vs vec) (* (in vs))] [[:a :b :c] :a :b :c :a]))
  (is (not (conforms? '[(:= vs vec) (* (in vs))] [[:a :b :c] :a :b :c :a :d])))
  (is (conforms? '[(:= vs list) (* (in vs))] '[(:a :b :c) :a :b :c :a]))
  (is (not (conforms? '[(:= vs list) (* (in vs))] '[(:a :b :c) :a :d :c :a]))))
  
(deftest collection-keys
  (is (conforms? '{:a int '[:foo "bar"] str} '{:a 10 [:foo "bar"] "baz"}))
  (is (conforms? '{:a int (? '[:foo "bar"]) str} '{:a 10 [:foo "bar"] "baz"}))
  (is (conforms? '{:a int (? '[:foo "bar"]) str} '{:a 10}))
  (is (not (conforms? '{:a int (? '[:foo "bar"]) str} '{:a "foo"})))
  (is (not (conforms? '{:a int (? '[:foo "bar"]) str} '{:a 10 [:foo "bar"] 13})))
  (is (conforms? '{'[:foo "bar"] str} '{:a 10 [:foo "bar"] "baz"}))
  (is (conforms? '{(? '[:foo "bar"]) str} '{:a 10 [:foo "bar"] "baz"}))
  (is (conforms? '{(? '[:foo "bar"]) str} '{:a 10}))
  (is (conforms? '{(? '[:foo "bar"]) str} '{:a "foo"}))
  (is (not (conforms? '{(? '[:foo "bar"]) str} '{[:foo "bar"] 13})))
  (is (not (conforms? '{(? '[:foo "bar"]) str} '{:a 10 [:foo "bar"] 13}))))

