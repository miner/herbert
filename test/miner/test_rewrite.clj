(ns miner.test-rewrite
  (:use clojure.test)
  (:require miner.herbert.predicates
            [miner.herbert :as h :exclude [conforms? conform]]
            [miner.herbert.canonical :as c]
            [miner.tagged :as tag]
            clojure.string))


(defn conforms? [schema val]
  (let [rewr (c/rewrite schema)]
   #_ (when-not (= schema rewr)
      (println "Schema: " (pr-str schema))
      (println "Rewrite:" (pr-str rewr))
      (println))    
    (h/conforms? rewr val)))

(defn conform [schema]
  (let [rewr (c/rewrite schema)]
    #_ (when-not (= schema rewr)
      (println "Schema: " (pr-str schema))
      (println "Rewrite:" (pr-str rewr))
      (println))    
    (h/conform rewr)))

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

(deftest and-or
  (is (conforms? '[(or int kw) (and int even)] [:a 4]))
  (is (conforms? '[(or kw sym) (and int odd)] [:a 7])))

(deftest not-constraints
  (is (conforms? '[(not sym)] [:a]))
  (is (conforms? '[(or int kw sym) (not int)] [:a :a]))
  (is (conforms? '[(or int kw sym) (and num (not even))] ['a 6.1])))

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
  (is (conforms? '(& {:a (:= a int) :b {:bb (a)}}) {:a 10 :b {:bb 10}}))
  (is (conforms? '(& {:a (:= a int) :b {:bb a}}) {:a 10 :b {:bb 10}}))
  (is (conforms? '(& {:a (:= a int) :b {:bb [a+]}}) {:a 10 :b {:bb [10 10 10]}}))
  (is (not (conforms? '(& {:a (:= a int) :b {:bb [a+]}}) {:a 10 :b {:bb 10}})))
  (is (not (conforms? '(& {:a (:= a int) :b {:bb a}}) {:a 10 :b {:bb 11}}))))

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
  (is (conforms? '(str "foo/.*") "foo/bar"))
  (is (conforms? '(str "fo+.ar") "fooooobar"))
  (is (conforms? '(sym "user/.*") 'user/foobar))
  (is (not (conforms? '(sym "user/.*") :user/foobar))))

(deftest quantifed-keys-vals
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
  (is (conforms? '(& (:= rec {:a int}) (when (instance? miner.test_rewrite.Foo rec)))
                 (->Foo 42)))
  (is (not (conforms? '(& (:= rec {:a int}) (when (instance? miner.test_rewrite.Foo rec)))
                 {:a 42})))
  (is (not (conforms? '(& (:= rec {:a int}) (when (instance? miner.test_rewrite.Foo rec)))
                 {->Foo "bar"}))))

(deftest on-records-by-class
  (is (conforms? '{:a int} (->Foo 42)))
  (is (conforms? '(and {:a int} (class miner.test_rewrite.Foo))
                 (->Foo 42)))
  (is (not (conforms? '(and {:a int} (class miner.test_rewrite.Foo))
                 {:a 42})))
  (is (not (conforms? '(and {:a int} (class miner.test_rewrite.Foo))
                 {->Foo "bar"}))))

(deftest records-by-tag
  (is (conforms? '(tag miner.test-rewrite/Foo) (->Foo 42)))
  (is (conforms? '(tag "miner[.]test-.*/Foo") (->Foo 42)))
  (is (not (conforms? '(tag "miner/test-.*Foo") (->Foo 42))))
  (is (conforms? '(tag foo.bar/Baz) (tag/read-string "#foo.bar/Baz {:a 42}")))
  (is (not (conforms? '(tag miner.test-rewrite/Bad) (->Foo 42))))
  (is (not (conforms? '(tag foo.wrong/Bar) (tag/read-string "#foo.bar/Baz {:a 42}"))))
  (is (conforms? '(tag miner.test-rewrite/Foo {:a int}) (->Foo 42)))
  (is (conforms? '(tag "miner[.]test-rewrite/F.*" {:a int}) (->Foo 42)))
  (is (not (conforms? '(tag "miner[.]test-rewrite/F.*" {:a sym}) (->Foo 42))))
  (is (conforms? '(tag foo.bar/Baz {:a int}) (tag/read-string "#foo.bar/Baz {:a 42}")))
  (is (not (conforms? '(tag miner.test-rewrite/Foo {:b any}) (->Foo 42))))
  (is (not (conforms? '(tag foo.wrong/Bar {:a int}) (tag/read-string "#foo.bar/Baz {:a 42}")))))

(deftest dates-and-uuid
  (is (conforms? '(tag inst) (java.util.Date.)))
  (is (conforms? '(tag inst) (java.sql.Timestamp. 0)))
  (is (conforms? '(tag inst) (java.util.Calendar/getInstance))))


(deftest readme-examples
  (is (= ((conform '[(:= a int) (:= b int) (:= c int+ a b)]) [3 7 4 5 6])
         '{c [4 5 6], b 7, a 3}))
  (is (= ((conform '[(:= max int) (:= xs int+ max)]) [7 3 5 6 4])
         '{xs [3 5 6 4], max 7}))
  (is (conforms? '{:a int :b [sym+] :c str} '{:a 42 :b [foo bar baz] :c "foo"}))
  (is (conforms? '{:a int :b sym :c? [str*]} '{:a 1 :b foo}))
  (is (conforms? '{:a int :b sym :c? [str*]} '{:a 1 :b foo :c ["foo" "bar" "baz"]}))
  (is (not (conforms? '{:a int :b sym :c? [str*]} '{:a foo :b bar})))
  (is (conforms? '{:a (:= a int) :b sym :c? [a+]} '{:a 1 :b foo :c [1 1 1]})))

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
    (is (= (h/schema-merge '[iii sss kkk] s1 s2 s3)
           '(grammar [iii sss kkk]
                    iii (int 3)
                    sss (sym "...")
                    kkk (kw ":..."))))
    (is (= (h/schema-merge '(grammar [jjj sss kkk] jjj {:a iii}) s1 s2 s3)
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

(deftest singleton-containers
  (is (conforms? '[int] [1]))
  (is (conforms? '(seq int) [1]))
  (is (conforms? '(vec int) [1]))
  (is (conforms? '(list int) '(1)))
  (is (not (conforms? '[int] [])))
  (is (not (conforms? '(seq int) [])))
  (is (not (conforms? '(list int) ())))
  (is (not (conforms? '(vec int) [])))
  (is (not (conforms? '[int] [1 2])))
  (is (not (conforms? '(seq int) [1 2])))
  (is (not (conforms? '(list int) '(1 2))))
  (is (not (conforms? '(vec int) [1 2]))))

(deftest pair-containers
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

(deftest empty-containers
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
