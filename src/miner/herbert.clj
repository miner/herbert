(ns miner.herbert
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [squarepeg.core :as sp]
            [miner.herbert.constraints :as con]
            [miner.herbert.proto :as proto]))

(def ^:dynamic *constraints* 
  "Map of user-defined constraint names to vars implementing the appropriate predicate." {})

(def constraints-ns (the-ns 'miner.herbert.constraints))
(def default-constraints (ns-publics constraints-ns))
(def reserved-ops '#{+ * ? & = == < > not= >= <= quote and or not guard vec seq list map mod})

;; SEM FIXME: maybe try Clojail or something to have a restricted eval
(defn safe-eval [expr]
  "Not actually safe at all."
  ;(println "Not really safe yet: " expr)
  (eval expr))

(defn literal? [con]
  (or (keyword? con) (number? con) (string? con) (false? con) (true? con) (nil? con)))

(defn str-last-char [^String s]
  (when-not (str/blank? s)
    (.charAt s (dec (.length s)))))

;; works with strings and symbols
(defn last-char [s]
  (str-last-char (name s)))

(defn strip-last [x]
  ;; x is symbol or keyword, typically used to strip \?
  (let [xname (name x)
        name1 (subs xname 0 (dec (count xname)))
        ns (namespace x)]
  (if (keyword? x) 
    (keyword ns name1)
    (symbol ns name1))))


(defrecord TaggedValue [tag value])

(defn taggedValue? [x]
  (instance? TaggedValue x))



;; loosey-goosey get or just yourself, sort of an ersatz lexical binding
(defn lookup [sym bindings]
  (if (symbol? sym)
    (get bindings sym sym)
    sym))


(defn mkprb
  "Like mkpr but allows extra args to be added before item.  Args are either literal values or keys
that are looked up in bindings.  If the key is not found, the value is the key itself."
  [pr args]
  (if-not args
    (sp/mkpr pr)
    (fn [input bindings context memo]
      (if (nil? (seq input))
        (sp/fail "End of input" memo)
        (let [i (first input)
              pred (apply partial pr (map #(lookup % bindings) args))]
          (if (pred i)
            (sp/succeed i [i] (rest input) bindings memo)
            (sp/fail (str i " does not match predicate.") memo)))))))


;; SEM FIXME -- maybe a little shakey on merging bindings and memo stuff
;; returns only the bindings, etc. of the first rule

(defn mkand
  "Create a rule that matches all of rules at the same time for a single input. 
Returns result of first rule."
  ([]
     #(sp/succeed nil [] %1 %2 %4))
  ([rule] rule)
  ([rule1 rule2]
     (fn [input bindings context memo]
       (let [r1 (rule1 input bindings context memo)]
         (if (sp/failure? r1)
           r1
           (let [r2 (rule2 input (:b r1) context (:m r1))]
             (if (sp/failure? r2)
               r2
               ;; maybe should use not identical?
               (if (not= (:r r1) (:r r2))
                 (sp/fail "Subrules matched differently" (merge (:m r1) (:m r2)))
                 r2)))))))
  ([rule1 rule2 & rules]
     (reduce mkand (mkand rule1 rule2) rules)))


;; could also test inval to be a vector, set or number to be safer
(defn mkin [rule inval]
  (fn [input bindings context memo]
    (let [res (rule input bindings context memo)]
      (if (sp/failure? res)
        res
        (let [inrule (sp/mkpr #(proto/in? inval %))
              inres (inrule (list (:r res)) (:b res) context memo)]
          (if (sp/failure? inres)
            inres
            res))))))


(defn iter= [iterfn coll]
  (= (seq coll) (when-first [fst coll] 
                  (take (count coll) (iterate iterfn fst)))))

;; maybe prettier but not faster in my tests
#_ (defn iter2= [iterfn coll]
  (every? identity (map = coll (when-first [fst coll] (iterate iterfn fst)))))


(defn indexed= [indexfn coll]
  ;; SEM debug
  #_  (println "indexed=" indexfn coll)
  (= coll (map indexfn (range (count coll)))))

(defn mkiter [rule iterfn]
  (fn [input bindings context memo]
    (let [res (rule input bindings context memo)]
      (if (sp/failure? res)
        res
        (if (iter= iterfn (:s res)) 
          res
          (sp/fail "Iterator failed to match input" memo))))))

(defn mkindexed [rule indexfn]
  (fn [input bindings context memo]
    (let [res (rule input bindings context memo)]
      (if (sp/failure? res)
        res
        (if (indexed= indexfn (:s res)) 
          res
          (sp/fail "Indexed fn failed to match input" memo))))))

(defn mkstep [rule step]
  {:pre [(number? step)]}
  (mkiter rule (partial + step)))

(defn tcon-pred [tcon]
  (or (get *constraints* tcon)
      (get default-constraints tcon)))

(defn tcon-symbol-quantifier [sym]
  (let [ch (last-char sym)]
    (case ch
      \+ :one-or-more
      \* :zero-or-more
      \? :optional
      nil)))

;; FIXME could use strip-last, or combine and make work with keywords
(defn simple-sym [sym]
  (let [sname (name sym)
        lch (str-last-char sname)]
    (case lch
      (\+ \* \?) (symbol (subs sname 0 (dec (.length sname))))
      sym)))

(defn simple-sym? [sym]
  (= sym (simple-sym sym)))

(defn quantified-sym? [sym]
  (not= sym (simple-sym sym)))

(declare tconstraint)

(defn tcon-symbol-constraint [sym]
  (let [lch (last-char sym)
        sym (simple-sym sym)
        brule (sp/mkpr (tcon-pred sym))]
    (case lch
      \+ (sp/mk1om brule)
      \* (sp/mkzom brule)
      \? (sp/mkopt brule)
      brule)))

;; SEM FIXME: be careful about where the iterfn is resolved
;; maybe should bind or use *ns* directly

;; args can be literals or keys into the bindings.  Unknown keys are just literal values.
(defn mkbase [pred args]
  (if args
    (mkprb pred args)
    (sp/mkpr pred)))

(defn mkopts [rule kwopts]
  (let [{asname :as inval :in step :step itername :iter indexed :indexed} kwopts]
    (assert (<= (count (keep identity [inval step itername])) 1))
    (-> rule
        (cond-> inval (mkin inval)
                step  (mkstep step)
                itername (mkiter (resolve itername))
                indexed (mkindexed (resolve indexed))
                asname  (sp/mkbind asname)))))


;; SEM FIXME -- broken for quantified op, should put the args inside (with con)
;;   and kwarg outside the quant

;; SEM FIXME BUG -- spliting at kw conflicts with binding args
;; could use custom args or restrict params versus kw opts

(defn tcon-list-simple-type [name lexpr]
  (let [[tcon & modifiers] lexpr
        lch (last-char tcon)
        tcon (simple-sym tcon)
        pred (tcon-pred tcon)
        [args kwargs] (split-with (complement keyword?) modifiers)
        brule (mkbase pred args)]
    (mkopts (case lch
              \+  (sp/mk1om brule)
              \* (sp/mkzom brule) 
              \? (sp/mkopt brule)
              brule)  
            (apply hash-map :as name kwargs))))

(defn tcon-list-complex-type [name lexpr]
  (let [[tcon & modifiers] lexpr
        base-rule (tconstraint tcon)
        [args kwargs] (split-with (complement keyword?) modifiers)]
    (assert (empty? args))
    (mkopts base-rule (apply hash-map :as name kwargs))))

;; SEM FIXME -- why doesn't an sp/mkscope work here as a wrapper?
(defn tcon-list-solo [bname]
  ;; simple name should match item equal to that binding
  (let [solo (gensym "name")]
     (sp/mkseq 
      (sp/mkbind sp/anything solo)
      (sp/mkpred (fn [bindings context] (= (get bindings bname) (get bindings solo)))))))

(defn bind-name [sym]
  (and (symbol? sym) 
       (not (contains? reserved-ops sym))
       (not (tcon-pred (simple-sym sym)))
       sym))

(defn tcon-list-type [lexpr]
  (when-first [fst lexpr]
    (let [bname (bind-name fst)
          expr (if bname (rest lexpr) lexpr)]
      (cond (and bname (nil? (seq expr))) (tcon-list-solo bname)
            (symbol? (first expr)) (tcon-list-simple-type bname expr)
            (list? (first expr)) (tcon-list-complex-type bname expr)
            :else (throw (ex-info "Unknown tcon-list" {:name bname :con lexpr}))))))

(defn tcon-quoted-sym [sym]
  ;; no special interpretation of symbol
  (sp/mkpr (tcon-pred sym)))

;; SEM FIXME -- drop support for (N t) -- just spell it out N times [t t t] or make a repeat op
;; SEM FIXME -- should use total cycle, not element count

;; n is the total number of desired items,
;; cs might have to be repeated to fill
(defn tcon-nseq 
  ([n cs]
     (case (long n)
       0 (sp/mkseq)
       1 (tconstraint (first cs))
       (apply sp/mkseq (take n (cycle (map tconstraint cs))))))
  ([lo hi cs]
     (let [tcons (cycle (map tconstraint cs))]
       (apply sp/mkseq (concat (take lo tcons)
                               (take (- hi lo) (map sp/mkopt (drop lo tcons))))))))

(defn tcon-seq [cs]
  (apply sp/mkseq (map tconstraint cs)))

(defn tcon-seq-constraint [vexpr]
  (sp/mksub (apply sp/mkseq (conj (mapv tconstraint vexpr) sp/end))))


;; SEM FIXME : dangerous eval
(defn tcon-guard [args body]
  {:pre [(vector? args) (list? body)]}
  ;; guard syntax is like an anonymous fn, first arg is a literal vector of binding names
  ;; which should have been previously declared.  Rest is a body.
  (let [pred (safe-eval `(fn [{:syms [~@args]}] ~@body))]
    (sp/mkpred (fn [bindings context] (pred (merge context bindings))))))

(defn tcon-list-constraint [lexpr]
  (let [op (first lexpr)]
    (case op
      or (apply sp/mkalt (map tconstraint (rest lexpr)))
      and (apply mkand (map tconstraint (rest lexpr)))
      not (sp/mkseq (sp/mknot (tconstraint (second lexpr))) sp/anything)
      quote (tcon-quoted-sym (second lexpr))
      guard (tcon-guard (second lexpr) (nnext lexpr))
      (deref clojure.core/deref) (tcon-list-solo (second lexpr))
      * (sp/mkzom (tcon-seq (rest lexpr)))
      + (sp/mk1om (tcon-seq (rest lexpr))) 
      ? (sp/mkopt (tcon-seq (rest lexpr)))  
      & (tcon-seq (rest lexpr))
      seq  (tcon-seq-constraint (rest lexpr))
      vec (mkand (sp/mkpr vector?) (tcon-seq-constraint (rest lexpr)))
      list (mkand (sp/mkpr list?) (tcon-seq-constraint (rest lexpr)))

      ;; else
      (cond (integer? op) (tcon-nseq op (rest lexpr))
            (vector? op) (tcon-nseq (first op) (second op) (rest lexpr))
            :else (tcon-list-type lexpr)))))


;; need to reduce the subrules and preserve the bindings
;; SEM FIXME -- not sure about merging memo.  This one just passes on original memo.
(defn mkmap [rules]
  (fn [input bindings context memo]
    (let [m (first input)]
      (if (and (seq input) (map? m))
        (let [mbindings (reduce (fn [mb rule]
                                    (if (false? mb)
                                      false
                                      (let [res (rule (list m) mb context memo)]
                                        (if (sp/success? res)
                                          (:b res)
                                          (reduced false)))))
                                  bindings
                                  rules)]
          (if (false? mbindings)
            (sp/fail "Input failed to match required map." memo)
            (sp/succeed m [m] (rest input) mbindings memo)))
        (sp/fail "Input failed to match required map." memo)))))


(defn has-keys? [m ks]
  (and (map? m)
       (every? (partial contains? m) ks)))

(defn optional-key? [kw]
  (and (keyword? kw)
       (= (last-char kw) \?)))

(defn simple-key [kw]
  (if (optional-key? kw)
    (strip-last kw)
    kw))

(defn optional-keys [m]
  (filter optional-key? (keys m)))

(defn required-keys [m]
  (remove optional-key? (keys m)))

(defn test-constraint? [con val]
  (sp/success? ((tconstraint con) (list val) {} {} {})))

;; SEM FIXME -- everywhere we use sp/success? we have to look for passing up the bindings
;; which means sp/mkpr is not going to be sufficient in many cases.

(defn mkkw
  "Takes kw and rule for associated val.  Fails if kw is missing or val doesn't match.  Does not
  consume anything."  
  [kw rule]
  (fn [input bindings context memo]
    (if (nil? (seq input))
      (sp/fail "End of input" memo)
      (let [m (first input)]
        (if (contains? m kw)
          (let [r (rule (list (get m kw)) bindings context memo)]
            (if (sp/failure? r)
              r
              (sp/succeed nil [] input (:b r) (:m r))))
          (sp/fail (str kw " is not in map.") memo))))))

;; SEM FIXME -- what if m is not a map?  For now, says OK for optional.
(defn mkkwopt
  "Takes kw and rule for associated val.  If kw is found, val must match rule.  Does not consume anything."
  [kw rule]
    (fn [input bindings context memo]
      (if (nil? (seq input))
        (sp/fail "End of input" memo)
        (let [m (first input)]
          (if (and (map? m) (contains? m kw))
            (let [r (rule (list (get m kw)) bindings context memo)]
              (if (sp/failure? r)
                r
                (sp/succeed nil [] input (:b r) (:m r))))
            (sp/succeed nil [] input bindings memo))))))


(defn tcon-map-entry [[kw con]]
  ;; FIXME -- only handles kw literals and optional :kw? for keys
  ;; doesn't carry context or results for individual key/val matches
  ;; Note: each rule expect full map as input, but only looks at one key
  (let [rule (tconstraint con)]
    (if (optional-key? kw)
      (mkkwopt (simple-key kw) rule)
      (mkkw kw rule))))

(defn tcon-map-constraint [mexpr]
  (mkmap (map tcon-map-entry mexpr)))

(defn tcon-set-sym [sym]
  (let [simple (simple-sym sym)
        rule (tconstraint simple)]
    (case (last-char sym)
      \* (sp/mkpr (fn [s] (every? #(sp/success? (rule (list %) {} {} {})) s)))
      \+ (sp/mkpr (fn [s] (and (seq s) (every? #(sp/success? (rule (list %) {} {} {})) s))))
      \? (sp/mkpr (fn [s] (or (empty? s) 
                              (and (empty? (rest s))
                                   (sp/success? (rule (seq s) {} {} {}))))))
      ;; else simple
      (sp/mkpr (fn [s] (some #(sp/success? (rule (list %) {} {} {})) s))))))

(defn tcon-set-list [lst]
  (let [[op con unexpected] lst
        quantified (case op (* + ?) true false)
        rule  (if quantified (tconstraint con) (tconstraint lst))]
    (when (and quantified unexpected)
      (throw (ex-info "Unexpectedly more" {:con lst})))
    (case op
      * (sp/mkpr (fn [s] (every? #(sp/success? (rule (list %) {} {} {})) s)))
      + (sp/mkpr (fn [s] (some #(sp/success? (rule (list %) {} {} {})) s)))
      ? (sp/mkpr (fn [s] (or (empty? s) 
                              (and (== (count s) 1)
                                   (sp/success? (rule (seq s) {} {} {}))))))
      ;; else quantified
      (sp/mkpr (fn [s] (some #(sp/success? (rule (list %) {} {} {})) s))))))


(defn tcon-set-element [con]
  (cond (symbol? con) (tcon-set-sym con)
        (list? con) (tcon-set-list con)
        (literal? con) (throw (ex-info "Literals should be handled separately" {:con con}))
        :else (throw (ex-info "I didn't think of that" {:con con}))))


(defn tcon-set-constraint [sexpr]
  (let [nonlits (remove literal? sexpr)
        litset (if (seq nonlits) (set (filter literal? sexpr)) sexpr)]
    (apply mkand (sp/mkpr #(set/subset? litset %)) (map tcon-set-element nonlits))))
           

(defn tconstraint 
  ([expr]
     (cond (symbol? expr) (tcon-symbol-constraint expr)
           (list? expr) (tcon-list-constraint expr)
           (vector? expr) (tcon-seq-constraint expr)
           (set? expr) (tcon-set-constraint expr)
           (map? expr) (tcon-map-constraint expr) 
           (string? expr) (sp/mklit expr)
           (keyword? expr) (sp/mklit expr)
           (nil? expr) (sp/mkpr nil?)
           (false? expr) (sp/mkpr false?)
           (true? expr) (sp/mkpr true?)
           (number? expr) (sp/mklit expr)
             :else (throw (ex-info "Unknown constraint form" {:con expr}))))
     
  ([expr expr2]
     (sp/mkseq (tconstraint expr) (tconstraint expr2)))

  ([expr expr2 & more]
     (apply sp/mkseq (tconstraint expr) (tconstraint expr2) (map tconstraint more))))


(defn confn [con]
  (let [cfn (tconstraint con)]
    (fn ff
      ([item] (ff item {} {} {}))
      ([item context] (ff item context {} {}))
      ([item context bindings memo] (cfn (list item) context bindings memo)))))



;; Too Clever?  Single arg creates predicate (for reuse).  Second arg immediately tests.
;; Con can be a fn already (presumed to be a predicate), or a "constraint expression" which
;; is compiled into a predicate.

(defn conformitor [con]
  (if (fn? con) con 
      #(let [res ((confn con) %)] 
         (when (sp/success? res)
           (:b res)))))

(defn conform
  ([con] (conformitor con))
  ([con x] ((conformitor con) x)))

(defn conforms? [con x] 
  (boolean (conform con x)))
