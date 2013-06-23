(ns miner.herbert
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [squarepeg.core :as sp]
            [miner.herbert.constraints]
            [miner.herbert.proto :as proto]))

(def ^:dynamic *constraints* 
  "Map of user-defined constraint names to vars implementing the appropriate predicate." {})

(def constraints-ns (the-ns 'miner.herbert.constraints))
(def reserved-ops '#{+ * ? & = == < > not= >= <= quote and or not assert vec seq list map mod})

(defn reserved-sym? [sym]
  (contains? reserved-ops sym))

(defn str-last-char [^String s]
  (when-not (str/blank? s)
    (.charAt s (dec (.length s)))))

;; works with strings and symbols
(defn last-char [s]
  (str-last-char (name s)))

(defn symbol-quantifier [sym]
  (let [ch (last-char sym)]
    (case ch
      \+ '+
      \* '*
      \? '?
      nil)))

;; FIXME could use strip-last, or combine and make work with keywords
(defn simple-sym [sym]
  (let [sname (name sym)
        lch (str-last-char sname)]
    (case lch
      (\+ \* \?) (symbol (subs sname 0 (dec (.length sname))))
      sym)))

(defn strip-last [x]
  ;; x is symbol or keyword, typically used to strip \?
  (let [xname (name x)
        name1 (subs xname 0 (dec (count xname)))
        ns (namespace x)]
  (if (keyword? x) 
    (keyword ns name1)
    (symbol ns name1))))


(def default-constraints 
  (into {} (map (fn [[k v]] [(simple-sym k) v]) (ns-publics constraints-ns))))

(defn defined-sym? [sym]
  (or (contains? default-constraints sym)
      (contains? *constraints* sym)))

(defn simple-sym? [sym]
  (= sym (simple-sym sym)))

(defn quantified-sym? [sym]
  (not= sym (simple-sym sym)))

;; SEM FIXME: maybe try Clojail or something to have a restricted eval
(defn safe-eval [expr]
  "Not actually safe at all."
  ;(println "Not really safe yet: " expr)
  (eval expr))

(defn literal? [con]
  (or (keyword? con) (number? con) (string? con) (false? con) (true? con) (nil? con)))



(defrecord TaggedValue [tag value])

(defn taggedValue? [x]
  (instance? TaggedValue x))


;; loosey-goosey get or just yourself, sort of an ersatz lexical binding
(defn lookup [sym bindings]
  (if (symbol? sym)
    (get bindings sym sym)
    sym))

;; copied from squarepeg and slightly modified to get my fix for the incoming bindings
(defn mkscope
  "Create a rule which contains the scope of the given rule. Bindings
made in rule do not escape this rule's scope."
  [rule]
  (fn [input bindings context memo]
    (let [r (rule input bindings context memo)]
      (if (sp/success? r)
        (sp/succeed (:r r) (:s r) (:i r) bindings (:m r))
        r))))

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
Returns the successful result of the last rule or the first to fail."
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

(declare mkconstraint)

(defn mk-solo [bname]
  ;; simple name should match item equal to that binding
  (let [solo (gensym bname)]
    (mkscope
     (sp/mkseq 
      (sp/mkbind sp/anything solo)
      (sp/mkpred (fn [bindings context] (= (get bindings bname) (get bindings solo))))))))

(defn mk-symbol-constraint [sym]
  (let [lch (last-char sym)
        sym (simple-sym sym)
        pred (tcon-pred sym)
        brule (if pred (sp/mkpr pred) (mk-solo sym))]
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

(defn mk-list-simple-type [name lexpr]
  (let [[tcon & modifiers] lexpr
        lch (last-char tcon)
        tcon (simple-sym tcon)
        pred (tcon-pred tcon)
        [args kwargs] (split-with (complement keyword?) modifiers)
        brule (mkbase pred args)]
    (mkopts (case lch
              \+ (sp/mk1om brule)
              \* (sp/mkzom brule) 
              \? (sp/mkopt brule)
              brule)  
            (apply hash-map :as name kwargs))))

(defn mk-list-complex-type [name lexpr]
  (let [[tcon & modifiers] lexpr
        base-rule (mkconstraint tcon)
        [args kwargs] (split-with (complement keyword?) modifiers)]
    (assert (empty? args))
    (mkopts base-rule (apply hash-map :as name kwargs))))

(defn bind-name [sym]
  (and (symbol? sym)
       (not (contains? reserved-ops sym))
       (not (tcon-pred (simple-sym sym)))
       sym))

(defn mk-list-type [lexpr]
  (when-first [fst lexpr]
    (let [bname (bind-name fst)
          expr (if bname (rest lexpr) lexpr)]
      (cond (and bname (nil? (seq expr))) (mk-solo bname)
            (symbol? (first expr)) (mk-list-simple-type bname expr)
            :else (mk-list-complex-type bname expr)))))

;; :else (throw (ex-info "Unknown mk-list-type" {:name bname :con lexpr}))))))


;; probably don't want this
(defn mk-quoted-sym [sym]
  (if-let [pred (tcon-pred sym)]
    (sp/mkpr pred)
    (throw (ex-info (str "No constraint function defined for " sym) {:sym sym}))))

;; SEM FIXME -- drop support for (N t) -- just spell it out N times [t t t] or make a repeat op
;; SEM FIXME -- should use total cycle, not element count

;; n is the total number of desired items,
;; cs might have to be repeated to fill
(defn mk-nseq 
  ([n cs]
     (case (long n)
       0 (sp/mkseq)
       1 (mkconstraint (first cs))
       (apply sp/mkseq (take n (cycle (map mkconstraint cs))))))
  ([lo hi cs]
     (let [tcons (cycle (map mkconstraint cs))]
       (apply sp/mkseq (concat (take lo tcons)
                               (take (- hi lo) (map sp/mkopt (drop lo tcons))))))))

(defn mk-con-seq [cs]
  (apply sp/mkseq (map mkconstraint cs)))

(defn mk-subseq-constraint [vexpr]
  (sp/mksub (apply sp/mkseq (conj (mapv mkconstraint vexpr) sp/end))))

(defn args-from-body 
  ([expr] (args-from-body () expr))
  ([res expr]
     (cond (symbol? expr) (conj res expr)
           (vector? expr) (concat (mapcat args-from-body expr) res)
           (seq? expr) 
             (case (first expr) 
               ;; ignore quoted values
               quote nil
               ;; disallow some fns
               (apply eval) (throw (ex-info "Herbert asserts do not allow 'apply' or 'eval'"
                                            {:form expr}))
               ;; for a normal list, skip the "fn", first element
               (concat (mapcat args-from-body (rest expr)) res)))))

;; SEM FIXME : dangerous eval
(defn mk-assert [body]
  {:pre [(seq? body)]}
  ;; assert syntax takes just a single expr.  Symbols are looked up from previously bound names,
  ;; except for the first "fn" position of a list.  Some fns are not allowed, such as "apply" and
  ;; "eval".  
  (let [args (args-from-body body)
        pred (safe-eval `(fn [{:syms [~@args]}] ~body))]
    (sp/mkpred (fn [bindings context] (pred (merge context bindings))))))

;; SEM BUG what about map support is limited to kw keys right now
(declare mk-map-constraint)

(defn mk-list-constraint [lexpr]
  (let [op (first lexpr)]
    (case op
      or (apply sp/mkalt (map mkconstraint (rest lexpr)))
      and (apply mkand (map mkconstraint (rest lexpr)))
      not (sp/mkseq (sp/mknot (mkconstraint (second lexpr))) sp/anything)
      quote (if (symbol? (second lexpr))
              ;; symbols can be used as literals if quoted
              (sp/mklit (second lexpr))
              ;; dequoting here is convenient for macros
              (mkconstraint (second lexpr)))
      (= == not= < > <= >=) (mk-assert lexpr)
      assert (mk-assert (second lexpr))
      * (sp/mkzom (mk-con-seq (rest lexpr)))
      + (sp/mk1om (mk-con-seq (rest lexpr))) 
      ? (sp/mkopt (mk-con-seq (rest lexpr)))  
      & (mk-con-seq (rest lexpr))
      seq  (mk-subseq-constraint (rest lexpr))
      vec (mkand (sp/mkpr vector?) (mk-subseq-constraint (rest lexpr)))
      list (mkand (sp/mkpr list?) (mk-subseq-constraint (rest lexpr)))
      map (mk-map-constraint (apply hash-map (rest lexpr)))
      ;; else
      (cond (integer? op) (mk-nseq op (rest lexpr))
            (vector? op) (mk-nseq (first op) (second op) (rest lexpr))
            :else (mk-list-type lexpr)))))


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
  (sp/success? ((mkconstraint con) (list val) {} {} {})))

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
          (if (and (map? m) (get m kw))
            ;; used get above instead of contains? because "optional" interpretation
            ;; treats nil value like having no kw, turns out to be convenient
            (let [r (rule (list (get m kw)) bindings context memo)]
              (if (sp/failure? r)
                r
                (sp/succeed nil [] input (:b r) (:m r))))
            (sp/succeed nil [] input bindings memo))))))


;; SEM UNIMPLEMENTED
(defn mk-entry [kcon vcon] nil)
  


(defn mk-map-entry [[kw con]]
  ;; FIXME -- only handles kw literals and optional :kw? for keys
  ;; doesn't carry context or results for individual key/val matches
  ;; Note: each rule expect full map as input, but only looks at one key
  (let [rule (mkconstraint con)]
    (if (optional-key? kw)
      (mkkwopt (simple-key kw) rule)
      (mkkw kw rule))))

(defn mk-map-constraint [mexpr]
  (mkmap (map mk-map-entry mexpr)))

(defn mk-set-sym [sym]
  (let [simple (simple-sym sym)
        rule (mkconstraint simple)]
    (case (last-char sym)
      \* (sp/mkpr (fn [s] (every? #(sp/success? (rule (list %) {} {} {})) s)))
      \+ (sp/mkpr (fn [s] (and (seq s) (every? #(sp/success? (rule (list %) {} {} {})) s))))
      \? (sp/mkpr (fn [s] (or (empty? s) 
                              (and (empty? (rest s))
                                   (sp/success? (rule (seq s) {} {} {}))))))
      ;; else simple
      (sp/mkpr (fn [s] (some #(sp/success? (rule (list %) {} {} {})) s))))))

(defn mk-set-list [lst]
  (let [[op con unexpected] lst
        quantified (case op (* + ?) true false)
        rule  (if quantified (mkconstraint con) (mkconstraint lst))]
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


(defn mk-set-element [con]
  (cond (symbol? con) (mk-set-sym con)
        (list? con) (mk-set-list con)
        (literal? con) (throw (ex-info "Literals should be handled separately" {:con con}))
        :else (throw (ex-info "I didn't think of that" {:con con}))))


(defn mk-set-constraint [sexpr]
  (let [nonlits (remove literal? sexpr)
        litset (if (seq nonlits) (set (filter literal? sexpr)) sexpr)]
    (apply mkand (sp/mkpr #(set/subset? litset %)) (map mk-set-element nonlits))))
           

(defn mkconstraint 
  ([expr]
     #_ (println "mkconstraint " expr)
     (cond (symbol? expr) (mk-symbol-constraint expr)
           ;; don't use list?, seq? covers Cons as well
           (seq? expr) (mk-list-constraint expr)
           (vector? expr) (mk-subseq-constraint expr)
           (set? expr) (mk-set-constraint expr)
           (map? expr) (mk-map-constraint expr) 
           (string? expr) (sp/mklit expr)
           (keyword? expr) (sp/mklit expr)
           (nil? expr) (sp/mkpr nil?)
           (false? expr) (sp/mkpr false?)
           (true? expr) (sp/mkpr true?)
           (number? expr) (sp/mklit expr)
           :else (throw (ex-info "Unknown constraint form" {:con expr}))))
     
  ([expr expr2]
     (sp/mkseq (mkconstraint expr) (mkconstraint expr2)))

  ([expr expr2 & more]
     (apply sp/mkseq (mkconstraint expr) (mkconstraint expr2) (map mkconstraint more))))


(defn constraint-fn [con]
  (let [cfn (mkconstraint con)]
    (fn ff
      ([item] (ff item {} {} {}))
      ([item context] (ff item context {} {}))
      ([item context bindings memo] (cfn (list item) context bindings memo)))))



;; Too Clever?  Single arg creates predicate (for reuse).  Second arg immediately tests.
;; Con can be a fn already (presumed to be a predicate), or a "constraint expression" which
;; is compiled into a predicate.

(defn conformitor [con]
  (if (fn? con) con 
      #(let [res ((constraint-fn con) %)] 
         (when (sp/success? res)
           (with-meta (:b res) {::constraint con})))))

(defn conform
  ([con] (conformitor con))
  ([con x] ((conformitor con) x)))

(defn conforms? [con x] 
  (boolean (conform con x)))

;; UGLY and unfinished
(defmacro conf? [con x]
  (println "conf?" con (type con))
  (let [dcon (if (and (seq? con) (= (first con) 'quote)) (second con) con)
        fcf (conform dcon)]
    (println "  dc" dcon (type dcon))
    (println "  cf " fcf (type fcf))
    `(let [res# (fcf ~x)]
       (when res#  (with-meta res# {::constraint ~con})))))
