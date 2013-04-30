(ns miner.herbert
  (:require [clojure.string :as str]
            [squarepeg.core :as sp]))


(defn unimplemented [x]
  (println "Unimplemented " x))


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

;; wrong -- don't add extra vars
(def int? integer?)
(def str? string?)

;; wrong -- not a collection, just multiple values inline
(defn sym->pred [sym]
  (let [sname (name sym)
        ch (last-char sname)]
    (case ch
      \+ (every-pred coll? (comp pos? count))
      \* coll?
      \? (every-pred coll? #(<= (count %) 1))
      (resolve (symbol (str sym "?"))))))



;; probably need a protocol to refactor code

(def simple? (complement coll?))

(defn wildcard? [x]
  (case x
    (* ? +) true
    false))

(defn basetype? [x]
  (case x
    (int float str char tag num sym kw nil true false bool) true
    false))

(defn colltype? [x]
  (case x
    (list vec seq map coll keys set) true
    false))


;; maybe need a protocol here
(defn process-map-spec [mspec]
  ;; FIXME
  mspec)

(defn process-vector-spec [vspec]
  ;; FIXME
  vspec)

(defn process-compound-spec [cspec]
  ;; FIXME
  cspec)


(defn process-spec [spec]
  ;; walk spec to canonicalize it
  (cond (simple? spec) spec
        (map? spec) (process-map-spec spec)
        (vector? spec) (process-vector-spec spec)
        (seq? spec) (process-compound-spec spec)
        :else (throw (java.lang.IllegalStateException. (str "Bad spec " (pr-str spec))))))
  
(defn process-compound-spec [spec]
  (let [head (first spec)]
    (cond (integer? head) (vec (repeat head (map process-spec (rest spec))))
          (wildcard? head) (list* head (map process-spec (rest spec)))
          ;;(comparison? head)
          )))

(defn schema [spec-map]
  "Returns a Sevrin schema for the spec-map"
  (reduce-kv (fn [m k v] (assoc m k (process-spec v))) {} spec-map))


(defn one-of? [type-set data]
  (let [c (class data)]
    (or (contains? type-set c)
        (some (or (supers c) #{}) type-set))))

(defn start-rule [schema]
  (get schema 'start))

  
;; needs work
(defn literal? [x]
  (or (not (coll? x))
      (let [[a b] x]
        (and (= a 'quote) (recur b)))))

(defrecord TaggedValue [tag value])

(defn taggedValue? [x]
  (instance? TaggedValue x))

;; define a type
(def single-test-fn
  {'int integer?
   'num number?
   'float float?
   'list seq?
   'literal literal?
   'char char?
   'str string?
   nil nil?
   true true?
   false false?
   'bool (some-fn true? false?)
   'sym symbol?
   'kw keyword?
   'empty empty?
   'any (constantly true)
   } )

(def coll-test-fn
  {'vec vector?
   'seq sequential?
   'coll coll?
   'keys map?
   'map map?
   'set set?
   } )

(def other-test-fn
  {'tag taggedValue?
   })

(def test-fn (merge single-test-fn coll-test-fn other-test-fn))


;; record per type with guard
;; or is it just a fn?

(defn map->test [m]
  (unimplemented "map->test"))

(defn coll->test [c]
  ;; handle guard etc
  (unimplemented "coll->test"))

(defn symbol->test [sym]
  (or (get test-fn sym)
      (sym->pred sym)
      (resolve sym)))

(defn spec->test [spec]
  (cond (symbol? spec) (symbol->test spec)
        (map? spec) (map->test spec)
        (coll? spec) (coll->test spec)
        :else (throw (ex-info "Unknown spec" {:spec spec}))))


(defn guard-spec-fn [spec]
  (unimplemented 'guard-spec-fn)
  (constantly true))

(defn single-item-spec? [spec]
  (boolean (single-test-fn spec)))

(defn single-spec-fn [spec]
  (every-pred (single-test-fn spec) (guard-spec-fn spec)))


(def single-spec-types (keys single-test-fn))

(defn accepts? [spec datum]
  ((spec->test spec) datum))

(defn accept [spec data]
  ;; data must be a sequence, typically a vector
  ;; returns map {:accepted stuff :remaining stuff}
  (if (single-item-spec? spec)
    (when ((single-spec-fn spec) (first data))
      {:accepted (first data) :remaining (rest data)})
    (when coll-test-fn
      ;; unfinished
      )))

;;; processing  :accepted :remaining :binding

(defn conforms? [data schema]
  (accepts? (start-rule schema) data))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SP rule takes 4 args:   input, bindings, context and memo.
;; Or it can be called like a fn with a sequence as input, and it parses as much as possible
;; 
;; (defn succeed
;;   [return sreturn input bindings memo]
;;   {:i input :b bindings :r return :s sreturn :m memo})

;; sp/end terminates a collection rule so that nothing else is allowed



;; "ac" short for "accept" -- a rule that accepts whatever
(sp/defrule ac-int (sp/mkpr integer?))
(sp/defrule ac-int* (sp/mkzom ac-int))
(sp/defrule ac-int+ (sp/mk1om ac-int))
(sp/defrule ac-int? (sp/mkopt ac-int))

(sp/defrule ac-int*end (sp/mkseq (sp/mkzom ac-int) sp/end))

;; make a macro to expand the quantified versions from the mkpr

(sp/defrule ac-vec (sp/mkpr vector?))
(sp/defrule ac-vec* (sp/mkzom ac-vec))
(sp/defrule ac-vec+ (sp/mk1om ac-vec))
(sp/defrule ac-vec? (sp/mkopt ac-vec))

(sp/defrule ac-sym (sp/mkpr symbol?))
(sp/defrule ac-sym* (sp/mkzom ac-sym))
(sp/defrule ac-sym+ (sp/mk1om ac-sym))
(sp/defrule ac-sym? (sp/mkopt ac-sym))

(sp/defrule ac-two-by-two (sp/mkseq ac-int ac-int ac-sym ac-sym))


(sp/defrule ac-sym-int-alt (sp/mk1om (sp/mkseq ac-sym ac-int)))

;; Note the rules are greedy and won't backtrack on failure.  Once a subrule succeeds, it's
;; committed.  That's it.  Some ambiguous `sym? sym int` might fail on a `sym int`, but `sym sym?
;; int` would succeed.
;;
;; This is intentional.  See the paper on PEGs. ? * + are greedy.  But you can use & to be
;; non-greedy.
;; 
;; http://pdos.csail.mit.edu/papers/parsing:popl04.pdf

;; surprisingly no backtracking
(sp/defrule ac-sosi (sp/mkseq ac-sym? ac-sym ac-int))

;; better
(sp/defrule ac-ssi (sp/mkseq ac-sym ac-sym? ac-int))


;; Issue, parsing mksub ignores extra junk.  Have to use my sp/end to force termination with nothing
;; extra.



(sp/defrule ac-iopt-si+ (sp/mkseq ac-int? (sp/mk1om (sp/mkseq ac-sym ac-int+))))

(sp/defrule ac-s-vint (sp/mkseq ac-sym (sp/mksub ac-int+)))

(sp/defrule ac-vsi (sp/mksub (sp/mkseq (sp/mkzom (sp/mkseq ac-sym ac-int)) sp/end)))

(sp/defrule ac-vsix (sp/mksub (sp/mkzom (sp/mkseq ac-sym ac-int))))                    

(sp/defrule ac-vsix2 (sp/mkret (sp/mkbind ac-vsix :contents)
                              (fn [b con] (vec (:contents b)))))

(sp/defrule ac-vsix1  (sp/mkbind ac-vsix :contents))
                     

(sp/defrule ac-s-vsix-s (sp/mkseq ac-sym (sp/mksub (sp/mkzom (sp/mkseq ac-sym ac-int))) ac-sym))


(sp/defrule ac-svs (sp/mkseq ac-sym ac-vsix ac-sym))

(sp/defrule ac-map (sp/mkpr map?))
(sp/defrule ac-map* (sp/mkzom ac-map))


(defn plant? [p]
  (and (map? p) 
       (when-let [id (:id p)]
         (zero? (mod (count id) 4)))))


(sp/defrule ac-plant* (sp/mkseq (sp/mkzom (sp/mkpr plant?)) sp/end))


(defn tcon-pred [tcon]
  (get test-fn tcon))

;; need to use eval
;; http://stackoverflow.com/questions/1824932/clojure-how-to-create-a-function-at-runtime

;; FIXME -- this is dangerous is you allow user-defined guards.  They could (launch-missiles)!
;; We should define allowed functions and audit the guard code.
(defn runtime-fn [arg expr]
  (eval `(fn [~arg] ~expr)))

(defn tpred [name con pred]
  (let [arg (case name (_ nil :when) '% name)
        form (if (nil? con)
               (list pred arg)
               (list 'and (list pred arg) con))]
    (runtime-fn arg form)))


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


(defn tcon-list-type [lexpr]
  (let [[tcon name con] lexpr
        lch (last-char tcon)
        tcon (simple-sym tcon)
        pred (tcon-pred tcon)
        brule (sp/mkpr (tpred name con pred))]
    (case lch
      \+ (sp/mk1om brule)
      \* (sp/mkzom brule)
      \? (sp/mkopt brule)
      brule)))

(defn tcon-quoted-sym [sym]
  ;; no special interpretation of symbol
  (sp/mkpr (tcon-pred sym)))

;; n is the total number of desired items,
;; cs might have to be repeated to fill
(defn tcon-nseq 
  ([n cs]
     (case n
       0 (sp/mkseq)
       1 (tconstraint (first cs))
       (apply sp/mkseq (take n (cycle (map tconstraint cs))))))
  ([lo hi cs]
     (let [tcons (cycle (map tconstraint cs))]
       (apply sp/mkseq (concat (take lo tcons)
                               (take (- hi lo) (map sp/mkopt (drop lo tcons))))))))

(defn tcon-seq [cs]
  (apply sp/mkseq (map tconstraint cs)))


;; SEM FIXME -- maybe a little shakey on merging bindings and memo stuff
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
                 r1)))))))
  ([rule1 rule2 & rules]
     (reduce mkand (mkand rule1 rule2) rules)))


(defn tcon-seq-constraint [vexpr]
  (sp/mksub (apply sp/mkseq (conj (mapv tconstraint vexpr) sp/end))))

(defn tcon-list-constraint [lexpr]
  (let [op (first lexpr)]
    (case op
      or (apply sp/mkalt (map tconstraint (rest lexpr)))
      and (apply mkand (map tconstraint (rest lexpr)))
      not (sp/mknot (tconstraint (second lexpr)))
      quote (tcon-quoted-sym (second lexpr))
      * (sp/mkzom (tcon-seq (rest lexpr)))
      + (sp/mk1om (tcon-seq (rest lexpr))) 
      ? (sp/mkopt (tcon-seq (rest lexpr)))  
      = (tcon-seq (rest lexpr))
      seq  (tcon-seq-constraint (rest lexpr))
      vec (mkand (list (sp/mkpr vector?) (tcon-seq-constraint (rest lexpr))))
      list (mkand (list (sp/mkpr list?) (tcon-seq-constraint (rest lexpr))))

      ;; else
      (cond (integer? op) (tcon-nseq op (rest lexpr))
            (vector? op) (tcon-nseq (first op) (second op) (rest lexpr))
            :else (tcon-list-type lexpr)))))

(defn testing-list-constraint [lexpr]
  (let [[tcon name con] lexpr
        pred (tcon-pred tcon)]
    (println "tcon = " tcon (type tcon))
    (println "name = " name (type name))
    (println "con = " con (type con))
    (println "pred = " pred (type pred))
    (flush)
    (tpred name con pred)))

;; SEM untested and unused
(defn mkguard
  "Create a rule that matches all of rules in order. Returns result of first rule.
The others are guard rules that should not consume any input."
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
               r1))))))
  ([rule1 rule2 & rules]
     (reduce mkguard (mkguard rule1 rule2) rules)))



;; kw cons are encoded into the rules
(defn mkmap [rules]
  (fn [input bindings context memo]
    (let [m (first input)]
      (if (and (seq input) (map? m)
               (every? (fn [rule] (sp/success? (rule (list m) bindings context memo)))
                       rules))
        (sp/succeed m [m] (rest input) bindings memo)
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


(defn tcon-map-entry [[kw con]]
  ;; FIXME -- only handles kw literals and optional :kw? for keys
  ;; doesn't carry context or results for individual key/val matches
  ;; Note: each rule expect full map as input, but only looks at one key
  (let [sk (simple-key kw)
        rule (tconstraint con)]
    (if (optional-key? kw)
      (sp/mkpr (fn [m]
                 (or (not (contains? m sk))
                     (sp/success? (rule (list (get m sk)) {} {} {})))))
      (sp/mkpr (fn [m]
                 (and (contains? m sk)
                      (sp/success? (rule (list (get m sk)) {} {} {}))))))))

(defn tcon-map-constraint [mexpr]
  (mkmap (map tcon-map-entry mexpr)))

(defn tconstraint 
  ([expr]
     (cond (symbol? expr) (tcon-symbol-constraint expr)
           (list? expr) (tcon-list-constraint expr)
           (vector? expr) (tcon-seq-constraint expr)
           (map? expr) (tcon-map-constraint expr) 
           (string? expr) (sp/mklit expr)
           (keyword? expr) (sp/mklit expr)
           (nil? expr) (sp/mkpr nil?)
           (false? expr) (sp/mkpr false?)
           (true? expr) (sp/mkpr true?)
           (number? expr) (sp/mklit expr)
             :else (throw (ex-info "Unknown constraint form" {:form expr}))))
     
  ([expr expr2]
     (sp/mkseq (tconstraint expr) (tconstraint expr2)))
  ([expr expr2 & more]
     (apply sp/mkseq (tconstraint expr) (tconstraint expr2) (map tconstraint more))))



(defn between-pred [lo hi]
  ;; inclusive
  #(<= lo % hi))

(defn confn [con]
  (let [cfn (tconstraint con)]
    (fn ff
      ([item] (ff item {} {} {}))
      ([item context] (ff item context {} {}))
      ([item context bindings memo] (cfn (list item) context bindings memo)))))

;; Too Clever?  Single arg creates predicate (for reuse).  Second arg immediately tests.
;; Con can be a fn already (presumed to be a predicate), or a "constraint expression" which
;; is compiled into a predicate.
(defn conforms? 
  ([con] (if (fn? con) con #(sp/success? ((confn con) %))))
  ([con x] ((conforms? con) x)))
