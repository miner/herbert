(ns miner.herbert
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [squarepeg.core :as sp]
            [miner.herbert.predicates]))


(def predicates-ns (the-ns 'miner.herbert.predicates))
(def reserved-ops '#{+ * ? & = == < > not= >= <= quote and or not assert vec seq list map mod})
(declare default-predicates)
;; default-predicates defined a bit later so it can use some fns

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

(defn ns->predicates [ns]
  (reduce-kv (fn [cm k v] (if (= (last-char k) \?) (assoc cm (simple-sym k) v) cm))
             {}
             (ns-publics (the-ns ns))))

(def default-predicates (ns->predicates predicates-ns))

(defn simple-sym? [sym]
  (= sym (simple-sym sym)))

(defn quantified-sym? [sym]
  (not= sym (simple-sym sym)))

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


(defn ext-rule [sym extensions]
  (get-in extensions [:expressions sym]))  

(defn ext-pred [sym extensions]
  (get-in extensions [:predicates sym]))

;; SEM FIXME -- a bit of extra work to test pred as var but safer
(defn tcon-pred [tcon extensions]
  (or (ext-pred tcon extensions)
      (get default-predicates tcon)))

(declare mkconstraint)

(defn mk-solo [bname]
  ;; simple name should match item equal to that binding
  (let [solo (gensym bname)]
    (mkscope
     (sp/mkseq 
      (sp/mkbind sp/anything solo)
      (sp/mkpred (fn [bindings context] (= (get bindings bname) (get bindings solo))))))))

;; extensions could have a user-defined erule
;; erule wins over built-in pred
(defn mk-symbol-constraint [sym extensions]
  (let [lch (last-char sym)
        sym (simple-sym sym)
        erule (ext-rule sym extensions)
        pred (when-not erule (tcon-pred sym extensions))
        brule (or erule (if pred (sp/mkpr pred) (mk-solo sym)))]
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

(defn mk-list-simple-type [name lexpr extensions]
  (let [[tcon & args] lexpr
        lch (last-char tcon)
        sym (simple-sym tcon)
        erule (ext-rule sym extensions)
        pred (when-not erule (tcon-pred sym extensions))
        brule (or erule (mkbase pred args))
        rule (case lch
               \+ (sp/mk1om brule)
               \* (sp/mkzom brule) 
               \? (sp/mkopt brule)
               brule)]
    (if name
      (sp/mkbind rule name)
      rule)))

;; complex implies something like a map or vector that needs further processing
(defn mk-list-complex-type [name lexpr extensions]
  (assert (empty? (rest lexpr)))
  (let [rule (mkconstraint (first lexpr) extensions)]
    (if name
      (sp/mkbind rule name)
      rule)))

(defn bind-name [sym extensions]
  (and (symbol? sym)
       (not (contains? reserved-ops sym))
       (not (contains? (:expressions extensions) sym))
       (not (tcon-pred (simple-sym sym) extensions))
       sym))

(defn mk-list-type [lexpr extensions]
  (when-first [fst lexpr]
    (let [bname (bind-name fst extensions)
          expr (if bname (rest lexpr) lexpr)]
      (cond (and bname (nil? (seq expr))) (mk-solo bname)
            (symbol? (first expr)) (mk-list-simple-type bname expr extensions)
            :else (mk-list-complex-type bname expr extensions)))))

;; :else (throw (ex-info "Unknown mk-list-type" {:name bname :con lexpr}))))))

(defn pred-term? [extensions sym]
  (and (symbol? sym)
       (or (contains? reserved-ops sym)
           (contains? (:expressions extensions) sym)
           (tcon-pred (simple-sym sym) extensions))
       sym))

(defn BAD-mk-list-type [lexpr extensions]
  (when-first [fst lexpr]
    (if-let [bname (bind-name fst extensions)]
      ;; first sym is clearly the bind name, not ambiguous
      (let [expr (rest lexpr)]
        (cond (nil? (seq expr)) (mk-solo bname)
              (symbol? (first expr)) (mk-list-simple-type bname expr extensions)
              :else (mk-list-complex-type bname expr extensions)))
      ;; first sym might shadow another term
      (let [sec (second lexpr)]
        (cond (coll? sec) (mk-list-complex-type fst (rest lexpr) extensions)
              (pred-term? extensions sec) (mk-list-simple-type fst (rest lexpr) extensions)
              (not (symbol? sec)) (mk-list-simple-type nil lexpr extensions)
              :else (mk-list-simple-type nil lexpr extensions))))))

;; :else (throw (ex-info (str "Mangled term: " (pr-str lexpr)) {:bad-expr lexpr})))))))


;; probably don't want this
(defn mk-quoted-sym [sym extensions]
  (if-let [pred (tcon-pred sym extensions)]
    (sp/mkpr pred)
    (throw (ex-info (str "No constraint function defined for " sym) 
                    {:sym sym :extensions extensions}))))

(defn mk-con-seq [cs extensions]
  (apply sp/mkseq (map #(mkconstraint % extensions) cs)))

(defn mk-subseq-constraint [vexpr extensions]
  (sp/mksub (apply sp/mkseq (conj (mapv #(mkconstraint % extensions) vexpr) sp/end))))

;; SEM FIXME: strictly speaking, anonymous fns might have some free symbols mixed in so really you
;; should disjoin the fn args within that scope but take the other symbols.
(defn args-from-body 
  ([expr] (args-from-body #{} expr))
  ([res expr]
     (cond (symbol? expr) (conj res expr)
           (vector? expr) (reduce set/union res (map args-from-body expr))
           (seq? expr) 
             (case (first expr) 
               ;; ignore anonymous fns and quoted values
               (fn quote) res
               ;; disallow some fns
               (apply eval) (throw (ex-info "Herbert asserts do not allow 'apply' or 'eval'"
                                            {:form expr}))
               ;; for a normal list, skip the "fn", first element
               (reduce set/union res (map args-from-body (rest expr)))))))

;; SEM FIXME: potentially dangerous eval;  Maybe try Clojail or something to have a restricted eval,
;; but the args-from-body restricts some obviously dangerous calls.
(defn runtime-pred 
  "Creates a fn that takes a single map argument, which should have keys corresponding to all
   the 'free' symbols in the body.  The body argument is a single form that will be evaluated at
   runtime."
  [body]
  {:pre [(seq? body)]}
  (let [args (args-from-body body)
        pred (eval `(fn [{:syms [~@args]}] ~body))]
    pred))

(defn mk-assert [body]
  ;; assert syntax takes just a single expr.  Symbols are looked up from previously bound names,
  ;; except for the first "fn" position of a list.  Some fns are not allowed, such as "apply" and
  ;; "eval".  
  (let [pred (runtime-pred body)]
    (sp/mkpred (fn [bindings context] (pred (merge context bindings))))))


(declare mk-map-constraint)

(defn third [s]
  (first (nnext s)))

(defn mk-list-constraint [lexpr extensions]
  (let [op (first lexpr)
        mkconstr #(mkconstraint % extensions)]
    (case op
      or (apply sp/mkalt (map mkconstr (rest lexpr)))
      and (apply mkand (map mkconstr (rest lexpr)))
      not (sp/mkseq (sp/mknot (mkconstr (second lexpr))) sp/anything)
      quote (if (symbol? (second lexpr))
              ;; symbols can be used as literals if quoted
              (sp/mklit (second lexpr))
              ;; dequoting here is convenient for macros
              (mkconstr (second lexpr)))
      (= == not= < > <= >=) (mk-assert lexpr)
      assert (mk-assert (second lexpr))
      * (sp/mkzom (mk-con-seq (rest lexpr) extensions))
      + (sp/mk1om (mk-con-seq (rest lexpr) extensions)) 
      ? (sp/mkopt (mk-con-seq (rest lexpr) extensions))
      & (mk-con-seq (rest lexpr) extensions)
      seq  (mk-subseq-constraint (rest lexpr) extensions)
      vec (mkand (sp/mkpr vector?) (mk-subseq-constraint (rest lexpr) extensions))
      list (mkand (sp/mkpr list?) (mk-subseq-constraint (rest lexpr) extensions))
      map (mk-map-constraint (second lexpr) (third lexpr) extensions)
      ;; else
      (mk-list-type lexpr extensions))))

;; need to reduce the subrules and preserve the bindings
;; SEM FIXME -- not sure about merging memo.  This one just passes on original memo.
(defn mkmap [rules]
  (fn [input bindings context memo]
    (let [m (first input)]
      (if (map? m)
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

;; SEM FIXME -- everywhere we use sp/success? we have to look for passing up the bindings
;; which means sp/mkpr is not going to be sufficient in many cases.

(defn mk-key
  "Takes key and rule for associated val.  Fails if key is missing or val doesn't match.  Does not
  consume anything."  
  [key rule]
  (fn [input bindings context memo]
    (if (nil? (seq input))
      (sp/fail "End of input" memo)
      (let [m (first input)]
        (if (contains? m key)
          (let [r (rule (list (get m key)) bindings context memo)]
            (if (sp/failure? r)
              r
              (sp/succeed nil [] input (:b r) (:m r))))
          (sp/fail (str key " is not in map.") memo))))))

;; SEM FIXME -- what if m is not a map?  For now, says OK for optional.
(defn mk-kw-opt
  "Takes kw and rule for associated val.  If kw is found, val must match rule.  As a special case, a
nil value also succeeds for an optional kw.  Does not consume anything."
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

(defn mk-map-entry [[key con] extensions]
  ;; FIXME -- only handles kw literals and optional :kw? for keys
  ;; doesn't carry context or results for individual key/val matches
  ;; Note: each rule expect full map as input, but only looks at one key
  ;; keys are literals (or quoted literals), not full constraints
  ;; so symbols get treated as literals, appropriate for literal '{foo 1} maps
  (let [rule (mkconstraint con extensions)]
    (cond (optional-key? key) (mk-kw-opt (simple-key key) rule)
          (or (literal? key) (symbol? key)) (mk-key key rule)
          (and (seq? key) (= (first key) 'quote)) (mk-key (second key) rule)
          :else (throw (ex-info (str "Unsupported literal key " (pr-str key)) 
                                {:key key :constraint con})))))

(defn mk-map-literal-constraint [mexpr extensions]
  (mkmap (map #(mk-map-entry % extensions) mexpr)))

;; SEM FIXME: bindings don't get passed down from krule and vrule
(defn mk-map-constraint [key-con val-con extensions]
  (let [krule (when key-con (mkconstraint key-con extensions))
        vrule (when val-con (mkconstraint val-con extensions))]
    (sp/mkpr (fn [m]
               (and (map? m)
                    (or (not krule) (every? #(sp/success? (krule (list %) {} {} {})) (keys m)))
                    (or (not vrule) (every? #(sp/success? (vrule (list %) {} {} {})) (vals m))))))))

(defn mk-set-sym [sym extensions]
  (let [simple (simple-sym sym)
        rule (mkconstraint simple extensions)]
    (case (last-char sym)
      \* (sp/mkpr (fn [s] (every? #(sp/success? (rule (list %) {} {} {})) s)))
      \+ (sp/mkpr (fn [s] (and (seq s) (every? #(sp/success? (rule (list %) {} {} {})) s))))
      \? (sp/mkpr (fn [s] (or (empty? s) 
                              (and (empty? (rest s))
                                   (sp/success? (rule (seq s) {} {} {}))))))
      ;; else simple
      (sp/mkpr (fn [s] (some #(sp/success? (rule (list %) {} {} {})) s))))))

(defn mk-set-list [lst extensions]
  (let [[op con unexpected] lst
        quantified (case op (* + ?) true false)
        rule  (if quantified (mkconstraint con extensions) (mkconstraint lst extensions))]
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


(defn mk-set-element [con extensions]
  (cond (symbol? con) (mk-set-sym con extensions)
        (list? con) (mk-set-list con extensions)
        (literal? con) (throw (ex-info "Literals should be handled separately" 
                                       {:con con :extensions extensions}))
        :else (throw (ex-info "I didn't think of that" {:con con :extensions extensions}))))


(defn mk-set-constraint [sexpr extensions]
  (let [nonlits (remove literal? sexpr)
        litset (if (seq nonlits) (set (filter literal? sexpr)) sexpr)]
    (apply mkand (sp/mkpr #(set/subset? litset %)) (map #(mk-set-element % extensions) nonlits))))
           

(defn mkconstraint 
  ([expr] (mkconstraint expr {}))
  ([expr extensions]
     #_ (println "mkconstraint " expr)
     (cond (symbol? expr) (mk-symbol-constraint expr extensions)
           ;; don't use list?, seq? covers Cons as well
           (seq? expr) (mk-list-constraint expr extensions)
           (vector? expr) (mk-subseq-constraint expr extensions)
           (set? expr) (mk-set-constraint expr extensions)
           (map? expr) (mk-map-literal-constraint expr extensions) 
           (string? expr) (sp/mklit expr)
           (keyword? expr) (sp/mklit expr)
           (nil? expr) (sp/mkpr nil?)
           (false? expr) (sp/mkpr false?)
           (true? expr) (sp/mkpr true?)
           (number? expr) (sp/mklit expr)
           :else (throw (ex-info "Unknown constraint form" {:con expr :extensions extensions})))))


;; util
(defn pairs [map-or-flatseq]
  ;; array-map will preserve order, but a hash-map gives no guaranteed order
  (if (map? map-or-flatseq)
    (seq map-or-flatseq)
    (partition-all 2 map-or-flatseq)))

;; SEM FIXME:  should we allow symbols instead of requiring vars for predicates?
;; SEM FIXME:  won't work with recursive constraints, order-sensistive
(defn as-extensions [exts]
  (let [preds (reduce (fn [res [k v]] (assoc res k (if (symbol? v) (resolve v) v))) 
                      {} 
                      (pairs (:predicates exts)))]
    (reduce (fn [es [k v]] (assoc-in es [:expressions k] (mkconstraint v es)))
            {:predicates preds :expressions {}}
            (pairs (:expressions exts)))))

  
;; exts is map of {:predicates? (map sym var) :expressions? [(* sym con-expr)]}
(defn constraint-fn 
  ([schema] (constraint-fn {} schema))
  ([context schema]
     (let [cfn (mkconstraint schema (as-extensions context))]
       (fn ff
         ([item] (ff item {} {} {}))
         ([item context] (ff item context {} {}))
         ([item context bindings memo] (cfn (list item) context bindings memo))))))


;; creates a fn that test for conformance to the schema with the given context
(defn conform-fn
  ([schema] (if (fn? schema) schema (conform-fn {} schema)))
  ([context schema]
     (let [schema-context (assoc context :schema schema)]
       (fn 
         ([] schema-context)
         ([x] (let [res ((constraint-fn context schema) x)]
                (when (sp/success? res)
                  (with-meta (:b res) {::schema-context schema-context}))))))))

(defn conform
  ([schema x] ((conform-fn {} schema) x))
  ([context schema x] ((conform-fn context schema) x)))

(defn conforms? 
  ([schema x] (conforms? {} schema x))
  ([context schema x]
     (boolean (conform context schema x))))
