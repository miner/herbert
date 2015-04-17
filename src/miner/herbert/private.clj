(ns miner.herbert.private
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [miner.tagged :as tag]
            [squarepeg.core :as sp]
            [miner.herbert.util :refer :all]
            [miner.herbert.predicates :as predicates])
  (:import miner.tagged.TaggedValue))



(def internal-predicates-ns (the-ns 'miner.herbert.predicates))

(def internal-reserved-ops '#{+ * ? & = == < > not= >= <= 
                              quote and or not when class pred
                              vec seq list set map mod tag
                              := grammar recur herbert})

(def ^:dynamic *herbert-readers* nil)

(def ^:dynamic *herbert-default-tag-reader* (constantly nil))

(defn reserved-sym? [sym]
  (contains? internal-reserved-ops sym))

(defn- symbol-without-last-char [sym]
  (let [sname (name sym)]
    (symbol (subs sname 0 (dec (.length sname))))))

(defn internal-ns->predicates [ns]
  (reduce-kv (fn [cm k v]
               (if (= (last-char k) \?) (assoc cm (symbol-without-last-char k) v) cm))
             {}
             (ns-publics (the-ns ns))))

(def internal-default-predicates (internal-ns->predicates internal-predicates-ns))


;;;; repeated in canonical
(defn quantified? [expr]
  (and (seq? expr) (case-of? (first expr) * + ?)))

(defn as-quantified [expr]
  (if (quantified? expr)
    expr
    (list '+ expr)))

(defn literal-or-quoted? [expr]
  (or (predicates/literal? expr)
      (and (seq? expr) (= (first expr) 'quote))))

(defn optional-literal? [expr]
  (and (seq? expr)
       (case-of? (first expr) ?)
       (literal-or-quoted? (second expr))))

(defn dequote [expr]
  (if (and (seq? expr) (= (first expr) 'quote))
    (second expr)
    expr))
;;;; END repeated in canonical

  
;; loosey-goosey get or just yourself, sort of an ersatz lexical binding
;; This is necessary to allow `step`, `iter` and `indexed` to work with fn names (see as-fn)
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
        (sp/succeed (:r r) (:s r) (:i r) (merge bindings (:b r)) (:m r))
        r))))

(defn mkprb
  "Like mkpr but allows extra args to be added before item.  The `symbolic` arg is the symbolic name
  of the predicate (used for error reporting). Args are either literal values or keys that are
  looked up in bindings.  If the key is not found, the value is the key itself."  
  ([pr] (mkprb pr pr nil))
  ([pr symbolic] (mkprb pr symbolic nil))
  ([pr symbolic args]
     (fn [input bindings context memo]
       (if (nil? (seq input))
         (sp/fail "End of input" memo)
         (let [i (first input)
               pred (if-let [args (seq args)] 
                      (apply partial pr (map #(lookup % bindings) args))
                      pr)]
          (if (pred i)
            (sp/succeed i [i] (rest input) bindings memo)
            (sp/fail (str i " does not match " symbolic) memo)))))))

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
           (let [r2 (rule2 input (merge bindings (:b r1)) context (:m r1))]
             (if (sp/failure? r2)
               r2
               ;; maybe should use not identical?
               (if (not= (:r r1) (:r r2))
                 (sp/fail "Subrules matched differently" (merge (:m r1) (:m r2)))
                 r2)))))))
  ([rule1 rule2 & rules]
     (reduce mkand (mkand rule1 rule2) rules)))


(defn ext-rule [sym extensions]
  (get-in extensions [:terms sym]))

;; SEM FIXME -- a bit of extra work to test pred as var but safer
;; SEM FIXME -- no longer need extensions here
(defn tcon-pred [tcon extensions]
  (get internal-default-predicates tcon))

(declare mkconstraint)
(declare mkrecursive)

(defn mk-lookup [bname]
  ;; simple name should match item equal to that binding
  (let [solo (gensym bname)]
    (mkscope
     (sp/mkseq 
      (sp/mkbind sp/anything solo)
      (sp/mkpred (fn [bindings context] (= (get bindings bname) (get bindings solo))))))))

;; extensions could have a user-defined erule
;; erule wins over built-in pred
(defn mk-symbol-constraint [sym extensions]
  (or (ext-rule sym extensions)
      (let [pred (tcon-pred sym extensions)]
        (if pred (mkprb pred sym) (mk-lookup sym)))))

;; SEM FIXME: be careful about where the iterfn is resolved
;; maybe should bind or use *ns* directly

(defn bind-symbol? [expr]
  ;; unqualified symbol with no dot
  (and (symbol? expr)
       (nil? (namespace expr))
       (not (.contains (name expr) "."))))

;; maybe a bit overly generic in handling nil name -- could yank guts for better refactoring
(defn mk-list-bind [name lexpr extensions]
  (assert (or (nil? name) (bind-symbol? name)))
  (let [name (and (not (nil? name)) (not= name '_) name)]
    (if (empty? (rest lexpr))
      (let [rule (mkrecursive (first lexpr) extensions name)]
        (if name
          (sp/mkbind rule name)
          rule))
      (let [[sym & args] lexpr
            ;; SEM FIXME erule ignores extra args
            rule (or (ext-rule sym extensions)
                     (mkprb (tcon-pred sym extensions) sym args))]
      (if name
        (sp/mkbind rule name)
        rule)))))


(defn mk-con-seq [cs extensions]
  (apply sp/mkseq (map #(mkconstraint % extensions) cs)))

;; modified version of sp/mksub, allows specialized test `pred` for exact container test
;; default sequential?, but seq? and vector? are also appropriate.
;; rule automatically gets sp/end attached at end (forces complete match of sequence)
(defn mk-sequential
  ([rule] (mk-sequential sequential? rule))
  ([pred rule]
     (let [subrule (if rule (sp/mkseq rule sp/end) sp/end)]
       (fn [input bindings context memo]
         (if (and (seq input) (pred (first input)))
           (let [r (subrule (first input) bindings context memo)]
             (if (sp/success? r)
               ;; try to maintain seq/vector distinction of input, :s value is vector
               (let [res (if (seq? (first input)) (seq (:s r)) (:s r))]
                 (sp/succeed res [res] (rest input) (merge bindings (:b r)) (:m r)))
               r))
           (sp/fail "Input not expected sequence type." memo))))))

(defn mk-subseq-constraint 
  ([vexprs extensions] (mk-subseq-constraint sequential? 'seq vexprs extensions))
  ([pred symbolic vexprs extensions]
     (if-let [vs (seq vexprs)]
       (mk-sequential pred (apply sp/mkseq (map #(mkconstraint % extensions) vs)))
       (mkprb pred symbolic))))


;; SEM FIXME: strictly speaking, anonymous fns might have some free symbols mixed in so really you
;; should disjoin the fn args within that scope but take the other symbols.
;; SEM FIXME: untested for maps and sets
;; SEM FIXME: let not supported (buggy)
;; SEM FIXME: overall, pretty rough
(defn args-from-body 
  ([expr] (args-from-body #{} expr))
  ([res expr]
     (cond (bind-symbol? expr) (conj res expr)
           (vector? expr) (reduce set/union res (map args-from-body expr))
           (seq? expr) 
             (case (first expr) 
               ;; ignore anonymous fns and quoted values
               (fn quote) res
               ;; disallow some fns
               (apply eval) (throw (ex-info "Herbert 'when' tests do not allow 'apply' or 'eval'"
                                            {:form expr}))
               ;; for a normal list, skip the "fn", first element
               (reduce set/union res (map args-from-body (rest expr))))
           (map? expr) (reduce set/union res (map args-from-body (vals expr)))
           (set? expr) (reduce set/union res (map args-from-body (seq expr))))))

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

(defn mk-when [body]
  ;; when syntax takes just a single expr.  Symbols are looked up from previously bound names,
  ;; except for the first "fn" position of a list.  Some fns are not allowed, such as "apply" and
  ;; "eval".  
  (let [pred (runtime-pred body)]
    (sp/mkpred (fn [bindings context] (pred (merge context bindings))))))


(defn mk-pred-args [sym args]
  (let [pred (if (fn? sym) sym (resolve sym))]
    (mkprb pred sym args)))

(defn mk-class [sym]
  (let [clazz (if (class? sym) sym (resolve sym))]
    (mkprb (fn [x] (instance? clazz x)) (list 'class sym))))

(defn- regex-sym-match? [regex-or-str sym]
  (and (re-matches (if (string? regex-or-str) (re-pattern regex-or-str) regex-or-str) (pr-str sym))
       true))

(defn tag-reader-constant [tag val]
  ;; returns the tagged literal constant or nil if not applicable
  ;; allow nil value in reader map to defeat a tag
  (when (and val (symbol? tag) (literal-or-quoted? val))
    (let [reader-map (first (filter #(contains? % tag)
                                 (list *herbert-readers* *data-readers*
                                       default-data-readers)))]
      (if-let [reader (and reader-map (get reader-map tag))]
        (reader (dequote val))
        (let [default-reader (tag/some-tag-reader *herbert-default-tag-reader*
                                                  tag/record-tag-reader
                                                  tag/->TaggedValue)]
          (default-reader tag (dequote val)))))))

(defn mk-tag
  ;; tag could be a symbol (exact match) or a string/regex to match pr-str of item's actual tag
  ([tag] (if (symbol? tag) 
           (mkprb #(= (tag/edn-tag %) tag) (list 'tag tag))
           (mkprb #(regex-sym-match? tag (tag/edn-tag %)) (list 'tag tag))))

  ([tag valpat extensions]
   (if-let [data-constant (tag-reader-constant tag valpat)]
     (sp/mklit data-constant)
     (let [vrule (when valpat (mkconstraint valpat extensions))]
       (fn [input bindings context memo]
         (let [item (first input)
               ival (tag/edn-value item)
               itag (tag/edn-tag item)]
           (if (if (symbol? tag) (= itag tag) (regex-sym-match? tag itag))
             (if vrule
               (let [res (vrule (list ival) bindings context memo)]
                 (if (sp/failure? res)
                   res
                   (sp/succeed item [item] (rest input) (merge bindings (:b res)) memo)))
               (sp/succeed item [item] (rest input) bindings memo))
             (sp/fail (str "Not tagged " tag) memo))))))))

(declare mk-hash-map-constraint)
(declare mk-set-constraint)
(declare schema->extensions)

;; symbols can be used as literals if quoted,
;; quote also defeats "optional" :k? so it's a literal,
;; also empty collections match themselves literally
(defn quotable-literal-expr? [expr]
  (or (symbol? expr) (keyword? expr)
      (and (coll? expr) (empty? expr))))

(defn mk-list-constraint [lexpr extensions]
  (let [op (first lexpr)
        mkconstr #(mkconstraint % extensions)]
    (case op
      or (apply sp/mkalt (map mkconstr (rest lexpr)))
      and (apply mkand (map mkconstr (rest lexpr)))
      not (sp/mkseq (sp/mknot (mkconstr (second lexpr))) sp/anything)
      quote (if (quotable-literal-expr? (second lexpr))
              (sp/mklit (second lexpr))
              ;; dequoting here is convenient for macros
              (mkconstr (second lexpr)))
      (= == not= < > <= >=) (mk-when lexpr)
      when (mk-when (second lexpr))
      * (sp/mkzom (mk-con-seq (rest lexpr) extensions))
      + (sp/mk1om (mk-con-seq (rest lexpr) extensions)) 
      ? (sp/mkopt (mk-con-seq (rest lexpr) extensions))
      & (mk-con-seq (rest lexpr) extensions)
      seq  (mk-subseq-constraint sequential? 'seq (rest lexpr) extensions)
      set (mk-set-constraint (rest lexpr) extensions)
      vec (mk-subseq-constraint vector? 'vec (rest lexpr) extensions)
      list (mk-subseq-constraint seq? 'list (rest lexpr) extensions)
      map (mk-hash-map-constraint (rest lexpr) extensions)
      :=  (mk-list-bind (second lexpr) (nnext lexpr) extensions)
      pred (mk-pred-args (second lexpr) (nnext lexpr))
      class (mk-class (second lexpr))
      tag (mk-tag (second lexpr) (third lexpr) extensions)
      grammar (mkconstraint (second lexpr) (schema->extensions lexpr))
       ;; else it must be a predicate constraint
      ;; SEM FIXME: apparently to support binding name in list (N)
      ;;   but do we need that with canonical form?  Yes, (in Ns)
      (mk-list-bind nil lexpr extensions))))




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
                                          (merge mb (:b res))
                                          (reduced false)))))
                                  bindings
                                  rules)]
          (if (false? mbindings)
            (sp/fail "Input failed to match required map." memo)
            (sp/succeed m [m] (rest input) mbindings memo)))
        (sp/fail "Input failed to match required map." memo)))))


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
              (sp/succeed nil [] input (merge bindings (:b r)) (:m r))))
          (sp/fail (str key " is not in map.") memo))))))

;; SEM FIXME -- what if m is not a map?  For now, says OK for optional.
(defn mk-kw-opt
  "Takes kw and rule for associated val.  If kw is found, val must match rule.  As a special case, a
nil value also succeeds for an optional kw.  Does not consume anything."
  [kw rule]
  ;; (assert (keyword? kw)) -- might be other keys
  (fn [input bindings context memo]
    (if (nil? (seq input))
      (sp/fail "End of input" memo)
      (let [m (first input)
            kw (dequote kw)]
        (if (and (map? m) (get m kw))
          ;; used get above instead of contains? because "optional" interpretation
          ;; treats nil value like having no kw, turns out to be convenient
          (let [r (rule (list (get m kw)) bindings context memo)]
            (if (sp/failure? r)
              r
              (sp/succeed nil [] input (merge bindings (:b r)) (:m r))))
          (sp/succeed nil [] input bindings memo))))))

;; helper for making an exception
(defn- bad-key-exception [k c] 
  (ex-info (str "Unsupported literal key " (pr-str k)) 
           {:key k :constraint c}))

(defn mk-map-pair [key con extensions]
  ;; FIXME -- only handles kw literals and optional :kw? for keys
  ;; doesn't carry context or results for individual key/val matches
  ;; Note: each rule expect full map as input, but only looks at one key
  ;; keys are literals (or quoted literals), not full constraints
  ;; so symbols get treated as literals, appropriate for literal '{foo 1} maps
  (let [rule (mkconstraint con extensions)]
    (cond 
          (or (predicates/literal? key) (symbol? key)) (mk-key key rule)
          (seq? key) (case (first key)
                       (quote +) (mk-key (second key) rule)
                       (? *) (mk-kw-opt (second key) rule)
                       (throw (bad-key-exception key con)))
          :else (throw (bad-key-exception key con)))))

(defn mk-map-entry [[key con] extensions]
  (mk-map-pair key con extensions))



(defn mk-keys-vals-constraint [keys-con vals-con extensions]
  (let [krule (sp/mkseq (mkconstraint keys-con extensions) sp/end)
        vrule (sp/mkseq (mkconstraint vals-con extensions) sp/end)]
    (fn [input bindings context memo]
      (if (nil? (seq input))
        (sp/fail "End of input" memo)
        (let [m (first input)]
          (if (map? m)
            (let [kr (krule (keys m) bindings {} {})]
              (if (sp/success? kr)
                (let [vr (vrule (vals m) (merge bindings (:b kr)) {} {})]
                  (if (sp/success? vr)
                    (sp/succeed m [m] (rest input) (merge bindings (:b kr) (:b vr)) memo)
                    (sp/fail (str m " does not match predicate.") memo)))
                (sp/fail (str m " does not match predicate.") memo)))
            (sp/fail (str m " does not match predicate.") memo)))))))


(defn mk-map-literal-constraint [mexpr extensions]
  (if (empty? mexpr)
    (sp/mklit {})
    (let [kvs (seq mexpr)
          single (nil? (next kvs))]
      (if (and single
               (not (literal-or-quoted? (key (first kvs))))
               (not (optional-literal? (key (first kvs)))))
        (mk-keys-vals-constraint (as-quantified (key (first kvs)))
                                 (as-quantified (val (first kvs)))
                                 extensions)
        (mkmap (map #(mk-map-entry % extensions) kvs))))))

(defn mk-hash-map-constraint [kvexprs extensions]
  (if (empty? kvexprs)
    (mkprb map? 'map)
    (let [kvs (partition 2 kvexprs)
          single (nil? (next kvs))]
      (if (and single 
               (not (literal-or-quoted? (first kvexprs)))
               (not (optional-literal? (first kvexprs))))
        (mk-keys-vals-constraint (as-quantified (first kvexprs))
                                 (as-quantified (second kvexprs))
                                 extensions)
        (mkmap (map #(mk-map-entry % extensions) kvs))))))


;; SEM FIXME: bindings don't get passed down rules

(defn set-zom [rule] 
  (fn [s] (every? #(sp/success? (rule (list %) {} {} {})) s)))

(defn set-1om [rule] 
  (fn [s] (and (seq s) (every? #(sp/success? (rule (list %) {} {} {})) s))))

(defn set-opt [rule] 
  (fn [s] (or (empty? s) 
              (and (empty? (rest s))
                   (sp/success? (rule (seq s) {} {} {}))))))

(defn set-some [rule]
  (fn [s] (some #(sp/success? (rule (list %) {} {} {})) s)))

(defn mk-set-sym [sym extensions]
  (mkprb (set-some (mkconstraint sym extensions)) sym))

(defn mk-set-list [lst extensions]
  (let [[op con unexpected] lst
        quantified (case op (* + ?) true false)
        rule  (if quantified (mkconstraint con extensions) (mkconstraint lst extensions))]
    (when (and quantified unexpected)
      (throw (ex-info "Unexpectedly more" {:con lst})))
    (case op
      * (mkprb (set-zom rule) lst)
      + (mkprb (set-1om rule) lst)
      ? (mkprb (set-opt rule) lst)
      ;; else quantified
      (mkprb (set-some rule) lst))))

(defn mk-set-element [con extensions]
  (cond (symbol? con) (mk-set-sym con extensions)
        (literal-or-quoted? con) (throw (ex-info "Literals should be handled separately" 
                                       {:con con :extensions extensions}))
        (seq? con) (mk-set-list con extensions)
        :else (throw (ex-info "I didn't think of that" {:con con :extensions extensions}))))

(defn mk-set-constraint [sexpr extensions]
  (let [nonlits (remove literal-or-quoted? sexpr)
        litset (map dequote (if (seq nonlits) (set (filter literal-or-quoted? sexpr)) sexpr))]
    (apply mkand 
           (mkprb set? sexpr)
           (mkprb #(set/subset? litset %) sexpr) 
           (map #(mk-set-element % extensions) nonlits))))

;; SEM FIXME: use a Protocol
(defn mkconstraint 
  ([expr] (mkconstraint expr {}))
  ([expr extensions]
     #_ (println "mkconstraint " expr)
     (cond (symbol? expr) (mk-symbol-constraint expr extensions)
           (and (coll? expr) (empty? expr)) (sp/mklit expr)
           ;; don't use list? -- seq? covers Cons as well
           (seq? expr) (mk-list-constraint expr extensions)
           ;; (vector? expr) (mk-subseq-constraint sequential? 'seq expr extensions)
           ;; (set? expr) (mk-set-constraint expr extensions)
           ;; (map? expr) (mk-map-literal-constraint expr extensions)
           ;; keep optional-key? before literal? test
           ;; (optional-key? expr) (sp/mkopt (sp/mklit (simple-key expr)))
           (predicates/literal? expr) (sp/mklit expr)
           :else (throw (ex-info "Unknown constraint form" {:con expr :extensions extensions})))))

(defn qsymbol? [x]
  (and (symbol? x) (namespace x)))

(defn grammar? [schema]
  (and (seq? schema) (= (first schema) 'grammar)))

(defn mkrecursive [start exts rsym]
  ;; rsym is the symbol used for a recursive rule, in a := binding or grammar rule
  (if rsym
    (letfn [(cfn [item context bindings memo]
              (let [mkcon (mkconstraint start (assoc-in exts [:terms rsym] cfn))]
                (mkcon item context bindings memo)))]
      (sp/mkmemo cfn))
    (sp/mkmemo (mkconstraint start exts))))

;; exts is map of {:terms? {sym* rule*}} with provision for future expansion
(defn schema->extensions [schema]
  (let [default-exts {:terms {}}]
    (if-not (grammar? schema)
      default-exts
      (reduce (fn [es [k v]] (assoc-in es [:terms k] (mkrecursive v es k)))
              default-exts
              (partition 2 (nnext schema))))))

(defn schema->start [schema]
  (if (grammar? schema)
    (second schema)
    schema))

