(ns miner.herbert.generators
  (:require [miner.herbert :as h]
            [miner.herbert.util :refer :all]
            [miner.herbert.canonical :as hc]
            [miner.herbert.regex :as hr]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [clojure.math.combinatorics :as mc :exclude [update]]
            [clojure.test.check.generators :as gen]))


;; gen-* functions return a t.c generator, general purpose, no knowledge of schemas
;; mk-* functions  return a t.c generator, use schemas or designed for schema parameters

;; SEM FIXME: want to turn "context" into "context"
;; make context first arg, allow variadics where natural (typically & schemas)


(defn gen-regex [regex]
  (if h/*string-from-regex-generator*
    (h/*string-from-regex-generator* regex)
    (hr/string-generator regex)))


(declare mk-gen)

;; like util/quantified? but also looks for &
(defn quantified-or-inline? [expr]
  (and (seq? expr)
       (case-of? (first expr) & * + ?)))

(defn gen-one-of [& gens]
  (gen/one-of gens))

;; I wanted to make sure that we get a few extreme values for int (Java long)
(def gen-int (gen/frequency [[10 gen/int] 
                             [1 (gen/return Long/MIN_VALUE)] 
                             [1 (gen/return Long/MAX_VALUE)]]))

(def gen-symbol (gen-one-of gen/symbol gen/symbol-ns))

(defn- safe-1 [n]
  ;; avoid overflow by +1 or -1 toward zero, away from extremes
  (unchecked-add n (if (neg? n) 1 -1)))

(def gen-even (gen/fmap (fn [n] (if (even? n) n (safe-1 n))) gen-int))

(def gen-odd (gen/fmap (fn [n] (if (odd? n) n (safe-1 n))) gen-int))

(def gen-seq (gen-one-of (gen/list gen/any-printable) (gen/vector gen/any-printable)))

;; Herbert float is Java double

(def gen-epsilon (gen/elements [0.0 (double Float/MIN_VALUE) 1.1E-10 1.5E-5]))

;; SEM see also
;; http://dev.clojure.org/jira/browse/TCHECK-5

(def gen-float (gen-one-of gen-epsilon
                            (gen/fmap - gen-epsilon)
                            (gen/fmap float gen/ratio)
                            (gen/elements [Double/MAX_VALUE (- Double/MAX_VALUE)
                                           Double/MIN_VALUE (- Double/MIN_VALUE)
                                           (double Float/MAX_VALUE) (- (double Float/MAX_VALUE))
                                           ;; Float/MIN_VALUE is in gen-epsilon
                                           1.0 -1.0 2.0 -2.0])))

(def gen-pos-float (gen/elements [1.0
                                  2.0
                                  1.1E-10
                                  1.5E-5
                                  Double/MAX_VALUE 
                                  Double/MIN_VALUE 
                                  (double Float/MAX_VALUE)
                                  (double Float/MIN_VALUE)]))

;; EDN doesn't have ratios or bignums
(def gen-num (gen-one-of gen-int gen-float))

(defn gen-tuple-seq
  "Like test.check.generators/tuple but returns a seq, not a vector and takes a collection
of generators, not variadic"
  [generators]
  (if (empty? generators)
    (gen/return ())
    (gen/fmap seq (apply gen/tuple generators))))


;; SEM: hack!
(def gen-error (gen/return '<ERROR>))

(def gen-kw (gen/frequency [[4 gen/keyword]
                            [1 gen/keyword-ns]
                            [1 (gen/fmap keyword gen-symbol)]]))

#_
(def symbol-gens {'int gen-int 
                  'even gen-even
                  'odd gen-odd
                  'pos (gen-one-of gen/s-pos-int gen-pos-float)
                  'neg (gen-one-of gen/s-neg-int (gen/fmap - gen-pos-float))
                  'float gen-float
                  'num gen-num
                  'sym gen-symbol
                  'kw gen-kw
                  'bool gen/boolean
                  'char gen/char
                  'str gen/string
                  'vec (gen/vector gen/any-printable)
                  'list (gen/list gen/any-printable)
                  'seq gen-seq
                  'map (gen/map gen/keyword gen/any-printable)
                  'any gen/any-printable
                  'and gen/any-printable  ;; degenerate AND
                  'or gen-error  ;; degenerate OR
                  'not gen-error ;; degenerat NOT
                  })

;; sufficient complements for testing, not logically complete
(def symbol-complements '{even odd
                          odd even
                          neg (or pos 0 0.0)
                          pos (or neg 0 0.0)
                          float int
                          int float
                          vec (or list sym kw int)
                          seq (or sym kw int)
                          map (or vec kw sym int)
                          sym (or vec kw int)
                          or any
                          str (or kw int sym)
                          kw (or int sym vec)
                          any or
                          char (or str int sym)
                          bool (or str kw int sym)
                          and or
                          num (or str sym kw)
                          list (or vec sym kw int)})

;; int is a Java long
;; a bit of extra bias for the extreme values helps test edge cases
(defn mk-int 
  ([] gen-int)
  ([hi] (gen-one-of (gen/choose 0 hi) (gen/return 0) (gen/return hi)))
  ([lo hi] (gen-one-of (gen/choose lo hi) (gen/return lo) (gen/return hi))))

;; Herbert float is Java double
(defn mk-float
  ([] gen-float)
  ([hi] (gen-one-of gen-epsilon (gen/fmap #(- hi %) gen-epsilon)))
  ([lo hi] (gen-one-of (gen/fmap #(+ lo %) gen-epsilon)
                       (gen/fmap #(- hi %) gen-epsilon))))

(defn mk-pos
  ([] (gen-one-of (mk-int 1 Long/MAX_VALUE) (mk-float Double/MIN_VALUE Double/MAX_VALUE)))
  ([hi] (gen-one-of (mk-int 1 hi) (mk-float Double/MIN_VALUE hi)))
  ([lo hi] (gen-one-of (mk-int lo hi) (mk-float lo hi)))) 

(defn mk-neg
  ([] (gen-one-of (mk-int Long/MIN_VALUE -1) (mk-float (- Double/MAX_VALUE) (- Double/MIN_VALUE))))
  ([lo] (gen-one-of (mk-int lo -1) (mk-float lo (- Double/MIN_VALUE))))
  ([lo hi] (gen-one-of (mk-int lo hi) (mk-float lo hi))))

;;; Long/MAX_VALUE is odd, Long/MIN_VALUE is even
(defn mk-even
  ([] (mk-even Long/MIN_VALUE Long/MAX_VALUE))
  ([hi] (mk-even 0 hi))
  ([lo hi] (gen/fmap #(if (even? %) % (dec %)) (mk-int (inc lo) hi))))

(defn mk-odd
  ([] (mk-odd Long/MIN_VALUE Long/MAX_VALUE))
  ([hi] (mk-odd 0 hi))
  ([lo hi] (gen/fmap #(if (odd? %) % (dec %)) (mk-int (inc lo) hi))))

(defn lookup [name context]
  ;;(println "SEM debug lookup" name context)
  (get (:lookup context) name name))

(defn mk-return [name context]
  (gen/return (lookup name context)))

#_
(defn mk-symbol-gen [schema context]
  (or (get context schema)
      (get symbol-gens schema)
      (mk-return schema context)
      (throw (ex-info "Unknown schema" {:schema schema}))))

(defn mk-kvs [allow-empty? key-schema val-schema context]
  (let [kgen (if key-schema (mk-gen key-schema context) gen/any-printable)
        vgen (if val-schema (mk-gen val-schema context) gen/any-printable)
        kvgen (gen/map kgen vgen)]
    (if allow-empty?
      kvgen
      (gen/not-empty kvgen))))

(defn dequantify 
  ([expr] (if (and (seq? expr) (case-of? (first expr) * + ?)) (second expr) expr))
  ([quant expr] (if (and (seq? expr) (= (first expr) quant)) (second expr) expr)))

(defn opt-pair? [[k v]]
  (first= k '?))

(defn opt-lit-keys [hm-schemas]
  (reduce (fn [opts sch] (if (and (first= sch '?)
                                  (literal? (second sch))
                                  (== (count sch) 2))
                           (conj opts (second sch))
                           opts))
          []
          ;; only consider the key schemas
          (take-nth 2 hm-schemas)))

(defn dequantify-keys [hm-schemas]
  (reduce (fn [res [k v]] (conj res (dequantify k) v))
          []
          (partition 2 hm-schemas)))

;; SEM FIXME -- try to use gen/hash-map (but needs literal keys)
;; SEM FIXME -- t.c 0.6 will have `shuffle` which might be a good replacement for mc/subsets

(defn mk-literal-hash-map [schemas context]
  (let [opt-keys (opt-lit-keys schemas)
        dequantified (dequantify-keys schemas)
        tup-gen (apply gen/tuple (map #(mk-gen % context) dequantified))]
    (if (empty? opt-keys)
      (gen/fmap #(apply hash-map %) tup-gen)
      (let [opt-subsets (mc/subsets opt-keys)]
        (gen/bind (gen/elements opt-subsets)
                  (fn [qopts]
                    (gen/fmap #(apply dissoc (apply hash-map %) qopts)
                              tup-gen)))))))

(defn mk-map [schemas context]
  (condp == (count schemas)
    0 (gen/return {})
    2 (if (literal? (first schemas))
        (mk-literal-hash-map schemas context)
        (mk-kvs (not= (ffirst schemas) '+) (dequantify (first schemas))
                (dequantify (second schemas)) context))
    (mk-literal-hash-map schemas context)))


(defn mk-cat-cycle [schemas minimum context]
  (gen/bind (gen/sized #(gen/choose 0 %))
            (fn [num]
              (gen/fmap #(apply concat %)
                        (gen/vector (gen-tuple-seq (map #(mk-gen % context) schemas))
                                    (max minimum num))))))

;;; expect caller to concatenate or mapcat results from generators
(defn mk-cat-gen [schema context]
  (if (seq? schema)
    (if (= (count schema) 2)
      (case (first schema)
        *  (gen/list (mk-gen (second schema) context))
        +  (gen/not-empty (gen/list (mk-gen (second schema) context)))
        ?  (gen-one-of (gen/return '(::void)) (gen/fmap list (mk-gen (second schema) context)))
        &  (gen/fmap list (mk-gen (second schema) context))
        (gen/fmap list (mk-gen schema context)))
      ;; general case, possibly with multiple schemas in cycles
      (case (first schema)
        *  (mk-cat-cycle (rest schema) 0 context)
        +  (mk-cat-cycle (rest schema) 1 context)
        ?  (gen-one-of (gen/return '(::void))
                       (gen-tuple-seq (map #(mk-gen % context) (rest schema))))
        &  (gen-tuple-seq (map #(mk-gen % context) (rest schema)))
        (gen/fmap list (mk-gen schema context))))
    (gen/fmap list (mk-gen schema context))))


(defn mk-seq-with-quants [schemas context]
  (let [gens (map #(mk-cat-gen % context) schemas)]
    (gen/fmap #(remove #{::void} (apply concat %))
              (apply gen/tuple gens))))

(defn single-maybe-quantified? [schemas]
  ;; special case where it makes sense to look for quantifier in single schema
  (and (== (count schemas) 1)
       (seq? (first schemas))
       (== (count (first schemas)) 2)))

(defn mk-list [schemas context]
  (cond (single-maybe-quantified? schemas)
          (case (ffirst schemas)
            *  (gen/list (mk-gen (second (first schemas)) context))
            +  (gen/not-empty (gen/list (mk-gen (second (first schemas)) context)))
            ?  (gen-one-of (gen/return ())
                           (gen/fmap list (mk-gen (second (first schemas)) context)))
            &  (gen/fmap list (mk-gen (second (first schemas)) context))
            (gen/fmap list (mk-gen (first schemas) context)))
        (some quantified-or-inline? schemas)
          (mk-seq-with-quants schemas context)
        :else
          (gen-tuple-seq (map #(mk-gen % context) schemas))))

(defn mk-vec [schemas context]
  (cond (single-maybe-quantified? schemas)
          (case (ffirst schemas)
            *  (gen/vector (mk-gen (second (first schemas)) context))
            +  (gen/not-empty (gen/vector (mk-gen (second (first schemas)) context)))
            ?  (gen-one-of (gen/return [])
                           (gen/fmap vector (mk-gen (second (first schemas)) context)))
            &  (gen/fmap vector (mk-gen (second (first schemas)) context))
            (gen/fmap vector (mk-gen (first schemas) context)))
        (some quantified-or-inline? schemas)
          (gen/fmap vec (mk-seq-with-quants schemas context))
        :else
          (apply gen/tuple (map #(mk-gen % context) schemas))))

;; look for literal and gen from that and test with others
;; make hierachies of schema types and start with most specific
;; beware of expensive such-that with unlikely success, it will try forever
(defn mk-and [schemas context]
  (condp = (count schemas)
    0 (mk-gen 'any)
    1 (mk-gen (first schemas) context)
    (let [literals (filter literal? schemas)]
      (if (seq literals)
        (if (and (apply = literals)
                 (h/conforms? (cons 'and (remove #{(first literals)} schemas)) (first literals)))
          (mk-gen (first literals) context)
          (throw (ex-info "mk-and given inconsistent literals" {:schema (cons 'and schemas)})))
        (let [symbols (filter symbol? schemas)]
          (if (seq symbols)
            ;; SEM FIXME -- need to consider the "best" symbol for the such-that
            ;; some cases might be subsume others
            (gen/such-that (h/conform (cons 'and (remove #{(first symbols)} schemas)))
                           (mk-gen (first symbols) context)
                           100)
            (throw (ex-info "Unimplemented mk-and" {:schema (cons 'and schemas)}))))))))

(defn mk-type-of-literal [lit]
  (cond (string? lit) (mk-gen 'str)
        (integer? lit) (mk-gen 'int)
        (float? lit) (mk-gen 'float)
        (keyword? lit) (mk-gen 'kw)
        (symbol? lit) (mk-gen 'sym)
        (vector? lit) (mk-gen 'vec)
        (list? lit) (mk-gen 'list)
        (seq? lit) (mk-gen 'seq)
        (char? lit) (mk-gen 'char)
        (true? lit) (mk-gen 'bool)
        (false? lit) (mk-gen 'bool)
        (map? lit) (mk-gen 'map)
        (set? lit) (mk-gen 'set)
        (nil? lit) (mk-gen nil)
          :else (throw (ex-info "Unimplemented mk-type-of-literal" {:schema lit}))))


;; sufficient complements for testing, not logically complete
    
#_ (defn complementable? [schema]
  (symbol? schema))

#_ (defn complemental-schema [schema]
  (get symbol-complements schema))


;; SEM FIXME:  need a multifn for make-generator-not

;; look for literals, invert by taking type and such-that
;; break down hierarchies and have map of inversions, or closed-world types
(defn mk-not [schema context]
  (cond (literal? schema) (gen/such-that #(not= schema %) (mk-type-of-literal schema))
        (symbol? schema) (mk-gen (get symbol-complements schema) context)
        :else   (throw (ex-info "Unimplemented mk-not" {:schema schema}))))

;; maybe useful
(defn regex? [r]
 (instance? java.util.regex.Pattern r))

(defn mk-str [regex context]
  ;; accepts regex or string (for EDN compatibility) or nil for any string (I like ascii)
  (cond (nil? regex) gen/string-ascii
        (string? regex) (gen-regex (re-pattern regex))
        :else (gen-regex regex)))


;; SEM FIXME: should we require the kw regex to start with a colon
;; for matching we do!    Not practical to cover all legal regex patterns.

(defn mk-kw [regex context]
  (let [decolonize (fn [s] (if (.startsWith ^String s ":") (subs s 1) s))
        kwize (comp keyword decolonize)]
    (cond (nil? regex) gen-kw
          (string? regex) (gen/fmap kwize (gen-regex (re-pattern regex)))
          :else (gen/fmap kwize (gen-regex regex)))))

(defn mk-sym [regex context]
    (cond (nil? regex) gen-symbol
          (string? regex) (gen/fmap symbol (gen-regex (re-pattern regex)))
          :else (gen/fmap symbol (gen-regex regex))))

;; SEM FIXME ::void is wrong, but maybe good for debugging
;;   really should throw
;;   non-coll handling is questionable, maybe should throw
(defn in-collection [coll]
  (cond (not (coll? coll)) (list coll)
        (empty? coll) (list ::void)
        (map? coll) (keys coll)
        :else (seq coll)))
  
(defn mk-in [coll context]
  (gen/bind (mk-gen coll context)
            (fn [c] (gen/elements (in-collection c)))))


(defn lookup-arg [x context]
  (if (symbol? x)
    (lookup x context)
    x))

(defn lookup-args [args context]
  (map #(lookup-arg % context) args))

#_
(defn mk-list-gen [schema context]
  (let [sym (first schema)]
    (case sym
      quote (gen/return (second schema))
      int (apply mk-int (lookup-args (rest schema) context))
      float (apply mk-float (lookup-args (rest schema) context))
      num (gen/one-of [(apply mk-int (lookup-args (rest schema) context))
                       (apply mk-float (lookup-args (rest schema) context))])
      seq (gen/one-of [(mk-vec (rest schema) context)
                       (mk-list (rest schema) context)])
      vec (mk-vec (rest schema) context)
      list (mk-list (rest schema) context)
      ;; kvs is used internally within the generators
      kvs (mk-kvs (second schema) (third schema) (fourth schema) context)
      map (mk-map (rest schema) context)
      or (gen/one-of (map #(mk-gen % context) (rest schema)))
      not (mk-not (second schema) context)
      and (mk-and (rest schema) context)
      str (mk-str (second schema) context)
      kw (mk-kw (second schema) context)
      sym (mk-sym (second schema) context)
      in (mk-in (second schema) context)
      ;; SEM FIXME many more
      pos (apply mk-pos (lookup-args (rest schema) context))
      neg (apply mk-neg (lookup-args (rest schema) context))
      even (apply mk-even (lookup-args (rest schema) context))
      odd (apply mk-odd (lookup-args (rest schema) context))
      := (mk-return (second schema) context)
      )))

#_
(defn mk-gen-ORIG 
  ([schema] (mk-gen schema nil))
  ([schema context]
       (cond (symbol? schema) (mk-symbol-gen schema context)
             (literal? schema) (gen/return schema)
             (and (coll? schema) (empty? schema)) (gen/return schema)
             (seq? schema) (mk-list-gen schema context)
             :else (throw (ex-info "Unhandled schema" {:schema schema})))))

;; new sig, better for variadic args to go last
;; BUT, could we just do the return a function trick that we did with predicate?
;; Maybe not with the operators AND, OR, etc. but for the simple types, which are all we
;; really need for extensibility.  It's a pain to always lookup-args in my methods

;; Might need a combinator one-of so you can combine them

#_ 
(defmulti make-generator (fn [context sym & args] sym))

(defmulti make-generator (fn [sym args context] sym))

(defmethod make-generator 'quote [_ args _]
  {:pre [(= (count args) 1)]}
  (gen/return (first args)))

;; SEM FIXME consider context as first arg
;; consider multi-arg case statements to avoid apply

(defmethod make-generator 'int [_ args context]
  (if (seq args)
    (apply mk-int (lookup-args args context))
    gen-int))

(defmethod make-generator 'float [_ args context]
  (if (seq args)
    (apply mk-float (lookup-args args context))
    gen-float))

(defmethod make-generator 'num [_ args context]
  (if (seq args)
    (let [largs (lookup-args args context)]
      (gen/one-of [(apply mk-int largs)
                   (apply mk-float largs)]))
    gen-num))


(defmethod make-generator 'seq [_ args context]
  (if (seq args)
    (gen/one-of [(mk-vec args context)
                 (mk-list args context)])
    gen-seq))

(defmethod make-generator 'vec [_ args context]
  (if (seq args)
    (mk-vec args context)
    (gen/vector gen/any-printable)))

(defmethod make-generator 'list [_ args context]
  (if (seq args)
    (mk-list args context)
    (gen/list gen/any-printable)))
  
(defmethod make-generator 'kvs [_ args context]
  ;; kvs is used internally within the generators
  (mk-kvs (first args) (second args) (third args) context))
  
(defmethod make-generator 'map [_ args context]
  (if (seq args)
    (mk-map args context)
    (gen/map gen/keyword gen/any-printable)))
  
(defmethod make-generator 'or [_ args context]
  (if (seq args)
    (gen/one-of (map #(mk-gen % context) args))
    gen-error))
  
(defmethod make-generator 'and [_ args context]
  (if (seq args)
    (mk-and args context)
    gen/any-printable))

(defmethod make-generator 'str [_ args context]
  (case (count args)
    0 gen/string
    1 (mk-str (first args) context)))
    

(defmethod make-generator 'kw [_ args context]
  (case (count args)
    0 gen-kw
    1 (mk-kw (first args) context)))

(defmethod make-generator 'sym [_ args context]
  (case (count args)
    0 gen-symbol
    1 (mk-sym (first args) context)))

(defmethod make-generator 'in [_ args context]
  {:pre [(= (count args) 1)]}              
  (mk-in (first args) context))
  

(defmethod make-generator 'pos [_ args context]
  (case (count args)
    0 (gen-one-of gen/s-pos-int gen-pos-float)
    1 (mk-pos (lookup-arg (first args) context))
    2 (mk-pos (lookup-arg (first args) context) (lookup-arg (second args) context))))

(defmethod make-generator 'neg [_ args context]
  (case (count args)
    0 (gen-one-of gen/s-neg-int (gen/fmap - gen-pos-float))
    1 (mk-neg (lookup-arg (first args) context))
    2 (mk-neg (lookup-arg (first args) context) (lookup-arg (second args) context))))


(defmethod make-generator 'even [_ args context]
  (case (count args)
    0 gen-even
    1 (mk-even (lookup-arg (first args) context))
    2 (mk-even (lookup-arg (first args) context) (lookup-arg (second args) context))))


(defmethod make-generator 'odd [_ args context]
  (case (count args)
    0 gen-odd
    1 (mk-odd (lookup-arg (first args) context))
    2 (mk-odd (lookup-arg (first args) context) (lookup-arg (second args) context))))

(defmethod make-generator := [_ args context]
  {:pre [(seq args)]}
  ;; ignore the other args at this point, they will already be processed as bindings
  (mk-return (first args) context))

(defmethod make-generator 'bool [_ args context]
  {:pre [(empty? args)]}
  gen/boolean)

(defmethod make-generator 'char [_ args context]
  {:pre [(empty? args)]}
  gen/char)

(defmethod make-generator 'any [_ args context]
  {:pre [(empty? args)]}
  gen/any-printable)


;; SEM FIXME: will need its own multifn for not-generators
(defmethod make-generator 'not [_ args context]
  {:pre [(= (count args) 1)]}
  (mk-not (first args) context))

(defmethod make-generator :default [sym args context]
  {:pre [(empty? args)]}
  (mk-return sym context))

;; NEW using multi
(defn mk-gen
  ([schema] (mk-gen schema nil))
  ([schema context]
       (cond (symbol? schema) (make-generator schema () context)
             (literal? schema) (gen/return schema)
             (and (coll? schema) (empty? schema)) (gen/return schema)
             (seq? schema) (make-generator (first schema) (rest schema) context)
             :else (throw (ex-info "Unhandled schema" {:schema schema})))))





;; SEM FIXME: OOPS, forgot to consider this!

;; your mgen could lookup-args on the args if it wants to
;; important point: context always first arg (allows lookups)






;; SEM FIXME -- none of this is properly tested
;; Did the replacement of quantifiers, but not yet the expansion of OR terms


;; O(n) but not bad
(defn mapcatv [f & colls]
  (reduce into [] (apply map f colls)))

;; inspired by tree-seq
(defn assignments [root]
  (let [walk (fn walk [node]
               (cond (and (seq? node) (= (first node) :=))
                       (conj (mapcatv walk (seq node)) node)
                     (coll? node)
                       (mapcatv walk (seq node))
                     :else nil))]
    (walk root)))


;; SEM FIXME -- need scope (levels), not just once at the top
;; SEM FIXME -- need to pass context
(defn binding-gen [bins]
  (when (seq bins)
    (let [bnames (map second bins)
          bexprs (map third bins)]
      (gen/fmap #(zipmap bnames %)
                (apply gen/tuple (map mk-gen bexprs))))))

(defn generator [schema]
  (let [canonical (hc/rewrite schema)
        bindins (assignments canonical)
        bgen (binding-gen bindins)]
    ;;  (when bgen (println "SEM Debug bindins" bindins))
    (if bgen
      (gen/bind bgen
                (fn [lookup]
                  (mk-gen canonical {:lookup lookup})))
      (mk-gen canonical))))

(defn sample 
  ([schema] (sample schema 20))
  ([schema num] (gen/sample (generator schema) num)))

(defn property 
  ([pred schema] (prop/for-all* [(generator schema)] pred))
  ([pred schema1 schema2] (prop/for-all* [(generator schema1) (generator schema2)] pred))
  ([pred schema1 schema2 & more] 
     (prop/for-all* (list* (generator schema1) (generator schema2) (map generator more)) pred)))

(defn check
  ([pred schema] (check 100 pred schema))
  ([trials pred schema] (tc/quick-check trials (property pred schema))))





