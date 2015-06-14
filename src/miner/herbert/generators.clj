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

(defn lookup [context name]
  ;;(println "SEM debug lookup" name context)
  (get (:lookup context) name name))

(defn mk-return [context name]
  (gen/return (lookup context name)))

(defn mk-kvs [context allow-empty? key-schema val-schema]
  (let [kgen (if key-schema (mk-gen context key-schema) gen/any-printable)
        vgen (if val-schema (mk-gen context val-schema) gen/any-printable)
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

(defn mk-literal-hash-map [context schemas]
  (let [opt-keys (opt-lit-keys schemas)
        dequantified (dequantify-keys schemas)
        tup-gen (apply gen/tuple (map #(mk-gen context %) dequantified))]
    (if (empty? opt-keys)
      (gen/fmap #(apply hash-map %) tup-gen)
      (let [opt-subsets (mc/subsets opt-keys)]
        (gen/bind (gen/elements opt-subsets)
                  (fn [qopts]
                    (gen/fmap #(apply dissoc (apply hash-map %) qopts)
                              tup-gen)))))))

(defn mk-map [context schemas]
  (condp == (count schemas)
    0 (gen/return {})
    2 (if (literal? (first schemas))
        (mk-literal-hash-map context schemas)
        (mk-kvs context
                (not= (ffirst schemas) '+) (dequantify (first schemas))
                (dequantify (second schemas))))
    (mk-literal-hash-map context schemas)))


(defn mk-cat-cycle [context schemas minimum]
  (gen/bind (gen/sized #(gen/choose 0 %))
            (fn [num]
              (gen/fmap #(apply concat %)
                        (gen/vector (gen-tuple-seq (map #(mk-gen context %) schemas))
                                    (max minimum num))))))

;;; expect caller to concatenate or mapcat results from generators
(defn mk-cat-gen [context schema]
  (if (seq? schema)
    (if (= (count schema) 2)
      (case (first schema)
        *  (gen/list (mk-gen context (second schema)))
        +  (gen/not-empty (gen/list (mk-gen context (second schema))))
        ?  (gen-one-of (gen/return '(::void)) (gen/fmap list (mk-gen context (second schema))))
        &  (gen/fmap list (mk-gen context (second schema)))
        (gen/fmap list (mk-gen context schema)))
      ;; general case, possibly with multiple schemas in cycles
      (case (first schema)
        *  (mk-cat-cycle context (rest schema) 0)
        +  (mk-cat-cycle context (rest schema) 1)
        ?  (gen-one-of (gen/return '(::void))
                       (gen-tuple-seq (map #(mk-gen context %) (rest schema))))
        &  (gen-tuple-seq (map #(mk-gen context %) (rest schema)))
        (gen/fmap list (mk-gen context schema))))
    (gen/fmap list (mk-gen context schema))))


(defn mk-seq-with-quants [context schemas]
  (let [gens (map #(mk-cat-gen context %) schemas)]
    (gen/fmap #(remove #{::void} (apply concat %))
              (apply gen/tuple gens))))

(defn single-maybe-quantified? [schemas]
  ;; special case where it makes sense to look for quantifier in single schema
  (and (== (count schemas) 1)
       (seq? (first schemas))
       (== (count (first schemas)) 2)))

(defn mk-list [context schemas]
  (cond (single-maybe-quantified? schemas)
          (case (ffirst schemas)
            *  (gen/list (mk-gen context (second (first schemas))))
            +  (gen/not-empty (gen/list (mk-gen context (second (first schemas)))))
            ?  (gen-one-of (gen/return ())
                           (gen/fmap list (mk-gen context (second (first schemas)))))
            &  (gen/fmap list (mk-gen context (second (first schemas))))
            (gen/fmap list (mk-gen context (first schemas))))
        (some quantified-or-inline? schemas)
          (mk-seq-with-quants context schemas)
        :else
          (gen-tuple-seq (map #(mk-gen context %) schemas))))

(defn mk-vec [context schemas]
  (cond (single-maybe-quantified? schemas)
          (case (ffirst schemas)
            *  (gen/vector (mk-gen context (second (first schemas))))
            +  (gen/not-empty (gen/vector (mk-gen context (second (first schemas)))))
            ?  (gen-one-of (gen/return [])
                           (gen/fmap vector (mk-gen context (second (first schemas)))))
            &  (gen/fmap vector (mk-gen context (second (first schemas))))
            (gen/fmap vector (mk-gen context (first schemas))))
        (some quantified-or-inline? schemas)
          (gen/fmap vec (mk-seq-with-quants context schemas))
        :else
          (apply gen/tuple (map #(mk-gen context %) schemas))))

;; look for literal and gen from that and test with others
;; make hierachies of schema types and start with most specific
;; beware of expensive such-that with unlikely success, it will try forever
(defn mk-and [context schemas]
  (condp = (count schemas)
    0 (mk-gen 'any-printable)
    1 (mk-gen context (first schemas))
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
                           (mk-gen context (first symbols))
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




;; SEM FIXME:  need a multifn for make-generator-not

;; look for literals, invert by taking type and such-that
;; break down hierarchies and have map of inversions, or closed-world types
(defn mk-not [context schema]
  (cond (literal? schema) (gen/such-that #(not= schema %) (mk-type-of-literal schema))
        (symbol? schema) (mk-gen context (get symbol-complements schema))
        :else   (throw (ex-info "Unimplemented mk-not" {:schema schema}))))

;; maybe useful
(defn regex? [r]
 (instance? java.util.regex.Pattern r))

;; SEM FIXME -- don't need context for all of these!

(defn mk-str [context regex]
  ;; accepts regex or string (for EDN compatibility) or nil for any string (I like ascii)
  (cond (nil? regex) gen/string-ascii
        (string? regex) (gen-regex (re-pattern regex))
        :else (gen-regex regex)))


;; SEM FIXME: should we require the kw regex to start with a colon
;; for matching we do!    Not practical to cover all legal regex patterns.

(defn mk-kw [context regex]
  (let [decolonize (fn [s] (if (.startsWith ^String s ":") (subs s 1) s))
        kwize (comp keyword decolonize)]
    (cond (nil? regex) gen-kw
          (string? regex) (gen/fmap kwize (gen-regex (re-pattern regex)))
          :else (gen/fmap kwize (gen-regex regex)))))

(defn mk-sym [context regex]
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
  
(defn mk-in [context coll]
  (gen/bind (mk-gen context coll)
            (fn [c] (gen/elements (in-collection c)))))


(defn lookup-arg [context x]
  (if (symbol? x)
    (lookup context x)
    x))

(defn lookup-args [context args]
  (map #(lookup-arg context %) args))

;; new sig, better for variadic args to go last
;; BUT, could we just do the return a function trick that we did with predicate?
;; Maybe not with the operators AND, OR, etc. but for the simple types, which are all we
;; really need for extensibility.  It's a pain to always lookup-args in my methods

;; Might need a combinator one-of so you can combine them

(defmulti make-generator (fn [context sym & args] sym))

(defmethod make-generator 'quote [_ _ arg]
  (gen/return arg))

(defmethod make-generator 'int
  ([_ _] gen-int)
  ([context _ hi] (mk-int (lookup-arg context hi)))
  ([context _ lo hi] (mk-int (lookup-arg context lo) (lookup-arg context hi))))

(defmethod make-generator 'float
  ([_ _] gen-float)
  ([context _ hi] (mk-float (lookup-arg context hi)))
  ([context _ lo hi] (mk-float (lookup-arg context lo) (lookup-arg context hi))))

(defmethod make-generator 'num
  ([_ _] gen-num)
  ([context _ hi] (gen/one-of (let [high (lookup-arg context hi)]
                                [(mk-int high) (mk-float high)])))
  ([context _ lo hi] (gen/one-of (let [high (lookup-arg context hi)
                                       low (lookup-arg context lo)]
                                   [(mk-int low high) (mk-float low high)]))))  

(defmethod make-generator 'seq
  ([_ _] gen-seq)
  ([context _ & args] (gen/one-of [(mk-vec context args)
                                   (mk-list context args)])))

(defmethod make-generator 'vec 
  ([_ _] (gen/vector gen/any-printable))
  ([context _ & args] (mk-vec context args)))

(defmethod make-generator 'list
  ([_ _] (gen/list gen/any-printable))
  ([context _ & args] (mk-list context args)))
  
(defmethod make-generator 'kvs [context _ allow-empty? ks vs]
  ;; kvs is used internally within the generators
  (mk-kvs context allow-empty? ks vs))

;; SEM FIXME -- could do better with arity, change mk-map
(defmethod make-generator 'map
  ([_ _] (gen/map gen/keyword gen/any-printable))
  ([context _ & args] (mk-map context args)))
  
(defmethod make-generator 'or [context _ & args]
  (gen/one-of (map #(mk-gen context %) args)))

(defmethod make-generator 'and [context _ & args]
  (mk-and context args))

;; SEM FIXME: do you want to support a lookup for the regex? or just literal, as is currently?
(defmethod make-generator 'str
  ([_ _] gen/string-ascii)
  ([_ _ regex] (gen-regex (if (string? regex) (re-pattern regex) regex))))

(defmethod make-generator 'kw
  ([_ _] gen-kw)
  ([context _ regex] (mk-kw context regex)))

(defmethod make-generator 'sym
  ([_ _] gen-symbol)
  ([context _ regex] (mk-sym context regex)))

(defmethod make-generator 'in [context _ coll]
  (mk-in context coll))

(defmethod make-generator 'pos
  ([_ _] (gen-one-of gen/s-pos-int gen-pos-float))
  ([context _ hi] (mk-pos (lookup-arg context hi)))
  ([context _ lo hi] (mk-pos (lookup-arg context lo) (lookup-arg context hi))))

(defmethod make-generator 'neg
  ([_ _] (gen-one-of gen/s-neg-int (gen/fmap - gen-pos-float)))
  ([context _ hi] (mk-neg (lookup-arg context hi)))
  ([context _ lo hi] (mk-neg (lookup-arg context lo) (lookup-arg context hi))))


(defmethod make-generator 'even
  ([_ _] gen-even)
  ([context _ hi] (mk-even (lookup-arg context hi)))
  ([context _ lo hi] (mk-even (lookup-arg context lo) (lookup-arg context hi))))


(defmethod make-generator 'odd
  ([_ _] gen-odd)
  ([context _ hi] (mk-odd (lookup-arg context hi)))
  ([context _ lo hi] (mk-odd (lookup-arg context lo) (lookup-arg context hi))))

(defmethod make-generator := [context _ name _]
  ;; ignore the other args at this point, they will already be processed as bindings
  (mk-return context name))

(defmethod make-generator 'bool [_ _]
  gen/boolean)

(defmethod make-generator 'char [_ _]
  gen/char)

(defmethod make-generator 'any [_ _]
  gen/any-printable)


;; SEM FIXME: will need its own multifn for not-generators
(defmethod make-generator 'not [context _ term]
  (mk-not context term))

(defmethod make-generator :default [context sym]
  (mk-return context sym))

;; NEW using multi
(defn mk-gen
  ([schema] (mk-gen nil schema))
  ([context schema]
       (cond (symbol? schema) (make-generator context schema)
             (literal? schema) (gen/return schema)
             (and (coll? schema) (empty? schema)) (gen/return schema)
             (seq? schema) (if-let [args (next schema)]
                             (apply make-generator context (first schema) args)
                             (make-generator context (first schema)))
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
                  (mk-gen {:lookup lookup} canonical)))
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





