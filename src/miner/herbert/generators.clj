(ns miner.herbert.generators
  (:require [miner.herbert :as h]
            [miner.herbert.private :as internal]
            [miner.herbert.util :refer :all]
            [miner.herbert.predicates :as predicates]
            [miner.herbert.canonical :as hc]
            [four.stateful :as four]
            [re-rand :as re]
            [clojure.test.check :as sc]
            [clojure.test.check.properties :as prop]
            [clojure.math.combinatorics :as mc :exclude [update]]
            [clojure.walk :as w]
            [clojure.test.check.rose-tree :as rose]
            [clojure.test.check.generators :as gen]))



(declare mk-gen)


(defn quantified? [expr]
  (and (seq? expr)
       (case-of? (first expr) & * + ?)))

(defn gen-one-of [& gens]
  (gen/one-of gens))

;; I wanted to make sure that we get a few extreme values for int (Java long)
(def gen-int (gen/frequency [[10 gen/int] 
                             [1 (gen/return Long/MIN_VALUE)] 
                             [1 (gen/return Long/MAX_VALUE)]]))

(def gen-symbol (gen-one-of gen/symbol gen/symbol-ns))

(def gen-even (gen/fmap (fn [n] (if (even? n) n (unchecked-add n (if (neg? n) 1 -1)))) gen-int))

(def gen-odd (gen/fmap (fn [n] (if (odd? n) n (unchecked-add n (if (neg? n) 1 -1)))) gen-int))

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


(def gen-error (gen/return '<ERROR>))

(def gen-kw (gen/frequency [[4 gen/keyword]
                            [1 gen/keyword-ns]
                            [1 (gen/fmap keyword gen-symbol)]]))

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

(defn lookup [name extensions]
  ;;(println "SEM debug lookup" name extensions)
  (get (:lookup extensions) name name))

(defn mk-return [name extensions]
  (gen/return (lookup name extensions)))

(defn mk-symbol-gen [schema extensions]
  (or (get extensions schema)
      (get symbol-gens schema)
      (mk-return schema extensions)
      (throw (ex-info "Unknown schema" {:schema schema}))))

(defn mk-kvs [allow-empty? key-schema val-schema extensions]
  (let [kgen (if key-schema (mk-gen key-schema extensions) gen/any-printable)
        vgen (if val-schema (mk-gen val-schema extensions) gen/any-printable)
        kvgen (gen/map kgen vgen)]
    (if allow-empty?
      kvgen
      (gen/not-empty kvgen))))

(defn dequantify 
  ([expr] (if (and (seq? expr) (case-of? (first expr) * + ?)) (second expr) expr))
  ([quant expr] (if (and (seq? expr) (= (first expr) quant)) (second expr) expr)))

(defn opt-pair? [[k v]]
  (hc/first= k '?))


(defn opt-lit-keys [hm-schemas]
  (reduce (fn [opts sch] (if (and (hc/first= sch '?)
                                  (predicates/literal? (second sch))
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

(defn mk-literal-hash-map [schemas extensions]
  (let [opt-keys (opt-lit-keys schemas)
        dequantified (dequantify-keys schemas)
        tup-gen (apply gen/tuple (map #(mk-gen % extensions) dequantified))]
    (if (empty? opt-keys)
      (gen/fmap #(apply hash-map %) tup-gen)
      (let [opt-subsets (mc/subsets opt-keys)]
        (gen/bind (gen/elements opt-subsets)
                  (fn [qopts]
                    (gen/fmap #(apply dissoc (apply hash-map %) qopts)
                              tup-gen)))))))

(defn mk-map [schemas extensions]
  (condp == (count schemas)
    0 (gen/return {})
    2 (if (predicates/literal? (first schemas))
        (mk-literal-hash-map schemas extensions)
        (mk-kvs (not= (ffirst schemas) '+) (dequantify (first schemas))
                (dequantify (second schemas)) extensions))
    (mk-literal-hash-map schemas extensions)))


(defn mk-cat-cycle [schemas minimum extensions]
  (gen/bind (gen/sized #(gen/choose 0 %))
            (fn [num]
              (gen/fmap #(apply concat %)
                        (gen/vector (gen-tuple-seq (map #(mk-gen % extensions) schemas))
                                    (max minimum num))))))

;;; expect caller to concatenate or mapcat results from generators
(defn mk-cat-gen [schema extensions]
  (if (seq? schema)
    (if (= (count schema) 2)
      (case (first schema)
        *  (gen/list (mk-gen (second schema) extensions))
        +  (gen/not-empty (gen/list (mk-gen (second schema) extensions)))
        ?  (gen-one-of (gen/return '(::void)) (gen/fmap list (mk-gen (second schema) extensions)))
        &  (gen/fmap list (mk-gen (second schema) extensions))
        (gen/fmap list (mk-gen schema extensions)))
      ;; general case, possibly with multiple schemas in cycles
      (case (first schema)
        *  (mk-cat-cycle (rest schema) 0 extensions)
        +  (mk-cat-cycle (rest schema) 1 extensions)
        ?  (gen-one-of (gen/return '(::void))
                       (gen-tuple-seq (map #(mk-gen % extensions) (rest schema))))
        &  (gen-tuple-seq (map #(mk-gen % extensions) (rest schema)))
        (gen/fmap list (mk-gen schema extensions))))
    (gen/fmap list (mk-gen schema extensions))))


(defn mk-seq-with-quants [schemas extensions]
  (let [gens (map #(mk-cat-gen % extensions) schemas)]
    (gen/fmap #(remove #{::void} (apply concat %))
              (apply gen/tuple gens))))

(defn single-maybe-quantified? [schemas]
  ;; special case where it makes sense to look for quantifier in single schema
  (and (== (count schemas) 1)
       (seq? (first schemas))
       (== (count (first schemas)) 2)))

(defn mk-list [schemas extensions]
  (cond (single-maybe-quantified? schemas)
          (case (ffirst schemas)
            *  (gen/list (mk-gen (second (first schemas)) extensions))
            +  (gen/not-empty (gen/list (mk-gen (second (first schemas)) extensions)))
            ?  (gen-one-of (gen/return ())
                           (gen/fmap list (mk-gen (second (first schemas)) extensions)))
            &  (gen/fmap list (mk-gen (second (first schemas)) extensions))
            (gen/fmap list (mk-gen (first schemas) extensions)))
        (some quantified? schemas)
          (mk-seq-with-quants schemas extensions)
        :else
          (gen-tuple-seq (map #(mk-gen % extensions) schemas))))

(defn mk-vec [schemas extensions]
  (cond (single-maybe-quantified? schemas)
          (case (ffirst schemas)
            *  (gen/vector (mk-gen (second (first schemas)) extensions))
            +  (gen/not-empty (gen/vector (mk-gen (second (first schemas)) extensions)))
            ?  (gen-one-of (gen/return [])
                           (gen/fmap vector (mk-gen (second (first schemas)) extensions)))
            &  (gen/fmap vector (mk-gen (second (first schemas)) extensions))
            (gen/fmap vector (mk-gen (first schemas) extensions)))
        (some quantified? schemas)
          (gen/fmap vec (mk-seq-with-quants schemas extensions))
        :else
          (apply gen/tuple (map #(mk-gen % extensions) schemas))))

;; look for literal and gen from that and test with others
;; make hierachies of schema types and start with most specific
;; beware of expensive such-that with unlikely success, it will try forever
(defn mk-and [schemas extensions]
  (condp = (count schemas)
    0 (mk-gen 'any)
    1 (mk-gen (first schemas) extensions)
    (let [literals (filter predicates/literal? schemas)]
      (if (seq literals)
        (if (and (apply = literals)
                 (h/conforms? (cons 'and (remove #{(first literals)} schemas)) (first literals)))
          (mk-gen (first literals) extensions)
          (throw (ex-info "mk-and given inconsistent literals" {:schema (cons 'and schemas)})))
        (let [symbols (filter symbol? schemas)]
          (if (seq symbols)
            ;; SEM FIXME -- need to consider the "best" symbol for the such-that
            ;; some cases might be subsume others
            (gen/such-that (h/conform (cons 'and (remove #{(first symbols)} schemas)))
                           (mk-gen (first symbols) extensions)
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


;; look for literals, invert by taking type and such-that
;; break down hierarchies and have map of inversions, or closed-world types
(defn mk-not [schema extensions]
  (cond (predicates/literal? schema) (gen/such-that #(not= schema %) (mk-type-of-literal schema))
        (symbol? schema) (mk-gen (get symbol-complements schema) extensions)
        :else   (throw (ex-info "Unimplemented mk-not" {:schema schema}))))

;; maybe useful
(defn regex? [r]
 (instance? java.util.regex.Pattern r))

;; originally from fredericksgary
(defn gen-regex [regex]
  (gen/make-gen (fn [r _size]
                  (binding [four/*rand* r]
                    (rose/pure (re/re-rand regex))))))

(defn mk-str [regex extensions]
  ;; accepts regex or string (for EDN compatibility) or nil for any string (I like ascii)
  (cond (nil? regex) gen/string-ascii
        (string? regex) (gen-regex (re-pattern regex))
        :else (gen-regex regex)))


;; SEM FIXME: should we require the kw regex to start with a colon
;; for matching we do!    Not practical to cover all legal regex patterns.

(defn mk-kw [regex extensions]
  (let [decolonize (fn [s] (if (.startsWith ^String s ":") (subs s 1) s))
        kwize (comp keyword decolonize)]
    (cond (nil? regex) gen-kw
          (string? regex) (gen/fmap kwize (gen-regex (re-pattern regex)))
          :else (gen/fmap kwize (gen-regex regex)))))

(defn mk-sym [regex extensions]
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
  
(defn mk-in [coll extensions]
  (gen/bind (mk-gen coll extensions)
            (fn [c] (gen/elements (in-collection c)))))

(defn mk-list-gen [schema extensions]
  (let [sym (first schema)]
    (case sym
      quote (gen/return (second schema))
      int (apply mk-int (rest schema))
      float (apply mk-float (rest schema))
      num (gen/one-of [(apply mk-int (rest schema)) (apply mk-float (rest schema))])
      seq (gen/one-of [(mk-vec (rest schema) extensions)
                       (mk-list (rest schema) extensions)])
      vec (mk-vec (rest schema) extensions)
      list (mk-list (rest schema) extensions)
      ;; kvs is used internally within the generators
      kvs (mk-kvs (second schema) (third schema) (fourth schema) extensions)
      map (mk-map (rest schema) extensions)
      or (gen/one-of (map #(mk-gen % extensions) (rest schema)))
      not (mk-not (second schema) extensions)
      and (mk-and (rest schema) extensions)
      str (mk-str (second schema) extensions)
      kw (mk-kw (second schema) extensions)
      sym (mk-sym (second schema) extensions)
      in (mk-in (second schema) extensions)
      ;; SEM FIXME many more
      pos (apply mk-pos (rest schema))
      neg (apply mk-neg (rest schema))
      even (apply mk-even (rest schema))
      odd (apply mk-odd (rest schema))
      := (mk-return (second schema) extensions)
      )))

(defn mk-gen 
  ([schema] (mk-gen schema nil))
  ([schema extensions]
       (cond (symbol? schema) (mk-symbol-gen schema extensions)
             (predicates/literal? schema) (gen/return schema)
             (and (coll? schema) (empty? schema)) (gen/return schema)
             (seq? schema) (mk-list-gen schema extensions)
             :else (throw (ex-info "Unhandled schema" {:schema schema})))))



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
;; SEM FIXME -- need to pass extensions
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
    ;; (when bgen (println "SEM Debug bindins" bindins))
    (if bgen
      (gen/bind bgen
                (fn [lookup]
                  (mk-gen canonical {:lookup lookup})))
      (mk-gen canonical))))

(defn sample 
  ([schema] (sample schema 20))
  ([schema num] (gen/sample (generator schema) num)))

(defn gen-prop [pred generator]
  (prop/for-all* [generator] pred))

(defn property 
  ([pred schema] (prop/for-all* [(generator schema)] pred))
  ([pred schema1 schema2] (prop/for-all* [(generator schema1) (generator schema2)] pred))
  ([pred schema1 schema2 & more] 
     (prop/for-all* (list* (generator schema1) (generator schema2) (map generator more)) pred)))

(defn check
  ([pred schema] (check 100 pred schema))
  ([trials pred schema] (sc/quick-check trials (property pred schema))))
