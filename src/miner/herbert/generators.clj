(ns miner.herbert.generators
  (:require [miner.herbert :as h]
            [miner.herbert.canonical :as hc]
            [four.stateful :as four]
            [re-rand :as re]
            [clojure.test.check :as sc]
            [clojure.test.check.properties :as prop]
            [clojure.math.combinatorics :as mc]
            [clojure.walk :as w]
            [clojure.test.check.rose-tree :as rose]
            [clojure.test.check.generators :as gen]))

(declare mk-gen)

(defn gen-one-of [& gens]
  (gen/one-of gens))

;; I wanted to make sure that we get a few extreme values for int (Java long)
(def gen-int (gen/frequency [[10 gen/int] 
                             [1 (gen/return Long/MIN_VALUE)] 
                             [1 (gen/return Long/MAX_VALUE)]]))

(def gen-symbol (gen/elements '[foo foo.bar/baz foo/bar foo-bar foo.bar/foo-bar x G__42]))

(def gen-even (gen/fmap (fn [n] (if (even? n) n (unchecked-add n (if (neg? n) 1 -1)))) gen-int))

(def gen-odd (gen/fmap (fn [n] (if (odd? n) n (unchecked-add n (if (neg? n) 1 -1)))) gen-int))

(def gen-seq (gen-one-of (gen/list gen/any-printable) (gen/vector gen/any-printable)))

;; Herbert float is Java double

(def gen-epsilon (gen/elements [0.0 (double Float/MIN_VALUE) 1.1E-10 1.5E-5]))

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
  "Like simple-check.generators/tuple but returns a seq, not a vector and takes a collection
of generators, not variadic"
  [generators]
  (if (empty? generators)
    (gen/return ())
    (gen/fmap seq (apply gen/tuple generators))))


(def gen-error (gen/return '<ERROR>))

(def gen-kw (gen/frequency [[4 gen/keyword]
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


(defn mk-symbol-gen [schema extensions]
  (or (get extensions schema) (get symbol-gens schema)
      (throw (ex-info "Unknown schema" {:schema schema}))))

(defn- third [lst]
  (first (nnext lst)))

(defn- fourth [lst]
  (first (next (nnext lst))))

(defn mk-kvs [allow-empty? key-schema val-schema extensions]
  (let [kgen (if key-schema (mk-gen key-schema extensions) gen/any-printable)
        vgen (if val-schema (mk-gen val-schema extensions) gen/any-printable)
        kvgen (gen/map kgen vgen)]
    (if allow-empty?
      kvgen
      (gen/such-that not-empty kvgen))))

;; assumes only canonical
(defn quantified-many? [expr]
  (and (seq? expr) (h/case-of? (first expr) * +)))

(defn dequantify 
  ([expr] (if (and (seq? expr) (h/case-of? (first expr) * + ?)) (second expr) expr))
  ([quant expr] (if (and (seq? expr) (= (first expr) quant)) (second expr) expr)))


(defn opt-pair? [[k v]]
  (hc/first= k '?))

(defn mk-opt-pair-seq [opts]
  ;; opts are [(* [(? kw) any])]
  ;; need to gen with opt or not in a flat seq [(* kw any)]
  ;; basically a combo of all ordered subsets
  (map #(apply concat %) (mc/subsets (map (fn [[quant val]] [(dequantify quant) val]) opts))))

(defn mk-literal-hash-map [schemas extensions]
  (let [pairs (partition 2 schemas)
        opts (filter opt-pair? pairs)
        reqs (remove opt-pair? pairs)]
    (gen/one-of
     (map (fn [qopts]
            (gen/fmap #(apply hash-map %) 
                      (apply gen/tuple (map mk-gen (concat qopts (mapcat identity reqs))))))
          (mk-opt-pair-seq opts)))))

(defn mk-map [schemas extensions]
  (condp == (count schemas)
    0 (gen/return {})
    2 (if (h/literal? (first schemas))
        (mk-literal-hash-map schemas extensions)
        (mk-kvs (not= (ffirst schemas) +) (dequantify (first schemas))
                (dequantify (second schemas)) extensions))
    (mk-literal-hash-map schemas extensions)))

(defn mk-seq [schemas extensions]
  (gen-tuple-seq (map #(mk-gen % extensions) schemas)))

(defn mk-vec [schemas extensions]
  (apply gen/tuple (map #(mk-gen % extensions) schemas)))

;; look for literal and gen from that and test with others
;; make hierachies of schema types and start with most specific
;; beware of expensive such-that with unlikely success, it will try forever
(defn mk-and [schemas extensions]
  (condp = (count schemas)
    0 (mk-gen 'any)
    1 (mk-gen (first schemas))
    (let [literals (filter h/literal? schemas)]
      (if (seq literals)
        (if (and (apply = literals)
                 (h/conforms? (cons 'and (remove #{(first literals)} schemas)) (first literals)))
          (mk-gen (first literals))
          (throw (ex-info "mk-and given inconsistent literals" {:schema (cons 'and schemas)})))
        (let [symbols (filter symbol? schemas)]
          (if (seq symbols)
            ;; SEM FIXME -- need to consider the "best" symbol for the such-that
            ;; some cases might be subsume others
            (gen/such-that (h/conform (cons 'and (remove #{(first symbols)} schemas)))
                           (mk-gen (first symbols))
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
  (cond (h/literal? schema) (gen/such-that #(not= schema %) (mk-type-of-literal schema))
        (symbol? schema) (mk-gen (get symbol-complements schema))
        :else   (throw (ex-info "Unimplemented mk-not" {:schema schema}))))

;; maybe useful
(defn regex? [r]
 (instance? java.util.regex.Pattern r))

;; originally from fredericksgary
(defn gen-regex [regex]
  {:gen (fn [r _size]
          (binding [four/*rand* r]
            (rose/pure (re/re-rand regex))))})

(defn mk-str [regex extensions]
  ;; accepts regex or string (for EDN compatibility) or nil for any string (I like ascii)
  (cond (nil? regex) gen/string-ascii
        (string? regex) (gen-regex (re-pattern regex))
        :else (gen-regex regex)))



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


(defn mk-list-gen [schema extensions]
  (let [sym (first schema)]
    (case sym
      quote (gen/return (second schema))
      int (apply mk-int (rest schema))
      float (apply mk-float (rest schema))
      num (gen/one-of [(apply mk-int (rest schema)) (apply mk-float (rest schema))])
      seq (gen/one-of [(mk-vec (rest schema) extensions)
                       (mk-seq (rest schema) extensions)])
      vec (mk-vec (rest schema) extensions)
      list (mk-seq (rest schema) extensions)
      ;; kvs is used internally within the generators
      kvs (mk-kvs (second schema) (third schema) (fourth schema) extensions)
      map (mk-map (rest schema) extensions)
      or (gen/one-of (map mk-gen (rest schema)))
      not (mk-not (second schema) extensions)
      and (mk-and (rest schema) extensions)
      str (mk-str (second schema) extensions)
      kw (mk-kw (second schema) extensions)
      sym (mk-sym (second schema) extensions)
      ;; SEM FIXME many more
      )))

(defn mk-gen 
  ([schema] (mk-gen schema nil))
  ([schema extensions]
       (cond (symbol? schema) (mk-symbol-gen schema extensions)
             (h/literal? schema) (gen/return schema)
             (and (coll? schema) (empty? schema)) (gen/return schema)
             (seq? schema) (mk-list-gen schema extensions)
             :else (throw (ex-info "Unhandled schema" {:schema schema})))))



(declare replace-quantifiers)

;; SEM FIXME -- none of this is properly tested
;; Did the replacement of quantifiers, but not yet the expansion of OR terms


(declare replace-quantifiers)

(defn quantified? [expr]
  (and (seq? expr)
       (h/case-of? (first expr) & * + ?)))

(defn quantified-within-functional-map? [expr]
  (and (seq? expr)
       (= (first expr) 'map)
       (== (count expr) 3)
       (seq? (second expr))
       (h/case-of? (first (second expr)) * +)))

(defn quantified-within-seq? [expr]
  (and (seq? expr)
       (h/case-of? (first expr) seq vec list)
       (some quantified? (rest expr))))

;; SEM: are you sure it's not a vector? YES, because vseq already ran
(defn convert-singleton [expr]
  (if (and (seq? expr) (== (count expr) 1))
    (case (first expr)
      seq '(or [] ())
      list ()
      vec []
      map {}
      (first expr))
    expr))

(defn quant-replacements [vs expr]
  (case (first expr)
    & (map #(reduce conj % (rest expr)) vs)
    * (concat (map #(reduce conj % (concat (rest expr) (rest expr))) vs)
              (map #(reduce conj % (rest expr)) vs)
              vs)
    + (concat (map #(reduce conj % (concat (rest expr) (rest expr))) vs)
              (map #(reduce conj % (rest expr)) vs))
    ? (concat (map #(reduce conj % (rest expr)) vs)
               vs)
    (map #(conj % expr) vs)))

(defn vseq [xs]
  (if (empty? xs)
    ;; preserve the empty coll
    xs
    (seq xs)))

(defn patch-up-singleton [exprs]
  ;; SEM FIXME -- we know it can only occur in the last element so we don't have to map
  ;; across all of them
  (map convert-singleton exprs))

(defn quantifier-replacements [seqex]
  (patch-up-singleton
  (map vseq
       (reduce (fn [vs expr]
                   (cond (or (symbol? expr) (h/literal? expr)
                             (and (seq? expr) (hc/first= expr 'quote))) (map #(conj % expr) vs)
                         (seq? expr) (quant-replacements vs expr)
                         :else (throw (ex-info "Unexpected element in seqex" {:seqex seqex}))))
               (list [])
               seqex))))

;; expr is canonical
(defn step-replace-quantifiers [expr]
  (cond (or (symbol? expr) (h/literal? expr)) expr
        (quantified-within-functional-map? expr) 
          (list* 'kvs (not= (ffirst (rest expr)) '+) (map dequantify (rest expr)))
        (quantified-within-seq? expr) (cons 'or (quantifier-replacements expr))
        :else expr))

(defn replace-all-quantifiers [expr]
  (if (quantified? expr) 
    (if (== (count expr) 2)
      (recur (second expr))
      (throw (ex-info "Unsupported quantified schema at top level" {:schema expr})))
    (w/postwalk step-replace-quantifiers expr)))

(defn generator [schema]
  (let [canonical (hc/rewrite schema)
        dequantified (replace-all-quantifiers canonical)]
    (mk-gen dequantified nil)))

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

