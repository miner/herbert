(ns miner.herbert.generators
  (:require [miner.herbert :as h]
            [miner.herbert.canonical :as hc]
            ;; [simple-check.core :as sc]
            ;; [simple-check.properties :as prop]
            [clojure.walk :as w]
            [simple-check.generators :as gen]))

(declare mk-gen)

(def gen-symbol (gen/elements '[foo foo.bar/baz foo/bar]))

(def gen-float (gen/fmap float gen/ratio))

(def gen-even (gen/fmap (fn [n] (if (even? n) n (unchecked-add n 1))) gen/int))

(def gen-odd (gen/fmap (fn [n] (if (odd? n) n (unchecked-subtract n 1))) gen/int))

(def gen-seq (gen/one-of [(gen/list gen/any-printable) (gen/vector gen/any-printable)]))

;; Herbert float is Java double

(def gen-epsilon (gen/elements [0.0 (double Float/MIN_VALUE) 1.1E-10 1.5E-5]))

(def gen-float (gen/one-of [gen-epsilon
                            (gen/fmap - gen-epsilon)
                            (gen/elements [Double/MAX_VALUE (- Double/MAX_VALUE)
                                           Double/MIN_VALUE (- Double/MIN_VALUE)
                                           (double Float/MAX_VALUE) (- (double Float/MAX_VALUE))
                                           (double Float/MIN_VALUE) (- (double Float/MIN_VALUE))
                                           1.0 -1.0])]))

;; EDN doesn't have ratios or bignums
(def gen-num (gen/one-of [gen/int gen-float]))

(defn gen-tuple-seq
  "Like simple-check.generators/tuple but returns a seq, not a vector and takes a collection
of generators, not variadic"
  [generators]
  (if (empty? generators)
    (gen/return ())
    (gen/fmap seq (apply gen/tuple generators))))


(defn gen-tuple-seq2
  "Like simple-check.generators/tuple but returns a seq, not a vector and takes a collection
of generators, not variadic"
  [generators]
  (if (empty? generators)
    (gen/return ())
    (gen/fmap seq (apply gen/tuple generators))))


(def symbol-gens {'int gen/int 
                  'even gen-even
                  'odd gen-odd
                  'float gen-float
                  'num gen-num
                  'sym gen-symbol
                  'kw gen/keyword
                  'bool gen/boolean
                  'char gen/char
                  'str gen/string
                  'vec (gen/vector gen/any-printable)
                  'list (gen/list gen/any-printable)
                  'seq gen-seq
                  'map (gen/hash-map :a gen/any-printable :b gen/any-printable)
                  })

(defn mk-int 
  ([] gen/int)
  ([hi] (gen/choose 0 hi))
  ([lo hi] (gen/choose lo hi)))

;; Herbert float is Java double
(defn mk-float
  ([] gen-float)
  ([hi] (gen/one-of [gen-epsilon
                    (gen/fmap #(- hi %) gen-epsilon)]))
  ([lo hi] (gen/one-of [(gen/fmap #(+ lo %) gen-epsilon)
                        (gen/fmap #(- hi %) gen-epsilon)])))



(defn mk-symbol-gen [schema extensions]
  (or (get extensions schema) (get symbol-gens schema)))

(defn- third [lst]
  (first (nnext lst)))

(defn mk-kvs [key-schema val-schema extensions]
  (let [kgen (if key-schema (mk-gen key-schema extensions) gen/any-printable)
        vgen (if val-schema (mk-gen val-schema extensions) gen/any-printable)]
    (gen/map kgen vgen)))

;; assumes only canonical
(defn quantified-many? [expr]
  (and (seq? expr) (h/case-of? (first expr) * +)))

(defn dequantify 
  ([expr] (if (and (seq? expr) (h/case-of? (first expr) * + ?)) (second expr) expr))
  ([quant expr] (if (and (seq? expr) (= (first expr) quant)) (second expr) expr)))


;; UNFINISHED  -- question whether quantifier canonical form can have multiple elements like
;; (* x y) or must use (* (& x y))

#_
(defn mk-quantified-map [key-schema val-schema extensions]
  ;; key and val are assumed to be the same quantifier
  (case (first key-schema)
    * (gen/one-of (mk-kvs (dequantify key-schema) (dequantify val-schemas) extensions)
                  (gen/return {}))
    + (gen/one-of (mk-kvs (dequantify key-schema) (dequantify val-schemas) extensions))
    ? (gen/one-of                  (gen/return {}))))



;; SEM BUG -- dequant needs to distinguish * from +
;; * allows empty, + doesn't
(defn mk-map [schemas extensions]
  (condp == (count schemas)
    0 (gen/return {})
    2 (if (quantified-many? (first schemas))
        (mk-kvs (dequantify (first schemas)) (dequantify (second schemas)) extensions)
        (gen/fmap #(apply hash-map %) 
                  (gen/tuple (mk-gen (first schemas)) (mk-gen (second schemas)))))
    (gen/fmap #(apply hash-map %) (apply gen/tuple (map mk-gen schemas)))))

(defn mk-seq [schemas extensions]
  (gen-tuple-seq (map #(mk-gen % extensions) schemas)))

(defn mk-vec [schemas extensions]
  (apply gen/tuple (map #(mk-gen % extensions) schemas)))

;; look for literal and gen from that and test with others
;; make hierachies of schema types and start with most specific
;; beware of expensive such-that with unlikely success, it will try forever
(defn mk-and [schemas extensions]
  (throw (ex-info "Unimplemented mk-and" {:schema schemas})))

;; look for literals, invert by taking type and such-that
;; break down hierarchies and have map of inversions, or closed-world types
(defn mk-not [schema extensions]
  (throw (ex-info "Unimplemented mk-not" {:schema schema})))



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
      kvs (mk-kvs (second schema) (third schema) extensions)
      map (mk-map (rest schema) extensions)
      or (gen/one-of (map mk-gen (rest schema)))
      not (mk-not (second schema) extensions)
      and (mk-and (rest schema) extensions)
      ;; SEM FIXME many more
      )))

(defn mk-gen 
  ([schema] (mk-gen schema nil))
  ([schema extensions]
       (cond (symbol? schema) (mk-symbol-gen schema extensions)
             (h/literal? schema) (gen/return schema)
             (and (sequential? schema) (empty? schema)) (gen/return schema)
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
       (h/case-of? (first (second expr)) * +)))

;; SEM not used
(defn quantified-within-hash-map? [expr]
  (and (seq? expr)
       (= (first expr) 'map)
       (some #(h/case-of? (first %) * +) (rest expr))))

(defn quantified-within-seq? [expr]
  (and (seq? expr)
       (h/case-of? (first expr) seq vec list)
       (some quantified? (rest expr))))

(defn convert-empty [expr]
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
  (map convert-empty exprs))

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
        (quantified-within-functional-map? expr) (cons 'kvs (map dequantify (rest expr)))
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

(defn sample [schema]
  (gen/sample (generator schema) 20))

(comment
(hg/sample '{kw* int*})


;NullPointerException   simple-check.generators/gen-bind/fn--621 (generators.clj:155)

(defn rep [expr] (hg/replace-all-quantifiers (hc/rewrite expr)))
)

