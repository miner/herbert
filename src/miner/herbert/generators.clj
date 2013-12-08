(ns miner.herbert.generators
  (:require [miner.herbert :as h]
            [miner.herbert.canonical :as hc]
            ;; [simple-check.core :as sc]
            ;; [simple-check.properties :as prop]
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
    (gen/elements [()])
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

(defn mk-keys [key-schema val-schema extensions]
  (let [kgen (if key-schema (mk-gen key-schema extensions) gen/any-printable)
        vgen (if val-schema (mk-gen val-schema extensions) gen/any-printable)]
    (gen/map kgen vgen)))

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
      keys (mk-keys (second schema) (third schema) extensions)
      map (gen/fmap #(apply hash-map %) (apply gen/tuple (map mk-gen (rest schema))))
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
             (seq? schema) (mk-list-gen schema extensions)
             :else (throw (ex-info "Unhandled schema" {:schema schema})))))

;; SEM FIXME -- replace quantifiers with OR


(declare replace-quantifiers)

;; SEM FIXME -- none of this is properly tested
;; Did the replacement of quantifiers, but not yet the expansion of OR terms


(declare replace-quantifiers)

(defn quantified? [expr]
  (and (seq? expr)
       (h/case-of? (first expr) & * + ?)))

(defn quantified-within-seq? [expr]
  (and (seq? expr)
       (h/case-of? (first expr) seq vec list)
       (some quantified? (rest expr))))

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
    (map #(conj % (replace-quantifiers expr)) vs)))

;; SEM FIXME -- should be recursive in replacing sub-exprs
(defn quantifier-replacements [seqex]
  (map seq
       (reduce (fn [vs expr]
                   (cond (or (symbol? expr) (h/literal? expr)) (map #(conj % expr) vs)
                         (seq? expr) (quant-replacements vs expr)
                         :else (throw (ex-info "Unexpected element in seqex" {:seqex seqex}))))
               (list [])
               seqex)))

;; expr is canonical
(defn replace-quantifiers [expr]
  (cond (or (symbol? expr) (h/literal? expr)) expr
        (quantified-within-seq? expr) (cons 'or (quantifier-replacements expr))
        (quantified? expr) (if (== (count expr) 2)
                             (recur (second expr))
                             (throw (ex-info "Unsupported quantified schema" {:schema
                                                                              expr})))
        (seq? expr) (map replace-quantifiers expr)
        :else expr))
    
(defn generator [schema]
  (let [canonical (hc/rewrite schema)
        dequantified (replace-quantifiers canonical)]
    (mk-gen dequantified nil)))

(defn sample [schema]
  (gen/sample (generator schema) 20))

(comment
(hg/sample '{kw* int*})

;NullPointerException   simple-check.generators/gen-bind/fn--621 (generators.clj:155)
)

