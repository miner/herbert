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
  "Like simple-check.generators/tuple but returns a seq, not a vector"
  [& generators]
  (gen/fmap seq (apply gen/tuple generators)))


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

(defn mk-list-gen [schema extensions]
  (let [sym (first schema)]
    (case sym
      quote (gen/return (second schema))
      int (apply mk-int (rest schema))
      float (apply mk-float (rest schema))
      num (gen/one-of [(apply mk-int (rest schema)) (apply mk-float (rest schema))])
      seq (gen/one-of [(apply gen/tuple (map mk-gen (rest schema)))
                       (apply gen-tuple-seq (map mk-gen (rest schema)))])
      vec (apply gen/tuple (map mk-gen (rest schema)))
      list (apply gen-tuple-seq (map mk-gen (rest schema)))
      keys (gen/map (or (second schema) gen/any-printable) (or (third schema) gen/any-printable))
      map (gen/fmap #(apply hash-map %) (apply gen/tuple (map mk-gen (rest schema))))
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

(defn generator [schema]
  (let [schema (hc/rewrite schema)]
    (mk-gen schema nil)))
