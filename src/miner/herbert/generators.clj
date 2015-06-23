(ns miner.herbert.generators
  (:require [miner.herbert :as h]
            [miner.herbert.util :refer :all]
            [miner.herbert.canonical :as hc]
            [miner.herbert.regex :as hr]
            [clojure.set :as set]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [clojure.math.combinatorics :as mc :exclude [update]]
            [clojure.test.check.generators :as gen]))





(defn direct-peers [type-info tag]
  (if-let [par (get-in type-info [:parents tag])]
    (disj (get-in type-info [:subtypes par]) tag)))

(defn parent-peers [type-info tag]
  (if-let [par (get-in type-info [:parents tag])]
    (direct-peers type-info par)))


(defn make-type-info [root]
  {:subtypes {}
   :hierarchy (make-hierarchy)
   :root root
   :complements {}})

;; main extensibility point
(defn add-subtype
  ([type-info parent child]
   (let [children (:subtypes type-info)]
     (assoc (assoc-in type-info [:parents child] parent)
            :subtypes (assoc children parent (conj (get children parent #{}) child))))))

(defn add-children [type-info parent children]
  ;; awkward code to preserve :subtypes that derive will drop from type-info
  (reduce #(add-subtype %1 parent %2) type-info children))

;; for declaring subgroups that divide a type, but aren't considered proper subtypes in the
;; sense of a type-info.  For example, pos/neg and odd/even
(defn add-complements
  ([type-info parent tag1 tag2]
   (assoc-in (assoc-in type-info [:complements tag1] (vector parent tag2))
             [:complements tag2] (vector parent tag1)))
  ([type-info parent tag1 tag2 & tags]
   (let [all (set (list* tag1 tag2 tags))]
     (reduce #(assoc-in %1 [:complements %2] (into [parent] (disj all %2))) type-info all))))
  

(defn standard-herbert-type-info []
  ;; pos/neg in both int and float
  ;; odd/even
  (-> (make-type-info 'any)
      (add-children 'any '[num sym kw str bool char seq map])
      (add-children 'num '[int float])
      (add-children 'seq '[list vec])
      (add-complements 'num 'pos 'neg)
      (add-complements 'int 'odd 'even)))

(def herbert-type-info (standard-herbert-type-info))



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

;; convenience, filters out nils, returns nil if no useful gens
(defn gen-one-of [& gens]
  (let [gens (remove nil? gens)]
    (when (seq gens)
      (if (next gens)
        (gen/one-of gens)
        (first gens)))))

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
#_
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


(defn lookup [context name]
  ;;(println "SEM debug lookup" name context)
  (get (:lookup context) name name))

(defn lookup-arg [context x]
  (if (symbol? x)
    (lookup context x)
    x))

(defn lookup-args [context args]
  (map #(lookup-arg context %) args))


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

(defn mk-map
  ([context] (gen/return {}))
  ([context kschema vschema]
   (if (literal? kschema)
     (mk-literal-hash-map context [kschema vschema])
     (mk-kvs context (not= (first kschema) '+) (dequantify kschema) (dequantify vschema))))
  ([context kschema vschema & schemas]
    (mk-literal-hash-map context (list* kschema vschema schemas))))


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

;; SEM FIXME: this could be extensible with a multifn
(defn symbolic-type-of-literal [lit]
  (cond (string? lit) 'str
        (integer? lit) 'int
        (float? lit) 'float
        (keyword? lit) 'kw
        (symbol? lit) 'sym
        (vector? lit) 'vec
        (list? lit) 'list
        (seq? lit) 'seq
        (char? lit) 'char
        (true? lit) 'bool
        (false? lit)  'bool
        (map? lit)  'map
        (set? lit) 'set
        (nil? lit) nil
        :else (throw (ex-info "Unimplemented symbolic-type-of-literal" {:schema lit}))))

(defn mk-type-of-literal [lit]
  (mk-gen (symbolic-type-of-literal lit)))



;; look for literals, invert by taking type and such-that
;; break down hierarchies and have map of inversions, or closed-world types
#_
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
(defn as-collection [coll]
  (cond (not (coll? coll)) (list coll)
        (empty? coll) (list ::void)
        (map? coll) (keys coll)
        :else (seq coll)))
  
(defn mk-in [context coll]
  (gen/elements (as-collection (lookup-arg context coll))))


;; new sig, better for variadic args to go last
;; BUT, could we just do the return a function trick that we did with predicate?
;; Maybe not with the operators AND, OR, etc. but for the simple types, which are all we
;; really need for extensibility.  It's a pain to always lookup-args in my methods

;; Might need a combinator one-of so you can combine them

(defmulti make-generator (fn [context sym & args] sym))

;; a generator that makes values that are not whatever. Obviously, it's hard to be comprehensive,
;; but try to be useful for testing.
(defmulti make-not-generator (fn [context sym & args] sym))

;; SEM IDEA: might be better to have a similiarly-not (same type, outside params) and a
;; typely-not (completely different base types) to make it easier to
;; rewrite complicated AND/OR/NOT expressions.


(defn make-complement-generator [context tag]
  (let [type-info (or (:type-info context) herbert-type-info)
        peers (direct-peers type-info tag)
        parpeers (parent-peers type-info tag)]
    (gen/one-of (remove nil? (map mk-gen (into peers parpeers))))))

(defmethod make-generator 'quote [_ _ arg]
  (gen/return arg))

(defmethod make-not-generator 'quote [_ _ arg]
  (gen/such-that #(not= arg %) gen/any-printable))

(defmethod make-generator 'zero
  ([_ _] (gen/one-of [(gen/return 0) (gen/return 0.0)])))

(defmethod make-not-generator 'zero
  ([_ _] (gen/such-that (complement zero?) gen/int)))

(defmethod make-generator 'int
  ([_ _] gen-int)
  ([context _ hi] (mk-int (lookup-arg context hi)))
  ([context _ lo hi] (mk-int (lookup-arg context lo) (lookup-arg context hi))))

(defmethod make-not-generator 'int
  ([context t] (make-complement-generator context t))
  ([context _ hi]
   (let [high (lookup-arg context hi)]
     (when (< 0 high Long/MAX_VALUE)
       (mk-int (inc high) Long/MAX_VALUE))))
  ([context _ lo hi]
   (let [high (lookup-arg context hi)
         low (lookup-arg context lo)]
     (when (< Long/MIN_VALUE low high Long/MAX_VALUE)
       (gen-one-of (mk-int (inc high) Long/MAX_VALUE)
                   (mk-int Long/MIN_VALUE (dec low)))))))

(defmethod make-generator 'float
  ([_ _] gen-float)
  ([context _ hi] (mk-float (lookup-arg context hi)))
  ([context _ lo hi] (mk-float (lookup-arg context lo) (lookup-arg context hi))))

(defmethod make-not-generator 'float
  ([context t] (make-complement-generator context t))
  ([context _ hi]
   (let [high (double (lookup-arg context hi))]
     (if (< 0 high Double/MAX_VALUE)
       (gen-one-of gen-int (mk-neg) (mk-float (Math/nextUp high) Double/MAX_VALUE))
       gen-int)))
  ([context _ lo hi]
   (let [high (double (lookup-arg context hi))
         low (double (lookup-arg context lo))]
     (if (< low high)
       (gen-one-of gen-int
                   (mk-float (Math/nextUp high) Double/MAX_VALUE)
                   (mk-float (- Double/MAX_VALUE) (Math/nextAfter low (- Double/MAX_VALUE))))
       gen-int))))

(defmethod make-generator 'num
  ([_ _] gen-num)
  ([context _ hi] (gen/one-of (let [high (lookup-arg context hi)]
                                [(mk-int high) (mk-float high)])))
  ([context _ lo hi] (gen/one-of (let [high (lookup-arg context hi)
                                       low (lookup-arg context lo)]
                                   [(mk-int low high) (mk-float low high)]))))  

(defmethod make-not-generator 'num
  ([context t] (make-complement-generator context t))
  ([context _ hi] (gen/one-of (let [high (lookup-arg context hi)]
                                [(mk-int (inc high) Long/MAX_VALUE)
                                 (mk-float (Math/nextUp (double high)) Double/MAX_VALUE)
                                 (mk-neg)
                                 gen/string-ascii gen-symbol gen-kw])))
  ([context _ lo hi] (gen/one-of (let [high (lookup-arg context hi)
                                       low (lookup-arg context lo)]
                                   [(mk-int Long/MIN_VALUE (dec low))
                                    (mk-int (inc high) Long/MAX_VALUE)
                                    (mk-float (- Double/MAX_VALUE)
                                              (Math/nextAfter low (- Double/MAX_VALUE)))
                                    (mk-float (Math/nextUp (double high)) Double/MAX_VALUE)
                                    gen/string-ascii gen-symbol gen-kw]))))

(defmethod make-generator 'seq
  ([_ _] gen-seq)
  ([context _ & args] (gen/one-of [(mk-vec context args)
                                   (mk-list context args)])))

;; SEM FIXME: ignoring params on many of these NOT GENs, could do seqs that don't match param types

(defmethod make-not-generator 'seq
  ([context t] (make-complement-generator context t))
  ([context _ & args] nil))


(defmethod make-generator 'vec 
  ([_ _] (gen/vector gen/any-printable))
  ([context _ & args] (mk-vec context args)))

(defmethod make-not-generator 'vec
  ([context t] (make-complement-generator context t))
  ([context _ & args] nil))


(defmethod make-generator 'list
  ([_ _] (gen/list gen/any-printable))
  ([context _ & args] (mk-list context args)))

(defmethod make-not-generator 'list
  ([context t] (make-complement-generator context t))
  ([context _ & args] nil))
  

(defmethod make-generator 'kvs
  ([context _ allow-empty? ks vs]
   ;; kvs is used internally within the generators
   (mk-kvs context allow-empty? ks vs)))

;; SEM FIXME: could be much better, zipmap of (not vs)
(defmethod make-not-generator 'kvs
   ;; kvs is used internally within the generators, basically same as map for not-gen
  ([context _] (make-complement-generator context 'map))
  ([context _ allow-empty? ks vs] nil))


(defmethod make-generator 'map
  ([_ _] (gen/map gen/keyword gen/any-printable))
  ([context _ kschema vschema] (mk-map context kschema vschema))
  ([context _ kschema vschema & schemas] (apply mk-map context kschema vschema schemas)))

;; SEM FIXME: could be specialized
(defmethod make-not-generator 'map
  ([context t] (make-complement-generator context t))
  ([_ t & _schemas] nil))



;; SEM FIXME: do you want to support a lookup for the regex? or just literal, as is currently?
(defmethod make-generator 'str
  ([_ _] gen/string-ascii)
  ([_ _ regex] (gen-regex (if (string? regex) (re-pattern regex) regex))))


;; SEM FIXME: could specialize on NOT regex
(defmethod make-not-generator 'str
  ([context t] (make-complement-generator context t))
  ([_ _ regex] nil))

(defmethod make-generator 'kw
  ([_ _] gen-kw)
  ([context _ regex] (mk-kw context regex)))

(defmethod make-not-generator 'kw
  ([context t] (make-complement-generator context t))
  ([_ _ regex] nil))



(defmethod make-generator 'sym
  ([_ _] gen-symbol)
  ([context _ regex] (mk-sym context regex)))

(defmethod make-not-generator 'sym
  ([context t] (make-complement-generator context t))
  ([_ _ regex] nil))


(defmethod make-generator 'in
  ([_ _] nil)
  ([context _ coll] (mk-in context coll)))

(defmethod make-not-generator 'in
  ([_ _] nil)
  ([_ _ coll] (let [cset (set (as-collection coll))]
                (gen/such-that #(not (contains? cset %))
                               gen/any-printable))))

(defmethod make-generator 'pos
  ([_ _] (gen-one-of gen/s-pos-int gen-pos-float))
  ([context _ hi] (mk-pos (lookup-arg context hi)))
  ([context _ lo hi] (mk-pos (lookup-arg context lo) (lookup-arg context hi))))

(defmethod make-not-generator 'pos
  ([_ _] (mk-int Long/MIN_VALUE 0))
  ([context _ hi] (let [high (lookup-arg context hi)]
                    (when (< high Long/MAX_VALUE)
                      (mk-pos (inc high) Long/MAX_VALUE))))
  ([context _ lo hi]
   (let [high (lookup-arg context hi)
         low (lookup-arg context lo)]
       (gen-one-of (when (pos? low) (mk-pos 0 (dec low)))
                   (when (< high Long/MAX_VALUE) (mk-pos (inc high) Long/MAX_VALUE))))))


(defmethod make-generator 'neg
  ([_ _] (gen-one-of gen/s-neg-int (gen/fmap - gen-pos-float)))
  ([context _ lo] (mk-neg (lookup-arg context lo)))
  ([context _ lo hi] (mk-neg (lookup-arg context lo) (lookup-arg context hi))))

(defmethod make-not-generator 'neg
  ([_ _] (mk-int Long/MAX_VALUE))
  ([context _ lo] (let [low (lookup-arg context lo)]
                    (when (> low Long/MIN_VALUE)
                      (mk-neg Long/MIN_VALUE (dec low)))))
  ([context _ lo hi]
   (let [high (lookup-arg context hi)
         low (lookup-arg context lo)]
       (gen-one-of (when (< Long/MIN_VALUE low) (mk-neg Long/MIN_VALUE (dec low)))
                   (when (< high -1) (mk-neg (inc high) -1))))))



(defmethod make-generator 'even
  ([_ _] gen-even)
  ([context _ hi] (mk-even (lookup-arg context hi)))
  ([context _ lo hi] (mk-even (lookup-arg context lo) (lookup-arg context hi))))

(defmethod make-not-generator 'even
  ([_ _] gen-odd)
  ([context _ hi] (let [high (lookup-arg context hi)]
                    (when (< high (- Long/MAX_VALUE 2))
                      (mk-even (inc high) Long/MAX_VALUE))))
  ([context _ lo hi] 
   (let [high (lookup-arg context hi)
         low (lookup-arg context lo)]
       (gen-one-of (when (< (inc Long/MIN_VALUE) low) (mk-even Long/MIN_VALUE (dec low)))
                   (when (<  high (dec Long/MAX_VALUE)) (mk-even (inc high) Long/MAX_VALUE))))))

(defmethod make-generator 'odd
  ([_ _] gen-odd)
  ([context _ hi] (mk-odd (lookup-arg context hi)))
  ([context _ lo hi] (mk-odd (lookup-arg context lo) (lookup-arg context hi))))

(defmethod make-not-generator 'odd
  ([_ _] gen-even)
  ([context _ hi] (let [high (lookup-arg context hi)]
                    (when (< high (- Long/MAX_VALUE 2))
                      (mk-odd (inc high) Long/MAX_VALUE))))
  ([context _ lo hi] 
   (let [high (lookup-arg context hi)
         low (lookup-arg context lo)]
       (gen-one-of (when (< (inc Long/MIN_VALUE) low) (mk-odd Long/MIN_VALUE (dec low)))
                   (when (<  high (dec Long/MAX_VALUE)) (mk-odd (inc high) Long/MAX_VALUE))))))



(defmethod make-generator := [context _ name _]
  ;; ignore the other args, they will already be processed as generator bindings
  (mk-return context name))

(defmethod make-not-generator := [context _ name _]
  ;; ignore the other args, they will already be processed as generator bindings
  (gen/such-that #(not= (lookup-arg context name) %) gen/any-printable))


(defmethod make-generator 'bool [_ _]
  gen/boolean)

(defmethod make-not-generator 'bool
  ([context t] (make-complement-generator context t)))


(defmethod make-generator 'char [_ _]
  gen/char)

(defmethod make-not-generator 'char
  ([context t] (make-complement-generator context t)))



(defmethod make-generator 'any [_ _]
  gen/any-printable)

(defmethod make-not-generator 'any
  ([_ _] nil))



(defmethod make-generator 'or
  ([_ _] nil)
  ([context _ term] (mk-gen context term))
  ([context _ term & terms]
   (gen/one-of (remove nil? (map #(mk-gen context %) (cons term terms))))))

;; SEM FIXME: consider a make-type-info of terms, take intersection of all complements
;; hier types and subtypes
;; subgroup odd/even, neg/pos splitting of parent type.  int.odd, int.pos, float.neg
;; union (of possibly multiple subgroups)  pos = int.pos + float.pos
;;    or num.pos = int.pos + float.pos -- num is the common parent of int and float
;; complement is set of siblings at one level of type-info
;;    could be at the subgroup level:  any = #{str sym kw num}


;; SEM FIXME:  lots more to think about.  Seems that no arg handles "whole" type (using
;; closed world assumption), params should
;; be just within type (or maybe parent type).

;; SEM FIXME:  type-info atom could hold canonical types, for closed world reasoning.

;; SEM FIXME: doesn't handle nested ANDs, ORs, NOTs, or quantifiers
(defn symbolic-type [expr]
  (cond (literal? expr) (symbolic-type-of-literal expr)
        (symbol? expr) expr
        (seq? expr) (first expr)))

(defmethod make-not-generator 'or
  ([_ _] nil)
  ([context _ expr] (make-generator context 'not expr))
  ([context _ expr & exprs]
   (let [tyinfo (or (:type-info context) herbert-type-info)
         terms (cons expr exprs)
         bases (map symbolic-type terms)
         lits (filter literal? terms)
         pars (remove nil? (map #(get-in tyinfo [:parents %]) bases))
         complements (set/difference (set (mapcat #(get-in tyinfo [:subtypes %]) pars))
                                     (conj nil (set bases))) ]
     (println "SEM debug lits" lits)
     (if (empty? complements)
       (gen-error)
       (if (seq lits)
         (gen/such-that #(not (contains? (set lits) %))
                        (gen/one-of (remove nil? (map mk-gen complements))))
         (gen/one-of (remove nil? (map mk-gen complements))))))))

;; SEM FIXME: AND should look for subsumption of types
(defmethod make-generator 'and
  ([_ _] gen/any-printable)
  ([context _ term] (mk-gen term))
  ([context _ term & terms] (mk-and context (cons term terms))))

;; (not (and a b)) ==> (or (not a) (not b))
(defmethod make-not-generator 'and
  ([context _] nil)
  ([context _ expr] (make-generator context 'not expr))
  ([context _ expr1 expr2] (mk-gen context (list 'or (list 'not expr1) (list 'not expr2))))
  ([context _ expr1 expr2 & exprs]
   (mk-gen context (cons 'or (map #(list 'not %) (list* expr1 expr2 exprs))))))

;; uses another multifn for the not-generators
(defmethod make-generator 'not [context _ schema]
  (cond (literal? schema) (gen/such-that #(not= schema %) (mk-type-of-literal schema))
        (symbol? schema) (make-not-generator context schema)
        (seq? schema) (gen-one-of (make-not-generator context (first schema))
                                  (apply make-not-generator context (first schema) (rest schema)))
        :else (throw (str "make-generator NOT case unimplemented for " schema))))

(defmethod make-not-generator 'not
  ([context _ schema] (mk-gen context schema)))


(defmethod make-generator :default [context sym]
  (mk-return context sym))

(defmethod make-not-generator :default [context sym]
  (mk-gen context (list 'not sym)))


;; SEM FIXME: Is the (next schema) really useful?  Maybe not worth distinguishing

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

