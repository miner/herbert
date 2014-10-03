(ns miner.herbert.canonical
  (:require [miner.herbert.util :refer :all]
            [miner.herbert.predicates :as predicates]
            [miner.herbert.private :as internal]))

;; canonical form eliminates convenience syntax such as the optional keys (:kw?)
;; and quantifier suffixes (str? int+ sym*)
;; :kw? ==> (? :kw)
;; int+ ==> (+ int)

;; Only empty collections use the literal forms [] () {}.
;; Everything else is written in the list form.   [1 2] ==> (seq 1 2)

(declare rewrite)


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

(defn optional-key? [kw]
  (and (keyword? kw)
       (= (last-char kw) \?)))

(defn simple-key [kw]
  (if (optional-key? kw)
    (strip-last kw)
    kw))

(defn quantified? [expr]
  (cond (symbol? expr) (case-of? (symbol-quantifier expr) * + ?)
        (seq? expr) (or (case-of? (first expr) * + ?)
                        (case-of? (symbol-quantifier (first expr)) * + ?))
        :else false))

(defn literal-or-quoted? [expr]
  (or (predicates/literal? expr)
      (and (seq? expr) (= (first expr) 'quote))))

(defn dequote [expr]
  (if (and (seq? expr) (= (first expr) 'quote))
    (second expr)
    expr))


;;; -------------------

(defn many-quantified [expr]
  ;; expr is already canonical
  (if (and (seq? expr) (case-of? (first expr) * +))
    expr
    (list '* expr)))

(defn sym-rewrite [sym]
  (let [quant (symbol-quantifier sym)]
    (if quant
      (list quant (simple-sym sym))
      sym)))

(defn key-rewrite [k]
  (cond (optional-key? k) (list '? (simple-key k))
        (keyword? k) k
        (and (seq? k) (= (first k) 'quote)) k
        :else (rewrite k)))

;; SEM FIXME: use mapmap variant
(defn kmap-rewrite [mp]
  (cons 'map (interleave
              (map key-rewrite (keys mp))
              (map rewrite (vals mp)))))

(defn hash-map-rewrite [mp]
  ;; empty map should have been checked before
  (let [kvs (seq mp)
        single (and kvs (nil? (next kvs)))]
    (if single
      (cond (literal-or-quoted? (key (first kvs)))
              (kmap-rewrite mp)
            (quantified? (key (first kvs)))
              ;; FIXME: really should insist on the same quantifier!
              (list 'map
                    (rewrite (key (first kvs)))
                    (rewrite (val (first kvs))))
            :else
              ;; special case where {kw int} is treated same as {kw* int*}
              (list 'map
                    (many-quantified (rewrite (key (first kvs))))
                    (many-quantified (rewrite (val (first kvs))))))
      (kmap-rewrite mp))))

(defn vec-rewrite [v]
  (cons 'seq (map rewrite v)))

;; not the best thing to use on known vectors
(defn first= [xs y]
  (and (sequential? xs) (= (first xs) y)))

(defn reduce-amp [exprs]
  (seq (reduce (fn [res x] (if (first= x '&) (reduce conj res (next x)) (conj res x)))
               []
               exprs)))


(defn seq-rewrite [s]
  (cond (empty? s) s
        (= (count s) 1) (rewrite (first s))
        :else
          (case (first s)
            pred s
          quote s
          when s
          grammar (if (= (count s) 2) 
                    (rewrite (second s)) 
                    (list* 'grammar (rewrite (second s))
                           (interleave (take-nth 2 (nnext s))
                                       (map rewrite (take-nth 2 (next (nnext s)))))))
          (= == not= < > <= >=) (list 'when s)
          := (if (== (count s) 3)
               (list := (second s) (rewrite (first (nnext s))))
               (list := (second s) (rewrite (nnext s))))
          ;; else
          (let [op (get internal/internal-reserved-ops (first s))]
                 (if op
                   (cons op (map rewrite (rest s)))
                   ;; pred and args
                   (let [pred (first s)
                         quant (symbol-quantifier pred)]
                     (if quant
                       (list quant (cons (simple-sym pred) (rest s)))
                  (cons (rewrite pred) (rest s)))))))))

(defn set-rewrite [st]
  (cons 'set (map rewrite st)))


;; SEM FIXME -- should use clojure.walk/postwalk
(defn rewrite [schema]
  (cond (and (coll? schema) (empty? schema)) schema
        (keyword? schema) (key-rewrite schema)
        (predicates/literal? schema) schema
        (symbol? schema) (sym-rewrite schema)
        (vector? schema) (vec-rewrite schema)
        (map? schema) (hash-map-rewrite schema)
        (set? schema) (set-rewrite schema)
        (seq? schema) (seq-rewrite schema)
        ;; strange case of providing a predicate, maybe not a good idea
        (fn? schema) schema
        :else (list 'UNIMPLEMENTED schema)))

#_
(defn vc? [schema val]
  (let [direct ((conform schema) val)
        rewr ((conform (rewrite schema)) val)]
    (println "direct" direct "; rewr" rewr)
    (= direct rewr)))

