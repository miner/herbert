(ns miner.herbert.tree
  (:require [miner.herbert.util :refer :all]
            [miner.herbert.terms :refer :all]
            [miner.herbert.predicates :as predicates]
            [miner.herbert.private :as internal]))

;; Tree form converts the sexpr form into a tree of node records

;; canonical form eliminates convenience syntax such as the optional keys (:kw?)
;; and quantifier suffixes (str? int+ sym*)
;; :kw? ==> (? :kw)
;; int+ ==> (+ int)

;; Only empty collections use the literal forms [] () {}.
;; Everything else is written in the list form.   [1 2] ==> (seq 1 2)

(declare parse)

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

;; SEM FIXME: probably should be separate predicates
(defn literal-or-quoted? [expr]
  (or (predicates/literal? expr)
      (and (seq? expr) (= (first expr) 'quote))))

(defn dequote [expr]
  (if (and (seq? expr) (= (first expr) 'quote))
    (second expr)
    expr))

;; test for single separately
(defn implied-quantifiable? [expr]
  (and (not (predicates/literal? expr))
       (not (and (seq? expr)
                 (case-of? (first expr) := * + ? & quote)))
       (not (quantified? expr))))

;;; -------------------

(defn sym-parse [sym]
  (let [quant (symbol-quantifier sym)]
    (if quant
      (list quant (simple-sym sym))
      sym)))

(defn key-parse [k]
  (cond (optional-key? k) (list '? (simple-key k))
        (keyword? k) k
        (and (seq? k) (= (first k) 'quote)) k
        :else (parse k)))

;; SEM FIXME: use mapmap variant
(defn kmap-parse [mp]
  (cons 'map (interleave
              (map key-parse (keys mp))
              (map parse (vals mp)))))

(defn hash-map-parse [mp]
  ;; empty map should have been checked before
  (let [kvs (seq mp)
        single (and kvs (nil? (next kvs)))]
    (if single
      (cond (literal-or-quoted? (key (first kvs)))
              (kmap-parse mp)
            (implied-quantifiable? (key (first kvs)))
              ;; special case where {kw int} is treated same as {kw+ int+}
              (list 'map
                    (list '+ (parse (key (first kvs))))
                    (list '+ (parse (val (first kvs)))))
            :else
              ;; FIXME: really should insist on the same quantifier!
              (list 'map
                    (parse (key (first kvs)))
                    (parse (val (first kvs)))))
      (kmap-parse mp))))

(defn vec-parse [v]
  (if (and (== (count v) 1) (implied-quantifiable? (first v)))
    (list 'seq (list '+ (parse (first v))))
    (cons 'seq (map parse v))))

;; not the best thing to use on known vectors
;; FIXME: NOT USED
(defn first= [xs y]
  (and (sequential? xs) (= (first xs) y)))

;; FIXME: NOT USED
(defn reduce-amp [exprs]
  (seq (reduce (fn [res x] (if (first= x '&) (reduce conj res (next x)) (conj res x)))
               []
               exprs)))


(defn seq-parse [s]
  (cond (empty? s) s
        (== (count s) 1) (parse (first s))
        :else
          (case (first s)
            pred s
            quote s
            when s
            grammar (if (== (count s) 2) 
                      (parse (second s)) 
                      (list* 'grammar (parse (second s))
                             (interleave (take-nth 2 (nnext s))
                                         (map parse (take-nth 2 (next (nnext s)))))))
            (= == not= < > <= >=) (list 'when s)
            := (if (== (count s) 3)
                 (list := (second s) (parse (first (nnext s))))
                 (list := (second s) (parse (nnext s))))
            ;; else
            (let [op (get internal/internal-reserved-ops (first s))]
              (if op
                (cond (and (= op 'map)
                           (== (count s) 3)
                           (implied-quantifiable? (second s)))
                        ;; implied quantifiable map
                        (list 'map 
                              (list '+ (parse (second s)))
                              (list '+ (parse (third s))))
                      (and (case-of? op vec list set seq)
                           (== (count s) 2)
                           (implied-quantifiable? (second s)))
                        ;; implied quantifiable
                        (list op (list '+ (parse (second s))))
                      :else
                        (cons op (map parse (rest s))))
                ;; pred and args
                (let [pred (first s)
                      quant (symbol-quantifier pred)]
                  (if quant
                    (list quant (cons (simple-sym pred) (rest s)))
                    (cons (parse pred) (rest s)))))))))

(defn set-parse [st]
  (if (and (== (count st) 1)
           (implied-quantifiable? (first st)))
    (list 'set (list '+ (parse (first st))))
    (cons 'set (map parse st))))


;; SEM FIXME -- should use clojure.walk/postwalk
(defn parse [schema]
  (cond (and (coll? schema) (empty? schema)) schema
        (keyword? schema) (key-parse schema)
        (predicates/literal? schema) schema
        (symbol? schema) (sym-parse schema)
        (vector? schema) (vec-parse schema)
        (map? schema) (hash-map-parse schema)
        (set? schema) (set-parse schema)
        (seq? schema) (seq-parse schema)
        ;; strange case of providing a predicate, maybe not a good idea
        (fn? schema) schema
        :else (list 'UNIMPLEMENTED schema)))

#_
(defn vc? [schema val]
  (let [direct ((conform schema) val)
        rewr ((conform (parse schema)) val)]
    (println "direct" direct "; rewr" rewr)
    (= direct rewr)))

