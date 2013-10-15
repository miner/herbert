(ns miner.herbert.canonical
  (:require [miner.herbert :refer :all]))

(declare rewrite)

(defn sym-rewrite [sym]
  (let [quant (symbol-quantifier sym)]
    (if quant
      (list quant (simple-sym sym))
      sym)))

(defn key-rewrite [k]
  (if (optional-key? k)
    (list '? (simple-key k))
    k))

;; SEM FIXME: use mapmap variant
(defn kmap-rewrite [mp]
  (cons 'map (interleave
              (map key-rewrite (keys mp))
              (map rewrite (vals mp)))))
  
(defn vec-rewrite [v]
  (cons 'seq (map rewrite v)))

(defn seq-rewrite [s]
  (cond (= (count s) 1) (rewrite (first s))
        (= (first s) 'when) s
        (case (first s) (= == not= < > <= >=) true false) (list 'when s)
        :else  (let [op (get reserved-ops (first s))]
                 (if op
                   (cons op (map rewrite (rest s)))
                   ;; pred and args
                   (let [pred (first s)
                         quant (symbol-quantifier pred)]
                     (if quant
                       (list quant (cons (simple-sym pred) (rest s)))
                       (cons (rewrite pred) (rest s))))))))

(defn set-rewrite [st]
  (cons 'set (map rewrite st)))

(defn rewrite [schema]
  (cond (keyword? schema) (key-rewrite schema)
        (literal? schema) schema
        (symbol? schema) (sym-rewrite schema)
        (vector? schema) (vec-rewrite schema)
        (map? schema) (kmap-rewrite schema)
        (set? schema) (set-rewrite schema)
        (seq? schema) (seq-rewrite schema)
        :else (list 'unimplemented schema)))


(defn vc? [schema val]
  (let [direct (conform schema val)
        rewr (conform (rewrite schema) val)]
    (println "direct" direct "; rewr" rewr)
    (= direct rewr)))


