(ns miner.herbert.util)

(defmacro case-of? 
  "Returns true if `expr` evaluates to any of the `constants`, otherwise false.
As with `case`, constants must be compile-time literals, and need not be quoted."
  [expr & constants]
  `(case ~expr
     ~constants true
     false))

(defn third [s]
  (first (nnext s)))

(defn fourth [s]
  (first (next (nnext s))))

