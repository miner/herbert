(ns miner.herbert.util
  (:require [clojure.string :as str]))

;; General utilities, not project specific

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

(defn str-last-char [^String s]
  (when-not (str/blank? s)
    (.charAt s (dec (.length s)))))

;; works with strings and symbols
(defn last-char [s]
  (str-last-char (name s)))

(defn literal? [x]
  (or (keyword? x) (number? x) (string? x) (false? x) (true? x) (nil? x) (char? x)))

;; not the best thing to use on known vectors
(defn first= [xs y]
  (and (sequential? xs) (= (first xs) y)))


;;;; project specific utilities


;; SEM FIXME: what about & ?

(defn quantified? [expr]
  (and (seq? expr) (case-of? (first expr) * + ?)))

(defn as-quantified [expr]
  (if (quantified? expr)
    expr
    (list '+ expr)))

(defn literal-or-quoted? [expr]
  (or (literal? expr)
      (and (seq? expr) (= (first expr) 'quote))))

(defn optional-literal? [expr]
  (and (seq? expr)
       (case-of? (first expr) ?)
       (literal-or-quoted? (second expr))))

(defn dequote [expr]
  (if (and (seq? expr) (= (first expr) 'quote))
    (second expr)
    expr))

