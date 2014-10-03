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
