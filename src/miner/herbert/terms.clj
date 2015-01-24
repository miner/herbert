(ns miner.herbert.terms
  (:require [miner.herbert.protocols :refer :all]
            ;; [miner.herbert.private :refer :all]
            [miner.herbert.predicates :as p]))



(defrecord Str [regex]
  PTermPredicate
  (make-predicate [term context]
    p/str?)  get args out of term
  
