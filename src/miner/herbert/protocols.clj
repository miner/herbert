(ns miner.herbert.protocols
  )



(defprotocol PTermPredicate
  (make-predicate [term context]))

(defprotocol PTermGenerator
  (make-generator [term context]))

