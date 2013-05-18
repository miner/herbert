(ns miner.herbert.proto)

(defprotocol Betweenable
  ;; inclusive
  (between? [x lo hi]))

(extend-protocol Betweenable
  nil
  (between? [x lo hi] (= nil lo hi))

  Number
  (between? [n lo hi] (<= lo n hi))

  Comparable
  (between? [x lo hi] (and (not (neg? (compare x lo)))  (not (neg? (compare hi x)))))

  ;; vectors are Comparable but seqs and lists aren't

  ;;clojure.lang.Sequential
  ;;(between? [x lo hi] (and (not (neg? (compare x lo)))  (not (neg? (compare hi x)))))

)


(defprotocol Inable
  (in? [inval x]))

(extend-protocol Inable
  clojure.lang.PersistentVector
  (in? [[lo hi] x]
    (between? x lo hi))

  clojure.lang.IPersistentSet
  (in? [pset x] (get pset x))

  Number
  (in? [n x] (if (neg? n) (between? x n 0) (between? x 0 n)))

  clojure.lang.IFn
  (in? [f x] (f x))
)

