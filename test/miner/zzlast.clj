(ns miner.zzlast
  (:require [clojure.test :refer :all]))

;; run last to take inventory of generated fns, looking for collisions

(deftest printing-tmp-vars
  ;; intentional collisions in `conform-vars` test are the only expected ones
  (is (= 6 (count (filter #(re-matches #"H[a-z].*" (name %))
                          (keys (ns-publics 'miner.herbert.tmp)))))))
