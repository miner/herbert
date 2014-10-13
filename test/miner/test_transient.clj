(ns miner.test-transient
  (:require [clojure.test :refer :all]
            [miner.herbert.generators :as hg]
            ;; [transient-test.core :refer :all]
            [clojure.test.check.clojure-test :refer (defspec)]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

;; Will fail on Clojure 1.5.1 because of bug CLJ-1285.  Fixed in Clojure 1.6+
;; http://dev.clojure.org/jira/browse/CLJ-1285

(defn transient?
  [x]
  (instance? clojure.lang.ITransientCollection x))

(def gen-conj
  (gen/fmap (fn [x]
              [:conj x])
            gen/int))

(def gen-disj
  (gen/fmap (fn [x]
              [:disj x])
            gen/int))


(def gen-action
  (gen/one-of [gen-conj
               gen-disj
               (gen/return [:transient])
               (gen/return [:persistent!])]))

(def herbert-action '(or [:conj int]
                         [:disj int]
                         [:transient]
                         [:persistent!]))


(defn reduce-actions
  [coll actions]
  (reduce
    (fn [c [f & [arg]]]
      (condp = [(transient? c) f]
        [true   :conj]          (conj! c arg)
        [false  :conj]          (conj c arg)
        [true   :disj]          (disj! c arg)
        [false  :disj]          (disj c arg)
        [true   :transient]     c
        [false  :transient]     (transient c)
        [true   :persistent!]   (persistent! c)
        [false  :persistent!]   c))
    coll
    actions))

(defn apply-actions
  [coll actions]
  (let [applied (reduce-actions coll actions)]
    (if (transient? applied)
      (persistent! applied)
      applied)))

;; original, no longer used
(defn Xfilter-actions
  [actions]
  (filter (fn [[a & args]]
            (#{:conj :disj} a))
          actions))

;; my new version
(defn filter-actions
  [actions]
  (remove (fn [[op]] (#{:persistent! :transient} op)) actions))

(def transient-property
  (prop/for-all
    [a (gen/vector gen-action)]
    (= (apply-actions #{} a)
       (apply-actions #{} (filter-actions a)))))

;; (def trials 100000)
;; needs to be big to catch anything in Clojure 1.5.1
;; not really worth trying in Clojure 1.6+ since the bug is fixed
(def trials 100)

(defspec transient-property-test trials transient-property)


(def herbert-transient-property
  (hg/property (fn [actions]
                 (= (apply-actions #{} actions)
                    (apply-actions #{} (filter-actions actions))))
               (vector (list '* herbert-action))))

(defspec herbert-transient-property-test trials herbert-transient-property)


(def implied-many-transient-property
  (hg/property (fn [actions]
                 (= (apply-actions #{} actions)
                    (apply-actions #{} (filter-actions actions))))
               `[~herbert-action]))

(defspec implied-many-transient-property-test trials implied-many-transient-property)
