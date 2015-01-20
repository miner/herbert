(ns miner.herbert.regex
  (:require [miner.herbert.util :refer :all]
            [clojure.set :as set]
            [clojure.test.check.generators :as gen]))

;; Minimal regex features
;; *?+ . [abc] [a-z] [^a] \n (a|b) \w \W \d \D \s \S
;; not supported: ^$ \1 (capture group) {M,N} [:alnum:]
;; http://en.wikipedia.org/wiki/Regular_expression

;; Try test.chuck for a better string-from-regex-generator, but it has some dependecies that
;; will not allow it to be used in a contrib library (which is on the roadmap for Herbert).
;; https://github.com/gfredericks/test.chuck


(declare parse-chars)

(defn slash [c]
  (case c
    \d '(:digit)
    \D '(:not-digit)
    \w '(:word)
    \W '(:not-word)
    \s '(:space)
    \S '(:not-space)
    \t '(:tab)
    \n '(:newline)
    \r '(:return)
    (\[ \] \* \+ \. \? \\ \( \)) c
    (throw (ex-info (str "Unsupported backslash char " c) {:unsupported-backslash c}))))
  
     
(defn parse-set-contents [cs result]
  ;;(println "parse-set-contents  " result "  " (first cs))
  (case (first cs)
    nil (throw (ex-info "Unterminated [set]" {:error :unterminated-set :partial result}))
    \] (if (not (seq result))
         (recur (rest cs) (conj result \]))
         [(list* :set result) (rest cs)])
    \- (if (or (not (seq result)) (= \] (second cs)))
         (recur (rest cs) (conj result \-))
         (recur (rest (rest cs)) (conj (pop result) (list :btw (peek result) (second cs)))))
    (recur (rest cs) (conj result (first cs)))))

;; already consumed first [
(defn parse-set [cs]
  (case (first cs)
    \^ (let [[setexp rst] (parse-set-contents (rest cs) [])]
         [(list* :inverted (rest setexp)) rst])    
    (parse-set-contents cs [])))


(defn parse-chars
  ([cs] (parse-chars cs [] []))
  ([cs group result]
   ;; (println group "  " result "  " (first cs))
   (case (first cs)
     nil (if (empty? result) group (conj result group))
     \( (recur (rest cs) [] (conj result group))
     \) (recur (rest cs) (conj (peek result) group) (pop result))
     \[ (let [[setexp rst] (parse-set (rest cs))] (recur rst (conj group setexp) result))
     \. (recur (rest cs) (conj group '(:any)) result)
     ;; :alt is temporarily inserted in place, to be regrouped later
     \| (recur (rest cs) (conj group :alt) result)
     \* (recur (rest cs) (conj (pop group) (list :* (peek group))) result)
     \+ (recur (rest cs) (conj (pop group) (list :+ (peek group))) result)
     \? (recur (rest cs) (conj (pop group) (list :? (peek group))) result)
     \\ (recur (rest (rest cs)) (conj group (slash (second cs))) result)
     (recur (rest cs) (conj group (first cs)) result))))


;; Hack alert!  regroup-alt walks throught the initial parse and figures out how to regroup
;; the infix :alt markers into sexp-style prefix notation.  Yes, it should have been done
;; that way from the start, but I couldn't figure out how to keep my groups straight in a
;; single pass.  So we have to live with this.

;; pre mid post refer to the alt groups [pre (* :alt mid) :alt post]
;; mid is (or nil [[x]+]) -- multiple groups in mid
;; post is (nil or [x]) -- nil means no :alt has been seen yet
;; keywords are used as special groupings or operators in the first position of a list (not vector)
(defn regroup-alt [coll]
  (loop [cs coll pre [] mid nil post nil]
    (if post
      (cond (empty? cs) (list* :alt pre (conj mid post))
            (coll? (first cs)) (recur (rest cs) pre mid (conj post (regroup-alt (first cs))))
            (= :alt (first cs)) (recur (rest cs) pre (conj mid post) [])
            :else (recur (rest cs) pre mid (conj post (first cs))))
      (cond (empty? cs) (if (keyword? (first pre)) (seq pre) pre)
            (coll? (first cs)) (recur (rest cs) (conj pre (regroup-alt (first cs))) nil nil)
            (= :alt (first cs)) (recur (rest cs) pre [] [])
            :else (recur (rest cs) (conj pre (first cs)) nil nil)))))

;; SEM -- probably don't need to regroup alts within (:set ...) or (:inverted ...), etc.
;; really only within vectors I think

;; regex can be either a string or a regex
(defn parse [regex]
  (regroup-alt (parse-chars (seq (if (string? regex) regex (str regex))))))

(declare tree->generator)

(defn between [ch-begin ch-end]
  ;; inclusive
  (set (map char (range (long ch-begin) (inc (long ch-end))))))
  

;; all these char ranges need double checking
(def digits #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
(def atoz (between \a \z))
(def AtoZ (between \A \Z))
(def space (set (seq " \t\n\r")))
(def punctuation (set (seq ":#$%^&*()-+=!@~`;'?/.|\\[]{},<>\"")))
(def underscore (set (seq "_")))

(def word (set/union atoz AtoZ digits underscore))
(def not-word (set/union space punctuation))
(def not-digits (set/union space punctuation atoz AtoZ underscore))
(def not-space (set/union word punctuation))

(def all-chars (set/union word punctuation space))


;; really should only be called with (:btw ...)
(defn charset-btw [tree]
  (if (and (seq? tree) (= (first tree) :btw))
    (apply between (rest tree))
    (set tree)))

(defn inverted [trees]
  (apply set/difference all-chars (set (filter char? trees)) (map charset-btw (filter seq? trees))))

(defn charset [trees]
  (apply set/union (set (filter char? trees)) (map charset-btw (filter seq? trees))))

(defn seq->generator [tree]
  (case (first tree)
    :any gen/char-ascii
    :* (gen/sized (fn [n] (gen/vector (tree->generator (second tree)) 0 (min n 5))))
    :+ (gen/sized (fn [n] (gen/vector (tree->generator (second tree)) 1 (max 1 (min n 5)))))
    :? (gen/one-of [(gen/return "") (tree->generator (second tree))])
    :alt (gen/one-of (map tree->generator (rest tree)))
    :set (gen/elements (charset (rest tree)))
    :inverted (gen/elements (inverted (rest tree)))
    :btw (gen/elements (apply between (rest tree)))
    :digit (gen/elements digits)
    :not-digit (gen/elements not-digits)
    :word (gen/elements word)
    :not-word (gen/elements not-word)
    :space  (gen/elements space)
    :not-space (gen/elements not-space)
    :tab (gen/return \t)
    :newline (gen/return \n)
    :return (gen/return \r)))


;; This is a bit hairy.  It's trying to group runs of single chars together to make a string
;; when possible.  Also, handles special case of single item as a single generator rather
;; than a tuple as used in the general case.

(defn vec->generator [trees]
  (loop [xs trees cs [] gens []]
    (cond (empty? xs) (let [gens (if (empty? cs) gens (conj gens (gen/return (apply str cs))))]
                        (cond (empty? gens) (gen/return "")
                              (= (count gens) 1) (first gens)
                              :else (apply gen/tuple gens)))
          (char? (first xs)) (recur (rest xs) (conj cs (first xs)) gens)
          :else (recur (rest xs) [] (if (empty? cs)
                                      (conj gens (tree->generator (first xs)))
                                      (conj gens (gen/return (apply str cs))
                                            (tree->generator (first xs))))))))

(defn tree->generator [tree]
  (cond (vector? tree) (vec->generator tree)
        (seq? tree) (seq->generator tree)
        (char? tree) (gen/return tree)
        (string? tree) (gen/return tree)
        :else (throw (ex-info (str "Unimplmented generator for " tree) {:unimplemented tree}))))

(defn string-generator [regex]
  (gen/fmap #(if (coll? %) (apply str (flatten %)) (str %))
            (tree->generator (parse regex))))
