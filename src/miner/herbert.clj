(ns miner.herbert
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [miner.tagged :as tag]
            [squarepeg.core :as sp]
            [miner.herbert.canonical :as canonical]
            [miner.herbert.private :as internal :refer :all])
  (:import miner.tagged.TaggedValue))



;; SEM FIXME: needs documenation to explain usage
;; inherit some things from internal so that they are public in the main namespace
(def reserved-ops internal-reserved-ops)

(def ns->predicates internal-ns->predicates)

(def default-predicates internal-default-predicates)

(def ^:dynamic *string-from-regex-generator*
  "When bound to a test.check generator, Herbert will use this generator internally for
  constraints that are parameterized by a regular expression.  The generator should take one
  argument, which can be either a java.util.regex.Pattern or a String, as the regex.  It
  should generate strings that match the given regex.  When nil (the default), Herbert will
  use its internal string generator which handles a limited but common regular expression
  syntax.  Regexs like \"[Ff]o+ba?r*\" or \"Ba(r|z)\\d+\" work as expected."
  nil)


;; SEM FIXME -- could do better
(defn- merge-context [extensions context]
  (assoc extensions :context context))

(defn- constraint-fn [schema user-context]
  (let [schema (canonical/rewrite schema)
        exts (schema->extensions schema)
        start (schema->start schema)
        ;;sp/mkmemo should be faster, need benchmarks
        cfn (sp/mkmemo (mkconstraint start (merge-context exts user-context)))]
       (fn ff
         ([item] (ff item {} {} {}))
         ([item context] (ff item context {} {}))
         ([item context bindings memo] (cfn (list item) context bindings memo)))))


(defn schema->grammar [schema]
  (if (grammar? schema)
    schema
    (list 'grammar schema)))

;; creates a fn that test for conformance to the schema
(defn conform
  ([schema] (if (fn? schema) 
              schema 
              (conform schema nil)))
  ([schema context]
    (let [grammar (schema->grammar schema)
          con-fn (constraint-fn schema context)]
       (fn 
         ([] grammar)
         ([x] (let [res (con-fn x)]
                (when (sp/success? res)
                  (with-meta (:b res) {::schema grammar}))))))))

;; SEM finish this with real context
(defn blame-fn [schema] 
  (let [grammar (schema->grammar schema)
        con-fn (constraint-fn schema nil)]
    (fn 
      ([] grammar)
      ([x] (let [res (con-fn x)]
                (when (sp/failure? res)
                  (:fail res)))))))

(defn blame [schema x]
  ((blame-fn schema) x))

(defn conforms?
  ([schema x] (conforms? schema nil x))
  ([schema context x] (boolean ((conform schema context) x))))

(defn schema-merge
  "Makes one schema expression out of several, ignoring the 'start' expression from all but the
first argument, arranging rules so that first schema can use rules from subsequent schemata."
  ([start] (schema->grammar start))
  ([start s1] (let [grammar (schema->grammar start)]
                (concat (list 'grammar (second grammar))
                        (when (grammar? s1) (nnext s1))
                        (nnext grammar))))
  ([start s1 & more] (let [grammar (schema->grammar start)]
                       (concat (list 'grammar (second grammar))
                               (mapcat #(when (grammar? %) (nnext %)) (cons s1 more))
                               (nnext grammar)))))
