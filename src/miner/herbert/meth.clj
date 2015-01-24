;; just experimenting
;; MOSTLY WRONG

(defn farg
  "Return first arg"
  ([arg] arg)
  ([arg _] arg)
  ([arg _ _] arg)
  ([arg _ _ & _] arg))


(defmulti herbert-generator farg)

(defmethod herbert-generator 'str [_sym extensions regex]
  (mk-str regex extensions))

(defn mk-meth-full-gen [schema extensions]
  (apply herbert-generator (first schema) extensions (rest schema)))

  (let [sym (first schema)
        hgen (herbert-gen sym)]
    ;; returns a fn that should be called with
    ;; extensions & args
    (apply hgen extensions (rest schema))))




(defmulti herbert-gen farg)

(defmethod herbert-gen 'str [_sym]
  (fn [extensions regex] (mk-str regex extensions)))

(defmethod herbert-gen 'pos [_sym]
  (fn [extensions & limits] (apply mk-pos (lookup-args limits extensions))))

(defn mk-meth-single-gen [schema extensions]
  (let [sym (first schema)
        hgen (herbert-gen sym)]
    ;; returns a fn that should be called with
    ;; extensions & args
    (apply hgen extensions (rest schema))))


(defprotocol PTermPredicate
  (conforms? [term context val]))

(defprotocol PTermGenerator
  (generator [term context]))


(defmulti term-factory (fn [sym] sym))



(defrecord Str [regex]
  PPredicateTerm
  (conforms? [term val]
    (if (nil? (:regex term))
      (string? val)
      (and (string? val) (regex-match? (:regex term) val))))

  PGeneratorTerm
  (generator [term context]
    (let [regex (:regex term)]
      (cond (nil? regex) gen/string-ascii
            (string? regex) (gen-regex (re-pattern regex))
            :else (gen-regex regex))))

  )

;; Or use a dynamic var like  clojure.core/*data-readers*

(def ^:dynamic *term-factories*
  {'str #'->Str
   'sym #'->Sym
   })

  

(defmethod term-factory 'str [_] ->Str)

(defmethod term-factory :default [term]
  (throw (ex-info (str "Undefined Herbert term: " term) {:undefined term})))





;; SEM or should it be a lookup instead of throwing?

;; parameterized term as grammar?
(grammar foo
  (ev N) (even 0 N)
  foo (int 0 10))


;;; ----------------------------------------------------------------------
;;; copied from generators.clj for reference

(defn mk-list-gen [schema extensions]
  (let [sym (first schema)]
    (case sym
      quote (gen/return (second schema))
      int (apply mk-int (lookup-args (rest schema) extensions))
      float (apply mk-float (lookup-args (rest schema) extensions))
      num (gen/one-of [(apply mk-int (lookup-args (rest schema) extensions))
                       (apply mk-float (lookup-args (rest schema) extensions))])
      seq (gen/one-of [(mk-vec (rest schema) extensions)
                       (mk-list (rest schema) extensions)])
      vec (mk-vec (rest schema) extensions)
      list (mk-list (rest schema) extensions)
      ;; kvs is used internally within the generators
      kvs (mk-kvs (second schema) (third schema) (fourth schema) extensions)
      map (mk-map (rest schema) extensions)
      or (gen/one-of (map #(mk-gen % extensions) (rest schema)))
      not (mk-not (second schema) extensions)
      and (mk-and (rest schema) extensions)
      str (mk-str (second schema) extensions)
      kw (mk-kw (second schema) extensions)
      sym (mk-sym (second schema) extensions)
      in (mk-in (second schema) extensions)
      ;; SEM FIXME many more
      pos (apply mk-pos (lookup-args (rest schema) extensions))
      neg (apply mk-neg (lookup-args (rest schema) extensions))
      even (apply mk-even (lookup-args (rest schema) extensions))
      odd (apply mk-odd (lookup-args (rest schema) extensions))
      := (mk-return (second schema) extensions)
      )))

(defn mk-symbol-gen [schema extensions]
  (or (get extensions schema)
      (get symbol-gens schema)
      (mk-return schema extensions)
      (throw (ex-info "Unknown schema" {:schema schema}))))

(defn mk-gen 
  ([schema] (mk-gen schema nil))
  ([schema extensions]
       (cond (symbol? schema) (mk-symbol-gen schema extensions)
             (predicates/literal? schema) (gen/return schema)
             (and (coll? schema) (empty? schema)) (gen/return schema)
             (seq? schema) (mk-list-gen schema extensions)
             :else (throw (ex-info "Unhandled schema" {:schema schema})))))

