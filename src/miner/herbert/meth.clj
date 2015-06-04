
;;; http://insideclojure.org/2015/04/27/poly-perf/
;; 
;; If you want fastest type-based dispatch with open extension, then protocols are the best.
;; If you want any other type of dispatch with open extension, then multimethods are the best.
;; If you want a closed system of constant choices, then case is best (constant-time lookup!)
;; If you want a closed system of arbitrary conditions, then cond is best.



Use namespace to lookup extensions

Could be record or naming convention

Predicate ends in ?

Generator ends in -gen or -gen-not

Standardize args

This extension Params value
Params as a vector, might map lookups across
  internal decision

Big idea... Evaluation is external to the schema.  Should not have link to predicates in
schema.  That should happen in context or namespace. Context might just be a namespace.

No, namespace is not necessary.  One multifn per aspect (pred, gen, gen-not, etc) is enough.

context is a bigger extensions
namespace for predicates

{:terms {foo #'mk-foo
         bar #'mk-bar}
}

Really just a multifn to do the symbol discrimination.

:dispatcher
:resolve-mfn
:mfn
:multifn

;; Question: how do ns aliases work inside quoted lists?  It's just a namespace name (not
"resolved") until you evaluate it.



(mkprb p sym args)
;; sym is used for error messages

That predicate p will be called as:

(apply p args)




Given a symbol
check schema extensions :terms first, return rule directly
check built-in predicates, use it to mkprb
:else (mk-lookup sym)

New idea
check schema extensions :terms first, return rule directly
check :dispatcher for predicate, use it to mkprb
:else (mk-lookup sym)



Given a list form
check head for built-in ops (or, and, not, etc.)  -- probably too many hard-coded
if singleton (int), dispatch as symbol
check head schema extensions :terms first, return rule directly
check head built-in predicates, use it to mkprb


New idea 
check head for built-in ops (or, and, not, etc.)  -- probably too many hard-coded
if singleton (int), dispatch as symbol
check head schema extensions :terms first, return rule directly
check head :dispatcher predicates, use it to mkprb


;; This schema does not allow syntax extension.
Could not write xor combinator:
(xor pat pat2)

but  parameterized grammar (almost a macro) could
(op xor A B) (or (and A (not B)) (and B (not A)))

Is this any better than just using a regular macro?



Confused question: Does multifn have to be fixed arity?  Irrelevant!  The returned predicate
has whatever arity, but the multimethod takes exactly one -- the symbol.  Remember it
returns a predicate function, not the final result.  Herbert will call that predicate to
make the match at runtime.






;; expr:  (str #"pattern")

expr:  predicate or term

term:
(or pat pat2)

pred:
(int 10)


(defmulti hsym identity)

(defmethod hsym :default [_form _context]
  nil)

(defmethod hsym 'str [_form context]
  


(defmulti hpredicate identity)

(defmethod hpredicate :default [_sym]
  nil)




(defmulti term-matcher (farg 1))

(defmethod term-matcher 'str [term context]




;; just experimenting
;; MOSTLY WRONG



;; NO to this next idea.  Simplify with just multimethods, but context has namespace.



;; Idea -- use a couple of levels of indirection, but a consistent pattern of calling with
;; args and extensions.
;;
;; defmulti returns tuple of calling convention and gen-maker fn
;; calling: :lookup - wrap args in a `lookup` + extensions
;;       or :apply - no wrap, just apply to args + extensions

;; hmmm, maybe extensions first would be better convention, more consistent for calling fn
;; could call it context for more general feel

(defn gen-maker [schema extensions]

  (let [[convention MAKER] (hgen-fn (first schema))]
    (case convention
      :lookup (apply MAKER (lookup-args (rest schema) extensions))
      :apply (apply MAKER (rest schema) extensions))))

;; Not so convenient calling convention.  Maybe implementor should do the lookup-args and
;; always take the extensions first


(defn list-hgen-maker [schema extensions]
  ;; list style
  (let [maker (hgen-maker (first schema))]
    (maker extensions (rest schema)))
  )


(defmulti hgen-fn identity)

(defmethod hgen-fn 'int [_]
  [:lookup mk-int])

(defmethod hgen-fn := [_]
  [:apply mk-return])


(defmethod hgen-fn :default [_]
  [:apply mk-return])



;; get better farg from miner or transmuters proj

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

