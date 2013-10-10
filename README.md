# Herbert

A schema language for **edn** (Clojure data).

[![Way to Eden](img/way-to-eden.png)](#star-trek-reference)

The _extensible data notation_ **(edn)** defines a useful subset of Clojure data types.  The goal
of the *Herbert* project is to provide a schema for defining **edn** data structures that can be
used for documentation, validation and conformance testing.  The schema expressions are
represented as **edn** values.

A significant feature of Clojure programming is the avoidance of required type declarations.  When
you're trying to get a project started, you don't want to be forced to declare every term.
Refactoring is also simpler without type declarations.  On the other hand, there are times when you
know the required *shape* of your data and you would like to guarantee that it conforms to
expectations.  I usually end up writing custom predicates and using `assert` statements as a sanity
check on my data.  They often catch simple typos and careless errors in my code and data files.

Documentation is required to explain the data structures used in a program.  In some cases, the
data format is more important than the code that manipulates it.  At times, I've found it tedious to
document the data formats of some of the nested data structures that I pass around in my programs.
Explaining that a map can have such-and-such keys referring to values of particular types takes a
lot of words if you want to be precise.  Most of the time, a simple data example helps to convey the
essence of the data format but it's hard to capture the full scope of possibilities with only a few
examples.

*Herbert* is designed to describe the format of **edn** data.  It provides a convenient way to turn
those data format descriptions into Clojure predicates which can be used for conformance testing.  A
*Herbert* test can be especially useful in `assert` statements and pre-conditions, but they're also
applicable to many other data matching tasks.  Naturally, the *Herbert* notation is appropriate for
documenting **edn** data structures.


## Leiningen

Add the dependency to your project.clj:

    [com.velisco/herbert "0.5.0"]

In case I forget to update the version number here in the README, the latest version is available on
Clojars.org:

[![Herbert on clojars.org][latest]][clojar]

[latest]: https://clojars.org/com.velisco/herbert/latest-version.svg "Herbert on clojars.org"
[clojar]: https://clojars.org/com.velisco/herbert

## Usage

The `conform` function with two arguments (the *schema* expression and the value to test) returns
either a map of bindings for a successful match or nil for a failed match.  The three-argument
variant takes an *schema-context* map as the first arg.  More about that later.

The `conform-fn` function returns a function that will execute the match against a schema
expression.  It also allows for an optional *schema-context* map.  If you need to check the same
schema multiple times, you can use `conform-fn` to define a predicate.

For the common case of testing conformation, the `conforms?` predicate takes a schema expression
and a value.  It returns `true` if the value conforms to the schema expression, `false`
otherwise.  (Again, there's a variant that takes an *schema-context* map as the first argument.)

Quick example:

	(require '[miner.herbert :as h])
	(h/conforms? '{:a int :b [sym+] :c str} '{:a 42 :b [foo bar baz] :c "foo"})
	;=> true


## Notation for Schema Expressions

* Literal constants match themselves: <BR>
**nil**, **true**, **false**, *numbers*, *"strings"*, *:keywords*

* A simple schema expression is named by a symbol: <BR>
**int**, **str**, **kw**, **sym**, **vec**, **list**, **seq**, **map**

* A quantified schema expression: adding a __*__, __+__ or __?__ at the end of a symbol for
  zero-or-more, one-or-more, or zero-or-one (optional): <BR>
**int***, **str+**, **sym?**
  
* A compound schema expression: using **and**, **or** and **not** <BR>
`(or sym+ nil)`  -- one or more symbols or nil <BR>
`(or (vec int*) (list kw+))`  -- either a vector of ints or a list of one or more keywords

* A quantified schema expression: a list beginning with __*__, __+__ or __?__ as the first element. <BR>
`(* kw sym)`  -- zero or more pairs of keywords and symbols

* A named schema expression is written as a list with the first item being a (non-reserved) symbol,
  which is used as a binding name for the value conforming to the rest of the schema expression.
  The names of predicates and special operators (like **and**, **or**, etc.) are not allowed as
  binding names.  As a special case, a name of underbar `_` means "don't care" and is ignored.  The
  name may be used as a parameter to other schemas and to assert expressions.<BR> 
`(n int)`

* A symbol by itself matches an element equal to the value that the name was bound to
  previously. <BR>
`[(n int) n n]` -- matches [3 3 3]
	
* A literal vector (in square brackets) matches any seq (not just a vector) with the contained
  pattern <BR>
`[(* kw sym)]`  -- matches (:a foo :b bar :c baz)

* A literal map (in curly braces) matches any map with the given literal keys and values matching
  the corresponding schemas.  Optional keywords are written with a ? suffix such as **:kw?**.  For
  convenience, an optional keyword schema implicitly allows **nil** for the corresponding
  value. <BR>
`{:a int :b sym :c? [int*]}`  -- matches {:a 10 :b foo :c [1 2 3]} as well as {:a 1 :b bar}

* A literal set with multiple schema expressions denotes the required element types, but does not
  exclude others.  A single element might match multiple schemas.  A set with a single quantified
  schema expression, defines the requirement on all elements. <BR>
`#{int :a :b}` -- matches #{:a :b :c 10}, but not #{:a 10} <BR>
`#{int+}` -- matches #{1 3 5}, but not #{1 :a 3}

* The `assert` form does not consume any input.  The expression is evaluated within the enviroment
  of the previous bindings -- if it returns a logical true, the match continues.  On a logical
  false, the whole match fails. <BR>
`[(n int) (m int) (assert (== (* 3 n) m))]` -- matches [2 6]

* A list starting with `=`, `==`, `not=`, `<`, `>`, `<=` or `>=` is an *implied assert* and treated
  as if the form was within an `assert` test. <BR>
`[(n int) (m int) (== (* 3 n) m)]` -- matches [2 6]

* Numeric schema expresssions, such as __int__, __even__, __odd__, __float__, or __num__, may take
  optional parameters in a list following the schema.  Numerics take a _low_ and a _high_ parameter.
  The value must be between to the _low_ and _high_ (inclusive) for it to match.  If only one
  parameter is given, it defines the _high_, and the _low_ defaults to 0 in that case.  If neither
  is given, there is no restriction on the high or low values.  Quantified numeric schemas apply the
  _high_ and _low_ to all the matched elements. A name maybe added as the optional first element.
  (See Named schemas above.)<BR>
`(int 1 10)`  -- matches 4, but not 12

* String, symbol and keyword schema expressions (such as __str__, __sym__ and __kw__) may take an
  optional regex argument, specified as a string (for EDN compatibility) or a Clojure regular
  expression (like *#"regex"*).  In that case, the `pr-str` of the element must match the
  regex. <BR>
`(kw ":user/.*")` -- matches :user/foo

* An inlined schema expression:  a list starting with `&` as the first element refers to multiple
  items in order (as opposed to being within a collection). <BR>
`[:a (& (n int) (f float) (> n f)) :b]` -- matches (:a 4 3.14 :b)

* The `map` schema predicate can take two optional arguments, the *key-schema* and the
  *val-schema*.  The matching element must be a map whose `keys` all satisfy *key-schema*
  and whose `vals` all satisfy *val-schema*. <BR>
`(map sym int)` -- matches {a 42}

* Users may extend the schema system in two ways: (1) by declaring new schema terms (predicates) and
  (2) by naming schema expressions.  Schema predicates are associated with Clojure predicate
  functions.  A named schema expression is a convenient way to encapsulate a constraint.  The
  `conform` function (and variants) take a `context` argument which is a map with two significant
  keys:  `:predicates` and `:terms`.  The value for :predicates is a map of symbols to vars.
  The vars name Clojure functions that implement the predicate test for the key symbol.  If the
  predicate is parameterized, the implementing function should take those parameters first.  In all
  cases, the last argument should be the item in question.  Note, the predicate should accept all
  values for consideration without throwing an exception.  For example, the `even` schema predicate
  is implemented with a test of `integer?` as well as `even?` because the latter will throw on
  non-integer values.  The default predicates are defined in the var
  `miner.herbert/default-predicates`.  The :terms value should be a vector of alternating
  symbol and schema-expression pairs (as in a `let` form).  The schema expressions are processed in
  order.  A schema expression can refer to previously named schema expressions.


## Examples

	(require '[miner.herbert :as h])

    (h/conforms? 'int 10)
	;=> true

    (h/conforms? {} 'int 10)
	; empty "schema-context" map has no effect
	;=> true

	(h/conforms? '{:a int :b sym :c? [str*]} '{:a 1 :b foo :c ["foo" "bar" "baz"]})
	;=> true

	(h/conforms? '{:a int :b sym :c? [str*]} '{:a 1 :b foo})
	; :c is optional so it's OK if it's not there at all.
	;=> true

	(h/conforms? '{:a int :b sym :c? [str*]} '{:a foo :b bar})
	;=> false

    (h/conforms? '{:a (a int) :b sym :c? [a+]} '{:a 1 :b foo :c [1 1 1]})
	; a is bound to the int associated with :a, and then used again to define the values in the
	; seq associated with :c.
    ;=> true

    (h/conforms? '(& {:a (a int) :b (b sym) :c (c [b+])} (assert (= (count c) a))) 
	           '{:a 2 :b foo :c [foo foo]})
    ; The & operator just means the following elements are found inline, not in a collection.
	; In this case, we use it to associate the assertion with the single map constraint.  The
	; assertion says that number or items in the :c value must be equal to the value associated
	; with :a.  Notice that all the elements in the :c seq must be equal to the symbol associated 
	; with :b.			   
    ;=> true

    (h/conform '[(a int) (b int) (c int+ a b)] [3 7 4 5 6])
	; Inside a seq, the first two ints establish the low and high range of the rest 
	; of the int values.
    ;=> {c [4 5 6], b 7, a 3}

	(def my-checker (h/conform-fn '[(max int) (xs int+ max)]))
	(my-checker [7 3 5 6 4])
	;=> {xs [3 5 6 4], max 7}

	(defn palindrome? [s]
		(and (string? s)
			(= s (clojure.string/reverse s))))
			
	(h/conforms? {:predicates {'palindrome #'palindrome?}
                  :terms '[pal {:len (len int) :palindrome (and palindrome (cnt len))}
                                 palindromes [pal+]]}
                 'palindromes
                 [{:palindrome "civic" :len 5}
                  {:palindrome "kayak" :len 5} 
                  {:palindrome "level" :len 5}
                  {:palindrome "ere" :len 3}
                  {:palindrome "racecar" :len 7}])
	;=> true


## References

* edn: http://edn-format.org
* Clojure: http://clojure.org
* Square Peg parser:  https://github.com/ericnormand/squarepeg
* clj-schema:  https://github.com/runa-dev/clj-schema


## Star Trek: _The Way to Eden_  

stardate 5832.3

**Space Hippies**: "Herbert, Herbert, Herbert ..."  
**Spock**: "Herbert was a minor official notorious for his rigid and limited patterns of thought."  
**Kirk**: "Well, I shall try to be less rigid in my thinking."  

video clip:  http://www.youtube.com/watch?v=PQONBf9xMss

[![Way to Eden](img/way-to-eden2.jpg)](http://www.youtube.com/watch?v=PQONBf9xMss)

## Copyright and License

Copyright (c) 2013 Stephen E. Miner.

Distributed under the Eclipse Public License, the same as Clojure.
