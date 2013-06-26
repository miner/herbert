# Herbert

A schema language for **edn** (Clojure data).

[![Way to Eden](img/way-to-eden.png)](#star-trek-reference)

The _extensible data notation_ **(edn)** defines a useful subset of Clojure data types.  The goal
of the *Herbert* project is to provide a schema for defining **edn** data structures that can be
used for documentation, validation and conformance testing.  The constraint expressions are
represented as **edn** values.

A significant feature of Clojure programming is the avoidance of required type declarations.  When
you're trying to get a project started, you don't want to be forced to declare every term.
Refactoring is also simpler without type declarations.  On the other hand, there are times when you
know the required *shape* of your data and you would like to guarantee that it conforms to
expectations.  I usually end up writing custom predicates and using `assert` statements as a sanity
check on my data.  They often catch simple typos and careless errors in my code and data files.

Documentation is also required to explain the data structures used in a program.  In some cases, the
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

    [com.velisco/herbert "0.3.4"]

I might forget to update the version number here in the README.  The latest version is available on
Clojars.org:

https://clojars.org/com.velisco/herbert


## Usage

The `conform` function with two arguments (the constraint expression and the value to test) returns
either a map of bindings for a successful match or nil for a failed match.  With a single argument
(the constraint expresion), the `conform` function returns a function that will execute the match
against that constraint.  This is useful if you need to check the same constraint multiple times.

For the common case of testing conformation, the `conforms?` predicate takes a constraint expression
and a value.  It returns `true` if the value conforms to the constraint expression, `false`
otherwise.

Quick example:

	(require '[miner.herbert :as h])
	(h/conforms? '{:a int :b [sym+] :c str} '{:a 42 :b [foo bar baz] :c "foo"})
	;=> true


## Notation

* Literal constants match themselves: <BR>
**nil**, **true**, **false**, *numbers*, *"strings"*, *:keywords*

* Simple constraints are named by symbols: <BR>
**int**, **str**, **kw**, **sym**, **vec**, **list**, **seq**, **map**

* Quantified constraints, adding a __*__, __+__ or __?__ at the end of a simple type for
  zero-or-more, one-or-more, or zero-or-one (optional): <BR>
**int***, **str+**, **sym?**
  
* Compound constraints, using **and**, **or** and **not** <BR>
`(or sym+ nil)`  -- one or more symbols or nil <BR>
`(or (vec int*) (list kw+))`  -- either a vector of ints or a list of one or more keywords

* Quantified constraints, a list beginning with __*__, __+__ or __?__ as the first element. <BR>
`(* kw sym)`  -- zero or more pairs of keywords and symbols

* Named constraints are written in a list with the first item a (non-reserved) symbol and the second
  item naming the constraint.  The built-in types and special operators (like **and**, **or**,
  etc.) are not allowed as binding names.  The name may be used as a parameter to other constraints
  and to assert expressions.<BR>
`(n int)`

* A symbol by itself matches an element equal to the value that the name was bound to
  previously. <BR>
`[(n int) n n]` -- matches [3 3 3]
	
* Square brackets match any seq (not just a vector) with the contained pattern <BR>
`[(* kw sym)]`  -- matches '(:a foo :b bar :c baz)

* Curly braces match any map with the given keys and value types.  Optional keywords are written
  with a ? suffix such as **:kw?**.  For convenience, an optional keyword constraint implicitly
  allows **nil** for the corresponding value. <BR>
`{:a int :b sym :c? [int*]}`  -- matches {:a 10 :b foo :c [1 2 3]}

* A set with multiple constraints denotes the required element types, but does not exclude others.
  A single element might match multiple constraints.  A set with a single quantified constraint,
  defines the requirement on all elements. <BR>
`#{int :a :b}` -- matches #{:a :b :c 10}, but not #{:a 10} <BR>
`#{int+}` -- matches #{1 3 5}, but not #{1 :a 3}

* The `assert` form does not consume any input.  The expression is evaluated within the enviroment
  of the previous bindings -- if it returns a logical true, the match continues.  On a logical
  false, the whole match fails. <BR>
`[(n int) (m int) (assert (== (* 3 n) m))]` -- matches [2 6]

* A list starting with `=`, `==`, `not=`, `<`, `>`, `<=` or `>=` is an *implied assert* and treated
  as if the form was within an `assert` test. <BR>
`[(n int) (m int) (== (* 3 n) m)]` -- matches [2 6]

* Numeric constraints, such as __int__, __even__, __odd__, __float__, or __num__, may take optional
  parameters in a list following the constraint.  Numerics take a _low_ and a _high_ parameter.  The
  value must be between to the _low_ and _high_ (inclusive) for it to match.  If only one parameter
  is given, it defines the _high_, and the _low_ defaults to 0 in that case.  If neither is given,
  there is no restriction on the high or low values.  Quantified numeric constraints apply the
  _high_ and _low_ to all the matched elements. A name maybe added as the optional first element.
  (See Named Constraints above.)<BR>
`(int 1 10)`  -- matches 4, but not 12

* Inlined constraints. A list starting with `&` as the first element refers to multiple items in
  order (as opposed to being within a container sequence). <BR>
`(& (n int) (f float) (assert (> n f)))` -- matches 4 3.14

* Users may define new constraints by binding the dynamic var `miner.herbert/*constraints*`.  It
  should be a map of symbols to vars, where the var names a function that implements the appropriate
  predicate.  If the constraint takes parameters, the implementing function should take those
  paramenters first.  In all cases, the last argument should be the item in question.  Note, the
  constraint function should accept all values for consideration without throwing an exception.  For
  example, the `even` constraint is implemented with a test of `integer?` as well as `even?` since
  the latter will throw on non-integer values.  The default constraints are defined in the var
  `miner.herbert/default-constraints`.

## Examples

    (conforms? 'int 10)
	;=> true
	
	(conforms? '{:a int :b sym :c? [str*]} '{:a 1 :b foo :c ["foo" "bar" "baz"]})
	;=> true

	(conforms? '{:a int :b sym :c? [str*]} '{:a 1 :b foo})
	; :c is optional so it's OK if it's not there at all.
	;=> true

	(conforms? '{:a int :b sym :c? [str*]} '{:a foo :b bar})
	;=> false

    (conforms? '{:a (a int) :b sym :c? [a+]} '{:a 1 :b foo :c [1 1 1]})
	; a is bound to the int associated with :a, and then used again to define the values in the
	; seq associated with :c.
    ;=> true

    (conforms? '(& {:a (a int) :b (b sym) :c (c [b+])} (assert (= (count c) a))) 
	           '{:a 2 :b foo :c [foo foo]})
    ; The & operator just means the following elements are found inline, not in a container.
	; In this case, we use it to associate the assertion with the single map constraint.  The
	; assertion says that number or items in the :c value must be equal to the value associated
	; with :a.  Notice that all the elements in the :c seq must be equal to the symbol associated 
	; with :b.			   
    ;=> true

    (conform '[(a int) (b int) (c int+ a b)] [3 7 4 5 6])
	; Inside a seq, the first two ints establish the low and high range of the rest 
	; of the int values.
    ;=> {c [4 5 6], b 7, a 3}

    (def my-checker (conform '[(a int) (b int) (c int+ a b)]))
	(my-checker [3 7 4 5 6])
    ;=> {c [4 5 6], b 7, a 3}
	

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
