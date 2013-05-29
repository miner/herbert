# Herbert

A schema for **edn** data.

[![Way to Eden](img/way-to-eden.png)](#star-trek-reference)

__Warning__: This project is new and rapidly changing.  It's probably not ready for other people to use yet.

The _extensible data notation_ **(edn)** defines a useful subset of Clojure data types.  The goal
of the *Herbert* project is to provide a schema for defining **edn** data structures that can be
used for documentation, validation and conformance testing.

A significant feature of Clojure programming is that it doesn't require type declarations.  When
you're trying to get a project started, you don't want to be forced to declare every term.
Refactoring is also simpler without type declarations.  On the other hand, there are times when you
know the required *shape* of your data and you would like to guarantee that it conforms to
expectations.  Clojure has `assert` statements and *pre-conditions* that be used to test data.  I
usually end up writing custom predicates as a sanity check on my data.  They often catch simple
typos and careless errors in my code and data files.

Documentation is also required to explain the data structures used in a program.  In some cases, the
data format is more important than the code that manipulates it.  At times, I've found it tedious to
document the data formats of some of the nested data structures that I pass around in my programs.
Explaining that a map can have such-and-such keys referring to values of particular types takes a
lot of words if you want to be precise.  Most of the time, a simple data example helps to convey the
essence of the data format but it's hard to capture the full scope of possibilities with only a few
examples.

*Herbert* is designed to describe the format of the **edn** data.  It provides a convenient way to
turn those data format descriptions into Clojure predicates which can be used for conformance
testing.  A *Herbert* expression can be useful in an assertion or pre-condition.  *Herbert* also
provides a convenient notation for documenting **edn** data structures.


## Leiningen

Add the dependency to your project.clj:

    [com.velisco/herbert "0.3.1"]

In your source:

    (ns my.project
		(:require [miner.herbert :as herb]))

I might forget to update the version number here in the README.  The latest version is available on
Clojars.org:

https://clojars.org/com.velisco/herbert

## Notation

* Literal constants match themselves: <BR>
**nil**, **true**, **false**, *numbers*, *"strings"*, *:keywords*

* Simple types: <BR>
**int**, **str**, **kw**, **sym**, **vec**, **list**, **seq**, **map**

* Quantified types, adding a __*__, __+__ or __?__ at the end of a simple type for zero-or-more,
  one-or-more, or zero-or-one (optional): <BR>
**int***, **str+**, **sym?**
  
* Compound constraints, using **and**, **or** and **not** <BR>
`(or sym+ nil)`  -- one or more symbols or nil <BR>
`(or (vec int*) (list kw+))`  -- either a vector of ints or a list of one or more keywords

* Quantified constraints, a list beginning with __*__, __+__ or __?__ as the first element. <BR>
`(* kw sym)`  -- zero or more pairs of keywords and symbols

* Named constraints are written in a list with the first item a (non-reserved) symbol and
  the second item naming the type constraint.  The built-in types and special operators (like
  **and**, **or**, etc.) are not allowed as binding names. <BR>
`(n int)`

* A name in a list by itself (n) matches an element equal to the value that the name was bound to
  previously.  The name may also be used as a parameter to other constraints and to guards.
`[(n int) (n) (n)]` -- matches [3 3 3]
	
* Square brackets match any seq (not just a vector) with the contained pattern <BR>
`[(* kw sym)]`  -- matches '(:a foo :b bar :c baz)

* Curly braces match any map with the given keys and value types.  Optional keywords are written
  with a ? suffix such as **:kw?** <BR>
`{:a int :b sym :c? [int*]}`  -- matches {:a 10 :b foo :c [1 2 3]}

* A set with multiple constraints denotes the required element types, but does not exclude others.
  A single element might match multiple constraints.  A set with a single quantified constraint,
  defines the requirement on all elements. <BR>
`#{int :a :b}` -- matches #{:a :b :c 10}, but not #{:a 10}
`#{int+}` -- matches #{1 3 5}, but not #{1 :a 3}

* The guard form does not consume any input.  The guard expression is similar to a fn declaration.
  It takes a vector of arguments which must match the names of previous binding elements.  The guard
  evaluates its body -- if it returns a logical true, the match continues.  On a logical false, the
  whole match fails. <BR>
`[(n int) (m int) (guard [n m] (= (* 3 n) m))]` -- matches [2 6]

* Numeric constraints, such as __int__, __even__, __odd__, __float__, or __num__, may take optional
  parameters in a list following the constraint.  Numerics take a _low_ and a _high_ parameter.  The
  value must be between to the _low_ and _high_ (inclusive) for it to match.  If only one parameter
  is given, it defines the _high_, and the _low_ defaults to 0 in that case.  If neither is given,
  there is no restriction on the high or low values.  Quantified numeric constraints apply the
  _high_ and _low_ to all the matched elements. <BR>
`(int 1 10)`  -- matches 4, but not 12

* Users may define new constraints by binding the dynamic var `miner.herbert/*constraints*`.  It
  should be a map of symbols to vars, where the var names a function that implements the appropriate
  predicate.  If the constraint takes parameters, the implementing function should take those
  paramenters first.  In all cases, the last argument should be the item in question.  Note, the
  constraint function should accept all values for consideration without throwing an exception.  For
  example, the `even` constraint is implemented with a test of `integer?` as well as `even?` since
  the later will throw on non-integer values.  The default constraints are defined in the var
  `miner.herbert/default-constraints`.


## Examples

    (conforms? 'int 10)
	;=> true
	
	(conforms? '{:a int :b sym :c? [str*]} '{:a 1 :b foo :c ["foo" "bar" "baz"]})
	;=> true

	(conforms? '{:a int :b sym :c? [str*]} '{:a 1 :b foo})
	;=> true

	(conforms? '{:a int :b sym :c? [str*]} '{:a foo :b bar})
	;=> false
	
## References

* edn: http://edn-format.org
* Clojure: http://clojure.org
* Square Peg parser:  https://github.com/ericnormand/squarepeg
* clj-schema:  https://github.com/runa-dev/clj-schema


## Star Trek reference

Star Trek, _The Way to Eden_  
stardate 5832.3

**Space Hippies**: "Herbert, Herbert, Herbert ..."  
**Spock**: "Herbert was a minor official notorious for his rigid and limited patterns of thought."  
**Kirk**: "Well, I shall try to be less rigid in my thinking."  

video clip:  http://www.youtube.com/watch?v=PQONBf9xMss

[![Way to Eden](img/way-to-eden2.jpg)](http://www.youtube.com/watch?v=PQONBf9xMss)

## Copyright and License

Copyright (c) Stephen E. Miner, 2013. All rights reserved.
The use and distribution terms for this software are covered by the
Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
which can be found in the file epl-v10.html at the root of this distribution.
By using this software in any fashion, you are agreeing to be bound by
the terms of this license.
You must not remove this notice, or any other, from this software.
