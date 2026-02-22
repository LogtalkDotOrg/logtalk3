________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________


`expecteds`
===========

This library provides an implementation of *expected terms* with an API
that is inspired by the `optional` library and C++ standardization proposals
for an `Expected<T>` type. An expected term is an opaque compound term that
either contains an expected value or an error informing why the expected
value is not present. Expected terms provide an alternative to generating
an exception (or a failure) when something unexpected happens when asking
for a value. This allows, e.g., separating the code that constructs expected
terms from the code that processes them, which is then free to deal if
necessary and at its convenience with any unexpected events.


API documentation
-----------------

Open the [../../apis/library_index.html#expecteds](../../apis/library_index.html#expecteds)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(expecteds(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(expecteds(tester)).


Usage
-----

The `expected` object provides constructors for expected terms. For example:

	| ?- expected::of_expected(1, Expected).
	...

The created expected terms can then be passed as parameters to the `expected/1`
parametric object. For example:

	| ?- expected::of_expected(1, Expected), expected(Expected)::or_else(Value, 0).
	Expected = expected(1),
	Value = 1
	yes

	| ?- expected::of_unexpected(-1, Expected), expected(Expected)::or_else(Value, 0).
	Expected = unexpected(-1),
	Value = 0
	yes

The `either` object provides types and predicates for extended type-checking
and predicates for handling lists of expected terms, including `sequence/2`
and `traverse/3`.

The `expected/1` parametric object provides `filter/3` for conditionally
rejecting values, converting them to unexpected terms with a given error:

	| ?- expected::of_expected(1, Expected),
	     expected(Expected)::filter(integer, not_integer, NewExpected).
	NewExpected = expected(1)
	yes

	| ?- expected::of_expected(a, Expected),
	     expected(Expected)::filter(integer, not_integer, NewExpected).
	NewExpected = unexpected(not_integer)
	yes

The `map_or_else/3` predicate applies a closure to the value if present,
returning a default value otherwise (symmetric with the optionals library):

	| ?- expected::of_expected(a, Expected),
	     expected(Expected)::map_or_else(char_code, 0, Value).
	Value = 97
	yes

	| ?- expected::of_unexpected(-1, Expected),
	     expected(Expected)::map_or_else(char_code, 0, Value).
	Value = 0
	yes

The `or/2` predicate chains expected terms, returning the current term if
it holds a value, or calling a closure to produce an alternative otherwise:

	| ?- expected::of_expected(1, Expected),
	     expected(Expected)::or(NewExpected, expected::of_expected(2)).
	NewExpected = expected(1)
	yes

	| ?- expected::of_unexpected(-1, Expected),
	     expected(Expected)::or(NewExpected, expected::of_expected(2)).
	NewExpected = expected(2)
	yes

The `zip/3` predicate combines two expected terms using a closure when
both hold values, returning the first error otherwise:

	| ?- expected::of_expected(1, E1), expected::of_expected(3, E2),
	     expected(E1)::zip([X,Y,Z]>>(Z is X+Y), E2, NewExpected).
	NewExpected = expected(4)
	yes

	| ?- expected::of_unexpected(-1, E1), expected::of_expected(3, E2),
	     expected(E1)::zip([X,Y,Z]>>(Z is X+Y), E2, NewExpected).
	NewExpected = unexpected(-1)
	yes

The `map_unexpected/2` predicate transforms the error held by an expected term:

	| ?- expected::of_unexpected(-1, Expected),
	     expected(Expected)::map_unexpected([X,Y]>>(Y is abs(X)), NewExpected).
	NewExpected = unexpected(1)
	yes

The `map_catching/2` predicate applies a closure that may throw an error,
catching it and wrapping it as an unexpected term:

	| ?- expected::of_expected(a, Expected),
	     expected(Expected)::map_catching(char_code, NewExpected).
	NewExpected = expected(97)
	yes

	| ?- expected::of_expected(1, Expected),
	     expected(Expected)::map_catching(char_code, NewExpected),
	     expected(NewExpected)::is_unexpected.
	yes

The `map_both/3` predicate is a bifunctor map that transforms both the
expected value and unexpected error using separate closures:

	| ?- expected::of_expected(1, Expected),
	     expected(Expected)::map_both([X,Y]>>(Y is X+1), [X,Y]>>(Y is abs(X)), NewExpected).
	NewExpected = expected(2)
	yes

The `swap/1` predicate swaps expected and unexpected terms:

	| ?- expected::of_expected(1, Expected),
	     expected(Expected)::swap(NewExpected).
	NewExpected = unexpected(1)
	yes

	| ?- expected::of_unexpected(error, Expected),
	     expected(Expected)::swap(NewExpected).
	NewExpected = expected(error)
	yes

The `flatten/1` predicate unwraps a nested expected term:

	| ?- expected::of_expected(1, Inner), expected::of_expected(Inner, Outer),
	     expected(Outer)::flatten(NewExpected).
	NewExpected = expected(1)
	yes

	| ?- expected::of_unexpected(oops, Inner), expected::of_expected(Inner, Outer),
	     expected(Outer)::flatten(NewExpected).
	NewExpected = unexpected(oops)
	yes

Conversion between expected and optional terms is provided by the
`to_optional/1`, `from_optional/3`, and `optional/1::to_expected/2`
predicates:

	| ?- expected::of_expected(1, Expected),
	     expected(Expected)::to_optional(Optional).
	Optional = optional(1)
	yes

	| ?- expected::of_unexpected(error, Expected),
	     expected(Expected)::to_optional(Optional).
	Optional = empty
	yes

	| ?- optional::of(1, Optional),
	     expected::from_optional(Optional, missing, Expected).
	Expected = expected(1)
	yes

	| ?- optional::empty(Optional),
	     expected::from_optional(Optional, missing, Expected).
	Expected = unexpected(missing)
	yes

Examples:

	| ?- expected::of_expected(1, E1), expected::of_expected(2, E2),
	     either::sequence([E1, E2], Expected).
	Expected = expected([1,2])
	yes

	| ?- either::traverse({expected}/[X,E]>>expected::of_expected(X, E), [1,2], Expected).
	Expected = expected([1,2])
	yes

	| ?- either::traverse({expected}/[X,E]>>(
	       integer(X) -> expected::of_expected(X, E)
	     ; expected::of_unexpected(not_integer(X), E)
	     ), [1,a,2], Expected).
	Expected = unexpected(not_integer(a))
	yes

	| ?- expected::of_expected(1, E1), expected::of_unexpected(e, E2),
	     either::sequence([E1, E2], Expected).
	Expected = unexpected(e)
	yes

See also
--------

The `optionals` and `validations` libraries.
