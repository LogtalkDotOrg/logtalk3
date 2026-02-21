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


`optionals`
===========

This library provides an implementation of *optional terms* with an API
modeled after the Java 8 `Optional` class (originally due to requests by
users working in Logtalk/Java hybrid applications). An optional term is
an opaque compound term that may or may not hold a value. Optional terms
avoid forcing the user to define a representation for the absence of a value
by providing an API with predicates that depend on the presence or absence
of a value. Optional terms also allow separating the code that constructs
optional terms from the code that processes them, which is then free to
deal if necessary and at its convenience with any case where the values
held by optional terms are not present.


API documentation
-----------------

Open the [../../apis/library_index.html#optionals](../../apis/library_index.html#optionals)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(optionals(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(optionals(tester)).


Usage
-----

The `optional` object provides constructors for optional terms. For example:

	| ?- optional::of(1, Optional).
	...

The created optional terms can then be passed as parameters to the `optional/1`
parametric object. For example:

	| ?- optional::of(1, Optional), optional(Optional)::or_else(Term, 0).
	Optional = optional(1),
	Term = 1
	yes

	| ?- optional::empty(Optional), optional(Optional)::or_else(Term, 0).
	Optional = empty,
	Term = 0
	yes

The `maybe` object provides types and predicates for type-checking of the
term held by optional terms. It also provides some predicates for handling
lists of optional terms, including `sequence/2` and `traverse/3`.

The `optional/1` parametric object also provides `map_or_else/3` for
applying a closure to the value held by the optional term if not empty,
returning a default value otherwise:

	| ?- optional::of(a, Optional),
	|    optional(Optional)::map_or_else(char_code, 0, Value).
	Value = 97
	yes

	| ?- optional::empty(Optional),
	|    optional(Optional)::map_or_else(char_code, 0, Value).
	Value = 0
	yes

The `zip/3` predicate combines two optional terms using a closure when
both are not empty:

	| ?- optional::of(1, O1), optional::of(3, O2),
	|    optional(O1)::zip([X,Y,Z]>>(Z is X+Y), O2, NewOptional).
	NewOptional = optional(4)
	yes

	| ?- optional::of(1, O1), optional::empty(O2),
	|    optional(O1)::zip([X,Y,Z]>>(Z is X+Y), O2, NewOptional).
	NewOptional = empty
	yes

The `flatten/1` predicate unwraps a nested optional term:

	| ?- optional::of(1, Inner), optional::of(Inner, Outer),
	|    optional(Outer)::flatten(NewOptional).
	NewOptional = optional(1)
	yes

	| ?- optional::empty(Inner), optional::of(Inner, Outer),
	|    optional(Outer)::flatten(NewOptional).
	NewOptional = empty
	yes

The `to_expected/2` predicate converts an optional to an expected term:

	| ?- optional::of(1, Optional),
	|    optional(Optional)::to_expected(missing, Expected).
	Expected = expected(1)
	yes

	| ?- optional::empty(Optional),
	|    optional(Optional)::to_expected(missing, Expected).
	Expected = unexpected(missing)
	yes

The `from_goal/3` and `from_goal/2` constructors silently convert goal
exceptions to empty optional terms. Use `from_goal_or_throw/3` or
`from_goal_or_throw/2` if exceptions should be propagated instead:

	| ?- optional::from_goal_or_throw(Y is 1+2, Y, Optional).
	Optional = optional(3)
	yes

	| ?- optional::from_goal_or_throw(2 is 3, _, Optional).
	Optional = empty
	yes

	| ?- catch(
	|      optional::from_goal_or_throw(Y is _, Y, _),
	|      Error,
	|      true
	|    ).
	Error = error(instantiation_error, ...)
	yes

Examples:

	| ?- optional::of(1, O1), optional::of(2, O2),
	|    maybe::sequence([O1, O2], Optional).
	Optional = optional([1,2])
	yes

	| ?- maybe::traverse({optional}/[X,O]>>optional::of(X, O), [1,2], Optional).
	Optional = optional([1,2])
	yes

	| ?- maybe::traverse({optional}/[X,O]>>(
	|      integer(X) -> optional::of(X, O)
	|    ; optional::empty(O)
	|    ), [1,a,2], Optional).
	Optional = empty
	yes

	| ?- optional::of(1, O1), optional::empty(O2),
	|    maybe::sequence([O1, O2], Optional).
	Optional = empty
	yes


See also
--------

The `expecteds` library.
