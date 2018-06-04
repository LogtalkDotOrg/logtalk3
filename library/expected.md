________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

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


About
-----

This library provides an implementation of *expected* terms with an API that
is inspired by the `optional` library and C++ standardization proposals for
an `Expected<T>` type.


Loading
-------

To load all entities in this library load the `expected_loader.lgt` utility
file:

	| ?- logtalk_load(library(expected_loader)).


Usage
-----

The `expected` object provides constructors for expected term references. For
example:

	| ?- expected::of_expected(1, Ref).
	...

The created expected term references can then be passed as parameters to the
`expected/1` parametric object. For example:

	| ?- expected::of_expected(1, Ref), expected(Ref)::or_else(Term, 0).
	Ref = expected(1),
	Term = 1
	yes

	| ?- expected::of_unexpected(-1, Ref), expected(Ref)::or_else(Term, 0).
	Ref = unexpected(-1),
	Term = 0
	yes

The `either` object provides types and predicates for extended type-checking
and predicate for handling lists of expected terms.
