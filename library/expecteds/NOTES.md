________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>  
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
for a value. This allows e.g. separating the code that constructs expected
terms from the code that processes them, which is then free to deal if
necessary and at its convenience with any unexpected events.


API documentation
-----------------

Open the [../../docs/library_index.html#expecteds](../../docs/library_index.html#expecteds)
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
and predicates for handling lists of expected terms.


See also
--------

The `optionals` library.
