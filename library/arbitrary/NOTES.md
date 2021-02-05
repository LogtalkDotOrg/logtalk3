________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


`arbitrary`
===========

The `arbitrary` library defines an `arbitrary` category providing predicates
for generating random values for selected types to the `type` object,
complementing its type checking predicates. Both the object and the category
predicates can be extended by the user with definitions for new types by
defining clauses for multifile predicates. This library is notably used in
the QuickCheck implementation by the `lgtunit` tool.


API documentation
-----------------

Open the [../../docs/library_index.html#arbitrary](../../docs/library_index.html#arbitrary)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(arbitrary(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(coroutining(tester)).


Usage
-----

The `arbitrary` category complements the `type` object and thus its predicates
are accessed via this object. For example:

	| ?- type::arbitrary(integer, Arbitrary).
	Arbitrary = -816
	yes

To define a generator of arbitrary values for a type, define a clause for the
`type::arbitrary/1` multifile predicate specifying the type and a clause for
the `type::arbitrary/2` multifile predicate generating an arbitrary term of
the specified type. For example:

    :- multifile(type::arbitrary/1).
    type::arbitrary(foo).

    :- multifile(type::arbitrary/2).
    type::arbitrary(foo, Arbitrary) :-
        ...

Optionally, define a clause for the `type::shrink/3` multifile predicate
for shrinking arbitrary values for QuickCheck usage. For example:

    :- multifile(type::shrink/3).
    type::shrink(foo, Large, Small) :-
        ...

It is also possible to define edge cases for a given type for use with
QuickCheck implementations. For example:

	:- multifile(edge_case/2).
    type::edge_case(cost, 0).

Edge cases are usually tried before resorting to generating arbitrary values
for a type.

The `arbitrary` category also provides access to the pseudo-random generator
it uses via the `get_seed/1` and `set_seed/1`. This allows sequences of
arbitrary values to be reproduced. For example:

	| ?- type::get_seed(Seed).
	Seed = seed(3172, 9814, 20125)
	yes

	| ?- type::arbitrary(integer, Arbitrary).
	Arbitrary = -816
	yes

	| ?- type::arbitrary(integer, Arbitrary).
	Arbitrary = -113
	yes

	| ?- type::arbitrary(integer, Arbitrary).
	Arbitrary = 446

	| ?- type::set_seed(seed(3172, 9814, 20125)).
	yes

	| ?- type::arbitrary(integer, Arbitrary).
	Arbitrary = -816
	yes

	| ?- type::arbitrary(integer, Arbitrary).
	Arbitrary = -113
	yes

	| ?- type::arbitrary(integer, Arbitrary).
	Arbitrary = 446
	yes

The seed should be regarded as an opaque term and handled using the
`get_seed/1` and `set_seed/1` predicates. These predicates are notably used
in the QuickCheck implementation provided by the `lgtunit` tool.

Examples
--------

See the implementation of the `optionals` and `expecteds` libraries.


Known issues
------------

Some Prolog systems either don't support the null character or provide buggy
results when calling `char_code/2` with a code of zero. When that's the case,
the null character is excluded when generating arbitrary characters or
character codes.
