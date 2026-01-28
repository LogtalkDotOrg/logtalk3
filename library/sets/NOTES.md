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


`sets`
======

This library provides a set protocol, two implementations of this protocol
using *ordered lists* (one of them a parametric object that takes the type
of the set elements as a parameter), and an implementation using *treaps*
(tree heaps).

The set representations should be regarded as *opaque terms* and only
constructed, accessed, and updated them using the library predicates.

For small sets, the ordered list implementations are likely to provide the
best performance. For larger sets, the treap implementation likely provides
better performance, notably for the `memberchk/2`, `insert/3`, and `delete/3`
operations. Benchmark both implementations to select the best one for your
application.

The current implementations use `==/2` for element comparison and standard
term ordering. This allows non-ground set elements. But requires caution with
later unifications with output arguments and when using the `member/2` and
`select/3` predicates, which can break the ordered representation. Note also
that, per the ISO Prolog Core Standard, variable ordering is implementation
dependent. This can result in unexpected results and portability issues.


API documentation
-----------------

Open the [../../apis/library_index.html#sets](../../apis/library_index.html#sets)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(sets(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(sets(tester)).


Usage
-----

First, select a set implementation. Use the `set(Type)` object if you want
to type-check the set elements. Otherwise, use the `set` object.

To create a new set, you can use the `new/1` predicate. For example:

	| ?- set::new(Set).
	Set = []
	yes

You can also create a new set with all unique elements from a list of terms
by using the `as_set/2` predicate. For example:

	| ?- set::as_set([1,3,2,1,2], Set).
	Set = [1, 2, 3]
	yes

Predicates are provided for the most common set operations. For example:

	| ?- set::(
			as_set([1,3,2,1,2], Set1),
			as_set([7,4,2,5,1], Set2),
			intersection(Set1, Set2, Intersection),
			symdiff(Set1, Set2, Difference)
		).
	Set1 = [1, 2, 3],
	Set2 = [1, 2, 4, 5, 7],
	Intersection = [1, 2],
	Difference = [3, 4, 5, 7]
	yes

When working with a custom type of set elements and the ordered list
representation, the corresponding object must implement the `comparingp`
protocol. For example:

	:- object(rainbow_colors,
		implements(comparingp)).

		order(red,    1).
		order(orange, 2).
		order(yellow, 3).
		order(green,  4).
		order(blue,   5).
		order(indigo, 6).
		order(violet, 7).

		Color1 < Color2 :-
			order(Color1, N1),
			order(Color2, N2),
			{N1 < N2}.

		Color1 =< Color2 :-
			order(Color1, N1),
			order(Color2, N2),
			{N1 =< N2}.

		...

	:- end_object.

We can then use this object with the `set/1` parametric object. For example:

	| ?- set(rainbow_colors)::as_set([blue, yellow, violet], Set).
	Set = [yellow, blue, violet]
	yes

For details on these and other provided predicates, consult the library
API documentation.


Credits
-------

Some predicates adapted from code authored by Richard O'Keefe.
