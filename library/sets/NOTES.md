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


`sets`
======

This library provides a set protocol and two implementations of this protocol
using ordered lists, one of them a parametric object that takes the type of
the set elements as a parameter. Although representing sets as ordered lists
is a common representation, is best practice to regard sets as opaque terms
and only access them using the library predicates.


API documentation
-----------------

Open the [../../docs/library_index.html#sets](../../docs/library_index.html#sets)
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
	Set = [1, 2, 3
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

For details on these and other provided predicates, consult the library
API documentation.


Credits
-------

Some predicates adapted from code authored by Richard O'Keefe.
