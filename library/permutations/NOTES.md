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


`permutations`
==============

This library provides predicates for generating and querying permutations over
lists. The following categories of predicates are provided:

* **Generation operations** - Predicates for generating permutations, k-permutations, and Cartesian products.
* **Ordering variants** - Predicates that support an additional order argument (`default`, `lexicographic`, or `shortlex`) for controlling output order.
* **Distinct-value generation** - Predicates for generating permutations while deduplicating equal-valued results.
* **Derangement operations** - Predicates for generating and testing derangements.
* **Lexicographic stepping** - Predicates for navigating permutations in lexicographic order.
* **Indexed access** - Predicates for direct access to permutations at specific positions.
* **Counting operations** - Predicates for counting permutations.
* **Random selection** - Predicates for randomly selecting permutations.

Dedicated `combinations` and `subsequences` libraries are also available for
focused APIs on those operations.


API documentation
-----------------

Open the [../../apis/library_index.html#permutations](../../apis/library_index.html#permutations)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(permutations(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(permutations(tester)).
