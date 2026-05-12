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


`arrangements`
==============

This library provides predicates for generating and querying arrangements
over lists. Arrangements are ordered selections of a given length with
element repetition allowed. The following categories of predicates are
provided:

* **Generation operations** - Predicates for generating arrangements.
* **Generalized Cartesian products** - Predicates for generating tuples by selecting one element from each of several lists.
* **Ordering variants** - Predicates that support an additional order argument (`default` or `lexicographic`) for controlling output order.
* **Distinct-value generation** - Predicates for generating arrangements while deduplicating equal-valued results.
* **Indexed access** - Predicates for direct access to arrangements at specific positions, including distinct arrangements.
* **Lexicographic stepping** - Predicates for navigating arrangements in lexicographic order.
* **Counting operations** - Predicates for counting arrangements and distinct arrangements.
* **Random selection** - Predicates for randomly selecting and sampling arrangements and distinct arrangements.

Dedicated `permutations`, `combinations`, `multisets`, and `subsequences`
libraries are also available for focused APIs on related operations.


API documentation
-----------------

Open the [../../apis/library_index.html#arrangements](../../apis/library_index.html#arrangements)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(arrangements(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(arrangements(tester)).
