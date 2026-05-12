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


`derangements`
==============

This library provides predicates for generating and querying derangements over
lists. Derangements are full-length permutations where no result element is
identical to the input element at the same position. The following categories
of predicates are provided:

* **Generation operations** - Predicates for generating derangements.
* **Ordering variants** - Predicates that support an additional order argument (`default`, `lexicographic`, or `shortlex`) for controlling output order.
* **Distinct-value generation** - Predicates for generating derangements while deduplicating equal-valued results.
* **Indexed access** - Predicates for direct access to derangements at specific positions, including distinct derangements.
* **Lexicographic stepping** - Predicates for navigating derangements in lexicographic order.
* **Counting operations** - Predicates for counting derangements and distinct derangements.
* **Random selection** - Predicates for randomly selecting and sampling derangements and distinct derangements.

Dedicated `permutations`, `arrangements`, `combinations`, `multisets`, and
`subsequences` libraries are also available for focused APIs on related
operations.


API documentation
-----------------

Open the [../../apis/library_index.html#derangements](../../apis/library_index.html#derangements)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(derangements(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(derangements(tester)).