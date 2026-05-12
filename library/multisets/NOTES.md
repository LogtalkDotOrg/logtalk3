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


`multisets`
===========

This library provides predicates for generating and querying multisets over
lists. Multisets are unordered selections of a given length with element
repetition allowed. The following categories of predicates are provided:

* **Generation operations** - Predicates for generating multisets.
* **Ordering variants** - Predicates that support an additional order argument (`default` or `lexicographic`) for controlling output order.
* **Distinct-value generation** - Predicates for generating multisets while deduplicating equal-valued results.
* **Indexed access** - Predicates for direct access to multisets at specific positions, including order-aware access.
* **Counting operations** - Predicates for counting multisets and distinct multisets.
* **Random selection** - Predicates for randomly selecting and sampling multisets and distinct multisets.
* **Lexicographic stepping** - Predicates for stepping to the next or previous multiset in lexicographic order.

Dedicated `arrangements`, `combinations`, `permutations`, and `subsequences`
libraries are also available for focused APIs on related operations.


API documentation
-----------------

Open the [../../apis/library_index.html#multisets](../../apis/library_index.html#multisets)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(multisets(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(multisets(tester)).
