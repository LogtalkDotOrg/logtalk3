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


`combinations`
==============

This library provides predicates for generating and querying combinations over
lists. The following categories of predicates are provided:

* **Generation operations** - Predicates for generating combinations, including combinations with replacement.
* **Ordering variants** - Predicates that support an additional order argument (`default`, `lexicographic`, or `shortlex`) for controlling output order.
* **Distinct-value generation** - Predicates for generating combinations while deduplicating equal-valued results.
* **Indexed access** - Predicates for direct access to combinations at specific positions.
* **Counting operations** - Predicates for counting combinations with and without replacement.
* **Random selection** - Predicates for randomly selecting combinations.

Dedicated `subsequences` and `permutations` libraries are also available for
focused APIs on those operations.


API documentation
-----------------

Open the [../../apis/library_index.html#combinations](../../apis/library_index.html#combinations)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(combinations(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(combinations(tester)).
