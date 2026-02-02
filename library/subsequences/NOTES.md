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


`subsequences`
==============

This library provides predicates for working with subsequences represented
using lists. The following categories of predicates are provided:

* **Generation operations** - Predicates for generating all subsequences or variants thereof.
* **Ordering variants** - Predicates that support an additional order argument (`default`, `lexicographic`, or `shortlex`) for controlling output order.
* **Filtered generation** - Predicates for generating specific types of subsequences (combinations, permutations).
* **Indexed access** - Predicates for direct access to subsequences at specific positions.
* **Searching and matching** - Predicates for finding specific subsequences with desired properties.
* **Prefix and suffix operations** - Predicates for checking and finding prefixes and suffixes.
* **Contiguous subsequences** - Predicates for working with contiguous subsequences (subslices, sliding windows).
* **Random selection** - Predicates for randomly selecting subsequences.
* **Constrained operations** - Predicates for generating subsequences with specific constraints.
* **Utility predicates** - Helper predicates for subsequence operations.


API documentation
-----------------

Open the [../../apis/library_index.html#subsequences](../../apis/library_index.html#subsequences)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(subsequences(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(subsequences(tester)).
