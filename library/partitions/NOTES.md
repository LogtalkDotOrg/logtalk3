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


`partitions`
============

This library provides predicates for generating and querying set partitions of
lists. A set partition divides a list into non-empty, non-overlapping blocks
whose union is the original list. The following categories of predicates are
provided:

* **Generation operations** - Predicates for generating all set partitions and set partitions with an exact number of blocks.
* **Ordering variants** - Predicates that support an additional order argument (`default` or `lexicographic`) for controlling output order.
* **Distinct-value generation** - Predicates for generating set partitions while deduplicating equal-valued partitions after canonicalizing block order.
* **Indexed access** - Predicates for retrieving partitions and distinct partitions by zero-based index and for recovering the index of a partition.
* **Random selection** - Predicates for selecting random partitions and taking random samples, with distinct-value and exact-block-count variants.
* **Lexicographic stepping** - Predicates for moving to the next or previous distinct partition value in lexicographic order.
* **Counting operations** - Predicates for counting set partitions and distinct set partitions, including exact-block counts.

Base predicates are position-sensitive, matching the existing combinatorics
libraries. Inputs with repeated values can therefore yield duplicate-valued
partitions. Distinct predicates collapse those equal-valued results.

The lexicographic stepping predicates operate on the distinct partition view,
using the same canonical block ordering as the distinct generation predicates.

The counting predicates are connected to Bell numbers (for all partitions of an
N-element list) and Stirling numbers of the second kind (for partitions with an
exact number of blocks).

Dedicated `arrangements`, `cartesian_products`, `combinations`,
`derangements`, `multisets`, `permutations`, and `subsequences`
libraries are also available for focused APIs on related operations.


API documentation
-----------------

Open the [../../apis/library_index.html#partitions](../../apis/library_index.html#partitions)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(partitions(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(partitions(tester)).