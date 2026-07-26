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


`integer_partitions`
====================

This library provides predicates for generating and querying integer
partitions. An integer partition of a non-negative integer N is a
non-increasing list of positive integers (its parts) that sum to N.
Zero has exactly one partition, represented by the empty list of parts.
The following categories of predicates are provided:

* **Generation operations** - Predicates for generating all partitions of an integer and partitions with an exact number of parts.
* **Ordering variants** - Predicates that support an additional order argument (`default` or `lexicographic`) for controlling output order.
* **Distinct-parts generation** - Predicates for generating partitions into pairwise distinct (non-repeating) parts, also known as strict partitions.
* **Indexed access** - Predicates for retrieving partitions and distinct partitions by zero-based index and for recovering the index of a partition.
* **Random selection** - Predicates for selecting random partitions and taking random samples, with distinct-parts and exact-part-count variants.
* **Lexicographic stepping** - Predicates for moving to the next or previous partition value in lexicographic order.
* **Counting operations** - Predicates for counting partitions and distinct partitions, including exact-part counts.

Because an integer, unlike a list, has no position-sensitive structure,
every generated partition is already a canonical value (its parts in
non-increasing order); there is no analogue of the position-sensitive
duplicates that the `partitions` library's distinct predicates collapse.
Here, "distinct" is instead the standard integer-partition notion of a
partition whose parts are pairwise different (strict partitions), and
`count_distinct_partitions/2` corresponds to the OEIS A000009 sequence,
just as `count_partitions/2` corresponds to A000041 (via the `natural`
object's `partition_number/2`).

Also note that, unlike the `partitions` library (where an exact-count
constraint can occupy the first argument position, distinguished from
the input list by type), here the integer being partitioned and an
exact part count are both integers. The part count is therefore always
given in the second argument position, distinguished from the order
argument by type (integer versus atom).

Dedicated `partitions` (set partitions), `combinations`, `permutations`,
`derangements`, `multisets`, `arrangements`, `cartesian_products`, and
`subsequences` libraries are also available for related operations.


API documentation
-----------------

Open the [../../apis/library_index.html#integer_partitions](../../apis/library_index.html#integer_partitions)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(integer_partitions(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(integer_partitions(tester)).
