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


`spade`
=======

SPADE sequential pattern miner for sequence datasets. The library
depends on the `sequential_pattern_mining_protocols` support library,
implements the generic `pattern_miner_protocol` defined in the
`pattern_mining_protocols` core library, and mines frequent sequential
patterns using Zaki's equivalence-class decomposition with temporal
joins over vertical occurrence lists keyed by sequence and event
identifiers.

Requires a dataset implementing `sequence_dataset_protocol` with sequences
represented as ordered lists of canonical sorted itemsets over a declared
item domain.


API documentation
-----------------

Open the [../../apis/library_index.html#spade](../../apis/library_index.html#spade)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(spade(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(spade(tester)).


Features
--------

- **Equivalence-Class Decomposition**: Mines frequent sequences using Zaki's equivalence-class decomposition over prefix-sharing classes.
- **Vertical Occurrence Lists**: Represents frequent patterns using sequence and event occurrence lists.
- **Active Class Candidate Pruning**: Restricts class joins to members that occur in the supporting sequences of the current prefix, using per-sequence member-id indexes instead of storing full pattern terms in the sequence index.
- **Same-Event and Sequence Extensions**: Supports both itemset growth and next-event sequence growth.
- **Canonical Sequences**: Validates that itemsets are sorted, duplicate-free, non-empty, and restricted to declared items.
- **Flexible Support Thresholds**: Supports relative minimum support and absolute minimum support count.
- **Model Export**: Mined pattern collections can be exported as predicate clauses or written to a file.


Options
-------

The `mine/3` predicate accepts the following options:

- `minimum_support/1`: Relative minimum support threshold in the interval `]0.0, 1.0]`. The default is `0.5`.
- `minimum_support_count/1`: Absolute minimum support count. When both support options are provided, this option takes precedence.
- `maximum_pattern_length/1`: Maximum total number of items in a mined sequential pattern. The default is `1000`, effectively capped by the longest sequence in the dataset.
- `minimum_pattern_length/1`: Minimum total number of items retained in the mined result. The default is `1`.


Pattern miner representation
----------------------------

The mined pattern miner result is represented by a compound term with
the functor chosen by the implementation and arity 3. For example:

	spade_pattern_miner(ItemDomain, Patterns, Options)

Where:

- `ItemDomain`: Canonical sorted list of declared dataset items.
- `Patterns`: List of `sequence_pattern(Pattern, SupportCount)` terms ordered first by total item count and then lexicographically.
- `Options`: Effective mining options used to mine the frequent sequential patterns.


References
----------

1. Zaki, M. J. (2001) - "SPADE: An efficient algorithm for mining frequent sequences".
