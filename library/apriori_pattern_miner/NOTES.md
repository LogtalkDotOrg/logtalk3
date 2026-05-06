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


`apriori_pattern_miner`
=========

Apriori frequent itemset miner for transaction datasets. The library
depends on the `frequent_pattern_mining_protocols` support library,
implements the generic `pattern_miner_protocol` defined in the
`pattern_mining_protocols` core library, and mines frequent itemsets
using deterministic level-wise candidate generation and anti-monotone
pruning with one transaction rescan per candidate level using a
candidate hash tree backed by keyed bucket dictionaries. Requires a
dataset implementing `transaction_dataset_protocol` with transactions
represented as canonical sorted lists of unique declared items.


API documentation
-----------------

Open the [../../apis/library_index.html#apriori_pattern_miner](../../apis/library_index.html#apriori_pattern_miner)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(apriori_pattern_miner(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(apriori_pattern_miner(tester)).


Features
--------

- **Deterministic Level-Wise Mining**: Builds frequent itemsets level by level by generating deterministic candidate combinations, pruning candidates whose subsets are infrequent, and rescanning transactions once per level to compute support counts for all candidates using a candidate hash tree.
- **Candidate Hash Tree Counting**: Counts supports for an entire candidate level by traversing a hash tree with keyed bucket and item dictionaries instead of linearly scanning bucket lists for every transaction.
- **Library Hashing**: Uses the `hashes` library `fnv1a_32` object to hash candidate items instead of relying on an ad hoc local hash function.
- **Apriori Join Step**: Generates level candidates by pairwise joins of the previous frequent itemsets with shared prefixes.
- **Apriori Pruning**: Rejects candidate itemsets whose immediate subsets are not all frequent using ordered subset checks over the previous level.
- **Canonical Transactions**: Validates that transactions are sorted, duplicate-free, and restricted to declared items.
- **Flexible Support Thresholds**: Supports relative minimum support and absolute minimum support count.
- **Model Export**: Mined pattern collections can be exported as predicate clauses or written to a file.


Options
-------

The `mine/3` predicate accepts the following options:

- `minimum_support/1`: Relative minimum support threshold in the interval `]0.0, 1.0]`. The default is `0.5`.
- `minimum_support_count/1`: Absolute minimum support count. When both support options are provided, this option takes precedence.
- `maximum_pattern_length/1`: Maximum itemset length to mine. The default is `1000`, which is effectively capped by the longest transaction in the dataset.
- `minimum_pattern_length/1`: Minimum itemset length retained in the mined result. The default is `1`.


Pattern miner representation
----------------------------

The mined pattern miner result is represented by a compound term with
the functor chosen by the implementation and arity 3. For example:

	apriori_pattern_miner(ItemDomain, Patterns, Options)

Where:

- `ItemDomain`: Canonical sorted list of declared dataset items.
- `Patterns`: List of `itemset(Items, SupportCount)` terms ordered first by pattern length and then lexicographically.
- `Options`: Effective mining options used to mine the frequent itemsets.


References
----------

1. Agrawal, R. and Srikant, R. (1994) - "Fast algorithms for mining association rules in large databases".
