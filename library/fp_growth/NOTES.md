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


`fp_growth`
===========

FP-growth frequent itemset miner for transaction datasets. The library
depends on the `frequent_pattern_mining_protocols` support library,
implements the generic `pattern_miner_protocol` defined in the
`pattern_mining_protocols` core library, and mines frequent itemsets
using recursive conditional pattern-base projection over a compact
FP-tree.


API documentation
-----------------

Open the [../../apis/library_index.html#fp_growth](../../apis/library_index.html#fp_growth)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(fp_growth(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(fp_growth(tester)).


Features
--------

- **FP-tree Construction**: Builds a compact prefix tree from frequent items ordered by global support.
- **Pattern Growth**: Mines frequent itemsets recursively from conditional pattern bases without candidate generation.
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

	fp_growth_pattern_miner(ItemDomain, Patterns, Options)

Where:

- `ItemDomain`: Canonical sorted list of declared dataset items.
- `Patterns`: List of `itemset(Items, SupportCount)` terms ordered first by pattern length and then lexicographically.
- `Options`: Effective mining options used to mine the frequent itemsets.


References
----------

1. Han, J., Pei, J., and Yin, Y. (2000) - "Mining frequent patterns without candidate generation".
