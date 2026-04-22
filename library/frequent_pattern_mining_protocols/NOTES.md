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


`frequent_pattern_mining_protocols`
===================================

This library provides support entities for frequent itemset mining
algorithms. Transactional datasets are represented as objects
implementing the `transaction_dataset_protocol` protocol. The generic
`pattern_miner_protocol` protocol and the `pattern_miner_common`
category used by concrete miners are loaded from the
`pattern_mining_protocols` core library.

The `frequent_pattern_mining_common` category builds on that generic
core with frequent-itemset-specific helpers for dataset validation,
support accumulation, and itemset ordering/filtering.

This library also provides reusable transaction smoke-test datasets and
a small smoke-test suite.


API documentation
-----------------

Open the [../../apis/library_index.html#frequent_pattern_mining_protocols](../../apis/library_index.html#frequent_pattern_mining_protocols)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(frequent_pattern_mining_protocols(loader)).


Testing
-------

To run the library smoke tests, load the `tester.lgt` file:

	| ?- logtalk_load(frequent_pattern_mining_protocols(tester)).


Test datasets
-------------

Several sample transaction datasets are included in the `test_datasets`
directory:

- `market_basket_basics.lgt`: A compact transaction dataset with 6
  transactions and 5 items intended for frequent-itemset smoke tests.

- `layered_baskets.lgt`: A transaction dataset with overlapping
  co-occurrence layers intended for support-count and candidate-pruning
  tests.

- `deep_intersection_baskets.lgt`: A compact transaction dataset with one
  frequent length-4 itemset and multiple overlapping length-3 itemsets
  intended to stress deeper vertical tidset intersections.

The directory also includes invalid fixtures useful for validation and
error-handling tests:

- `invalid_undeclared_item_baskets.lgt`: Uses an item not listed in the
  declared item domain.

- `invalid_unsorted_transaction_baskets.lgt`: Uses a transaction whose
  items are not in canonical sorted order.

- `invalid_duplicate_item_baskets.lgt`: Uses a transaction with a
  duplicate item.

- `invalid_empty_baskets.lgt`: Declares an item domain but no
  transactions.

- `invalid_item_domain_baskets.lgt`: Declares a non-canonical item
  domain with duplicate items.
