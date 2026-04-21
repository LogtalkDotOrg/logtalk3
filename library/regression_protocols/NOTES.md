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


`regression_protocols`
======================

This library provides protocols used in the implementation of machine
learning regression algorithms. Datasets are represented as objects
implementing the `regression_dataset_protocol` protocol. Regressors are
represented as objects importing the `regressor_common` category. This
category provides shared helpers for regressor defaults, dataset
validation, export, and pretty-printing support.

This library also provides regression test datasets under the
`test_datasets` directory.


API documentation
-----------------

Open the [../../apis/library_index.html#regression_protocols](../../apis/library_index.html#regression_protocols)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(regression_protocols(loader)).


Testing
-------

To test this library predicates, shared categories, and datasets, load the
`tester.lgt` file:

  | ?- logtalk_load(regression_protocols(tester)).


Test datasets
-------------

The `test_datasets` directory includes the following compact regression
datasets:

- `intercept_only.lgt`: constant-target dataset for intercept-learning tests.
- `invalid_target.lgt`: invalid dataset with a non-numeric target for negative tests.
- `mixed_signal.lgt`: mixed continuous and categorical regression dataset.
- `plane.lgt`: two-feature regression dataset following the plane `z = 3x1 - 2x2 + 5`.
- `simple_line.lgt`: single-feature regression dataset following the line `y = 2x + 1`.
- `step_signal.lgt`: piecewise-constant regression dataset for tree-based and neighborhood regressor testing.
