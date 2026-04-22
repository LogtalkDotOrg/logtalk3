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


`random_forest_regression`
==========================

Random Forest regressor supporting continuous and mixed-feature datasets.
The library implements the `regressor_protocol` defined in the
`regression_protocols` library and learns an ensemble of regression
trees trained on bootstrap samples and random feature subsets.


API documentation
-----------------

Open the [../../apis/library_index.html#random_forest_regression](../../apis/library_index.html#random_forest_regression)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(random_forest_regression(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(random_forest_regression(tester)).

To run the reference timing and fit benchmarks, load the `tester_performance.lgt` file:

	| ?- logtalk_load(random_forest_regression(tester_performance)).


Features
--------

- **Bootstrap Ensembles**: Trains multiple regression trees on bootstrap samples.
- **Random Feature Subsets**: Trains each tree using a random subset of the available dataset attributes.
- **Portable Seeded Sampling**: Uses `fast_random(xoshiro128pp)` so feature subset selection is portable and reproducible.
- **Tree Averaging**: Predicts numeric targets using the arithmetic mean of the tree predictions.
- **Tree Configuration**: Exposes the underlying regression-tree depth, minimum-leaf, variance-reduction, and scaling options.
- **Model Export**: Learned regressors can be exported as predicate clauses or written to a file.
- **Reference Benchmarks**: Includes a dedicated performance suite reporting training time, RMSE, and MAE for representative regression datasets.


Options
-------

The `learn/3` predicate accepts the following options:

- `number_of_trees/1`: Number of regression trees to train in the ensemble. Increasing this value usually improves stability at the cost of additional training and prediction time. The default is `10`.
- `maximum_features_per_tree/1`: Number of dataset attributes randomly selected for each tree before bootstrap training. When omitted, the library uses the square root of the total number of available attributes, with a minimum of one attribute.
- `maximum_depth/1`: Maximum depth allowed for each regression-tree base learner. The default is `10`.
- `minimum_samples_leaf/1`: Minimum number of training examples required in each leaf of a base learner tree. The default is `1`.
- `minimum_variance_reduction/1`: Minimum split gain required by each base learner tree before accepting a partition. The default is `0.0`.
- `feature_scaling/1`: Controls z-score standardization of continuous attributes inside each regression-tree base learner. Accepted values are `true` and `false`. The default is `false`.
- `random_seed/1`: Positive integer seed used by the portable `fast_random(xoshiro128pp)` pseudo-random generator when drawing bootstrap samples and random feature subsets. Using the same seed with the same dataset and options reproduces the same learned regressor. The default is `1357911`.
