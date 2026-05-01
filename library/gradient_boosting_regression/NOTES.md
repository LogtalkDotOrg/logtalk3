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


`gradient_boosting_regression`
==============================

Gradient boosting regression supporting continuous and mixed-feature
datasets. The library implements the `regressor_protocol` defined in the
`regression_protocols` library and learns an additive ensemble of
regression trees fitted to successive residuals.


API documentation
-----------------

Open the [../../apis/library_index.html#gradient_boosting_regression](../../apis/library_index.html#gradient_boosting_regression)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(gradient_boosting_regression(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(gradient_boosting_regression(tester)).

To run the reference timing and fit benchmarks, load the `tester_performance.lgt` file:

	| ?- logtalk_load(gradient_boosting_regression(tester_performance)).


Features
--------

- **Residual Fitting**: Fits each regression tree to the residuals of the current additive model under squared-error loss.
- **Shrinkage**: Supports a fixed learning rate to scale stage contributions.
- **Continuous and Mixed Features**: Supports continuous attributes and categorical attributes encoded by the underlying regression-tree learner.
- **Tree Configuration**: Exposes the underlying regression-tree depth, minimum-leaf, variance-reduction, and scaling options.
- **Diagnostics Metadata**: Learned regressors record model name, target, training example count, initial prediction, fitted stage count, and effective options, accessible using the shared regression diagnostics predicates.
- **Model Export**: Learned regressors can be exported as predicate clauses or written to a file.
- **Reference Benchmarks**: Includes a dedicated performance suite reporting training time, RMSE, and MAE for representative regression datasets.


Regressor representation
------------------------

The learned regressor is represented by default as:

- `gradient_boosting_regressor(InitialPrediction, WeightedTrees, Diagnostics)`

The exported predicate clauses therefore use the shape:

- `Functor(InitialPrediction, WeightedTrees, Diagnostics)`


Options
-------

The `learn/3` predicate accepts the following options:

- `number_of_estimators/1`: Maximum number of boosting stages to fit. Each stage adds one regression tree to the ensemble. The default is `50`. Training can stop before reaching this limit when the residual sum of squares becomes negligible.
- `learning_rate/1`: Shrinkage factor applied to each stage prediction before it is added to the current model. Smaller values usually require more stages but can improve generalization. The default is `0.1`.
- `maximum_depth/1`: Maximum depth allowed for each regression tree used as a base learner. Lower values produce weaker, simpler trees; higher values allow each stage to model more complex residual structure. The default is `3`.
- `minimum_samples_leaf/1`: Minimum number of training examples required in each leaf of a base learner tree. Increasing this value makes the fitted trees more conservative and can reduce overfitting. The default is `1`.
- `minimum_variance_reduction/1`: Minimum reduction in target variance required to accept a split when fitting a base learner tree. Larger values make tree growth stricter by rejecting weak splits. The default is `0.0`.
- `feature_scaling/1`: Controls continuous-feature scaling in the underlying regression-tree learner. The accepted values are `true` and `false`. The default is `false`.
