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


`lasso_regression`
==================

Lasso regression regressor supporting continuous and mixed-feature
datasets. The library implements the `regressor_protocol` defined in the
`regression_protocols` library and learns a linear model using cyclic
coordinate descent with L1 regularization and soft-thresholding updates.


API documentation
-----------------

Open the [../../apis/library_index.html#lasso_regression](../../apis/library_index.html#lasso_regression)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(lasso_regression(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(lasso_regression(tester)).

To run the performance benchmark suite, load the `tester_performance.lgt`
file:

	| ?- logtalk_load(lasso_regression(tester_performance)).


Features
--------

- **Continuous and Mixed Features**: Supports continuous attributes and categorical attributes encoded using one-hot vectors.
- **Feature Scaling**: Continuous attributes can be standardized using z-score scaling.
- **Missing Values**: Missing numeric and categorical values are encoded using explicit missing-value indicator features.
- **Unknown Values**: Prediction requests containing categorical values that are not declared by the dataset raise a domain error.
- **Coefficient-wise L1 Shrinkage**: Applies soft-thresholding updates independently to every encoded feature, including categorical dummy and missing-indicator features.
- **Diagnostics Metadata**: Learned regressors record model name, target, training example count, optimization stop reason, completed iterations, final parameter delta, encoded feature count, and effective options, accessible using the shared regression diagnostics predicates.
- **Model Export**: Learned regressors can be exported as predicate clauses or written to a file.
- **Reference Benchmarks**: Includes a dedicated performance suite reporting training time, RMSE, and MAE for representative regression datasets.


Regressor representation
------------------------

The learned regressor is represented by default as:

- `lasso_regressor(Encoders, Bias, Weights, Diagnostics)`

The exported predicate clauses therefore use the shape:

- `Functor(Encoders, Bias, Weights, Diagnostics)`


Diagnostics syntax
------------------

The `diagnostics/2` predicate returns a list of metadata terms with the form:

	[
		model(lasso_regression),
		target(Target),
		training_example_count(TrainingExampleCount),
		options(Options),
		convergence(Status),
		iterations(Iterations),
		final_delta(FinalDelta),
		encoded_feature_count(FeatureCount)
	]

Where:

- `model(lasso_regression)` identifies the learning algorithm that produced the regressor.
- `target(Target)` stores the target attribute name declared by the training dataset.
- `training_example_count(TrainingExampleCount)` stores the number of examples used during training.
- `options(Options)` stores the effective learning options after merging the user options with the library defaults.
- `convergence(Status)` records the optimization stop condition. The current values are `tolerance` when the maximum Karush-Kuhn-Tucker optimality violation across the intercept and all encoded features is within the configured tolerance and `maximum_iterations_exhausted` when training stops because the iteration cap is reached.
- `iterations(Iterations)` stores the number of coordinate-descent sweeps completed during training.
- `final_delta(FinalDelta)` stores the maximum Karush-Kuhn-Tucker optimality violation measured during the final optimization check.
- `encoded_feature_count(FeatureCount)` stores the number of numeric features induced by the encoder list, including missing-value indicator features.

Use the `regression_protocols` `diagnostic/2` and `regressor_options/2` helper predicates when you only need a single metadata term or the effective options.

Options
-------

The `learn/3` predicate accepts the following options:

- `maximum_iterations/1`: Maximum number of coordinate-descent sweeps to run before stopping even if the tolerance criterion has not been met. The default is `2000`.
- `tolerance/1`: Convergence threshold for the maximum Karush-Kuhn-Tucker optimality violation in a full coordinate-descent sweep. Training stops early when both the intercept condition and all encoded-feature subgradient conditions are satisfied within this value. The default is `1.0e-7`.
- `regularization/1`: L1 penalty coefficient applied independently to every encoded feature during optimization. Higher values increase shrinkage and can reduce overfitting. The default is `0.01`.
- `feature_scaling/1`: Controls z-score standardization of continuous attributes before training and prediction. Accepted values are `true` and `false`. The default is `true`.
