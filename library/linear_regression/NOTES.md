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


`linear_regression`
===================

Linear regression regressor supporting continuous and mixed-feature
datasets. The library implements the `regressor_protocol` defined in the
`regression_protocols` library and learns a linear model using a direct
ordinary least-squares solve.


API documentation
-----------------

Open the [../../apis/library_index.html#linear_regression](../../apis/library_index.html#linear_regression)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(linear_regression(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(linear_regression(tester)).

To run the performance benchmark suite, load the `tester_performance.lgt`
file:

	| ?- logtalk_load(linear_regression(tester_performance)).


Features
--------

- **Continuous and Mixed Features**: Supports continuous attributes and categorical attributes encoded using reference-level dummy coding.
- **Feature Scaling**: Continuous attributes can be standardized using z-score scaling.
- **Missing Values**: Missing numeric and categorical values are encoded using explicit missing-value indicator features.
- **Unknown Values**: Prediction requests containing categorical values that are not declared by the dataset raise a domain error.
- **Rank Handling**: Encoded columns that are numerically dependent on the intercept or on earlier selected columns are dropped from the direct solve and assigned zero coefficients.
- **Diagnostics Metadata**: Learned regressors record model name, target, training example count, solver name, residual sum of squares, effective rank, active feature count, encoded feature count, and effective options, accessible using the shared regression diagnostics predicates.
- **Model Export**: Learned regressors can be exported as predicate clauses or written to a file.
- **Reference Benchmarks**: Includes a dedicated performance suite reporting training time, RMSE, and MAE for representative regression datasets.


Regressor representation
------------------------

The learned regressor is represented by default as:

- `linear_regressor(Encoders, Bias, Weights, Diagnostics)`

The exported predicate clauses therefore use the shape:

- `Functor(Encoders, Bias, Weights, Diagnostics)`


Diagnostics syntax
------------------

The `diagnostics/2` predicate returns a list of metadata terms with the form:

	[
		model(linear_regression),
		target(Target),
		training_example_count(TrainingExampleCount),
		options(Options),
		solver(Solver),
		residual_sum_of_squares(ResidualSumOfSquares),
		effective_rank(EffectiveRank),
		active_feature_count(ActiveFeatureCount),
		encoded_feature_count(FeatureCount)
	]

Where:

- `model(linear_regression)` identifies the learning algorithm that produced the regressor.
- `target(Target)` stores the target attribute name declared by the training dataset.
- `training_example_count(TrainingExampleCount)` stores the number of examples used during training.
- `options(Options)` stores the effective learning options after merging the user options with the library defaults.
- `solver(Solver)` records the direct least-squares solver family used for the fit. The current value is `modified_gram_schmidt_column_pivoting`, which is now reported by the shared regression core while delegating the actual solve to the `linear_algebra` library.
- `residual_sum_of_squares(ResidualSumOfSquares)` stores the training residual sum of squares for the fitted regressor.
- `effective_rank(EffectiveRank)` stores the rank of the fitted row-oriented design matrix, including the intercept column.
- `active_feature_count(ActiveFeatureCount)` stores the number of encoded feature columns retained after subtracting the intercept contribution from the fitted design-matrix rank.
- `encoded_feature_count(FeatureCount)` stores the number of numeric features induced by the encoder list, including missing-value indicator features.

Use the `regression_protocols` `diagnostic/2` and `regressor_options/2` helper predicates when you only need a single metadata term or the effective options.

Options
-------

The `learn/3` predicate accepts the following options:

- `feature_scaling/1`: Controls z-score standardization of continuous attributes before training and prediction. Accepted values are `true` and `false`. The default is `true`.
