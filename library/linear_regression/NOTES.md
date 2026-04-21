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
`regression_protocols` library and learns a linear model using batch
gradient descent.


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

To run the reference timing and fit benchmarks, load the `tester_performance.lgt` file:

	| ?- logtalk_load(linear_regression(tester_performance)).


Features
--------

- **Continuous and Mixed Features**: Supports continuous attributes and categorical attributes encoded using one-hot vectors.
- **Feature Scaling**: Continuous attributes can be standardized using z-score scaling.
- **Missing Values**: Missing numeric and categorical values are encoded using explicit missing-value indicator features.
- **Regularization**: Supports optional L2 regularization.
- **Model Export**: Learned regressors can be exported as predicate clauses or written to a file.
- **Reference Benchmarks**: Includes a dedicated performance suite reporting training time, RMSE, and MAE for representative regression datasets.


Options
-------

The `learn/3` predicate accepts the following options:

- **`learning_rate/1`**: Step size used by batch gradient descent when updating the bias and weights. Larger values speed up training but can overshoot; smaller values are more conservative. The default is `0.05`.
- **`maximum_iterations/1`**: Maximum number of gradient-descent iterations to run before stopping even if the tolerance criterion has not been met. The default is `2000`.
- **`tolerance/1`**: Convergence threshold for the maximum parameter update. Training stops early when the largest absolute change in the bias or any weight is at or below this value. The default is `1.0e-7`.
- **`l2_regularization/1`**: L2 penalty coefficient applied to the weight vector during optimization. Higher values increase shrinkage and can reduce overfitting. The default is `0.0`.
- **`feature_scaling/1`**: Controls z-score standardization of continuous attributes before training and prediction. Accepted values are `on` and `off`. The default is `on`.
