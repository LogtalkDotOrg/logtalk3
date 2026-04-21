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


`knn_regression`
================

k-Nearest Neighbors regressor supporting continuous and mixed-feature
datasets. The library implements the `regressor_protocol` defined in the
`regression_protocols` library and predicts numeric targets using the
weighted average of the nearest encoded neighbors.


API documentation
-----------------

Open the [../../apis/library_index.html#knn_regression](../../apis/library_index.html#knn_regression)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(knn_regression(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(knn_regression(tester)).

To run the reference timing and fit benchmarks, load the `tester_performance.lgt` file:

	| ?- logtalk_load(knn_regression(tester_performance)).


Features
--------

- **Distance-Based Regression**: Predicts targets using the weighted average of the nearest neighbors.
- **Multiple Metrics**: Supports Euclidean, Manhattan, Chebyshev, and Minkowski distances.
- **Weighting Schemes**: Supports uniform, inverse-distance, and Gaussian weighting.
- **Continuous and Mixed Features**: Supports continuous attributes and categorical attributes encoded using one-hot vectors.
- **Optional Feature Scaling**: Continuous attributes can be standardized using z-score scaling.
- **Missing Values**: Missing numeric and categorical values are encoded using explicit missing-value indicator features.
- **Model Export**: Learned regressors can be exported as predicate clauses or written to a file.
- **Reference Benchmarks**: Includes a dedicated performance suite reporting training time, RMSE, and MAE for representative regression datasets.


Options
-------

The `learn/3` predicate accepts the following options:

- **`k/1`**: Number of nearest neighbors considered for each prediction. Smaller values make predictions more local; larger values smooth them by averaging over more training rows. The default is `3`.
- **`distance_metric/1`**: Distance function used to compare encoded feature vectors. Accepted values are `euclidean`, `manhattan`, `chebyshev`, and `minkowski`. The default is `euclidean`.
- **`weight_scheme/1`**: Neighbor weighting policy used when averaging targets. Accepted values are `uniform`, `distance`, and `gaussian`. The default is `uniform`.
- **`minkowski_power/1`**: Exponent used when `distance_metric(minkowski)` is selected. Larger values increase the influence of larger coordinate differences. The default is `3.0`.
- **`feature_scaling/1`**: Controls z-score standardization of continuous attributes before storing rows and encoding prediction requests. Accepted values are `on` and `off`. The default is `on`.
