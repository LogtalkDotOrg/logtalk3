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


`knn`
=====

k-Nearest Neighbors classifier supporting multiple distance metrics,
weighting schemes, and both categorical and continuous features.

The library implements the `classifier_protocol` defined in the
`classifier_protocols` library. It provides predicates for learning a
classifier from a dataset, using it to make predictions, and exporting
it as a list of predicate clauses or to a file.

Datasets are represented as objects implementing the `dataset_protocol`
protocol from the `classifier_protocols` library. See `test_files` directory
for examples.


API documentation
-----------------

Open the [../../docs/library_index.html#knn](../../docs/library_index.html#knn)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(knn(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(knn(tester)).


Features
--------

- **Multiple Distance Metrics**: Euclidean, Manhattan, Chebyshev, Minkowski
- **Flexible Weighting**: Uniform, distance-based, and Gaussian weighting
- **Mixed Features**: Automatically handles categorical and continuous features
- **Configurable Options**: k value, distance metric, and weighting scheme via predicate options
- **Probability Estimation**: Provides confidence scores for predictions
- **Classifier Export**: Learned classifiers can be exported as predicate clauses


Usage
-----

### Learning a Classifier

    % Learn from a dataset object with default options (k=3, euclidean, uniform)
    | ?- knn::learn(my_dataset, Classifier).
    ...

    % Learn with custom options
    | ?- knn::learn(my_dataset, Classifier, [k(5), distance_metric(manhattan)]).
    ...

### Making Predictions

    % Predict class for a new instance
    | ?- Instance = [attr1-value1, attr2-value2, ...],
         knn::learn(my_dataset, Classifier),
         knn::predict(Classifier, Instance, PredictedClass).
    PredictedClass = ...
    ...

    % Predict with custom options
    | ?- knn::predict(Classifier, Instance, PredictedClass, [k(5), weight_scheme(distance)]).
    ...

    % Get probability distribution
    | ?- knn::predict_probabilities(Classifier, Instance, Probabilities).
    Probabilities = [class1-0.67, class2-0.33]
    ...

### Exporting the Classifier

Learned classifiers can be exported as a list of clauses or to a file for later use.

    % Export as predicate clauses
    | ?- knn::learn(my_dataset, Classifier),
         knn::classifier_to_clauses(my_dataset, Classifier, my_classifier, Clauses).
    Clauses = [my_classifier(...)]
    ...

    % Export to a file
    | ?- knn::learn(my_dataset, Classifier),
         knn::classifier_to_file(my_dataset, Classifier, my_classifier, 'classifier.pl').
    ...

### Using a learned classifier

Learned and saved classifiers can later be used for predictions without needing
to access the original training dataset.

    % Later, load the file and use the classifier
    | ?- consult('classifier.pl'),
         my_classifier(Classifier),
         Instance = [...],
         knn::predict(Classifier, Instance, Class).
    Class = ...
    ...


Options
-------

The following options can be passed to the `learn/3`, `predict/4`, and
`predict_probabilities/4` predicates:

- `k(K)`: Number of neighbors to consider (default: 3)
- `distance_metric(Metric)`: Distance metric to use. Options: `euclidean` (default), `manhattan`, `chebyshev`, `minkowski`
- `weight_scheme(Scheme)`: Weighting scheme for neighbor votes. Options: `uniform` (default), `distance`, `gaussian`


Classifier Representation
-------------------------

The learned classifier is represented as a compound term with the functor chosen
by the user when exporting the classifier and arity 4. For example, assuming the
`my_classifier/1` functor:

    knn_classifier(AttributeNames, FeatureTypes, Labels, Options)

Where:

- `AttributeNames`: List of attribute names in order
- `FeatureTypes`: List of types (`numeric` or `categorical`)
- `Labels`: List of `Instance-Class` pairs (the training data)
- `Options`: List of options used during learning


References
----------

1. Cover, T. & Hart, P. (1967). "Nearest neighbor pattern classification". IEEE Transactions on Information Theory.
2. Hastie, T., Tibshirani, R., & Friedman, J. (2009). "The Elements of Statistical Learning". Chapter 13.
3. Mitchell, T. (1997). "Machine Learning". Chapter 8: Instance-Based Learning.
