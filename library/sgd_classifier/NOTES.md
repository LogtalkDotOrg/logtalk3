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


`sgd_classifier`
================

Linear stochastic-gradient classifier for tabular datasets using a
one-vs-rest scheme with configurable losses. The implementation reuses
the shared linear encoder pipeline so continuous and categorical
features, including missing values, are represented consistently with the
existing linear classifiers.

The library implements the `classifier_protocol` defined in the
`classification_protocols` library. It provides predicates for learning a
classifier from a dataset, using it to make predictions, estimating class
probabilities, and exporting it as a list of predicate clauses or to a
file.

Datasets are represented as objects implementing the `dataset_protocol`
protocol from the `classification_protocols` library. Continuous,
categorical, and mixed-feature datasets are supported.


API documentation
-----------------

Open the [../../docs/library_index.html#sgd_classifier](../../docs/library_index.html#sgd_classifier)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(sgd_classifier(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(sgd_classifier(tester)).


Features
--------

- **Binary and Multiclass Classification**: Learns one-vs-rest linear models and predicts the class with the highest decision score.
- **Multiple Losses**: Supports `log_loss`, `hinge`, `squared_hinge`, `modified_huber`, and `perceptron` losses.
- **Mixed Features**: Reuses the shared tabular encoders for continuous and categorical attributes, including missing-value indicators.
- **Probability Estimation**: Provides class probabilities using a softmax over linear decision scores.
- **Configurable Optimization**: Exposes learning-rate scheduling, convergence tolerance, and L2 regularization options.
- **Classifier Export**: Learned classifiers can be exported as predicate clauses or written to a file.


Options
-------

The `learn/3` predicate supports these options:

- `loss/1` - optimization loss, one of `log_loss`, `hinge`, `squared_hinge`, `modified_huber`, or `perceptron` (default: `log_loss`)
- `learning_rate/1` - base learning rate for optimization (default: `0.05`)
- `learning_schedule/1` - learning-rate schedule, either `constant` or `inverse_scaling(Power)` (default: `constant`)
- `maximum_iterations/1` - maximum number of optimization epochs (default: `100`)
- `tolerance/1` - convergence threshold for the maximum parameter update (default: `1.0e-5`)
- `l2_regularization/1` - L2 penalty factor applied to the weight vectors (default: `0.0001`)
- `feature_scaling/1` - whether to standardize continuous attributes before encoding (default: `true`)


Usage
-----

### Learning a classifier

	| ?- sgd_classifier::learn(weather, Classifier).

	| ?- sgd_classifier::learn(mixed, Classifier, [loss(hinge), maximum_iterations(250)]).

### Making predictions

	| ?- sgd_classifier::learn(weather, Classifier),
	     sgd_classifier::predict(Classifier, [outlook-rainy, temperature-mild, humidity-normal, windy-false], Class).

	| ?- sgd_classifier::learn(missing_mixed, Classifier),
	     sgd_classifier::predict_probabilities(Classifier, [age-38, income-72000, student-yes, credit_rating-fair], Probabilities).

### Exporting the classifier

	| ?- sgd_classifier::learn(weather, Classifier),
	     sgd_classifier::export_to_clauses(weather, Classifier, classify, Clauses).

	| ?- sgd_classifier::learn(weather, Classifier),
	     sgd_classifier::export_to_file(weather, Classifier, classify, 'classifier.pl').


Classifier representation
-------------------------

The learned classifier is represented as a compound term with the form:

	sgd_classifier(Classes, Encoders, Loss, Models, Options)

Where:

- `Classes`: list of class labels
- `Encoders`: list of continuous scaling descriptors and categorical value encoders
- `Loss`: selected optimization loss
- `Models`: list of `class_model(Class, Bias, Weights)` terms
- `Options`: merged training options used to learn the classifier

When exported using `export_to_clauses/4` or `export_to_file/4`,
this classifier term is serialized directly as the single argument of the
generated predicate clause so that the exported model can be loaded and
reused as-is.


References
----------

1. Bottou, L. (2010). "Large-Scale Machine Learning with Stochastic Gradient Descent".
2. Shalev-Shwartz, S. and Ben-David, S. (2014). "Understanding Machine Learning". Chapter 15.
3. Hastie, T., Tibshirani, R. and Friedman, J. (2009). "The Elements of Statistical Learning". Chapter 12.
