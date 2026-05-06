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


`kernel_svm_classifier`
=======================

Kernel support vector machine classifier using one-vs-rest dual margin
models with linear, polynomial, and radial basis function kernels. The
implementation encodes tabular datasets using the shared linear encoder
pipeline so mixed continuous and categorical datasets are handled
consistently with the existing linear classifiers.

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

Open the [../../docs/library_index.html#kernel_svm_classifier](../../docs/library_index.html#kernel_svm_classifier)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(kernel_svm_classifier(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(kernel_svm_classifier(tester)).


Features
--------

- **Binary and Multiclass Classification**: Learns one-vs-rest dual models and predicts the class with the highest decision score.
- **Multiple Kernels**: Supports `linear`, `polynomial(Degree, Gamma, Coef0)`, and `rbf(Gamma)` kernels.
- **Mixed Features**: Reuses the shared tabular encoders for continuous and categorical attributes, including missing-value indicators.
- **Probability Estimation**: Provides class probabilities using a softmax over kernel decision scores.
- **Regularized Training**: Supports configurable learning-rate scheduling, tolerance, and L2 regularization.
- **Classifier Export**: Learned classifiers can be exported as predicate clauses or written to a file.


Options
-------

The `learn/3` predicate supports these options:

- `kernel/1` - kernel function to use (default: `linear`)
- `learning_rate/1` - base learning rate for the dual optimization loop (default: `0.5`)
- `learning_schedule/1` - learning-rate schedule, either `constant` or `inverse_scaling(Power)` (default: `constant`)
- `maximum_iterations/1` - maximum number of optimization epochs (default: `25`)
- `tolerance/1` - convergence threshold for the maximum parameter update (default: `1.0e-5`)
- `l2_regularization/1` - L2 penalty factor applied during optimization (default: `0.001`)
- `feature_scaling/1` - whether to standardize continuous attributes before encoding (default: `true`)


Usage
-----

### Learning a classifier

	| ?- kernel_svm_classifier::learn(weather, Classifier).

	| ?- kernel_svm_classifier::learn(mixed, Classifier, [kernel(rbf(0.5)), maximum_iterations(50)]).

### Making predictions

	| ?- kernel_svm_classifier::learn(mixed, Classifier),
	     kernel_svm_classifier::predict(Classifier, [age-35, income-65000, student-yes, credit_rating-fair], Class).

	| ?- kernel_svm_classifier::learn(weather, Classifier),
	     kernel_svm_classifier::predict_probabilities(Classifier, [outlook-sunny, temperature-hot, humidity-high, windy-false], Probabilities).

### Exporting the classifier

	| ?- kernel_svm_classifier::learn(weather, Classifier),
	     kernel_svm_classifier::export_to_clauses(weather, Classifier, classify, Clauses).

	| ?- kernel_svm_classifier::learn(weather, Classifier),
	     kernel_svm_classifier::export_to_file(weather, Classifier, classify, 'classifier.pl').


Classifier representation
-------------------------

The learned classifier is represented as a compound term with the form:

	kernel_svm_classifier(Classes, Encoders, Kernel, TrainingRows, Models, Options)

Where:

- `Classes`: list of class labels
- `Encoders`: list of continuous scaling descriptors and categorical value encoders
- `Kernel`: selected kernel specification
- `TrainingRows`: encoded feature vectors for the training examples
- `Models`: list of `class_model(Class, Bias, Coefficients)` terms
- `Options`: merged training options used to learn the classifier

When exported using `export_to_clauses/4` or `export_to_file/4`,
this classifier term is serialized directly as the single argument of the
generated predicate clause so that the exported model can be loaded and
reused as-is.


References
----------

1. Cortes, C. and Vapnik, V. (1995). "Support-Vector Networks".
2. Scholkopf, B. and Smola, A.J. (2002). "Learning with Kernels".
3. Bishop, C.M. (2006). "Pattern Recognition and Machine Learning". Chapter 7.
