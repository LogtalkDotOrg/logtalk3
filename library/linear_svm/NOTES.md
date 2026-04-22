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


`linear_svm`
============

Linear support vector machine classifier supporting both binary and
multiclass classification. Multiclass classification is implemented
using a one-vs-rest scheme with one linear margin model per class.

The library implements the `classifier_protocol` defined in the
`classification_protocols` library. It provides predicates for learning a
classifier from a dataset object, using it to make predictions, and
exporting the learned model as a list of predicate clauses or to a file.

Datasets are represented as objects implementing the `dataset_protocol`
protocol from the `classification_protocols` library. Existing datasets in
`classification_protocols/test_datasets` can be used for binary,
multiclass, continuous, categorical, and mixed-feature testing.


API documentation
-----------------

Open the [../../docs/library_index.html#linear_svm](../../docs/library_index.html#linear_svm)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(linear_svm(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(linear_svm(tester)).

To run the reference timing and fit benchmarks, load the `tester_performance.lgt` file:

	| ?- logtalk_load(linear_svm(tester_performance)).


Features
--------

- **Binary and Multiclass Classification**: Learns one-vs-rest linear margin models and predicts the class with the highest score.
- **Continuous Features**: Standardizes numeric attributes using z-score scaling derived from the training data.
- **Categorical Features**: Expands discrete attributes using one-hot encoding based on the declared dataset attribute values and rejects unseen values with a domain error.
- **Missing Values**: Encodes missing numeric and categorical values using explicit missing-value indicator features.
- **Classifier Export**: Learned classifiers can be exported as predicate clauses or written to a file.
- **Pretty Printing**: Prints a compact summary of classes, encoders, and learned weight vector sizes.
- **Reference Benchmarks**: Includes a dedicated performance suite covering the `weather`, `mixed`, `iris_small`, `missing_mixed`, and `breast_cancer` datasets with reported training time and training accuracy.


Options
-------

The `learn/3` predicate supports these options:

- `learning_rate/1` - base gradient descent learning rate (default: `0.1`)
- `maximum_iterations/1` - maximum number of optimization iterations (default: `1000`)
- `tolerance/1` - convergence threshold for the maximum parameter update (default: `1.0e-6`)
- `l2_regularization/1` - L2 penalty factor applied to weights (default: `0.01`)


Usage
-----

### Learning a classifier

	| ?- linear_svm::learn(weather, Classifier).

	| ?- linear_svm::learn(iris_small, Classifier, [learning_rate(0.05), maximum_iterations(1500)]).

### Making predictions

	| ?- linear_svm::learn(mixed, Classifier),
	     linear_svm::predict(Classifier, [age-45, income-75000, student-no, credit_rating-fair], Class).

### Exporting the classifier

	| ?- linear_svm::learn(weather, Classifier),
	     linear_svm::export_to_clauses(weather, Classifier, classify, Clauses).

	| ?- linear_svm::learn(weather, Classifier),
	     linear_svm::export_to_file(weather, Classifier, classify, 'classifier.pl').


Classifier representation
-------------------------

The learned classifier is represented as a compound term with the form:

	linear_svm_classifier(Classes, Encoders, Models, Options)

Where:

- `Classes`: list of class labels
- `Encoders`: list of continuous scaling descriptors and categorical value lists
- `Models`: list of `class_model(Class, Bias, Weights)` terms
- `Options`: merged training options used to learn the model

When exported using `export_to_clauses/4` or `export_to_file/4`,
this classifier term is serialized directly as the single argument of the
generated predicate clause so that the exported model can be loaded and
reused as-is.


References
----------

1. Cortes, C. and Vapnik, V. (1995). "Support-Vector Networks".
2. Bishop, C.M. (2006). "Pattern Recognition and Machine Learning". Section 7.1.
3. Hastie, T., Tibshirani, R. and Friedman, J. (2009). "The Elements of Statistical Learning". Section 12.3.
