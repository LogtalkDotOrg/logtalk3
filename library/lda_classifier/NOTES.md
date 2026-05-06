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


`lda_classifier`
================

Linear Discriminant Analysis classifier for continuous datasets using a
shared pooled covariance estimate with diagonal regularization. The
implementation learns one linear discriminant function per class and
predicts the class with the highest discriminant score.

The library implements the `classifier_protocol` defined in the
`classification_protocols` library. It provides predicates for learning a
classifier from a dataset, using it to make predictions, inspecting class
scores, and exporting it as a list of predicate clauses or to a file.

Datasets are represented as objects implementing the `dataset_protocol`
protocol from the `classification_protocols` library. All dataset
attributes must be declared as `continuous`.


API documentation
-----------------

Open the [../../docs/library_index.html#lda_classifier](../../docs/library_index.html#lda_classifier)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(lda_classifier(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(lda_classifier(tester)).


Features
--------

- **Continuous Datasets**: Accepts only datasets whose attributes are all declared as continuous.
- **Pooled Covariance Model**: Learns a shared covariance matrix and class-specific means and priors.
- **Regularized Estimation**: Applies configurable diagonal regularization to stabilize covariance inversion.
- **Feature Scaling**: Supports optional z-score scaling of continuous features before fitting the model.
- **Score Inspection**: Provides discriminant scores for all classes using `predict_scores/3`.
- **Classifier Export**: Learned classifiers can be exported as predicate clauses or written to a file.


Options
-------

The `learn/3` predicate supports these options:

- `feature_scaling/1` - whether to standardize continuous attributes before training (default: `true`)
- `regularization/1` - positive diagonal value added to the pooled covariance matrix before inversion (default: `1.0e-6`)


Usage
-----

### Learning a classifier

	| ?- lda_classifier::learn(iris_small, Classifier).

	| ?- lda_classifier::learn(iris_small, Classifier, [regularization(1.0e-5)]).

### Making predictions

	| ?- lda_classifier::learn(iris_small, Classifier),
	     lda_classifier::predict(Classifier, [sepal_length-5.1, sepal_width-3.5, petal_length-1.4, petal_width-0.2], Class).

	| ?- lda_classifier::learn(iris_small, Classifier),
	     lda_classifier::predict_scores(Classifier, [sepal_length-6.0, sepal_width-2.9, petal_length-4.5, petal_width-1.5], Scores).

### Exporting the classifier

	| ?- lda_classifier::learn(iris_small, Classifier),
	     lda_classifier::export_to_clauses(iris_small, Classifier, classify, Clauses).

	| ?- lda_classifier::learn(iris_small, Classifier),
	     lda_classifier::export_to_file(iris_small, Classifier, classify, 'classifier.pl').


Classifier representation
-------------------------

The learned classifier is represented as a compound term with the form:

	lda_classifier(Encoders, Models, Options)

Where:

- `Encoders`: list of continuous feature encoders with learned scaling parameters
- `Models`: list of `class_model(Class, Prior, Mean, Weights, Offset)` terms
- `Options`: merged training options used to learn the classifier

When exported using `export_to_clauses/4` or `export_to_file/4`,
this classifier term is serialized directly as the single argument of the
generated predicate clause so that the exported model can be loaded and
reused as-is.


References
----------

1. Fisher, R.A. (1936). "The use of multiple measurements in taxonomic problems".
2. Hastie, T., Tibshirani, R. and Friedman, J. (2009). "The Elements of Statistical Learning". Section 4.3.
3. Bishop, C.M. (2006). "Pattern Recognition and Machine Learning". Section 4.1.
