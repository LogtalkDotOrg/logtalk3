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


`adaptive_boosting_classifier`
==============================

Adaptive Boosting (aka AdaBoost) classifier using C4.5 decision trees as
base learners. Implements the SAMME (Stagewise Additive Modeling using a
Multi-class Exponential loss function) variant, which supports multi-class
classification by adjusting the weight update formula to account for the
number of classes. Builds an ensemble of weighted decision trees where each
subsequent tree focuses on the examples misclassified by previous trees:
after each iteration, the weights of misclassified examples are increased
so that subsequent learners focus more on difficult cases.

The library implements the `classifier_protocol` defined in the
`classification_protocols` library. It provides predicates for learning an
ensemble classifier from a dataset, using it to make predictions (with
class probabilities), and exporting it as a list of predicate clauses or
to a file.

Datasets are represented as objects implementing the `dataset_protocol`
protocol from the `classification_protocols` library. See that library's
`test_datasets` directory for examples.


API documentation
-----------------

Open the [../../apis/library_index.html#adaptive_boosting_classifier](../../apis/library_index.html#adaptive_boosting_classifier)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(adaptive_boosting_classifier(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(adaptive_boosting_classifier(tester)).


Features
--------

- **Adaptive Boosting**: Iteratively trains weighted decision trees, focusing on misclassified examples
- **SAMME Algorithm**: Supports multi-class classification via the SAMME variant of AdaBoost
- **C4.5 Base Learners**: Uses C4.5 decision trees as weak learners for each boosting round
- **Weighted Voting**: Final predictions determined by weighted voting where more accurate learners have higher weight
- **Probability Estimation**: Provides confidence scores based on weighted vote proportions
- **Early Stopping**: Training stops early if a perfect classifier is found or if a base learner is no better than random
- **Configurable Options**: Number of estimators (boosting rounds) configurable via predicate options
- **Classifier Export**: Learned classifiers can be exported as predicate clauses


Options
-------

The following options can be passed to the `learn/3` predicate:

- `number_of_estimators(N)`: Positive integer maximum number of boosting rounds / weak learners (default: 10)


Training behavior
-----------------

The `learn/2-3` predicates require a non-empty dataset with complete attribute
bindings. Missing values represented by variables are supported. Singleton-class
datasets produce a constant classifier, and datasets without attributes can
produce constant leaf trees.

Each round trains an unweighted C4.5 tree on an independent weighted sample
with replacement. This approximates weighted training through resampling;
it does not use weighted C4.5 split statistics. Sampling preserves draw order
and excludes zero-weight examples. Resetting the `fast_random(xoshiro128pp)`
seed reproduces a training run for the same dataset and options.

A learner with error greater than or equal to `(K - 1) / K`, where `K` is the
number of declared classes, is rejected. If earlier learners were accepted,
training returns that ensemble. Otherwise, learning throws
`domain_error(non_empty_ensemble, Dataset)` instead of returning an empty model.
A learner with zero weighted error replaces earlier estimators with a single
tree of weight `1.0`, and training stops. Temporary sample objects are abolished
when tree training succeeds, fails, or throws an exception.

The `predict_probabilities/3` predicate returns normalized weighted-vote
proportions, not calibrated posterior probabilities. Prediction ties are
resolved in favor of the first class in the declared class values list.


Performance
-----------

Bootstrap sampling sorts independent random draws and sweeps the cumulative
distribution once, requiring `O(N log N)` work per round for `N` examples.
Training predictions are computed once per example per round and reused for
weight updates. Algebraically normalized updates avoid exponentiating learner
weights and forming potentially overflowing error ratios.

Dataset metadata is prepared once, and ensemble construction uses a difference
list to preserve estimator order without repeatedly copying earlier trees.
Prediction shares filtered instances across trees with matching feature lists.
The `predict/3` predicate selects directly from weighted votes without building
a probability distribution. Large multiclass ensembles use AVL dictionaries
for vote totals; smaller ensembles retain a simple scan.


Classifier representation
-------------------------

The learned classifier is represented as a compound term:

	ab_classifier(WeightedTrees, ClassValues, Options)

Where:

- `WeightedTrees`: List of `weighted_tree(Alpha, C45Tree, AttributeNames)` elements
- `ClassValues`: List of possible class values
- `Options`: List of options used during learning

The `check_classifier/1` and `valid_classifier/1` predicates can be used
to check and validate a model. Ensembles must be non-empty proper lists;
learner weights must be positive floats; tree leaf classes must belong to
`ClassValues`; and each split attribute must occur in that tree's distinct
attribute names list. Constant leaf trees can have an empty attribute names
list.

When exported using `export_to_clauses/4` or `export_to_file/4`,
this classifier term is serialized directly as the single argument of the
generated predicate clause so that the exported model can be loaded and
reused as-is.


References
----------

1. Freund, Y. and Schapire, R.E. (1997). "A Decision-Theoretic Generalization of On-Line Learning and an Application to Boosting". Journal of Computer and System Sciences, 55(1), 119-139.
2. Zhu, J., Zou, H., Rosset, S., and Hastie, T. (2009). "Multi-class AdaBoost". Statistics and Its Interface, 2(3), 349-360.
3. Quinlan, J.R. (1993). "C4.5: Programs for Machine Learning". Morgan Kaufmann.


Usage
-----

### Learning a Classifier

	% Learn an AdaBoost classifier with default options (10 estimators)
	| ?- adaptive_boosting_classifier::learn(play_tennis, Classifier).
	...

	% Learn with custom options
	| ?- adaptive_boosting_classifier::learn(play_tennis, Classifier, [number_of_estimators(20)]).
	...

### Making Predictions

	% Predict class for a new instance
	| ?- adaptive_boosting_classifier::learn(play_tennis, Classifier),
	     adaptive_boosting_classifier::predict(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-weak], Class).
	Class = no
	...

	% Get probability distribution from weighted voting
	| ?- adaptive_boosting_classifier::learn(play_tennis, Classifier),
	     adaptive_boosting_classifier::predict_probabilities(Classifier, [outlook-overcast, temperature-mild, humidity-normal, wind-weak], Probabilities).
	Probabilities = [yes-0.9, no-0.1]
	...

### Exporting the Classifier

	% Export as predicate clauses
	| ?- adaptive_boosting_classifier::learn(play_tennis, Classifier),
	     adaptive_boosting_classifier::export_to_clauses(play_tennis, Classifier, my_boost, Clauses).
	...

	% Export to a file
	| ?- adaptive_boosting_classifier::learn(play_tennis, Classifier),
	     adaptive_boosting_classifier::export_to_file(play_tennis, Classifier, my_boost, 'boost.pl').
	...

### Using a Saved Classifier

	% Load and use a previously saved classifier
	| ?- logtalk_load('boost.pl'),
	     my_boost(Classifier),
	     adaptive_boosting_classifier::predict(Classifier, [outlook-sunny, temperature-cool, humidity-normal, wind-weak], Class).
	Class = yes
	...

### Printing the Classifier

	% Print a summary of the AdaBoost classifier
	| ?- adaptive_boosting_classifier::learn(play_tennis, Classifier),
	     adaptive_boosting_classifier::print_classifier(Classifier).

	AdaBoost Classifier
	===================

	Learning options: [number_of_estimators(10)]

	Class values: [yes,no]
	Number of estimators: 10

	Weighted trees:
	  Estimator 1 (alpha=1.2345, features: [outlook,temperature,humidity,wind]):
	    -> tree rooted at outlook
	  ...
	...
