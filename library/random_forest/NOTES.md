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


`random_forest`
===============

Random Forest classifier using C4.5 decision trees as base learners. Builds
an ensemble of decision trees trained on bootstrap samples with random feature
subsets and combines their predictions through majority voting.

The library implements the `classifier_protocol` defined in the
`classifier_protocols` library. It provides predicates for learning an
ensemble classifier from a dataset, using it to make predictions (with
class probabilities), and exporting it as a list of predicate clauses or
to a file.

Datasets are represented as objects implementing the `dataset_protocol`
protocol from the `classifier_protocols` library. See `test_files` directory
for examples.


API documentation
-----------------

Open the [../../docs/library_index.html#random_forest](../../docs/library_index.html#random_forest)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(random_forest(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(random_forest(tester)).


Features
--------

- **Ensemble Learning**: Combines multiple C4.5 decision trees for robust predictions
- **Bootstrap Sampling**: Each tree is trained on a random sample with replacement
- **Feature Randomization**: Random subset of features selected for each tree (default: sqrt(total_features))
- **Majority Voting**: Final predictions determined by voting across all trees
- **Probability Estimation**: Provides confidence scores based on vote proportions
- **Configurable Options**: Number of trees and max features per tree via predicate options
- **Classifier Export**: Learned classifiers can be exported as predicate clauses


Options
-------

The following options can be passed to the `learn/3` predicate:

- `number_of_trees(N)`: Number of trees in the forest (default: 10)
- `maximum_features_per_tree(N)`: Maximum number of features to consider per tree (default: sqrt(total_features))


Classifier Representation
-------------------------

The learned classifier is represented as a compound term with the functor chosen
by the user when exporting the classifier and arity 2. The default functor is
`rf_classifier/3`:

    rf_classifier(Trees, ClassValues)

Where:

- `Trees`: List of `tree(C45Tree, AttributeNames)` pairs
- `ClassValues`: List of possible class values
- `Options`: List of options used during learning


References
----------

1. Breiman, L. (2001). "Random Forests". Machine Learning, 45(1), 5-32.
2. Ho, T.K. (1995). "Random Decision Forests". Proceedings of the 3rd International Conference on Document Analysis and Recognition.
3. Quinlan, J.R. (1993). "C4.5: Programs for Machine Learning". Morgan Kaufmann.


Usage
-----

### Learning a Classifier

	% Learn a random forest with default options (10 trees)
	| ?- random_forest::learn(play_tennis, Classifier).
	...

	% Learn with custom options
	| ?- random_forest::learn(play_tennis, Classifier, [number_of_trees(20), maximum_features_per_tree(2)]).
	...

### Making Predictions

	% Predict class for a new instance
	| ?- random_forest::learn(play_tennis, Classifier),
	     random_forest::predict(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-weak], Class).
	Class = no
	...

	% Get probability distribution from ensemble voting
	| ?- random_forest::learn(play_tennis, Classifier),
	     random_forest::predict_probabilities(Classifier, [outlook-overcast, temperature-mild, humidity-normal, wind-weak], Probabilities).
	Probabilities = [yes-0.9, no-0.1]
	...

### Exporting the Classifier

	% Export as predicate clauses
	| ?- random_forest::learn(play_tennis, Classifier),
	     random_forest::classifier_to_clauses(play_tennis, Classifier, my_forest, Clauses).
	Clauses = [my_forest(random_forest_classifier(...))]
	...

	% Export to a file
	| ?- random_forest::learn(play_tennis, Classifier),
	     random_forest::classifier_to_file(play_tennis, Classifier, my_forest, 'forest.pl').
	...

### Using a Saved Classifier

	% Load and use a previously saved classifier
	| ?- logtalk_load('forest.pl'),
	     my_forest(Classifier),
	     random_forest::predict(Classifier, [outlook-sunny, temperature-cool, humidity-normal, wind-weak], Class).
	Class = yes
	...

### Printing the Classifier

	% Print a summary of the random forest
	| ?- random_forest::learn(play_tennis, Classifier),
	     random_forest::print_classifier(Classifier).

	Random Forest Classifier
	========================

	Number of trees: 10
	Class values: [yes,no]
	Options: [number_of_trees(10)]

	Trees:
	  Tree 1 (features: [outlook,humidity]):
	    -> tree rooted at outlook
	  ...
	...
