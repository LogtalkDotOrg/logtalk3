
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


`c45`
=====

This library implements the C4.5 decision tree learning algorithm. C4.5
is an extension of the ID3 algorithm that uses information gain ratio
instead of information gain for attribute selection, which avoids bias
towards attributes with many values. The implementation supports both
discrete (categorical) and continuous (numeric) attributes.

The library provides predicates for learning a decision tree from a
dataset and exporting the learned tree as a list of predicate clauses
or to a file.

Datasets are represented as objects implementing the `dataset_protocol`
protocol. This protocol defines the following predicates:

- `attribute_values/2` - enumerates attributes and their possible values;
  for discrete attributes, the values is a list of possible values; for
  continuous attributes, the value is the atom `continuous`
- `class/1` - returns the name of the target class attribute
- `class_values/1` - returns the list of possible class values
- `example/3` - enumerates the examples in the dataset


Implemented features
--------------------

- Information gain ratio for attribute selection (avoids bias towards
  attributes with many values)
- Handling of discrete (categorical) attributes with multi-way splits
- Handling of continuous (numeric) attributes with binary threshold
  splits (selects the threshold with the highest gain ratio from
  midpoints between consecutive sorted values)
- Handling of missing attribute values (represented using anonymous
  variables): examples with missing values for an attribute are
  distributed to all branches during tree construction; gain ratio
  computation uses only examples with known values for the attribute
  being evaluated
- Export of learned decision trees as lists of predicate clauses or
  to files
- Pretty-printing of learned decision trees


Limitations
-----------

- No tree pruning (builds full unpruned trees, which may lead to
  overfitting on training data)
- No incremental learning (the tree must be rebuilt from scratch when
  new examples are added)


References
----------

- Quinlan, J.R. (1986). Induction of Decision Trees. *Machine Learning*,
  1(1), 81-106.
  https://doi.org/10.1007/BF00116251

- Quinlan, J.R. (1993). *C4.5: Programs for Machine Learning*. Morgan
  Kaufmann Publishers.
  https://doi.org/10.1016/C2009-0-27846-9

- Mitchell, T.M. (1997). *Machine Learning*. McGraw-Hill. Chapter 3:
  Decision Tree Learning.


Test datasets
-------------

Four sample datasets are included in the `test_files` directory:

- **Play Tennis** — The classic weather/tennis dataset with 14 examples
  and 4 discrete attributes (outlook, temperature, humidity, wind).
  Originally from Quinlan (1986) and widely used in machine learning
  textbooks including Mitchell (1997). Also available from the UCI
  Machine Learning Repository:
  https://archive.ics.uci.edu/dataset/349/tennis+major+tournament+match+statistics

- **Contact Lenses** — A dataset with 24 examples and 4 discrete
  attributes (age, spectacle prescription, astigmatism, tear production
  rate) for deciding the type of contact lenses to prescribe. Originally
  from Cendrowska, J. (1987). PRISM: An algorithm for inducing modular
  rules. *International Journal of Man-Machine Studies*, 27(4), 349-370.
  Available from the UCI Machine Learning Repository:
  https://archive.ics.uci.edu/dataset/58/lenses

- **Iris** — The classic Iris flower dataset with 150 examples and 4
  continuous attributes (sepal length, sepal width, petal length, petal
  width) for classifying iris species (setosa, versicolor, virginica).
  Originally from Fisher, R.A. (1936). The use of multiple measurements
  in taxonomic problems. *Annals of Eugenics*, 7(2), 179-188. Available
  from the UCI Machine Learning Repository:
  https://archive.ics.uci.edu/dataset/53/iris

- **Breast Cancer** — A dataset with 286 examples and 9 discrete
  attributes (age, menopause, tumor size, inv-nodes, node-caps, degree
  of malignancy, breast, breast quadrant, irradiation) for predicting
  breast cancer recurrence events. Contains missing values (9 examples
  with missing values in the node-caps and breast-quad attributes,
  represented using anonymous variables). Originally from the Institute
  of Oncology, University Medical Centre, Ljubljana, Yugoslavia. Donors:
  Ming Tan and Jeff Schlimmer. Available from the UCI Machine Learning
  Repository:
  https://archive.ics.uci.edu/dataset/14/breast+cancer


API documentation
-----------------

Open the [../../apis/library_index.html#c45](../../apis/library_index.html#c45)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(c45(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(c45(tester)).


Usage
-----

To learn a decision tree from a dataset:

	| ?- c45::learn(play_tennis, Tree).

To export the tree as a list of predicate clauses:

	| ?- c45::learn(play_tennis, Tree),
	     c45::tree_to_clauses(play_tennis, Tree, classify, Clauses).

To export the tree to a file:

	| ?- c45::learn(play_tennis, Tree),
	     c45::tree_to_file(play_tennis, Tree, classify, 'tree.pl').

To print the tree to the current output:

	| ?- c45::learn(play_tennis, Tree),
	     c45::print_tree(Tree).

