
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


`classification_protocols`
==========================

This library provides protocols used in the implementation of machine
learning classifier algorithms. Datasets are represented as objects
implementing the `dataset_protocol` protocol. Classifiers are represented
as objects implementing the `classifier_protocol` protocol.

This library also provides reusable shared categories, smoke tests, and
test datasets. See below for details.

Logtalk currently provides several classifiers including `c45`, `knn`,
`linear_svm`, `logistic_regression`, `naive_bayes`, `nearest_centroid`,
and `random_forest`. See these libraries documentation for details.


Shared category
---------------

The library includes one reusable category intended to be imported by
classifier algorithm implementations:

- `classifier_common` — shared diagnostics accessors and classifier export
  helpers.

This category keeps diagnostics access and file export behavior separate
from the algorithm-specific learning, prediction, and pretty-printing code.


Diagnostics
-----------

The `classifier_common` category provides shared accessor predicates
such as `diagnostics/2`, `diagnostic/2`, and `classifier_options/2`. These
predicates make it possible to inspect learned-classifier metadata without
depending on the exact term representation used by a particular classifier
implementation.

The detailed contents of the diagnostics data are classifier-dependent.
For example, some classifiers report effective training options, while
others report structural metadata such as attribute names, feature types,
or the number of training examples or models.


Export header format
--------------------

The shared classifier exporter in the `classifier_common` category writes
a header before the exported clauses in the following format:

    % exported classifier predicate: Functor/Arity
    % training dataset: Dataset
    % dataset prediction schema: Functor(Attribute1, ..., AttributeN, Class)
    % diagnostics: Diagnostics
    % Functor(Classifier)
    Functor(Classifier)

The `dataset prediction schema` line always uses an ASCII-only title case
conversion for the attribute names and class. This line documents the
dataset-level prediction interface for readability, even when the exported
clauses serialize a model term instead of an executable predictor relation.

When exporting a serialized classifier term, using a noun such as
`classifier/1` or `model/1` is recommended.


API documentation
-----------------

Open the [../../apis/library_index.html#classification_protocols](../../apis/library_index.html#classification_protocols)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

  | ?- logtalk_load(classification_protocols(loader)).


Testing
-------

To test this library predicates, shared categories, and datasets, load the
`tester.lgt` file:

  | ?- logtalk_load(classification_protocols(tester)).


Test datasets
-------------

Several sample datasets are included in the `test_datasets` directory:

- `play_tennis.lgt` — The classic weather/tennis dataset with 14 examples
  and 4 discrete attributes (outlook, temperature, humidity, wind).
  Originally from Quinlan (1986) and widely used in machine learning
  textbooks including Mitchell (1997). Also available from the UCI
  Machine Learning Repository:
  https://archive.ics.uci.edu/dataset/349/tennis+major+tournament+match+statistics

- `contact_lenses.lgt` — A dataset with 24 examples and 4 discrete
  attributes (age, spectacle prescription, astigmatism, tear production
  rate) for deciding the type of contact lenses to prescribe. Originally
  from Cendrowska, J. (1987). PRISM: An algorithm for inducing modular
  rules. *International Journal of Man-Machine Studies*, 27(4), 349-370.
  Available from the UCI Machine Learning Repository:
  https://archive.ics.uci.edu/dataset/58/lenses

- `iris.lgt` — The classic Iris flower dataset with 150 examples and 4
  continuous attributes (sepal length, sepal width, petal length, petal
  width) for classifying iris species (setosa, versicolor, virginica).
  Originally from Fisher, R.A. (1936). The use of multiple measurements
  in taxonomic problems. *Annals of Eugenics*, 7(2), 179-188. Available
  from the UCI Machine Learning Repository:
  https://archive.ics.uci.edu/dataset/53/iris

- `breast_cancer.lgt` — A dataset with 286 examples and 9 discrete
  attributes (age, menopause, tumor size, inv-nodes, node-caps, degree
  of malignancy, breast, breast quadrant, irradiation) for predicting
  breast cancer recurrence events. Contains missing values (9 examples
  with missing values in the node-caps and breast-quad attributes,
  represented using anonymous variables). Originally from the Institute
  of Oncology, University Medical Centre, Ljubljana, Yugoslavia. Donors:
  Ming Tan and Jeff Schlimmer. Available from the UCI Machine Learning
  Repository:
  https://archive.ics.uci.edu/dataset/14/breast+cancer
