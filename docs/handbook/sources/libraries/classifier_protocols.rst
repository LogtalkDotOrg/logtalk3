.. _library_classifier_protocols:

``classifier_protocols``
========================

This library provides protocols used in the implementation of machine
learning classifier algorithms. Datasets are represented as objects
implementing the ``dataset_protocol`` protocol. Classifiers are
represented as objects implementing the ``classifier_protocol``
protocol.

This library also provides reusable shared categories, smoke tests, and
test datasets. See below for details.

Logtalk currently provides several classifiers including ``c45``,
``knn``, ``linear_svm``, ``logistic_regression``, ``naive_bayes``,
``nearest_centroid``, and ``random_forest``. See these libraries
documentation for details.

Shared category
---------------

The library includes one reusable category intended to be imported by
classifier algorithm implementations:

- ``classifier_common`` — shared diagnostics accessors and classifier
  export helpers.

This category keeps diagnostics access and file export behavior separate
from the algorithm-specific learning, prediction, and pretty-printing
code.

Diagnostics
-----------

The ``classifier_common`` category provides shared accessor predicates
such as ``diagnostics/2``, ``diagnostic/2``, and
``classifier_options/2``. These predicates make it possible to inspect
learned-classifier metadata without depending on the exact term
representation used by a particular classifier implementation.

The detailed contents of the diagnostics data are classifier-dependent.
For example, some classifiers report effective training options, while
others report structural metadata such as attribute names, feature
types, or the number of training examples or models.

Export header convention
------------------------

The shared classifier exporter in the ``classifier_common`` category
writes a standard comment header before the exported clauses:

- ``% exported classifier predicate: Functor``
- ``% dataset prediction schema: Functor(TitleCaseAttribute1, ..., TitleCaseAttributeN, TitleCaseClass)``
- ``% diagnostics: Diagnostics``

The ``dataset prediction schema`` line always uses the same ASCII-only
TitleCase conversion already used by ``c45`` for header comments. This
line documents the dataset-level prediction interface for readability,
even when the exported clauses serialize a model term instead of an
executable predictor relation.

API documentation
-----------------

Open the
`../../apis/library_index.html#classifier_protocols <../../apis/library_index.html#classifier_protocols>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(classifier_protocols(loader)).

Testing
-------

To test this library predicates, shared categories, and datasets, load
the ``tester.lgt`` file:

\| ?- logtalk_load(classifier_protocols(tester)).

Test datasets
-------------

Several sample datasets are included in the ``test_datasets`` directory:

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
  Repository: https://archive.ics.uci.edu/dataset/14/breast+cancer

- **Gaussian Anomalies** — A synthetic 2D anomaly detection dataset with
  50 examples and 2 continuous attributes (x, y). Normal points are
  sampled from a standard normal distribution centered at the origin.
  Anomalous points are placed far from the cluster center. Inspired by
  the canonical test case used in the Extended Isolation Forest paper by
  Hariri et al. (2019).

- **Shuttle Anomalies** — A subset of the Statlog Shuttle dataset with
  50 examples and 9 continuous attributes representing sensor readings
  from the NASA Space Shuttle. Class 1 (Rad Flow) is the majority class
  (normal), while all other classes are treated as anomalies. Originally
  from Catlett, J. (1991). Available from the UCI Machine Learning
  Repository: https://archive.ics.uci.edu/dataset/148/statlog+shuttle

- **Water Potability** — A water potability dataset with 50 examples and
  9 continuous attributes (pH, hardness, solids, chloramines, sulfate,
  conductivity, organic carbon, trihalomethanes, turbidity). Normal
  instances represent potable water samples within acceptable ranges.
  Anomalous instances represent water samples with hazardous
  contamination levels. Based on the publicly available Water Quality
  dataset (Kadiwal, A., 2020, Kaggle).

- **Sensor Anomalies** — A synthetic industrial sensor anomaly dataset
  with 40 examples and 3 continuous attributes (temperature, pressure,
  vibration). Contains missing values (14 examples with missing values,
  represented using anonymous variables). Normal readings cluster around
  typical operating ranges. Anomalous readings show extreme values
  indicating equipment malfunction.
