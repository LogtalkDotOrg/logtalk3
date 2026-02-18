.. _library_classifier_protocols:

``classifier_protocols``
========================

This library provides protocols used in the implementation of machine
learning algorithms, aka classifiers. Datasets are represented as
objects implementing the ``dataset_protocol`` protocol. Classifiers are
represented as objects implementing the ``classifier_protocol``
protocol.

This library also provides test datasets. See below for details.

Logtalk currently provides ``c45``, ``knn``, ``naive_bayes``, and
``random_forest`` classifiers. See these libraries documentation for
details.

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

Test datasets
-------------

Several sample datasets are included in the ``test_files`` directory:

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
