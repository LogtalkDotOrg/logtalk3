.. _library_nearest_centroid:

``nearest_centroid``
====================

Nearest Centroid classifier.

The library implements the ``classifier_protocol`` defined in the
``classifier_protocols`` library. It provides predicates for learning a
classifier from a dataset, using it to make predictions, and exporting
it as a list of predicate clauses or to a file.

Datasets are represented as objects implementing the
``dataset_protocol`` protocol from the ``classifier_protocols`` library.
See ``test_files`` directory for examples.

API documentation
-----------------

Open the
`../../docs/library_index.html#nearest_centroid <../../docs/library_index.html#nearest_centroid>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(nearest_centroid(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(nearest_centroid(tester)).

Features
--------

- **Multiple Distance Metrics**: Euclidean, Manhattan, cosine
- **Mixed Features**: Automatically handles categorical and continuous
  features
- **Configurable Option**: distance metric via predicate options
- **Probability Estimation**: Provides confidence scores for predictions
- **Classifier Export**: Learned classifiers can be exported as
  predicate clauses

Usage
-----

Learning a Classifier
~~~~~~~~~~~~~~~~~~~~~

::

   % Learn from a dataset object with default options (euclidean distance)
   | ?- nearest_centroid::learn(my_dataset, Classifier).
   ...

   % Learn with custom options
   | ?- nearest_centroid::learn(my_dataset, Classifier, [distance_metric(manhattan)]).
   ...

Making Predictions
~~~~~~~~~~~~~~~~~~

::

   % Predict class for a new instance
   | ?- Instance = [attr1-value1, attr2-value2, ...],
        nearest_centroid::learn(my_dataset, Classifier),
        nearest_centroid::predict(Classifier, Instance, PredictedClass).
   PredictedClass = ...
   ...

   % Predict with custom options
   | ?- nearest_centroid::predict(Classifier, Instance, PredictedClass, [distance_metric(cosine)]).
   ...

   % Get probability distribution
   | ?- nearest_centroid::predict_probabilities(Classifier, Instance, Probabilities).
   Probabilities = [class1-0.67, class2-0.33]
   ...

Exporting the Classifier
~~~~~~~~~~~~~~~~~~~~~~~~

Learned classifiers can be exported as a list of clauses or to a file
for later use.

::

   % Export as predicate clauses
   | ?- nearest_centroid::learn(my_dataset, Classifier),
        nearest_centroid::classifier_to_clauses(my_dataset, Classifier, my_classifier, Clauses).
   Clauses = [my_classifier(...)]
   ...

   % Export to a file
   | ?- nearest_centroid::learn(my_dataset, Classifier),
        nearest_centroid::classifier_to_file(my_dataset, Classifier, my_classifier, 'classifier.pl').
   ...

Using a learned classifier
~~~~~~~~~~~~~~~~~~~~~~~~~~

Learned and saved classifiers can later be used for predictions without
needing to access the original training dataset.

::

   % Later, load the file and use the classifier
   | ?- consult('classifier.pl'),
        my_classifier(AttributeNames, FeatureTypes, Centroids),
        Instance = [...],
        nearest_centroid::predict(my_classifier(AttributeNames, FeatureTypes, Centroids), Instance, Class).
   Class = ...
   ...

Options
-------

The following options can be passed to the ``predict/4`` and
``predict_probabilities/4`` predicates:

- ``distance_metric(Metric)``: Distance metric to use. Options:
  ``euclidean`` (default), ``manhattan``, ``cosine``

Classifier Representation
-------------------------

The learned classifier is represented as a compound term with the
functor chosen by the user when exporting the classifier and arity 4.
For example, assuming the ``my_classifier/1`` functor:

::

   nc_classifier(AttributeNames, FeatureTypes, Centroids)

Where:

- ``AttributeNames``: List of attribute names in order
- ``FeatureTypes``: List of types (``numeric`` or ``categorical``)
- ``Centroids``: List of computed ``Class-Centroid`` pairs

References
----------

1. Manning, Raghavan & Schütze (2008) - "Introduction to Information
   Retrieval". Cambridge University Press.
2. Tibshirani, Hastie, Narasimhan & Chu (2002) - "Diagnosis of multiple
   cancer types by shrunken centroids of gene expression". Proceedings
   of the National Academy of Sciences, 99(10), 6567-6572.
3. Hastie, Tibshirani & Friedman (2009) - "The Elements of Statistical
   Learning: Data Mining, Inference, and Prediction" (2nd Edition).
   Springer.
