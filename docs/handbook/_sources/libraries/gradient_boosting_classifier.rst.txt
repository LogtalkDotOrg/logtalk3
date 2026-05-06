.. _library_gradient_boosting_classifier:

``gradient_boosting_classifier``
================================

Gradient boosting classifier for tabular datasets using multinomial
additive models fitted by regression trees to softmax residuals. At each
boosting stage the implementation fits one regression tree per class and
updates additive class scores using the configured learning rate.

The library implements the ``classifier_protocol`` defined in the
``classification_protocols`` library. It provides predicates for
learning a classifier from a dataset, using it to make predictions,
estimating class probabilities, and exporting it as a list of predicate
clauses or to a file.

Datasets are represented as objects implementing the
``dataset_protocol`` protocol from the ``classification_protocols``
library. Continuous, categorical, and mixed-feature datasets are
supported through the reused ``regression_tree`` backend.

API documentation
-----------------

Open the
`../../docs/library_index.html#gradient_boosting_classifier <../../docs/library_index.html#gradient_boosting_classifier>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(gradient_boosting_classifier(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(gradient_boosting_classifier(tester)).

Features
--------

- **Multiclass Boosting**: Fits one additive score function per class
  and predicts using the highest boosted score.
- **Regression Tree Backend**: Reuses the ``regression_tree`` library to
  fit residual models at each boosting stage.
- **Probability Estimation**: Provides class probabilities using a
  softmax over the final additive scores.
- **Configurable Tree Complexity**: Exposes boosting-stage tree depth,
  minimum leaf size, and minimum variance reduction options.
- **Tabular Datasets**: Supports continuous, categorical, and
  mixed-feature datasets.
- **Classifier Export**: Learned classifiers can be exported as
  predicate clauses or written to a file.

Options
-------

The ``learn/3`` predicate supports these options:

- ``number_of_estimators/1`` - number of boosting stages to fit
  (default: ``25``)
- ``learning_rate/1`` - shrinkage factor applied to each stage
  prediction (default: ``0.1``)
- ``maximum_depth/1`` - maximum depth of each regression tree (default:
  ``3``)
- ``minimum_samples_leaf/1`` - minimum number of examples in a leaf
  (default: ``1``)
- ``minimum_variance_reduction/1`` - minimum variance reduction required
  for a split (default: ``0.0``)
- ``feature_scaling/1`` - whether to scale continuous features in the
  regression-tree backend (default: ``false``)

Usage
-----

Learning a classifier
~~~~~~~~~~~~~~~~~~~~~

::

   | ?- gradient_boosting_classifier::learn(weather, Classifier).

   | ?- gradient_boosting_classifier::learn(iris_small, Classifier, [number_of_estimators(50), learning_rate(0.05)]).

Making predictions
~~~~~~~~~~~~~~~~~~

::

   | ?- gradient_boosting_classifier::learn(weather, Classifier),
        gradient_boosting_classifier::predict(Classifier, [outlook-overcast, temperature-mild, humidity-high, windy-false], Class).

   | ?- gradient_boosting_classifier::learn(mixed, Classifier),
        gradient_boosting_classifier::predict_probabilities(Classifier, [age-40, income-60000, student-yes, credit_rating-fair], Probabilities).

Exporting the classifier
~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- gradient_boosting_classifier::learn(weather, Classifier),
        gradient_boosting_classifier::export_to_clauses(weather, Classifier, classify, Clauses).

   | ?- gradient_boosting_classifier::learn(weather, Classifier),
        gradient_boosting_classifier::export_to_file(weather, Classifier, classify, 'classifier.pl').

Classifier representation
-------------------------

The learned classifier is represented as a compound term with the form:

::

   gradient_boosting_classifier(Classes, InitialScores, StageTrees, Options)

Where:

- ``Classes``: list of class labels
- ``InitialScores``: list of initial log-prior scores, one per class
- ``StageTrees``: list of ``stage_trees(ClassTrees)`` terms, where each
  ``ClassTrees`` value contains
  ``class_tree(Class, LearningRate, Tree)`` terms
- ``Options``: merged training options used to learn the classifier

When exported using ``export_to_clauses/4`` or ``export_to_file/4``,
this classifier term is serialized directly as the single argument of
the generated predicate clause so that the exported model can be loaded
and reused as-is.

References
----------

1. Friedman, J.H. (2001). "Greedy Function Approximation: A Gradient
   Boosting Machine".
2. Hastie, T., Tibshirani, R. and Friedman, J. (2009). "The Elements of
   Statistical Learning". Chapter 10.
3. Bishop, C.M. (2006). "Pattern Recognition and Machine Learning".
   Section 14.4.
