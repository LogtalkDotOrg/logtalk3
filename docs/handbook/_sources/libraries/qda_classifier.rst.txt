.. _library_qda_classifier:

``qda_classifier``
==================

Quadratic Discriminant Analysis classifier for continuous datasets using
class-specific covariance estimates with diagonal regularization. The
implementation learns one quadratic discriminant model per class and
predicts the class with the highest class-specific score.

The library implements the ``classifier_protocol`` defined in the
``classification_protocols`` library. It provides predicates for
learning a classifier from a dataset, using it to make predictions,
inspecting class scores, and exporting it as a list of predicate clauses
or to a file.

Datasets are represented as objects implementing the
``dataset_protocol`` protocol from the ``classification_protocols``
library. All dataset attributes must be declared as ``continuous``.

API documentation
-----------------

Open the
`../../docs/library_index.html#qda_classifier <../../docs/library_index.html#qda_classifier>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(qda_classifier(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(qda_classifier(tester)).

Features
--------

- **Continuous Datasets**: Accepts only datasets whose attributes are
  all declared as continuous.
- **Class-Specific Covariances**: Learns one covariance matrix, mean
  vector, and prior per class.
- **Regularized Estimation**: Applies configurable diagonal
  regularization to each class covariance matrix before inversion.
- **Feature Scaling**: Supports optional z-score scaling of continuous
  features before training.
- **Score Inspection**: Provides class scores for all classes using
  ``predict_scores/3``.
- **Classifier Export**: Learned classifiers can be exported as
  predicate clauses or written to a file.

Options
-------

The ``learn/3`` predicate supports these options:

- ``feature_scaling/1`` - whether to standardize continuous attributes
  before training (default: ``true``)
- ``regularization/1`` - positive diagonal value added to each class
  covariance matrix before inversion (default: ``1.0e-6``)

Usage
-----

Learning a classifier
~~~~~~~~~~~~~~~~~~~~~

::

   | ?- qda_classifier::learn(iris_small, Classifier).

   | ?- qda_classifier::learn(iris_small, Classifier, [regularization(1.0e-5)]).

Making predictions
~~~~~~~~~~~~~~~~~~

::

   | ?- qda_classifier::learn(iris_small, Classifier),
        qda_classifier::predict(Classifier, [sepal_length-5.9, sepal_width-3.0, petal_length-5.1, petal_width-1.8], Class).

   | ?- qda_classifier::learn(iris_small, Classifier),
        qda_classifier::predict_scores(Classifier, [sepal_length-6.4, sepal_width-3.2, petal_length-4.5, petal_width-1.5], Scores).

Exporting the classifier
~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- qda_classifier::learn(iris_small, Classifier),
        qda_classifier::export_to_clauses(iris_small, Classifier, classify, Clauses).

   | ?- qda_classifier::learn(iris_small, Classifier),
        qda_classifier::export_to_file(iris_small, Classifier, classify, 'classifier.pl').

Classifier representation
-------------------------

The learned classifier is represented as a compound term with the form:

::

   qda_classifier(Encoders, Models, Options)

Where:

- ``Encoders``: list of continuous feature encoders with learned scaling
  parameters
- ``Models``: list of
  ``class_model(Class, Prior, Mean, Precision, LogDeterminant, Constant)``
  terms
- ``Options``: merged training options used to learn the classifier

When exported using ``export_to_clauses/4`` or ``export_to_file/4``,
this classifier term is serialized directly as the single argument of
the generated predicate clause so that the exported model can be loaded
and reused as-is.

References
----------

1. Hastie, T., Tibshirani, R. and Friedman, J. (2009). "The Elements of
   Statistical Learning". Section 4.3.
2. Bishop, C.M. (2006). "Pattern Recognition and Machine Learning".
   Section 4.2.
3. Duda, R.O., Hart, P.E. and Stork, D.G. (2001). "Pattern
   Classification". Chapter 2.
