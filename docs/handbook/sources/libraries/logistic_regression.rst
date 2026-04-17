.. _library_logistic_regression:

``logistic_regression``
=======================

Logistic regression classifier supporting both binary and multiclass
classification. Multiclass classification is implemented using a joint
softmax model trained across all classes.

The library implements the ``classifier_protocol`` defined in the
``classifier_protocols`` library. It provides predicates for learning a
classifier from a dataset object, using it to make predictions,
returning class probabilities, and exporting the learned model as a list
of predicate clauses or to a file.

Datasets are represented as objects implementing the
``dataset_protocol`` protocol from the ``classifier_protocols`` library.
Existing datasets in ``classifier_protocols/test_datasets`` can be used
for binary, multiclass, continuous, categorical, and mixed-feature
testing.

API documentation
-----------------

Open the
`../../docs/library_index.html#logistic_regression <../../docs/library_index.html#logistic_regression>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(logistic_regression(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(logistic_regression(tester)).

To run the reference timing and fit benchmarks, load the
``tester_performance.lgt`` file:

::

   | ?- logtalk_load(logistic_regression(tester_performance)).

Features
--------

- **Binary and Multiclass Classification**: Learns a joint softmax
  logistic model with one parameter vector per class.
- **Continuous Features**: Standardizes numeric attributes using z-score
  scaling derived from the training data.
- **Categorical Features**: Expands discrete attributes using one-hot
  encoding based on the declared dataset attribute values and rejects
  unseen values with a domain error.
- **Missing Values**: Encodes missing numeric and categorical values
  using explicit missing-value indicator features.
- **Probability Estimation**: Provides class probability distributions
  in addition to class predictions.
- **Classifier Export**: Learned classifiers can be exported as
  predicate clauses or written to a file.
- **Reference Benchmarks**: Includes a dedicated performance suite
  covering the ``weather``, ``mixed``, ``iris_small``,
  ``missing_mixed``, and ``breast_cancer`` datasets with reported
  training time, training accuracy, and mean log loss.

Options
-------

The ``learn/3`` predicate supports these options:

- ``learning_rate/1`` - gradient descent learning rate (default:
  ``0.1``)
- ``maximum_iterations/1`` - maximum number of optimization iterations
  (default: ``1000``)
- ``tolerance/1`` - convergence threshold for the maximum parameter
  update (default: ``1.0e-6``)
- ``l2_regularization/1`` - L2 penalty factor applied to weights
  (default: ``0.0``)

Usage
-----

Learning a classifier
~~~~~~~~~~~~~~~~~~~~~

::

   | ?- logistic_regression::learn(weather, Classifier).

   | ?- logistic_regression::learn(iris_small, Classifier, [learning_rate(0.05), maximum_iterations(1500)]).

Making predictions
~~~~~~~~~~~~~~~~~~

::

   | ?- logistic_regression::learn(mixed, Classifier),
        logistic_regression::predict(Classifier, [age-45, income-75000, student-no, credit_rating-fair], Class).

   | ?- logistic_regression::learn(iris_small, Classifier),
        logistic_regression::predict_probabilities(Classifier, [sepal_length-6.4, sepal_width-3.0, petal_length-5.8, petal_width-2.2], Probabilities).

Exporting the classifier
~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- logistic_regression::learn(weather, Classifier),
        logistic_regression::classifier_to_clauses(weather, Classifier, classify, Clauses).

   | ?- logistic_regression::learn(weather, Classifier),
        logistic_regression::classifier_to_file(weather, Classifier, classify, 'classifier.pl').

Classifier representation
-------------------------

The learned classifier is represented internally as a compound term with
the form:

::

   lr_classifier(Classes, Encoders, Models, Options)

When exported, the functor is chosen by the caller.

Where:

- ``Classes``: list of class labels
- ``Encoders``: list of continuous scaling descriptors and categorical
  value lists
- ``Models``: list of ``class_model(Class, Bias, Weights)`` terms
- ``Options``: merged training options used to learn the model

References
----------

1. Hosmer, D.W., Lemeshow, S. and Sturdivant, R.X. (2013). "Applied
   Logistic Regression".
2. Bishop, C.M. (2006). "Pattern Recognition and Machine Learning".
   Chapter 4.
3. Hastie, T., Tibshirani, R. and Friedman, J. (2009). "The Elements of
   Statistical Learning". Chapter 4.
