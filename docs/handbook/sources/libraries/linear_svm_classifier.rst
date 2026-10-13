.. _library_linear_svm_classifier:

``linear_svm_classifier``
=========================

Linear support vector machine classifier supporting both binary and
multiclass classification. Multiclass classification is implemented
using a one-vs-rest scheme with one linear margin model per class, and
training uses batch subgradient descent to fit each hinge-loss model.

For binary classification, the second margin model is the negation of
the first model, avoiding a second optimization run. Both models are
stored in the declared class order. Single-class and multiclass datasets
use ordinary per-class training. Training minimizes mean hinge loss with
an L2 penalty on the weights; the bias is not regularized.

The library implements the ``classifier_protocol`` defined in the
``classification_protocols`` library. It provides predicates for
learning a classifier from a dataset object, using it to make
predictions, and exporting the learned model as a list of predicate
clauses or to a file.

Datasets are represented as objects implementing the
``dataset_protocol`` protocol from the ``classification_protocols``
library. Existing datasets in ``classification_protocols/test_datasets``
can be used for binary, multiclass, continuous, categorical, and
mixed-feature testing.

API documentation
-----------------

Open the
`../../apis/library_index.html#linear_svm_classifier <../../apis/library_index.html#linear_svm_classifier>`__
link in a web browser.

Loading
-------

To load this library, load its ``loader.lgt`` file:

::

   | ?- logtalk_load(linear_svm_classifier(loader)).

Testing
-------

To test this library, load its ``tester.lgt`` file:

::

   | ?- logtalk_load(linear_svm_classifier(tester)).

To run the performance benchmark suite, load the
``tester_performance.lgt`` file:

::

   | ?- logtalk_load(linear_svm_classifier(tester_performance)).

The unit suite retains fresh deterministic learning tests and caches
their models for downstream tests, with lazy loading for isolated tests.

Features
--------

- **Binary and Multiclass Classification**: Learns one-vs-rest linear
  margin models and predicts the class with the highest score.
- **Continuous Features**: Standardizes numeric attributes using z-score
  scaling derived from the training data.
- **Scaling Defaults**: Uses population variance over known values.
  All-missing continuous attributes use mean ``0.0`` and scale ``1.0``;
  singleton and constant attributes use scale ``1.0``.
- **Categorical Features**: Expands discrete attributes using one-hot
  encoding based on the declared dataset attribute values and rejects
  unseen values with a domain error.
- **Missing Values**: Encodes missing numeric and categorical values
  represented using anonymous variables using explicit missing-value
  indicator features.
- **Unknown values**: Prediction requests containing categorical values
  that are not declared by the dataset raise a domain error.
- **Bias-Only Models**: Non-empty datasets without attributes produce
  models with empty weight vectors. Prediction compares their biases.
- **Tie Handling**: Equal scores select the first stored margin model.
- **Classifier Export**: Learned classifiers can be exported as
  predicate clauses or written to a file.
- **Pretty Printing**: Prints a compact summary of classes, encoders,
  and learned weight vector sizes.
- **Reference Benchmarks**: Includes a dedicated performance suite
  covering the ``weather``, ``mixed``, ``iris_small``,
  ``missing_mixed``, and ``breast_cancer`` datasets with reported
  training time and training accuracy.

Options
-------

The ``learn/3`` predicate supports these options:

- ``learning_rate(Rate)`` - Positive numeric base learning rate
  (default: ``0.1``). The step at zero-based iteration ``Iteration`` is
  ``Rate / sqrt(Iteration + 1.0)``.
- ``maximum_iterations(Count)`` - Positive integer iteration limit per
  margin model (default: ``1000``).
- ``tolerance(Tolerance)`` - Non-negative numeric threshold for the
  maximum absolute bias or weight update (default: ``1.0e-6``). This
  criterion depends on the learning rate and does not establish
  objective convergence.
- ``l2_regularization(Regularization)`` - Non-negative numeric L2 factor
  applied to weights, not biases (default: ``0.01``). Zero disables it.

Repeated options are accepted; the first occurrence determines the
value. Training examples must contain each declared attribute exactly
once. Unbound values denote missing observations; omitted prediction
attributes are also treated as missing.

Usage
-----

Learning a classifier
~~~~~~~~~~~~~~~~~~~~~

::

   | ?- linear_svm_classifier::learn(weather, Classifier).

   | ?- linear_svm_classifier::learn(iris_small, Classifier, [learning_rate(0.05), maximum_iterations(1500)]).

Making predictions
~~~~~~~~~~~~~~~~~~

::

   | ?- linear_svm_classifier::learn(mixed, Classifier),
        linear_svm_classifier::predict(Classifier, [age-45, income-75000, student-no, credit_rating-fair], Class).

Exporting the classifier
~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- linear_svm_classifier::learn(weather, Classifier),
        linear_svm_classifier::export_to_clauses(weather, Classifier, classify, Clauses).

   | ?- linear_svm_classifier::learn(weather, Classifier),
        linear_svm_classifier::export_to_file(weather, Classifier, classify, 'classifier.pl').

Classifier representation
-------------------------

The learned classifier is represented as a compound term with the form:

::

   linear_svm_classifier(Classes, Encoders, Models, Options)

Where:

- ``Classes``: list of class labels
- ``Encoders``: list of continuous scaling descriptors and categorical
  value lists
- ``Models``: list of ``class_model(Class, Bias, Weights)`` terms
- ``Options``: merged training options used to learn the model

When exported using ``export_to_clauses/4`` or ``export_to_file/4``,
this classifier term is serialized directly as the single argument of
the generated predicate clause so that the exported model can be loaded
and reused as-is.

The outer functor can be user-chosen when constructing or restoring a
classifier. Validation, prediction, diagnostics, printing, and export
use its four positional arguments rather than its name. Encoders and
coefficients retain the same dense representation for binary,
multiclass, and bias-only models.

Validation
----------

The ``check_classifier/1`` predicate raises an instantiation error for
an unbound or non-ground classifier, without completing partial model
labels, weights, encoders, or options. Ground terms with invalid
classes, encoders, model dimensions, coefficients, or options raise
``domain_error(classifier, Classifier)``. The ``valid_classifier/1``
predicate fails for both kinds of invalid input. Stored options follow
the shared ``options`` validation rules; a ground list is not rejected
solely because it omits defaults.

Performance measurements
------------------------

The reference-fit benchmarks time one fit and evaluate that same model.
Reported accuracy is training accuracy, not held-out generalization
accuracy. Additional benchmarks cover categorical workloads with 66 and
258 encoded features, encoding, prediction, and continuous
preprocessing. Repeated measurements use a separate warm-up and the
median of five samples; fixture construction and learning are excluded
from timed encoding and prediction batches.

The implementation resolves training options and row-count scaling once,
reuses immutable zero-gradient vectors, constructs categorical feature
vectors directly, and selects the predicted class while scoring models.
Sparse training remains a separate optimization requiring portable
indexed weight access and dense-model equivalence checks. The benchmark
suite retains a multi-pass reference for future comparisons.

Limitations
-----------

- Decision boundaries are linear in the encoded feature space. Kernel
  methods and automatic feature interactions are not supported.
- Encoded examples and model weights use dense lists. High-cardinality
  categorical attributes increase memory use and training cost even when
  most encoded features are zero.
- Training is batch-based and keeps all encoded examples in memory. Each
  optimization iteration scans the training rows; multiclass training
  performs a separate optimization for each class. Incremental and
  mini-batch learning are not supported.
- Batch subgradient descent is sensitive to the learning rate,
  regularization, tolerance, and iteration limit. A small parameter
  update or an exhausted iteration budget does not certify that the
  objective has reached its minimum.
- Categorical domains and continuous scaling statistics are fixed at
  training time. Unseen categorical values are rejected; adapting to
  changed domains or distributions requires retraining.
- Margin scores are not calibrated class probabilities. Class-specific
  penalties and per-example weights are not supported.
- The reference-fit benchmarks measure training accuracy only. They do
  not establish generalization performance on unseen examples.

References
----------

1. Cortes, C. and Vapnik, V. (1995). "Support-Vector Networks".
2. Bishop, C.M. (2006). "Pattern Recognition and Machine Learning".
   Section 7.1.
3. Hastie, T., Tibshirani, R. and Friedman, J. (2009). "The Elements of
   Statistical Learning". Section 12.3.
