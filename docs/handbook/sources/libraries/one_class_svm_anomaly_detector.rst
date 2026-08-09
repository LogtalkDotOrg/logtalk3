.. _library_one_class_svm_anomaly_detector:

``one_class_svm_anomaly_detector``
==================================

One-class support vector machine anomaly detector for continuous,
categorical, and mixed-feature datasets. The implementation uses the
same linear, polynomial, and radial basis function kernel specifications
as the ``kernel_svm_classifier`` library.

The library implements the ``anomaly_detector_protocol`` defined in the
``anomaly_detection_protocols`` library. Training examples are selected
using the shared baseline class options. The learned dual coefficients
satisfy the one-class SVM capped-simplex constraints. Decision values
are converted to bounded anomaly scores using their empirical rank among
the training values.

API documentation
-----------------

Open the
`../../docs/library_index.html#one-class-svm-anomaly-detector <../../docs/library_index.html#one-class-svm-anomaly-detector>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(one_class_svm_anomaly_detector(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(one_class_svm_anomaly_detector(tester)).

Features
--------

- **One-Class Learning**: Fits a boundary using only selected baseline
  examples.
- **Multiple Kernels**: Supports ``linear``,
  ``polynomial(Degree, Gamma, Coef0)``, and ``rbf(Gamma)`` kernels.
- **Mixed Features**: Encodes continuous and categorical attributes with
  explicit missing-value indicators. Unseen categorical values use the
  corresponding missing-value indicator.
- **Bounded Scores**: Returns empirical anomaly scores in the interval
  ``[0.0, 1.0]``.
- **Support-Vector Pruning**: Omits zero and optionally negligible
  coefficients from learned detectors.
- **Model Export**: Learned detectors can be exported as predicate
  clauses or written to a file.

Options
-------

The ``learn/3`` predicate supports these options:

- ``kernel/1`` - kernel function to use (default: ``rbf(0.5)``)
- ``nu/1`` - upper bound on the fraction of training anomalies and lower
  bound on the fraction of support vectors (default: ``0.1``)
- ``learning_rate/1`` - base projected-gradient learning rate (default:
  ``0.1``)
- ``learning_schedule/1`` - learning-rate schedule, either ``constant``
  or ``inverse_scaling(Power)`` (default: ``constant``)
- ``support_vector_tolerance/1`` - coefficients at or below this value
  are omitted from the learned detector; the largest coefficient is
  always retained (default: ``0.0``)
- ``maximum_iterations/1`` - maximum number of optimization iterations
  (default: ``100``)
- ``tolerance/1`` - convergence threshold for the maximum coefficient
  update (default: ``1.0e-6``)
- ``feature_scaling/1`` - whether to standardize continuous attributes
  (default: ``true``)
- ``anomaly_threshold/1`` - score threshold used by prediction (default:
  ``0.95``)
- ``baseline_class_values/1`` - class values accepted as baseline
  examples (default: ``[normal]``)
- ``baseline_selection_policy/1`` - either ``reject`` or ``filter``
  (default: ``reject``)

Limitations
-----------

- Training materializes the full kernel Gram matrix. For ``N`` baseline
  examples, memory use is quadratic in ``N`` and each optimization
  iteration is also quadratic. Prediction evaluates the kernel against
  every retained support vector. The implementation is therefore
  intended for small and medium-sized datasets rather than large-scale
  or streaming use.
- Anomaly scores are empirical ranks of raw kernel decision values. They
  are discrete with resolution ``1/N``, are not calibrated
  probabilities, and should not be compared directly across detectors
  trained on different datasets. The ``anomaly_threshold/1`` option
  controls prediction independently of the ``nu/1`` constraint.
- The projected-gradient optimizer can stop at ``maximum_iterations/1``
  before satisfying ``tolerance/1``. The ``iterations/1`` and
  ``final_delta/1`` diagnostics should be inspected when tuning
  difficult datasets.
- Results can be sensitive to the kernel parameters, feature scaling,
  and contamination of the selected baseline examples.

Usage
-----

::

   | ?- one_class_svm_anomaly_detector::learn(gaussian_anomalies, Detector, [baseline_selection_policy(filter)]),
        one_class_svm_anomaly_detector::predict(Detector, [x-4.5, y-4.2], Prediction).

   | ?- one_class_svm_anomaly_detector::learn(mixed_anomalies, Detector, [baseline_selection_policy(filter), kernel(rbf(0.25))]),
        one_class_svm_anomaly_detector::score(Detector, [age-19, income-150000, student-no, credit_rating-excellent], Score).

Detector representation
-----------------------

The learned detector is represented by:

::

   one_class_svm_detector(Encoders, Kernel, SupportVectors, Coefficients, ReferenceScores, Diagnostics)

References
----------

1. Scholkopf, B., Platt, J., Shawe-Taylor, J., Smola, A. and Williamson,
   R. (2001). "Estimating the Support of a High-Dimensional
   Distribution".
