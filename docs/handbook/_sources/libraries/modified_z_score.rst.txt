.. _library_modified_z_score:

``modified_z_score``
====================

Statistical modified Z-score anomaly detector for continuous datasets.
It is a statistical anomaly-detection method based on the modified
Z-score defined by Iglewicz and Hoaglin (1993): for each known
continuous attribute value ``x``, the detector computes
``0.6745 * (x - median) / mad``, where ``median`` is the learned sample
median and ``mad`` is the learned median absolute deviation, and then
aggregates the per-attribute modified Z-scores using the selected
learn-time ``score_mode/1`` option.

The library implements the ``anomaly_detector_protocol`` defined in the
``anomaly_detection_protocols`` library. It learns a detector from a
continuous dataset, computes anomaly scores for new instances, predicts
``normal`` or ``anomaly``, and exports learned detectors as clauses or
files.

Datasets are represented as objects implementing the
``anomaly_dataset_protocol`` protocol from the
``anomaly_detection_protocols`` library. See the
``anomaly_detection_protocols/test_datasets`` directory for examples.

API documentation
-----------------

Open the
`../../apis/library_index.html#modified-z-score <../../apis/library_index.html#modified-z-score>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(modified_z_score(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(modified_z_score(tester)).

Features
--------

- **Statistical method**: implements robust anomaly detection based on
  the modified Z-score described by Iglewicz and Hoaglin (1993), using
  the sample median and median absolute deviation of each continuous
  attribute to measure how far new observations deviate from the
  training data distribution.

- **Classical per-attribute modified Z-score**: for each known attribute
  value ``x``, the library computes the standard robust score
  ``0.6745 * (x - median) / mad``, where ``median`` is the learned
  sample median for that attribute and ``mad`` is the learned median
  absolute deviation.

- **Continuous features only**: accepts datasets whose declared
  attributes are all ``continuous``.

- **Robust statistics**: reuses the ``statistics`` library ``sample``
  object to compute per-attribute medians and the median of absolute
  deviations.

- **Baseline training selection**: supports learn-time
  ``baseline_class_values(ClassValues)`` and
  ``baseline_selection_policy(Policy)`` options. The default baseline
  class values are ``[normal]``. The default ``reject`` policy throws an
  error if non-baseline examples are present, while ``filter`` removes
  them before fitting.

- **Missing-value tolerant**: ignores missing values when fitting
  attribute statistics. During scoring, queries must provide at least
  one known value. In the default ``score_mode(root_mean_square)``, the
  raw score is normalized by the number of known values so that scores
  remain comparable across different missing-value patterns.

- **Configurable scoring semantics**: supports both dense multivariate
  deviation scoring using ``score_mode(root_mean_square)`` and sparse
  anomaly detection using ``score_mode(any_feature_extreme)``. The
  default root-mean-square mode reuses the ``numberlist`` library
  Euclidean norm predicate as part of the computation. The
  ``score_mode/1`` option only controls how the per-attribute modified
  Z-scores are aggregated into a single raw anomaly score.

- **Bounded scoring**: maps the raw multivariate modified Z-score to
  ``[0.0, 1.0)`` using ``Score = Raw / (1 + Raw)``.

- **Default threshold**: the default
  ``anomaly_threshold(0.7777777777777778)`` corresponds to the classical
  raw modified Z-score cutoff ``3.5`` recommended by Iglewicz and
  Hoaglin (1993), while remaining overrideable in ``learn/3`` and
  ``predict/4``.

- **Learn-time score mode**: ``score_mode/1`` is recorded in the learned
  detector and reused for subsequent scoring and prediction. Passing a
  ``score_mode/1`` option to ``predict/4`` does not override the learned
  mode.

- **All-missing queries rejected**: scoring and prediction throw a
  ``domain_error(non_empty_known_values, AttributeNames)`` exception
  when every declared feature is missing in the query.

- **Featureless datasets rejected**: datasets must declare at least one
  continuous feature; otherwise ``learn/2-3`` throws a
  ``domain_error(non_empty_features, Dataset)`` exception.

- **Detector export**: learned detectors can be exported as predicate
  clauses.

- **Explicit validation and diagnostics**: supports the shared
  ``check_anomaly_detector/1``, ``valid_anomaly_detector/1``,
  ``diagnostics/2``, ``diagnostic/2``, and
  ``anomaly_detector_options/2`` predicates.

Options
-------

The following options are supported by the public API:

- ``anomaly_threshold(Threshold)``: Threshold for ``predict/3-4``
  (default: ``0.7777777777777778``)
- ``baseline_class_values(ClassValues)``: Learn-time class labels that
  are admissible for baseline fitting (default: ``[normal]``)
- ``baseline_selection_policy(Policy)``: Learn-time handling of examples
  whose class is not listed in ``baseline_class_values/1``. Supported
  values are ``filter`` and ``reject`` (default: ``reject``)
- ``score_mode(Mode)``: Learn-time score aggregation mode for
  ``learn/3``. Supported values are ``root_mean_square`` and
  ``any_feature_extreme`` (default: ``root_mean_square``). If passed to
  ``predict/4``, it is ignored and the value stored in the learned
  detector is used.

Detector representation
-----------------------

The learned detector is represented by default as:

::

   modified_z_score_detector(TrainingDataset, Encoders, Diagnostics)

Where:

- ``TrainingDataset``: training dataset object identifier
- ``Encoders``: list of ``modified_zscore(Attribute, Median, Scale)``
  records
- ``Diagnostics``: learned metadata terms including ``model/1``,
  ``training_dataset/1``, ``attribute_names/1``, ``feature_count/1``,
  ``example_count/1``, and ``options/1``

When exported using ``export_to_clauses/4`` or ``export_to_file/4``,
this detector term is serialized directly as the single argument of the
generated predicate clause so that the exported model can be loaded and
reused as-is.

Notes
-----

Scoring has three stages. First, the detector computes one classical
per-attribute modified Z-score for each known attribute value using
``0.6745 * (x - median) / mad``. Second, those per-attribute modified
Z-scores are aggregated into a single raw anomaly score according to the
learned ``score_mode/1`` option. Third, the raw score is mapped to the
interval ``[0.0, 1.0)`` using ``Score = Raw / (1 + Raw)``.

The ``score_mode/1`` option does not change the classical per-attribute
formula. It only changes the aggregation step. With
``score_mode(root_mean_square)``, the raw score is the root mean square
of the per-attribute modified Z-scores. With
``score_mode(any_feature_extreme)``, the raw score is the maximum
absolute per-attribute modified Z-score.

The ``baseline_class_values/1`` option declares which dataset class
labels are admissible for fitting the baseline medians and median
absolute deviations. The ``baseline_selection_policy/1`` option then
controls what happens when other labels are present in the training
data. The default ``reject`` policy raises a
``domain_error(baseline_only_training_data, Dataset)`` exception when
any non-baseline example is found. The ``filter`` policy removes
non-baseline examples before fitting.

Attributes with zero observed median absolute deviation are assigned a
fallback scale of ``1.0``. This keeps the detector well-defined for
singleton datasets or constant columns while still yielding zero score
for matching values and positive scores for deviating values.

The root-mean-square aggregation keeps the default threshold stable as
the number of observed dimensions grows and avoids penalizing partially
observed queries solely for having fewer known attributes.

Use ``score_mode(any_feature_extreme)`` when a single extreme feature
should be sufficient to flag an anomaly in high-dimensional data.
