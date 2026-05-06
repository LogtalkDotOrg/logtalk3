.. _library_knn_distance_anomaly_detector:

``knn_distance_anomaly_detector``
=================================

k-nearest-neighbor distance anomaly detector supporting multiple
distance metrics, mixed continuous and categorical features, and missing
values. The detector memorizes the training instances and computes an
anomaly score from normalized distances to the nearest neighbors. Larger
distances indicate more isolated and therefore more anomalous instances.

The library implements the ``anomaly_detector_protocol`` defined in the
``anomaly_detection_protocols`` library. It learns a compact detector
from a dataset by selecting baseline training examples from the declared
class labels, computes anomaly scores for new instances, predicts
``normal`` or ``anomaly``, and exports learned detectors as clauses or
files.

Datasets are represented as objects implementing the
``anomaly_dataset_protocol`` protocol from the
``anomaly_detection_protocols`` library. See the
``anomaly_detection_protocols/test_datasets`` directory for examples.

API documentation
-----------------

Open the
`../../apis/library_index.html#knn_distance_anomaly_detector <../../apis/library_index.html#knn_distance_anomaly_detector>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(knn_distance_anomaly_detector(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(knn_distance_anomaly_detector(tester)).

Features
--------

- **Distance-based anomaly scoring**: supports both distance to the k-th
  neighbor and average distance to the k nearest neighbors.

- **Mixed features**: automatically handles continuous and categorical
  features declared by the dataset.

- **Missing values**: ignores missing dimensions while normalizing
  distances (distances are normalized by the number of comparable
  dimensions).

- **Baseline training selection**: ``baseline_class_values/1`` declares
  which class labels are admissible for fitting the detector, while
  ``baseline_selection_policy/1`` controls whether non-baseline examples
  are rejected (default) or filtered before training.

- **Multiple metrics**: supports Euclidean, Manhattan, Chebyshev, and
  Minkowski distance metrics.

- **Detector export**: learned detectors can be exported as predicate
  clauses.

- **Dataset validation**: learning rejects empty datasets with a
  ``domain_error(non_empty_dataset, Dataset)`` exception.

Options
-------

The following options can be passed to the ``learn/3`` and ``predict/4``
predicates:

- ``k(K)``: Number of neighbors to consider (default: 5)
- ``distance_metric(Metric)``: Distance metric to use. Options:
  ``euclidean`` (default), ``manhattan``, ``chebyshev``, ``minkowski``
- ``score_mode(Mode)``: Score computation mode. Options:
  ``kth_distance`` (default) and ``mean_distance``
- ``anomaly_threshold(Threshold)``: Threshold for ``predict/3-4``
  (default: ``0.5``)
- ``baseline_class_values(Classes)``: Learn-time list of admissible
  baseline class labels (default: ``[normal]``)
- ``baseline_selection_policy(Policy)``: Learn-time handling of
  non-baseline examples. Supported values are ``reject`` (default) and
  ``filter``

Detector representation
-----------------------

The learned detector is represented by default as:

::

   knn_distance_detector(TrainingDataset, AttributeNames, FeatureTypes, AttributeScales, Instances, ReferenceScores, Diagnostics)

Where:

- ``AttributeNames``: List of attribute names in order
- ``FeatureTypes``: List of feature types (``numeric`` or
  ``categorical``)
- ``AttributeScales``: Normalization scales for numeric features
- ``Instances``: List of retained baseline training ``Id-Class-Values``
  triples
- ``ReferenceScores``: Cached leave-one-out raw training scores for the
  retained baseline training instances
- ``Diagnostics``: Learned metadata terms including ``model/1``,
  ``training_dataset/1``, ``attribute_names/1``, ``feature_types/1``,
  ``example_count/1``, ``reference_score_count/1``, and ``options/1``

The ``score/3`` predicate always treats its input as a fresh query. Only
``score_all/3`` on the original training dataset with the ``reject``
baseline selection policy reuses the cached leave-one-out
``ReferenceScores`` for all examples. With the ``filter`` policy,
retained baseline training examples reuse the cached leave-one-out
scores while excluded examples are scored as fresh queries against the
learned baseline detector.

When exported using ``export_to_clauses/4`` or ``export_to_file/4``,
this detector term is serialized directly as the single argument of the
generated predicate clause so that the exported model can be loaded and
reused as-is.

References
----------

1. Angiulli, F. and Pizzuti, C. (2002). "Fast outlier detection in high
   dimensional spaces". PKDD.
2. Chandola, V., Banerjee, A., and Kumar, V. (2009). "Anomaly detection:
   A survey". ACM Computing Surveys.
