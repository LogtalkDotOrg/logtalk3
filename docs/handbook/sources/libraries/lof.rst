.. _library_lof:

``lof``
=======

Local Outlier Factor anomaly detector supporting multiple distance
metrics, mixed continuous and categorical features, and missing values.

The library implements the ``anomaly_detector_protocol`` defined in the
``anomaly_protocols`` library. It learns a compact detector from a
dataset, computes normalized anomaly scores for new instances, predicts
``normal`` or ``anomaly``, and exports learned detectors as clauses or
files.

Datasets are represented as objects implementing the
``anomaly_dataset_protocol`` protocol from the ``anomaly_protocols``
library. See the ``anomaly_protocols/test_datasets`` directory for
examples.

API documentation
-----------------

Open the
`../../apis/library_index.html#lof <../../apis/library_index.html#lof>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(lof(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(lof(tester)).

Features
--------

- **Density-based anomaly scoring**: computes Local Outlier Factor
  scores from local reachability densities.

- **Mixed features**: automatically handles continuous and categorical
  features declared by the dataset.

- **Missing values**: ignores missing dimensions while normalizing
  distances.

- **Multiple metrics**: supports Euclidean, Manhattan, Chebyshev, and
  Minkowski distance metrics.

- **Detector export**: learned detectors can be exported as predicate
  clauses.

Options
-------

The following options can be passed to the ``learn/3`` and ``predict/4``
predicates:

- ``k(K)``: Number of neighbors to consider (default: 5)
- ``distance_metric(Metric)``: Distance metric to use. Options:
  ``euclidean`` (default), ``manhattan``, ``chebyshev``, ``minkowski``
- ``anomaly_threshold(Threshold)``: Threshold for ``predict/3-4``
  (default: ``0.4``)

Detector Representation
-----------------------

The learned detector is represented by default as:

::

   lof_detector(AttributeNames, FeatureTypes, AttributeScales, Instances, Options)

Where:

- ``AttributeNames``: List of attribute names in order
- ``FeatureTypes``: List of feature types (``numeric`` or
  ``categorical``)
- ``AttributeScales``: Normalization scales for numeric features
- ``Instances``: List of training ``Id-Class-Values`` triples
- ``Options``: Learned options

References
----------

1. Breunig, M. M., Kriegel, H.-P., Ng, R. T., and Sander, J. (2000).
   "LOF: Identifying density-based local outliers". SIGMOD.
