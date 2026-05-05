.. _library_knn_regression:

``knn_regression``
==================

k-Nearest Neighbors regressor supporting continuous and mixed-feature
datasets. Learns lazily by storing encoded training rows and predicts
targets as the weighted average of the k nearest neighbors.

The library implements the ``regressor_protocol`` defined in the
``regression_protocols`` library, learns lazily by storing encoded
training rows, and predicts numeric targets using the weighted average
of the nearest encoded neighbors.

API documentation
-----------------

Open the
`../../apis/library_index.html#knn_regression <../../apis/library_index.html#knn_regression>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(knn_regression(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(knn_regression(tester)).

To run the performance benchmark suite, load the
``tester_performance.lgt`` file:

::

   | ?- logtalk_load(knn_regression(tester_performance)).

Features
--------

- **Distance-Based Regression**: Predicts targets using the weighted
  average of the nearest neighbors.
- **Multiple Metrics**: Supports Euclidean, Manhattan, Chebyshev, and
  Minkowski distance metrics over encoded numeric feature vectors.
- **Weighting Schemes**: Supports uniform, inverse-distance, and
  Gaussian weighting of neighbors.
- **Continuous and Mixed Features**: Supports continuous attributes and
  categorical attributes encoded using one-hot vectors from the declared
  dataset attribute values.
- **Optional Feature Scaling**: Continuous attributes can be
  standardized using z-score scaling.
- **Missing Values**: Missing numeric and categorical values represented
  using anonymous variables are encoded using explicit missing-value
  indicator features.
- **Diagnostics Metadata**: Learned regressors record model name,
  target, training example count, encoded feature count, and effective
  options, accessible using the shared regression diagnostics
  predicates.
- **Model Export**: Learned regressors can be exported as predicate
  clauses or written to a file.
- **Reference Benchmarks**: Includes a dedicated performance suite
  reporting training time, RMSE, and MAE for representative regression
  datasets.

Regressor representation
------------------------

The learned regressor is represented by default as:

- ``knn_regressor(Encoders, Rows, Diagnostics)``

The exported predicate clauses therefore use the shape:

- ``Functor(Encoders, Rows, Diagnostics)``

In this representation, ``Rows`` stores encoded feature vectors paired
with numeric targets and ``Diagnostics`` stores training metadata
including the effective options.

Diagnostics syntax
------------------

The ``diagnostics/2`` predicate returns a list of metadata terms with
the form:

::

   [
       model(knn_regression),
       target(Target),
       training_example_count(TrainingExampleCount),
       options(Options),
       encoded_feature_count(FeatureCount)
   ]

Where:

- ``model(knn_regression)`` identifies the learning algorithm that
  produced the regressor.
- ``target(Target)`` stores the target attribute name declared by the
  training dataset.
- ``training_example_count(TrainingExampleCount)`` stores the number of
  examples used during training.
- ``options(Options)`` stores the effective learning options after
  merging the user options with the library defaults.
- ``encoded_feature_count(FeatureCount)`` stores the number of numeric
  features induced by the encoder list, including missing-value
  indicator features.

Use the ``regression_protocols`` ``diagnostic/2`` and
``regressor_options/2`` helper predicates when you only need a single
metadata term or the effective options.

Options
-------

The ``learn/3`` predicate accepts the following options:

- ``k/1``: Number of nearest neighbors considered for each prediction.
  Smaller values make predictions more local; larger values smooth them
  by averaging over more training rows. The default is ``3``.
- ``distance_metric/1``: Distance function used to compare encoded
  feature vectors. Accepted values are ``euclidean``, ``manhattan``,
  ``chebyshev``, and ``minkowski``. The default is ``euclidean``.
- ``weight_scheme/1``: Neighbor weighting policy used when averaging
  targets. Accepted values are ``uniform``, ``distance``, and
  ``gaussian``. The default is ``uniform``.
- ``minkowski_power/1``: Exponent used when
  ``distance_metric(minkowski)`` is selected. Larger values increase the
  influence of larger coordinate differences. The default is ``3.0``.
- ``feature_scaling/1``: Controls z-score standardization of continuous
  attributes before storing rows and encoding prediction requests.
  Accepted values are ``true`` and ``false``. The default is ``true``.
