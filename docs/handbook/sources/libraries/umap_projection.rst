.. _library_umap_projection:

``umap_projection``
===================

Portable Uniform Manifold Approximation and Projection for continuous,
categorical, and mixed datasets. The library implements the
``dimension_reducer_protocol`` using exact neighbor discovery, a fuzzy
simplicial graph, spectral initialization, and stochastic layout
optimization.

API documentation
-----------------

Open the
`../../apis/library_index.html#umap_projection <../../apis/library_index.html#umap_projection>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(umap_projection(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(umap_projection(tester)).

Features
--------

- Accepts continuous, categorical, and mixed datasets.
- Encodes categorical attributes using declaration-ordered one-hot
  components and a dedicated missing-value indicator.
- Supports Euclidean, Manhattan, and cosine input metrics.
- Computes exact nearest neighbors and canonical smooth k-NN
  memberships.
- Uses fuzzy union/intersection mixing to construct the training graph.
- Uses normalized spectral initialization with reproducible random
  fallback.
- Fits the low-dimensional UMAP curve parameters from ``min_dist`` and
  ``spread``.
- Uses scheduled stochastic attractive and negative-sampling updates.
- Preserves caller random-generator state during training and
  transformation.
- Supports missing continuous values represented by variables using mean
  imputation.
- Transforms new instances by optimizing each point against the fixed
  training embedding.

Limitations
-----------

- Exact neighbor discovery and dense spectral initialization require
  quadratic time and memory, making this implementation suitable for
  small and medium datasets.
- Approximate nearest-neighbor indexes, sparse eigensolvers, supervised
  UMAP, inverse transformation, and backend-specific acceleration are
  not implemented.
- Training is non-convex. Different seeds can produce different valid
  embeddings; identical data, options, and seeds produce reproducible
  results.
- Embedding axes, orientation, and absolute scale have no standalone
  meaning.
- Out-of-sample points are transformed independently and do not update
  the training embedding or interact with each other.
- High-cardinality categorical attributes expand the encoded row width
  and increase exact-neighbor and dense-spectral costs.
- Missing values must be variables in present ``Attribute-Value``
  bindings. Omitted, duplicate, undeclared, invalid observed, and
  all-missing attributes are rejected.

Options
-------

- ``n_components/1``: Embedding dimensions. The default is ``2``.
- ``feature_scaling/1``: Standardize continuous attributes when
  ``true``. The default is ``true``.
- ``n_neighbors/1``: Requested local neighborhood size. The default is
  ``15``; it is clamped to the sample count.
- ``distance_metric/1``: Input metric: ``euclidean``, ``manhattan``, or
  ``cosine``. The default is ``euclidean``.
- ``initialization/1``: ``spectral`` or ``random``. The default is
  ``spectral``.
- ``min_dist/1``: Non-negative minimum embedding distance. The default
  is ``0.1``.
- ``spread/1``: Positive embedding scale and an upper bound for
  ``min_dist``. The default is ``1.0``.
- ``local_connectivity/1``: Positive local-connectivity adjustment. The
  default is ``1.0``.
- ``set_op_mix_ratio/1``: Fuzzy union/intersection mix in the interval
  ``[0,1]``. The default is ``1.0``.
- ``learning_rate/1``: Positive initial learning rate. The default is
  ``1.0``.
- ``repulsion_strength/1``: Non-negative negative-sampling repulsion
  multiplier. The default is ``1.0``.
- ``negative_sample_rate/1``: Positive number of negative samples per
  positive update. The default is ``5``.
- ``maximum_iterations/1``: Positive number of training epochs. The
  default is ``500``.
- ``random_seed/1``: Positive training seed. The default is ``1357911``.
- ``transform_seed/1``: Positive out-of-sample transformation seed. The
  default is ``42``.

Usage
-----

::

   | ?- logtalk_load(dimension_reduction_protocols('test_datasets/correlated_plane')).

   | ?- umap_projection::learn(correlated_plane, DimensionReducer, [n_neighbors(3)]).

   | ?- umap_projection::transform(DimensionReducer, [x-2.0, y-4.0, z-6.0], ReducedInstance).

Missing values use variables in present bindings:

::

   | ?- umap_projection::transform(DimensionReducer, [x-_, y-4.0, z-6.0], ReducedInstance).

Categorical attributes declare their possible values as a list. Mixed
datasets use the same protocol and instance representation:

::

   attribute_values(age, continuous).
   attribute_values(channel, [online, retail]).

   | ?- umap_projection::transform(DimensionReducer, [age-35.0, channel-retail], ReducedInstance).

Categorical values are expanded in declaration order. For
``[online, retail]``, the encoded components are
``[Online, Retail, Missing]``: ``online`` becomes ``[1.0, 0.0, 0.0]``,
``retail`` becomes ``[0.0, 1.0, 0.0]``, and a variable becomes
``[0.0, 0.0, 1.0]``. Euclidean, Manhattan, and cosine distances operate
on the resulting numeric rows.

Dimension reducer representation
--------------------------------

Learned reducers use this representation:

::

   umap_reducer(Encoders, ExampleIds, TrainingRows, EmbeddingRows, FuzzyGraph, Diagnostics)

``FuzzyGraph`` is a sorted list of ``edge(Source, Target, Weight)``
terms with zero-based sample indexes. Diagnostics record options, fitted
curve parameters, initialization mode, source and encoded feature
counts, graph size, iteration count, and initial/final cross-entropy.

References
----------

1. McInnes, L., Healy, J., and Melville, J. (2018) - "UMAP: Uniform
   Manifold Approximation and Projection for Dimension Reduction".
