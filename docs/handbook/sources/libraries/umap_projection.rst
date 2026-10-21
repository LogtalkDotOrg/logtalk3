.. _library_umap_projection:

``umap_projection``
===================

Portable Uniform Manifold Approximation and Projection for continuous,
categorical, and mixed datasets. The library implements
``dimension_reducer_protocol`` using exact neighbor discovery, a fuzzy
simplicial graph, spectral initialization, and stochastic layout
optimization.

API documentation
-----------------

Open the
`../../apis/library_index.html#umap-projection <../../apis/library_index.html#umap-projection>`__
link in a web browser.

Loading
-------

To load this library, load its ``loader.lgt`` file:

::

   | ?- logtalk_load(umap_projection(loader)).

Testing
-------

To test this library, load its ``tester.lgt`` file:

::

   | ?- logtalk_load(umap_projection(tester)).

The tests build shared fixtures during setup for metadata and
transformation checks. Learning determinism, initialization, and
random-state tests learn fresh reducers. Fixed reference calculations
check graph memberships, neighbor ordering, spectral components,
gradients, and exact cross-entropy.

Features
--------

- Accepts continuous, categorical, and mixed datasets.
- Encodes categorical attributes using declaration-ordered one-hot
  components and a dedicated missing-value indicator.
- Supports Euclidean, Manhattan, and cosine input metrics.
- Computes exact nearest neighbors and canonical smooth k-NN
  memberships.
- Uses fuzzy union/intersection mixing to construct the training graph.
- Uses partial normalized spectral initialization with residual and
  orthogonality checks and reproducible random fallback.
- Fits the low-dimensional UMAP curve parameters from ``min_dist`` and
  ``spread``.
- Uses scheduled stochastic attractive and negative-sampling updates.
- Samples negative targets for both endpoints of each training edge. A
  self draw consumes one sampling attempt without moving the point.
- Preserves caller random-generator state during training and
  transformation.
- Supports missing continuous values represented by variables using mean
  imputation.
- Transforms new instances by optimizing each point against the fixed
  training embedding.
- Excludes zero-weight memberships from transform optimization
  schedules, including weights that underflow to zero.
- Uses AVL dictionaries for graph and row indexes and bounded max heaps
  for larger exact-neighbor searches.

Implementation
--------------

Training sets with at most 32 examples use sorted exact distances to
avoid small-input heap overhead. Larger sets compute each unordered
distance once and retain bounded per-row heaps. Both paths place self
first and use sample indexes to break other distance ties. Cosine norms
are computed once per row. Rejected heap candidates leave the
corresponding AVL dictionary entries unchanged. Transformation streams
distances through a bounded heap.

Directed memberships are sorted by canonical endpoint pair and merged
once. AVL-indexed adjacency and weighted degrees support graph traversal
and dense normalized matrix construction. Cross-entropy diagnostics
remain exact: sorted graph edges are consumed alongside all unordered
embedding pairs, including pairs without a graph edge.

Spectral initialization builds the graph index once for connectivity
checks and embedding construction. It shifts normalized adjacency by
twice the identity and deflates the known degree-derived trivial
component. This makes partial eigenvalue selection follow algebraic
order, including for bipartite graphs. Eigenpair residuals and
orthogonality are checked against the original matrix. Disconnected
graphs and unsuccessful spectral results use seeded random
initialization.

Positive edge schedules bind both endpoint handles before optimization.
Negative targets use AVL row lookup. Transformation leaves training rows
fixed and optimizes the new coordinates for 100 epochs. Curve-fitting
sample targets and logarithms are precomputed; training attribute
observations are collected once before encoding.

Proper attribute binding lists with ground keys are indexed once per
example or transformation input. Validation and encoding reuse the index
while preserving declaration order, binding error precedence, and
missing-value variables. Other binding forms retain list-based
processing. Reducer validation uses an AVL set to check ground example
identifier uniqueness in logarithmic time per identifier, excluding term
comparison costs. Indexes remain internal and are not stored in learned
reducers.

Options
-------

- ``n_components/1``: Embedding dimensions. The default is ``2``.
- ``feature_scaling/1``: Standardize continuous attributes when
  ``true``. The default is ``true``.
- ``n_neighbors/1``: Requested self-inclusive training neighborhood
  size. The default is ``15``; it is clamped to the sample count.
  Training uses the self point followed by the nearest other points;
  transformation uses that many nearest training points.
- ``distance_metric/1``: Input metric: ``euclidean``, ``manhattan``, or
  ``cosine``. The default is ``euclidean``.
- ``initialization/1``: ``spectral`` or ``random``. The default is
  ``spectral``.
- ``min_dist/1``: Non-negative minimum embedding distance. The default
  is ``0.1``.
- ``spread/1``: Positive embedding scale and an upper bound for
  ``min_dist``. The default is ``1.0``.
- ``local_connectivity/1``: Positive local-connectivity adjustment. The
  default is ``1.0``. Fractional values interpolate between positive
  distance ranks; values below one interpolate from zero to the first
  positive distance.
- ``set_op_mix_ratio/1``: Fuzzy union/intersection mix in the interval
  ``[0,1]``. The default is ``1.0``.
- ``learning_rate/1``: Positive initial learning rate. The default is
  ``1.0``.
- ``repulsion_strength/1``: Non-negative negative-sampling repulsion
  multiplier. The default is ``1.0``.
- ``negative_sample_rate/1``: Positive number of negative sampling
  attempts per endpoint of a positive training update, or per positive
  transform update. The default is ``5``.
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

Learned reducers use the following term representation:

::

   umap_reducer(Encoders, ExampleIds, TrainingRows, EmbeddingRows, FuzzyGraph, Diagnostics)

``FuzzyGraph`` is a sorted list of ``edge(Source, Target, Weight)``
terms with zero-based sample indexes. Diagnostics record options, fitted
curve parameters, initialization mode, source and encoded feature
counts, graph size, iteration count, and initial/final cross-entropy.

Reducer validation requires the complete effective options. Effective
neighbor counts equal the minimum of the requested neighborhood size and
the sample count. Internal AVL dictionaries and optimization handles are
not serialized.

Training neighborhoods place self first by index, even for duplicate
rows. Other equal-distance neighbors are ordered by their sample
indexes.

Reducers must be ground. The ``check_dimension_reducer/1`` predicate
rejects insufficiently instantiated reducers with an instantiation error
and rejects inconsistent dimensions, diagnostics, or effective options
with a domain error. Repeated options are accepted; the first occurrence
determines the value.

Limitations
-----------

- Exact neighbor discovery requires all pairwise training distances;
  bounded selection and AVL lookup add logarithmic overhead. Dense
  spectral matrices require quadratic memory, and partial eigensolver
  cost depends on the requested dimensions and convergence. This
  implementation is suitable for small and medium datasets.
- Exact cross-entropy diagnostics require all unordered embedding pairs.
  Sparse graph merging avoids scanning every possible pair against the
  full edge list, but does not remove this quadratic diagnostic work.
- Approximate nearest-neighbor indexes, sparse eigensolvers, supervised
  UMAP, inverse transformation, and backend-specific acceleration are
  not implemented.
- Training is non-convex. Different seeds can produce different valid
  embeddings; identical data, options, and seeds produce reproducible
  results within a backend. Floating-point and eigensolver behavior can
  differ across backends.
- Embedding axes, orientation, and absolute scale have no standalone
  meaning.
- Out-of-sample points are transformed independently and do not update
  the training embedding or interact with each other.
- High-cardinality categorical attributes expand the encoded row width
  and increase exact-neighbor and dense-spectral costs.
- Missing values must be variables in present ``Attribute-Value``
  bindings. Omitted, duplicate, undeclared, invalid observed, and
  all-missing attributes are rejected.

References
----------

1. McInnes, L., Healy, J., and Melville, J. (2018) - "UMAP: Uniform
   Manifold Approximation and Projection for Dimension Reduction".
