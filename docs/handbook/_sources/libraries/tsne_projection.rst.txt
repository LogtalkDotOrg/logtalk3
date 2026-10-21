.. _library_tsne_projection:

``tsne_projection``
===================

Exact t-distributed Stochastic Neighbor Embedding for continuous
datasets. The library implements the ``dimension_reducer_protocol`` and
uses portable full-batch optimization. Its time and memory requirements
are quadratic in the number of training examples.

API documentation
-----------------

Open the
`../../apis/library_index.html#tsne-projection <../../apis/library_index.html#tsne-projection>`__
link in a web browser.

Loading
-------

To load this library, load its ``loader.lgt`` file:

::

   | ?- logtalk_load(tsne_projection(loader)).

Testing
-------

To test this library, load its ``tester.lgt`` file:

::

   | ?- logtalk_load(tsne_projection(tester)).

Features
--------

- Accepts datasets containing only continuous attributes.
- Computes exact Gaussian affinities with reported perplexity matching
  outcomes.
- Uses exact Student-t affinities and full-batch gradient optimization.
- Supports early exaggeration, momentum, and adaptive coordinate gains.
- Provides reproducible Gaussian initialization without changing caller
  RNG state.
- Supports missing continuous values represented by variables using mean
  imputation.
- Exports learned embeddings as ordinary predicate clauses.
- Transforms new instances by optimizing one coordinate against the
  fixed training embedding.

Options
-------

- ``n_components/1``: Positive integer embedding dimensions, no greater
  than the feature count and less than the sample count. The default is
  ``2``.
- ``feature_scaling/1``: Standardize continuous attributes when
  ``true``. The default is ``true``.
- ``perplexity/1``: Float between ``1.0`` and the sample count minus
  one, inclusive. The default is ``5.0``.
- ``learning_rate/1``: Positive float optimization learning rate. The
  default is ``200.0``.
- ``early_exaggeration/1``: Positive float affinity multiplier used
  during the initial training optimization phase. The default is
  ``12.0``.
- ``early_exaggeration_iterations/1``: Non-negative integer number of
  initial exaggerated iterations, no greater than the maximum iteration
  count. The default is ``250``.
- ``maximum_iterations/1``: Positive integer optimization iteration
  limit. The default is ``1000``.
- ``tolerance/1``: Positive float stopping tolerance for the maximum raw
  coordinate update before centering. Training tolerance stopping is
  enabled only after the early exaggeration phase. The default is
  ``1.0e-7``.
- ``random_seed/1``: Positive integer seed for reproducible
  initialization. The default is ``1357911``.

Usage
-----

::

   | ?- logtalk_load(dimension_reduction_protocols('test_datasets/correlated_plane')).

   | ?- tsne_projection::learn(correlated_plane, DimensionReducer, [perplexity(2.0)]).

   | ?- tsne_projection::transform(DimensionReducer, [x-2.0, y-4.0, z-6.0], ReducedInstance).

Missing continuous values are written as variables. Encoder means and
scales are computed from observed numeric training values only. A
missing value is then encoded as normalized zero, which is equivalent to
imputing the observed training mean:

::

   | ?- tsne_projection::transform(DimensionReducer, [x-_, y-4.0, z-6.0], ReducedInstance).

The out-of-sample transformation is an approximate independent
extension. It minimizes conditional KL divergence against the fixed
embedding. Its conditional gradient uses a factor of two, while the
symmetric training objective uses a factor of four; both use the
configured learning rate. Transforming several new instances separately
is not equivalent to jointly refitting t-SNE with those instances
included in the training dataset.

Dimension reducer representation
--------------------------------

Learned reducers use the following representation:

::

   tsne_reducer(Encoders, ExampleIds, TrainingRows, EmbeddingRows, Diagnostics)

Example identifiers must be ground and distinct. Insufficiently
instantiated identifiers or reducer terms raise an instantiation error.
Reducer validation requires complete effective training options and
diagnostics consistent with the stored dimensions, iteration count, and
preprocessing. Repeated options are accepted; the first occurrence
determines the effective value.

The reducer stores encoded training rows because t-SNE does not learn a
linear projection matrix. Diagnostics include convergence information
and the initial and final unexaggerated Kullback-Leibler divergences.
The preprocessing diagnostics include
``missing_values(mean_imputation)``.

The ``diagnostics/2`` and ``diagnostic/2`` predicates return the
affinity-search summary and achieved training perplexity range using
this term:

::

   affinity_search(
       summary(MatchedCount, TieLimitedCount, IterationLimitCount),
       achieved_perplexity_range(Minimum, Maximum)
   )

The counts sum to the number of training examples. Tied nearest
distances can make a requested perplexity unattainable; the limiting
distribution is then used and reported as tie-limited. A precision
search that reaches its bounded iteration budget is reported separately.
New instances use all training points as their eligible neighbors. The
``transform/3`` predicate emits one structured warning when its achieved
perplexity does not match the request, while still returning best-effort
coordinates.

Complexity and benchmarks
-------------------------

Let ``n`` be the number of training examples, ``d`` the number of
original attributes, ``m`` the number of embedding coordinates, ``B``
the bounded number of Gaussian precision-search steps, and ``T`` the
optimizer iteration count. The numerical training work is
``O(n^2*d + B*n^2 + T*n^2*m)`` and memory is ``O(n^2 + n*(d+m))``.
Indexed input preparation adds ``O(n*d*log(d) + n*log(n))`` work,
excluding identifier term-comparison costs. These remain exact dense
methods, not sparse or approximate algorithms.

Probability symmetrization traverses the conditional matrix and its
transpose in quadratic time. Distances and Student-t weights use
triangular pair traversals with shared row-tail cursors instead of
allocating a matrix transpose. Each pair is computed once and fills both
mirrored cells. Weight normalization sums are accumulated during
construction in row-major order, including the already-filled lower row
prefixes, without doubling triangular subtotals or performing a separate
whole-matrix summation pass.

Training computes and applies one gradient row at a time against the
unchanged old embedding, without retaining a complete gradient matrix.
Centering still follows all coordinate updates, and stopping uses the
maximum raw update before centering. Gradients and KL diagnostics
consume weights and their normalization sum without materializing a
normalized probability matrix. Gaussian search reuses shifted distances
and normalizes only its terminal weights. Attribute values are validated
and aligned once during training; AVL indexes check identifiers and
binding occurrences. Centering uses row-wise sums and subtraction, and
optimizer settings are resolved once before iteration.

The row-tail cursor lists still require quadratic allocation, and the
dense matrices retain quadratic storage. These optimizations reduce
temporary structures and repeated traversals without changing the
asymptotic bounds. The matrix, sum, and optimizer regression tests
compare results with independent unfused references, including
floating-point accumulation order.

The numerical work for one new point is ``O(n*d + B*n + T*n*m)``. Each
public transformation also validates the complete stored reducer,
including ``O(n*log(n))`` identifier checks and ``O(n*(d+m))``
stored-row checks, excluding identifier term-comparison costs. The fixed
embedding columns are prepared once for the new-point optimizer.

The ``tsne_projection_benchmarks::run/0`` predicate prints median CPU
and wall times from five measurements after one warm-up. Load the
optimized library and benchmark dependencies before running it:

::

   | ?- logtalk_load(tsne_projection(loader)),
        logtalk_load(os(loader)),
        logtalk_load(dimension_reduction_protocols('test_datasets/correlated_plane')),
        logtalk_load(tsne_projection(test_benchmarks_objects), [optimize(on)]),
        tsne_projection_benchmarks::run.

Fixed workloads cover 100-example affinity matrices, gradients and
centering, 100-by-50 preprocessing, 1,000-identifier validation,
1,000-by-20 centering, optimizer option dispatch, eight-example
training, and repeated transformations. Additional paired workloads
compare matrix construction, weight construction with summation, and
complete gradient/update steps against benchmark-only unfused reference
methods for 100 and 300 examples with two and five coordinates. Inputs,
weights, and optimizer states are prepared outside timed sections. The
synthetic rows do not repeat on a fixed 97-example cycle. A
deterministic 100-example dataset measures public training with a
100-iteration limit and reports the achieved iteration count. The wider
centering, larger identifier, and option-dispatch workloads also include
benchmark-only reference methods. Timings are informative measurements,
not unit-test thresholds; instrumented test timings are not production
benchmarks.

Limitations
-----------

- The implementation computes all pairwise affinities exactly and
  therefore requires quadratic time and memory in the number of training
  examples. It is intended for small and medium datasets.
- Barnes-Hut, FFT-based interpolation, approximate nearest-neighbor
  search, sparse affinities, and backend-specific numerical acceleration
  are not currently implemented.
- Training is a non-convex optimization. Different random seeds can
  produce different valid embeddings, and convergence does not guarantee
  a global optimum. Using the same seed and options with the same
  implementation and backend produces a reproducible embedding.
  Floating-point operation reordering can change embeddings between
  implementations.
- Coordinate values, axis order, orientation, and scale have no
  standalone meaning. Interpret local neighborhoods instead of comparing
  coordinate signs or expecting global distances and densities to be
  preserved.
- t-SNE does not learn a parametric projection function. ``transform/3``
  independently optimizes each new point against the fixed training
  embedding and is only an approximation; it does not update existing
  coordinates or model interactions among multiple new points.
- No inverse transformation from embedding coordinates to original
  attributes is provided.
- Categorical attributes are not supported.
- Missing continuous values must be represented by variables in present
  ``Attribute-Value`` bindings. Omitted, duplicate, or undeclared
  bindings are rejected, as are attributes with no observed training
  values.

References
----------

1. van der Maaten, L. and Hinton, G. (2008) - "Visualizing Data using
   t-SNE".
