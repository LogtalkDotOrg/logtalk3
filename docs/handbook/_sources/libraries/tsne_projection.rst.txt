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
`../../apis/library_index.html#tsne_projection <../../apis/library_index.html#tsne_projection>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(tsne_projection(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(tsne_projection(tester)).

Features
--------

- Accepts datasets containing only continuous attributes.
- Computes exact perplexity-matched Gaussian affinities.
- Uses exact Student-t affinities and full-batch gradient optimization.
- Supports early exaggeration, momentum, and adaptive coordinate gains.
- Provides reproducible Gaussian initialization without changing caller
  RNG state.
- Exports learned embeddings as ordinary predicate clauses.
- Transforms new instances by optimizing one coordinate against the
  fixed training embedding.

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
  a global optimum. Using the same seed and options produces a
  reproducible embedding.
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
- Only complete datasets with continuous, numeric attributes are
  supported. Missing values and categorical attributes are rejected.

Options
-------

- ``n_components/1``: Embedding dimensions. The default is ``2``.
- ``feature_scaling/1``: Standardize continuous attributes when
  ``true``. The default is ``true``.
- ``perplexity/1``: Positive float strictly smaller than the sample
  count. The default is ``5.0``.
- ``learning_rate/1``: Positive optimization learning rate. The default
  is ``200.0``.
- ``early_exaggeration/1``: Positive affinity multiplier used during the
  initial optimization phase. The default is ``12.0``.
- ``early_exaggeration_iterations/1``: Non-negative number of initial
  exaggerated iterations. The default is ``250``.
- ``maximum_iterations/1``: Positive optimization iteration limit. The
  default is ``1000``.
- ``tolerance/1``: Positive stopping tolerance for the maximum
  coordinate update. The default is ``1.0e-7``.
- ``random_seed/1``: Positive seed for reproducible initialization. The
  default is ``1357911``.

Usage
-----

::

   | ?- logtalk_load(dimension_reduction_protocols('test_datasets/correlated_plane')).

   | ?- tsne_projection::learn(correlated_plane, DimensionReducer, [perplexity(2.0)]).

   | ?- tsne_projection::transform(DimensionReducer, [x-2.0, y-4.0, z-6.0], ReducedInstance).

The out-of-sample transformation is an approximate independent
extension. Transforming several new instances separately is not
equivalent to jointly refitting t-SNE with those instances included in
the training dataset.

Dimension reducer representation
--------------------------------

Learned reducers use the following representation:

::

   tsne_reducer(Encoders, ExampleIds, TrainingRows, EmbeddingRows, Diagnostics)

The reducer stores encoded training rows because t-SNE does not learn a
linear projection matrix. Diagnostics include convergence information
and the initial and final unexaggerated Kullback-Leibler divergences.

References
----------

1. van der Maaten, L. and Hinton, G. (2008) - "Visualizing Data using
   t-SNE".
