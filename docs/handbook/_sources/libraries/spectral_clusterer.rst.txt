.. _library_spectral_clusterer:

``spectral_clusterer``
======================

Normalized spectral clusterer for continuous datasets. It builds a dense
Gaussian RBF affinity matrix, computes a normalized spectral embedding,
and uses deterministic k-means clustering in the embedding space. New
instances are assigned using a degree-normalized Nyström extension.

The library implements the ``clusterer_protocol`` defined in the
``clustering_protocols`` library. Datasets are represented as objects
implementing the ``clustering_dataset_protocol`` protocol.

API documentation
-----------------

Open the
`../../apis/library_index.html#spectral-clusterer <../../apis/library_index.html#spectral-clusterer>`__
link in a web browser.

Loading
-------

To load this library, load its ``loader.lgt`` file:

::

   | ?- logtalk_load(spectral_clusterer(loader)).

Testing
-------

To test this library, load its ``tester.lgt`` file:

::

   | ?- logtalk_load(spectral_clusterer(tester)).

Common learned models are cached only within a test run, after coverage
is reset. Dedicated learning, numerical, option, and convergence tests
still train their own models. The suite checks the complete ring
fixture, numerical residuals, malformed models, exact median selection,
empty clusters, and prepared prediction against an independent Nyström
formula. Individually selected tests also build their required fixtures;
cleanup removes the cache.

Features
--------

- **Continuous Datasets**: Accepts datasets containing only continuous
  attributes.
- **Normalized Spectral Embedding**: Uses the leading eigenvectors of
  the symmetric normalized affinity matrix.
- **Gaussian RBF Affinity**: Supports an explicit positive sigma or
  automatic estimation from the median positive pairwise distance.
- **Nyström Prediction**: Projects unseen instances into the learned
  embedding before assigning the nearest embedding centroid.
- **Deterministic Optimization**: Uses deterministic eigendecomposition
  and ``first_k`` or ``spread`` centroid initialization.
- **Optional Feature Scaling**: Continuous attributes can be
  standardized using z-score scaling.
- **Portable Export**: Learned clusterers can be exported as clauses or
  files and reused later.

Options
-------

The following options can be passed to the ``learn/3`` predicate:

- ``k(K)``: Number of clusters to learn. Default is ``2``.
- ``sigma(Sigma)``: Gaussian RBF scale. It can be ``auto`` (default) or
  a positive number. Automatic estimation uses the median positive
  pairwise distance.
- ``maximum_iterations(Iterations)``: Maximum number of centroid
  optimization iterations. Default is ``1000``.
- ``tolerance(Tolerance)``: Centroid-shift convergence tolerance.
  Default is ``1.0e-8``.
- ``eigensolver_maximum_iterations(Iterations)``: Maximum power
  iterations per initial candidate and component. Default is ``1000``.
- ``eigensolver_tolerance(Tolerance)``: Non-negative residual and
  orthogonality tolerance. Default is ``1.0e-8``.
- ``eigenvalue_cutoff(Cutoff)``: Non-negative cutoff for usable positive
  eigenvalues. Default is ``1.0e-8``.
- ``initialization(Initialization)``: Embedding centroid initialization
  strategy. Options: ``spread`` (default) or ``first_k``.
- ``feature_scaling(FeatureScaling)``: Whether to standardize continuous
  attributes before clustering. Options: ``on`` (default) or ``off``.

Diagnostics
-----------

The ``diagnostics/2`` predicate returns a list containing:

- ``model(spectral_clusterer)``
- ``cluster_count(Count)``
- ``training_example_count(Count)``
- ``resolved_sigma(Sigma)``
- ``eigenvalues(Eigenvalues)``
- ``convergence(Reason)``
- ``iterations(Count)``
- ``final_shift(Shift)``
- ``options(Options)``
- ``eigensolver_convergence(tolerance)``
- ``eigensolver_iterations(Counts)``
- ``eigenpair_residuals(Residuals)``
- ``maximum_eigenpair_residual(Residual)``
- ``orthogonality_error(Error)``

The convergence, iterations, and final shift entries describe centroid
optimization only. Spectral residuals are measured against the original
normalized adjacency and bounded by eigensolver tolerance times the
greater of one and its infinity norm; orthogonality error is bounded by
that tolerance. Insufficient accuracy raises an error. Too few
eigenvalues above the cutoff raise a component count domain error. The
analytical leading component has zero iterative work. Other counts refer
to the selected initial candidates; all candidates contribute to
execution cost.

Algorithm
---------

For encoded training rows ``x_i``, the affinity and degree values are:

::

   W_ij = exp(-||x_i-x_j||^2 / (2*sigma^2))
   d_i  = sum_j W_ij

The symmetric normalized affinity matrix is:

::

   A_ij = W_ij / sqrt(d_i*d_j)

The leading ``K`` eigenvectors of ``A`` are the bottom ``K``
eigenvectors of the corresponding symmetric normalized Laplacian. Their
transposed rows are normalized to unit length and clustered using
deterministic k-means.

The leading eigenvalue is one, with eigenvector proportional to
square-root degrees. This eigenpair is constructed directly; a bounded
partial solver extracts only the remaining ``K-1`` components after
deflation. Components are reorthogonalized and checked against the
original matrix before use. Repeated eigenvalues admit different valid
bases, so cluster labels have no intrinsic order.

Automatic sigma excludes zero pairwise distances and uses exact
median-of-medians selection. Squared-distance keys avoid taking square
roots of every pair; overflow or underflow switches the entire selection
to scaled ordinary-distance keys. For an even number of distances, sigma
averages the two middle distances, not the two squared keys. Difference
lists avoid copying pairwise segments.

For a new instance ``x``, the Nyström embedding coordinate for component
``j`` is computed as:

::

   z_j(x) = (1/mu_j) * sum_i A(x,x_i) * v_j(i)

where ``mu_j`` and ``v_j`` are a selected eigenvalue and eigenvector.
The resulting row is normalized before nearest-centroid assignment.

Clusterer representation
------------------------

The learned clusterer is represented as a compound term with the functor
chosen by the user when exporting the clusterer and arity 10:

::

   spectral_clusterer(Encoders, TrainingRows, Degrees, Components, Eigenvalues, ProjectionCoefficients, Centroids, ResolvedSigma, Options, Diagnostics)

The training rows, degrees, components, eigenvalues, and resolved sigma
are retained to support Nyström prediction.

Projection coefficients are prepared once during learning:

::

   B_ji = v_j(i) / (mu_j * sqrt(d_i))

Prediction rescales raw Gaussian affinities by their maximum before
projection to avoid underflow in coefficient products. Both this common
scale and the query-degree factor cancel when normalizing the embedding
row; zero query degree is still rejected. The current representation
requires the prepared coefficients and complete spectral diagnostics.
Arity-nine terms are invalid.

The ``check_clusterer/1`` predicate requires a ground model with
positive feature, training-example, and cluster counts, consistent
dimensions and diagnostics, and an effective ``k(K)`` matching the
stored cluster count. An explicit sigma must match the resolved sigma.
Repeated options use their first occurrence. Insufficiently instantiated
models raise an instantiation error; invalid ground models raise a
clusterer domain error. The ``valid_clusterer/1`` predicate fails for
either kind of invalid model. Exported model functors remain
unrestricted. Training requires at least one declared continuous
attribute and one example; empty declarations raise a
``non_empty_attributes`` domain error. The cluster count ranges from one
to the number of training examples, including singleton datasets when
sigma is explicit. Automatic sigma requires a positive pairwise distance
and therefore rejects a singleton or entirely identical dataset.

Gaussian affinities use sigma-scaled differences instead of forming
sigma squared or its inverse. A logarithmic fallback avoids overflowing
intermediate distances for extreme finite bandwidths. Affinities may
legitimately round to zero or one; a large bandwidth can leave too few
positive components. A query with zero affinity to every training
example raises a positive spectral value domain error rather than
receiving an arbitrary cluster. Feature values must remain representable
after encoding. Embedding normalization uses scaled norms.

Complexity
----------

The implementation uses dense matrices. Affinity construction and
storage require quadratic time and space in the number of training
examples. The portable partial symmetric eigendecomposition is intended
for small and medium datasets rather than large sparse graphs.

Each unordered pair contributes one Gaussian evaluation; diagonal
entries are one and symmetric entries share the same value. Reciprocal
square-root degrees are computed once per training example rather than
once per matrix entry.

Centroid assignment accumulates per-cluster sums and counts in an AVL
dictionary, preserving training-row order. Empty clusters retain their
previous centroids and distance ties select the first centroid.
Assignment costs ``O(N*K^2)`` for ``K``-dimensional embedding rows;
aggregation avoids rescanning all rows for each cluster and does not
retain an intermediate assignment list.

For ``N`` examples, ``D`` encoded features, ``K`` components, and ``M``
positive unordered distances, pairwise distance work costs ``O(N^2*D)``.
Exact median selection costs ``O(M)`` time and storage, with
``M <= N*(N-1)/2``. Peak dense matrix storage remains ``O(N^2)``; the
retained model costs ``O(N*D + N*K + K^2)`` space. Prediction costs
``O(N*D + N*K + K^2)`` and reuses prepared coefficients. The bounded
eigensolver extracts only ``K-1`` iterative components but still tries
multiple initial vectors; its cost depends on the iteration budget,
matrix size, and spectral gaps.

Limitations
-----------

- Only continuous attributes are supported. Encoded feature values and
  the distances required for automatic sigma estimation must remain
  representable.
- Dense affinity matrices require quadratic storage, limiting practical
  use to small and medium datasets. Sparse graphs and approximate
  affinity construction are not supported.
- The cluster count is configured rather than inferred. Results depend
  on sigma, feature scaling, and centroid initialization. Deterministic
  k-means can converge to a local optimum rather than the globally best
  partition.
- The bounded eigensolver can fail to reach the requested accuracy,
  particularly when spectral gaps are small. Learning rejects
  insufficient spectral accuracy or too few eigenvalues above the
  configured cutoff.
- Automatic sigma requires a positive pairwise distance, so singleton
  and entirely identical datasets require an explicit sigma. Extreme
  bandwidths can produce numerically rank-deficient affinities or zero
  query affinity.
- Nyström prediction is an approximate out-of-sample extension of the
  fixed training embedding. It does not add the instance to the training
  graph or recompute its degrees, eigenvectors, or partition, and need
  not match clustering the augmented dataset from scratch. Instances
  with zero affinity to every training example are rejected.
- Cluster labels have no intrinsic order. Repeated eigenvalues allow
  different valid spectral bases, and small data changes can alter the
  resulting partition.

References
----------

1. Ng, Jordan, and Weiss (2002) - "On Spectral Clustering: Analysis and
   an Algorithm". Advances in Neural Information Processing Systems 14.
2. Bengio, Paiement, Vincent, Delalleau, Le Roux, and Ouimet (2004) -
   "Out-of-Sample Extensions for LLE, Isomap, MDS, Eigenmaps, and
   Spectral Clustering". Advances in Neural Information Processing
   Systems 16.
