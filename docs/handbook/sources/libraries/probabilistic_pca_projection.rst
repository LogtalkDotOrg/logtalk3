.. _library_probabilistic_pca_projection:

``probabilistic_pca_projection``
================================

Probabilistic Principal Component Analysis reducer for continuous
datasets. The library implements the ``dimension_reducer_protocol``
defined in the ``dimension_reduction_protocols`` library and learns a
linear latent-variable projection by centering the training data,
optionally standardizing continuous attributes, estimating the sample
covariance matrix, extracting deterministic leading eigenvectors using
portable power iteration with deflation, and converting them into the
closed-form maximum-likelihood PPCA loading matrix and posterior latent
projection.

API documentation
-----------------

Open the
`../../apis/library_index.html#probabilistic_pca_projection <../../apis/library_index.html#probabilistic_pca_projection>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(probabilistic_pca_projection(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(probabilistic_pca_projection(tester)).

Features
--------

- **Continuous Datasets**: Accepts datasets containing only continuous
  attributes. Missing or nonnumeric values are rejected.
- **Centering and Optional Scaling**: Centers all attributes and
  optionally standardizes them before fitting the covariance model.
- **Probabilistic Latent Model**: Estimates the PPCA loading matrix and
  isotropic observation noise variance from the learned covariance
  eigensystem.
- **Configurable Shortfall Handling**: Lets callers choose whether a
  numerical-rank shortfall raises an error or returns a truncated
  reducer with explicit diagnostics.
- **Projection API**: Transforms a new instance into posterior latent
  means returned as ``component_N-Value`` pairs.
- **Model Export**: Learned reducers can be exported as predicate
  clauses or written to a file.

Options
-------

The ``learn/3`` predicate accepts the following options:

- ``n_components/1``: Number of latent dimensions to extract. Requests
  that exceed the structural PPCA limit
  ``min(FeatureCount, SampleCount - 1)`` raise
  ``domain_error(component_count, Requested-Maximum)``. The default is
  ``2``.
- ``feature_scaling/1``: Whether to standardize continuous attributes
  before fitting the covariance model. Options: ``true`` (default) or
  ``false``.
- ``shortfall_policy/1``: Controls what happens when the covariance
  matrix yields fewer numerically significant components than requested
  after passing the structural PPCA bound above. Options: ``truncate``
  (default), which returns a reducer with fewer components and records a
  ``shortfall(truncated(Requested, Learned, ResidualEigenvalue, Tolerance))``
  diagnostic, or ``error``, which raises
  ``domain_error(component_count, Requested-Learned)``.
- ``maximum_iterations/1``: Maximum number of power-iteration steps used
  when estimating each covariance eigenvector. The default is ``1000``.
- ``tolerance/1``: Positive convergence tolerance used both for
  power-iteration stopping and for deciding when residual eigenvalues
  are negligible. The default is ``1.0e-8``.

Usage
-----

The following examples use the sample datasets shipped with the
``dimension_reduction_protocols`` library:

::

   | ?- logtalk_load(dimension_reduction_protocols('test_datasets/correlated_plane')).

Learning a reducer
~~~~~~~~~~~~~~~~~~

::

   | ?- probabilistic_pca_projection::learn(correlated_plane, DimensionReducer).

   | ?- probabilistic_pca_projection::learn(correlated_plane, DimensionReducer, [n_components(1), feature_scaling(false), shortfall_policy(error)]).

Transforming new instances
~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- probabilistic_pca_projection::learn(correlated_plane, DimensionReducer, [n_components(2)]),
        probabilistic_pca_projection::transform(DimensionReducer, [x-2.0, y-4.0, z-6.0], ReducedInstance).

Exporting and reusing the reducer
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- probabilistic_pca_projection::learn(correlated_plane, DimensionReducer, [n_components(1)]),
        probabilistic_pca_projection::export_to_file(correlated_plane, DimensionReducer, reducer, 'probabilistic_pca_reducer.pl').

   | ?- logtalk_load('probabilistic_pca_reducer.pl'),
        reducer(Reducer),
        probabilistic_pca_projection::transform(Reducer, [x-1.0, y-2.0, z-3.0], ReducedInstance).

Dimension reducer representation
--------------------------------

The learned dimension reducer is represented by a compound term with the
functor chosen by the implementation and arity 6. For example:

::

   probabilistic_pca_reducer(Encoders, Components, Loadings, NoiseVariance, ExplainedVariances, Diagnostics)

Where:

- ``Encoders``: List of continuous attribute encoders storing attribute
  name, mean, and scale.
- ``Components``: List of posterior latent projection vectors in
  descending explained-variance order.
- ``Loadings``: List of maximum-likelihood PPCA loading vectors aligned
  with the extracted latent dimensions.
- ``NoiseVariance``: Estimated isotropic observation noise variance.
- ``ExplainedVariances``: List of retained covariance eigenvalues
  matching the extracted latent dimensions.
- ``Diagnostics``: Learned metadata including the effective training
  options, sample count, retained explained variances, estimated noise
  variance, preprocessing details, and optional truncate-mode shortfall
  details.

When exported using ``export_to_clauses/4`` or ``export_to_file/4``,
this reducer term is serialized directly as the single argument of the
generated predicate clause so that the exported model can be loaded and
reused as-is.

References
----------

1. Tipping, M. E. and Bishop, C. M. (1999) - "Probabilistic Principal
   Component Analysis".
2. Bishop, C. M. (2006) - "Pattern Recognition and Machine Learning".
   Section 12.2.
