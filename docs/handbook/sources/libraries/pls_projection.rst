.. _library_pls_projection:

``pls_projection``
==================

Partial Least Squares projection reducer for target-valued continuous
datasets. The library implements the ``dimension_reducer_protocol``
defined in the ``dimension_reduction_protocols`` library and learns a
deterministic PLS1 projection using repeated cross-covariance
maximization and sequential deflation, storing the resulting direct
projection rotations for future transforms.

API documentation
-----------------

Open the
`../../apis/library_index.html#pls_projection <../../apis/library_index.html#pls_projection>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(pls_projection(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(pls_projection(tester)).

Features
--------

- **Continuous Datasets**: Accepts datasets containing only continuous
  attributes and rejects datasets that also declare the target name as
  an input feature.
- **Target-Supervised Learning**: Uses a numeric target attribute
  declared by datasets implementing
  ``target_supervised_dimension_reduction_dataset_protocol``.
- **Centering and Optional Scaling**: Centers all attributes and
  optionally standardizes them before learning latent directions.
- **Deterministic Components**: Learns components deterministically from
  feature-target cross-covariance without stochastic initialization.
- **Configurable Shortfall Handling**: Lets callers choose whether an
  extraction shortfall raises an error or truncates the learned reducer
  with explicit diagnostics.
- **Projection API**: Transforms a new instance into a list of
  ``component_N-Value`` pairs using the learned direct PLS rotation
  vectors.
- **Model Export**: Learned reducers can be exported as predicate
  clauses or written to a file.

Options
-------

The ``learn/3`` predicate accepts the following options:

- ``n_components/1``: Number of latent PLS components to extract.
  Requests that exceed ``min(FeatureCount, SampleCount - 1)`` raise
  ``domain_error(component_count, Requested-Maximum)``. The default is
  ``2``.
- ``feature_scaling/1``: Whether to standardize continuous attributes
  before projection. Options: ``true`` (default) or ``false``.
- ``shortfall_policy/1``: Controls what happens when residual
  feature-target cross-covariance becomes negligible before all
  requested components are extracted. Options: ``truncate`` (default),
  which returns a reducer with fewer components and records a
  ``shortfall(truncated(Requested, Learned, ScoreEnergy, Tolerance))``
  diagnostic, or ``error``, which raises
  ``domain_error(component_count, Requested-Learned)``.
- ``tolerance/1``: Positive numerical tolerance used to stop extracting
  components when the residual feature-target cross-covariance becomes
  negligible. The default is ``1.0e-8``.

Usage
-----

The following example uses the sample target-valued dataset shipped with
the ``dimension_reduction_protocols`` library:

::

   | ?- logtalk_load(dimension_reduction_protocols('test_datasets/target_latent_measurements')).

Learning a reducer
~~~~~~~~~~~~~~~~~~

::

   | ?- pls_projection::learn(target_latent_measurements, DimensionReducer).

   | ?- pls_projection::learn(target_latent_measurements, DimensionReducer, [n_components(1), feature_scaling(false)]).

Transforming new instances
~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- pls_projection::learn(target_latent_measurements, DimensionReducer, [n_components(2)]),
        pls_projection::transform(DimensionReducer, [f1-4.0, f2-8.0, f3-2.0, f4-(-2.0)], ReducedInstance).

Exporting and reusing the reducer
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- pls_projection::learn(target_latent_measurements, DimensionReducer, [n_components(2)]),
        pls_projection::export_to_file(target_latent_measurements, DimensionReducer, reducer, 'pls_projection_reducer.pl').

   | ?- logtalk_load('pls_projection_reducer.pl'),
        reducer(Reducer),
        pls_projection::transform(Reducer, [f1-4.0, f2-8.0, f3-2.0, f4-(-2.0)], ReducedInstance).

Dimension reducer representation
--------------------------------

The learned dimension reducer is represented by a compound term with the
functor chosen by the implementation and arity 3. For example:

::

   pls_projection_reducer(Encoders, Rotations, Diagnostics)

Where:

- ``Encoders``: List of continuous attribute encoders storing attribute
  name, mean, and scale.
- ``Rotations``: List of direct PLS projection vectors derived from the
  learned latent weights and loadings.
- ``Diagnostics``: Learned metadata including the effective training
  options, target information, target loadings, and optional
  truncate-mode shortfall details.

When exported using ``export_to_clauses/4`` or ``export_to_file/4``,
this reducer term is serialized directly as the single argument of the
generated predicate clause so that the exported model can be loaded and
reused as-is.

References
----------

1. Wold, H. (1975) - "Soft modelling by latent variables; the nonlinear
   iterative partial least squares (NIPALS) approach".
2. Geladi, P. and Kowalski, B. R. (1986) - "Partial least-squares
   regression: a tutorial".
