.. _library_ica_projection:

``ica_projection``
==================

Independent Component Analysis reducer for continuous datasets (missing
or non-numeric values are rejected). The library implements the
``dimension_reducer_protocol`` defined in the
``dimension_reduction_protocols`` library and learns a linear unmixing
projection by centering the training data, optionally standardizing
continuous attributes, whitening the covariance matrix using the shared
deterministic symmetric eigen-decomposition from ``linear_algebra``, and
then extracting independent components using a deterministic cubic
FastICA fixed-point iteration with orthogonal deflation.

API documentation
-----------------

Open the
`../../apis/library_index.html#ica_projection <../../apis/library_index.html#ica_projection>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(ica_projection(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(ica_projection(tester)).

Features
--------

- **Continuous Datasets**: Accepts datasets containing only continuous
  attributes.
- **Shared Symmetric Whitening Plus Fixed-Point ICA**: Whitens the
  training covariance matrix using the shared symmetric
  eigendecomposition from ``linear_algebra`` and then extracts
  independent directions using deterministic cubic FastICA with
  orthogonal deflation.
- **Always Centered**: Training and transform inputs are always centered
  using the training-set means before whitening and projection.
- **Optional Scaling**: Can optionally standardize each continuous
  attribute before whitening.
- **Training Diagnostics**: Records whitening eigenvalues plus
  per-component convergence reasons, iteration counts, and final deltas.
- **Projection API**: Transforms a new instance into a list of
  ``component_N-Value`` pairs.
- **Model Export**: Learned reducers can be exported as predicate
  clauses or written to a file.

Options
-------

The ``learn/3`` predicate accepts the following options:

- ``n_components/1``: Number of independent components to extract.
  Because training data is always centered, requests that exceed
  ``min(feature_count, sample_count - 1)`` raise
  ``domain_error(component_count, Requested-Maximum)``. Requests that
  still exceed the numerical rank of the whitened covariance matrix
  raise ``domain_error(component_count, Requested-Extracted)``. The
  default is ``2``.
- ``feature_scaling/1``: Whether to divide each continuous attribute by
  its training-set standard deviation before whitening. Options:
  ``true`` or ``false`` (default).
- ``maximum_iterations/1``: Maximum iteration bound used both by the
  whitening eigensolver and by the FastICA fixed-point steps for each
  independent component. The default is ``1000``.
- ``tolerance/1``: Positive convergence tolerance used both for
  whitening rank detection and for FastICA fixed-point stopping. The
  default is ``1.0e-8``.

The learned diagnostics also include:

- ``whitening_eigenvalues(Values)``: Eigenvalues used to build the
  whitening transform, aligned with the extracted components.
- ``convergence(Statuses)``: Per-component stop reasons, such as
  ``tolerance`` or ``maximum_iterations_exhausted``.
- ``iterations(Counts)``: Per-component iteration counts aligned with
  the extracted components.
- ``final_delta(Deltas)``: Per-component final update magnitudes aligned
  with the extracted components.

Usage
-----

The following examples use the sample dataset shipped with the
``dimension_reduction_protocols`` library:

::

   | ?- logtalk_load(dimension_reduction_protocols('test_datasets/mixed_independent_sources')).

Learning a reducer
~~~~~~~~~~~~~~~~~~

::

   | ?- ica_projection::learn(mixed_independent_sources, DimensionReducer).

   | ?- ica_projection::learn(mixed_independent_sources, DimensionReducer, [n_components(2), feature_scaling(true), maximum_iterations(200), tolerance(1.0e-7)]).

Transforming new instances
~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- ica_projection::learn(mixed_independent_sources, DimensionReducer),
        ica_projection::transform(DimensionReducer, [x1-(-5.0), x2-(-4.0), x3-(-4.0)], ReducedInstance).

Exporting and reusing the reducer
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- ica_projection::learn(mixed_independent_sources, DimensionReducer, [n_components(2)]),
        ica_projection::export_to_file(mixed_independent_sources, DimensionReducer, reducer, 'ica_reducer.pl').

   | ?- logtalk_load('ica_reducer.pl'),
        reducer(Reducer),
        ica_projection::transform(Reducer, [x1-(-5.0), x2-(-4.0), x3-(-4.0)], ReducedInstance).

Dimension reducer representation
--------------------------------

The learned dimension reducer is represented by a compound term with the
functor chosen by the implementation and arity 3. For example:

::

   ica_reducer(Encoders, Components, Diagnostics)

Where:

- ``Encoders``: List of continuous attribute encoders storing attribute
  name, centering offset, and scale factor.
- ``Components``: List of learned unmixing vectors in feature space.
- ``Diagnostics``: Learned reducer metadata including the effective
  training options, whitening eigenvalues, and per-component convergence
  information.

References
----------

1. Hyvarinen, A. and Oja, E. (2000) - "Independent Component Analysis:
   Algorithms and Applications".
