.. _library_pca:

``pca``
=======

Principal Component Analysis reducer for continuous datasets. The
library implements the ``dimension_reducer_protocol`` defined in the
``dimension_reduction_protocols`` library and learns a linear projection
using a portable power-iteration eigensolver over the covariance matrix.

API documentation
-----------------

Open the
`../../apis/library_index.html#pca <../../apis/library_index.html#pca>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(pca(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(pca(tester)).

Features
--------

- **Continuous Datasets**: Accepts datasets containing only continuous
  attributes.
- **Centering and Optional Scaling**: Centers all attributes and
  optionally standardizes them before extracting principal directions.
- **Portable Eigensolver**: Uses deterministic power iteration with
  deflation instead of backend-specific linear algebra libraries.
- **Projection API**: Transforms a new instance into a list of
  ``component_N-Value`` pairs.
- **Model Export**: Learned reducers can be exported as predicate
  clauses or written to a file.

Options
-------

The ``learn/3`` predicate accepts the following options:

- ``n_components/1``: Number of principal components to extract. The
  default is ``2``.
- ``feature_scaling/1``: Whether to standardize continuous attributes
  before extracting components. Options: ``on`` (default) or ``off``.
- ``maximum_iterations/1``: Maximum number of power-iteration steps used
  when estimating each principal direction. The default is ``1000``.
- ``tolerance/1``: Positive convergence tolerance used both for
  power-iteration stopping and for deciding when deflated eigenvalues
  are negligible. The default is ``1.0e-8``.

Usage
-----

The following examples use the sample datasets shipped with the
``dimension_reduction_protocols`` library:

::

   | ?- logtalk_load(dimension_reduction_protocols('test_datasets/correlated_plane')),
        logtalk_load(dimension_reduction_protocols('test_datasets/high_dimensional_measurements')).

Learning a reducer
~~~~~~~~~~~~~~~~~~

::

   | ?- pca::learn(correlated_plane, DimensionReducer).

   | ?- pca::learn(correlated_plane, DimensionReducer, [n_components(1), feature_scaling(off), maximum_iterations(200), tolerance(1.0e-7)]).

Transforming new instances
~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- pca::learn(high_dimensional_measurements, DimensionReducer),
        pca::transform(DimensionReducer, [f1-0.9, f2-1.1, f3-1.0, f4-2.0, f5-2.2, f6-2.1], ReducedInstance).

   | ?- pca::learn(correlated_plane, DimensionReducer, [n_components(1)]),
        pca::transform(DimensionReducer, [x-1.0, y-2.0, z-3.0], ReducedInstance).

Exporting and reusing the reducer
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- pca::learn(correlated_plane, DimensionReducer, [n_components(1)]),
        pca::export_to_file(correlated_plane, DimensionReducer, reducer, 'pca_reducer.pl').

   | ?- logtalk_load('pca_reducer.pl'),
        reducer(Reducer),
        pca::transform(Reducer, [x-1.0, y-2.0, z-3.0], ReducedInstance).

Dimension reducer representation
--------------------------------

The learned dimension reducer is represented by a compound term with the
functor chosen by the implementation and arity 4. For example:

::

   pca_reducer(Encoders, Components, ExplainedVariances, Options)

Where:

- ``Encoders``: List of continuous attribute encoders storing attribute
  name, mean, and scale.
- ``Components``: List of principal direction vectors in descending
  variance order.
- ``ExplainedVariances``: List of eigenvalues matching the extracted
  components.
- ``Options``: Effective training options used to learn the reducer.

When exported using ``export_to_clauses/4`` or ``export_to_file/4``,
this reducer term is serialized directly as the single argument of the
generated predicate clause so that the exported model can be loaded and
reused as-is.

References
----------

1. Pearson, K. (1901) - "On lines and planes of closest fit to systems
   of points in space".
2. Hotelling, H. (1933) - "Analysis of a complex of statistical
   variables into principal components".
