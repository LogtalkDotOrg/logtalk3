.. _library_lda_projection:

``lda_projection``
==================

Linear Discriminant Analysis projection for labeled continuous datasets.
Supports continuous attributes only.

The library implements the ``dimension_reducer_protocol`` defined in the
``dimension_reduction_protocols`` library and learns discriminant
directions by centering the training data, optionally standardizing
continuous attributes, building regularized within-class and
between-class scatter matrices, whitening the Fisher criterion using a
Cholesky factorization, and extracting discriminant directions using
deterministic power iteration with deflation.

Requires a dataset implementing
``supervised_dimension_reduction_dataset_protocol`` and therefore uses
class labels during training.

API documentation
-----------------

Open the
`../../apis/library_index.html#lda_projection <../../apis/library_index.html#lda_projection>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(lda_projection(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(lda_projection(tester)).

Features
--------

- **Supervised Continuous Datasets**: Accepts labeled datasets
  implementing ``supervised_dimension_reduction_dataset_protocol`` and
  containing only continuous attributes.
- **Centering and Optional Scaling**: Centers all attributes and
  optionally standardizes them before estimating class scatter matrices.
- **Portable Fisher Eigensolver**: Uses regularized within-class
  scatter, Cholesky whitening, and deterministic power iteration instead
  of backend-specific linear algebra libraries.
- **Projection API**: Transforms a new instance into a list of
  ``component_N-Value`` pairs.
- **Model Export**: Learned reducers can be exported as predicate
  clauses or written to a file.
- **Missing Values**: Missing or nonnumeric values are rejected.

Options
-------

The ``learn/3`` predicate accepts the following options:

- ``n_components/1``: Maximum number of discriminant components to
  extract. Requests that exceed the minimum of the number of features
  and ``number_of_classes - 1`` raise
  ``domain_error(component_count, Requested-Maximum)``. The default is
  ``2``.
- ``feature_scaling/1``: Whether to standardize continuous attributes
  before estimating scatter matrices. Options: ``true`` (default) or
  ``false``.
- ``maximum_iterations/1``: Maximum number of power-iteration steps used
  when estimating each discriminant direction. The default is ``1000``.
- ``tolerance/1``: Positive convergence tolerance used both for
  power-iteration stopping and for deciding when additional discriminant
  directions are numerically negligible. The default is ``1.0e-8``.
- ``regularization/1``: Positive diagonal regularization added to the
  within-class scatter matrix before whitening. The default is
  ``1.0e-6``.

Usage
-----

The following examples use the sample labeled dataset shipped with the
``dimension_reduction_protocols`` library:

::

   | ?- logtalk_load(dimension_reduction_protocols('test_datasets/labeled_measurements')).

Learning a reducer
~~~~~~~~~~~~~~~~~~

::

   | ?- lda_projection::learn(labeled_measurements, DimensionReducer).

   | ?- lda_projection::learn(labeled_measurements, DimensionReducer, [n_components(1), feature_scaling(false), maximum_iterations(250), tolerance(1.0e-7), regularization(1.0e-5)]).

Transforming new instances
~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- lda_projection::learn(labeled_measurements, DimensionReducer),
        lda_projection::transform(DimensionReducer, [length-5.1, width-3.5, height-1.4, weight-0.2], ReducedInstance).

   | ?- lda_projection::learn(labeled_measurements, DimensionReducer, [n_components(1)]),
        lda_projection::transform(DimensionReducer, [length-6.2, width-3.4, height-5.4, weight-2.3], ReducedInstance).

Exporting and reusing the reducer
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- lda_projection::learn(labeled_measurements, DimensionReducer, [n_components(1)]),
        lda_projection::export_to_file(labeled_measurements, DimensionReducer, reducer, 'lda_projection_reducer.pl').

   | ?- logtalk_load('lda_projection_reducer.pl'),
        reducer(Reducer),
        lda_projection::transform(Reducer, [length-5.1, width-3.5, height-1.4, weight-0.2], ReducedInstance).

Dimension reducer representation
--------------------------------

The learned dimension reducer is represented by a compound term with the
functor chosen by the implementation and arity 4. For example:

::

   lda_projection_reducer(Encoders, Components, ClassValues, Diagnostics)

Where:

- ``Encoders``: List of continuous attribute encoders storing attribute
  name, mean, and scale.
- ``Components``: List of learned discriminant vectors in component
  order.
- ``ClassValues``: Ordered list of class labels used during training.
- ``Diagnostics``: Learned reducer metadata including the effective
  training options and model details.

When exported using ``export_to_clauses/4`` or ``export_to_file/4``,
this reducer term is serialized directly as the single argument of the
generated predicate clause so that the exported model can be loaded and
reused as-is.

References
----------

1. Fisher, R. A. (1936) - "The use of multiple measurements in taxonomic
   problems".
2. Rao, C. R. (1948) - "The utilization of multiple measurements in
   problems of biological classification".
