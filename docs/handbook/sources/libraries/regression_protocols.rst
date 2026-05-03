.. _library_regression_protocols:

``regression_protocols``
========================

This library provides protocols used in the implementation of machine
learning regression algorithms. Datasets are represented as objects
implementing the ``regression_dataset_protocol`` protocol. Regressors
are represented as objects importing the ``regressor_common`` category.
This category provides shared helpers for regressor defaults, dataset
validation, diagnostics metadata, export, and pretty-printing support.

Learned regressors expose diagnostics using the shared
``diagnostics/2``, ``diagnostic/2``, and ``regressor_options/2``
predicates. Concrete regressor implementations store effective training
options in the diagnostics metadata under an ``options(Options)`` term.

This library also provides regression test datasets under the
``test_datasets`` directory.

API documentation
-----------------

Open the
`../../apis/library_index.html#regression_protocols <../../apis/library_index.html#regression_protocols>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(regression_protocols(loader)).

Testing
-------

To test this library predicates, shared categories, and datasets, load
the ``tester.lgt`` file:

\| ?- logtalk_load(regression_protocols(tester)).

Test datasets
-------------

The ``test_datasets`` directory includes the following compact
regression datasets and validation fixtures:

- ``duplicate_attribute_declaration.lgt``: invalid dataset fixture
  containing a duplicated attribute declaration in the dataset schema.
- ``duplicate_attribute_example.lgt``: invalid dataset fixture
  containing a duplicated declared attribute binding in one example.
- ``grouped_categorical_signal.lgt``: regression dataset with one
  relevant continuous attribute and one irrelevant categorical attribute
  for testing shrinkage of encoded categorical coefficients.
- ``intercept_only.lgt``: constant-target dataset for intercept-learning
  tests.
- ``invalid_target.lgt``: invalid dataset with a non-numeric target for
  negative tests.
- ``mixed_signal.lgt``: mixed continuous and categorical regression
  dataset.
- ``plane.lgt``: two-feature regression dataset following the plane
  ``z = 3x1 - 2x2 + 5``.
- ``simple_line.lgt``: single-feature regression dataset following the
  line ``y = 2x + 1``.
- ``sparse_mixed_signal.lgt``: mixed regression dataset with omitted
  attribute-value pairs used to exercise missing-value handling during
  training and prediction.
- ``sparse_signal.lgt``: two-feature regression dataset where only the
  signal attribute contributes to the target and the noise attribute is
  orthogonal to it.
- ``step_signal.lgt``: piecewise-constant regression dataset for
  tree-based and neighborhood regressor testing.
- ``undeclared_attribute_example.lgt``: invalid dataset fixture
  containing an undeclared attribute binding in one example.
- ``wide_mixed_signal.lgt``: synthetic wide mixed regression dataset
  with many continuous and categorical predictors for non-trivial
  linear-model benchmarking.
