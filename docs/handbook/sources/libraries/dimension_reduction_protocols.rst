.. _library_dimension_reduction_protocols:

``dimension_reduction_protocols``
=================================

This library provides protocols and reusable support predicates used in
the implementation of machine learning dimension reduction algorithms.
Unsupervised datasets are represented as objects implementing the
``dimension_reduction_dataset_protocol`` protocol. Labeled datasets for
supervised reducers such as Linear Discriminant Analysis are represented
as objects implementing the
``supervised_dimension_reduction_dataset_protocol`` protocol.
Target-valued datasets for reducers such as partial least squares
projections are represented as objects implementing the
``target_supervised_dimension_reduction_dataset_protocol`` protocol.
Dimension reducers are represented as objects importing the
``dimension_reducer_common`` category.

This library also provides reusable test datasets and a small smoke-test
suite.

Concrete dimension reduction algorithms are intentionally out of scope
for this package. The goal is to provide a portable foundation for
libraries such as ``pca``, ``truncated_svd``, ``random_projection``,
``nmf``, ``lda_projection``, and ``pls_projection``, including shared
reducer helpers for common tasks such as feature encoding, projection,
reducer validation, diagnostics, export, and pretty-printing.

Exported reducers are serialized protocol-wide as single-argument
predicates such as ``reducer(Reducer)``, allowing the saved reducer term
to be loaded and passed directly to ``transform/3``.

All reducers importing the shared category are also expected to record a
diagnostics list containing at least ``model/1`` and ``options/1``
terms. The shared protocol surface exposes this metadata using the
``diagnostics/2``, ``diagnostic/2``, and ``dimension_reducer_options/2``
predicates and validates serialized reducers using
``check_dimension_reducer/1``.

API documentation
-----------------

Open the
`../../apis/library_index.html#dimension_reduction_protocols <../../apis/library_index.html#dimension_reduction_protocols>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(dimension_reduction_protocols(loader)).

Common options
--------------

The ``dimension_reducer_common`` category supports the
``feature_scaling/1`` option used by importing reducers to control
continuous feature normalization before projection:

- ``feature_scaling(true)`` standardizes each continuous attribute using
  its training-set mean and standard deviation.

- ``feature_scaling(false)`` only centers each continuous attribute
  using its training-set mean.

The current ``pca``, ``random_projection``, ``lda_projection``, and
``pls_projection`` libraries all define ``feature_scaling(true)`` as
their default.

Reducers can also specialize preprocessing behavior. For example, the
``nmf`` library only supports ``center(false)`` because the learned
feature representation must remain non-negative.

Testing
-------

To run the library smoke tests, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(dimension_reduction_protocols(tester)).

Test datasets
-------------

Several sample datasets are included in the ``test_datasets`` directory:

- ``correlated_plane.lgt`` — A compact continuous dataset with 8
  examples and 3 continuous attributes (``x``, ``y``, ``z``) where the
  features are strongly correlated. It is intended for PCA and
  projection smoke tests.

- ``high_dimensional_measurements.lgt`` — A small continuous dataset
  with 10 examples and 6 continuous attributes (``f1`` through ``f6``).
  It is intended for testing projected dimensionality and reducer output
  shape.

- ``low_rank_rectangular.lgt`` — A compact continuous dataset with 4
  examples and 3 continuous attributes whose third feature is the sum of
  the first two. It is intended for testing matrix-rank truncation and
  singular-value-based reducers such as ``truncated_svd``.

- ``labeled_measurements.lgt`` — A compact labeled continuous dataset
  with 9 examples, 4 continuous attributes (``length``, ``width``,
  ``height``, ``weight``), and 3 class labels (``alpha``, ``beta``,
  ``gamma``). It is intended for testing supervised reducers such as
  ``lda_projection``.

- ``parts_based_measurements.lgt`` — A compact non-negative continuous
  dataset with 5 examples and 4 continuous attributes (``f1`` through
  ``f4``) built from two latent additive parts. It is intended for
  testing parts-based reducers such as ``nmf``.

- ``target_latent_measurements.lgt`` — A compact target-valued
  continuous dataset with 6 examples, 4 continuous attributes (``f1``
  through ``f4``), and a numeric target attribute (``score``) influenced
  by two latent directions. It is intended for testing target-supervised
  reducers such as ``pls_projection``.

- Target-supervised smoke coverage is currently provided by the
  ``target_measurements`` object in the library smoke tests, which
  exercises the
  ``target_supervised_dimension_reduction_dataset_protocol`` contract.
