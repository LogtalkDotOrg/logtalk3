.. _library_regression_tree:

``regression_tree``
===================

Regression tree regressor supporting continuous and mixed-feature
datasets. The library implements the ``regressor_protocol`` defined in
the ``regression_protocols`` library and learns a binary regression tree
using recursive variance-reduction splits.

API documentation
-----------------

Open the
`../../apis/library_index.html#regression_tree <../../apis/library_index.html#regression_tree>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(regression_tree(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(regression_tree(tester)).

To run the reference timing and fit benchmarks, load the
``tester_performance.lgt`` file:

::

   | ?- logtalk_load(regression_tree(tester_performance)).

Export header format
--------------------

The shared exporter in the ``regressor_common`` category writes a header
before the exported clauses in the following format:

::

   % exported regressor predicate: Functor/Arity
   % training dataset: Dataset
   % target: Target
   % attributes: Attributes
   % options: Options
   % Functor(Trees, Options)
   Functor(Trees, Options)

The exported clauses serialize the learned regressor state so that
loading the file gives a regressor term that can be passed directly to
the ``predict/3`` predicate.

When exporting a serialized regressor term, using a noun such as
``regressor/2`` or ``model/2`` is recommended.

Features
--------

- **Variance-Reduction Splits**: Selects binary thresholds over encoded
  features to reduce target variance.
- **Continuous and Mixed Features**: Supports continuous attributes and
  categorical attributes encoded using one-hot vectors.
- **Missing Values**: Missing numeric and categorical values are encoded
  using explicit missing-value indicator features.
- **Optional Feature Scaling**: Continuous attributes can be
  standardized using z-score scaling before tree induction.
- **Model Export**: Learned regressors can be exported as predicate
  clauses or written to a file.
- **Readable Trees**: Includes a pretty-printer for inspecting learned
  tree structure.
- **Reference Benchmarks**: Includes a dedicated performance suite
  reporting training time, RMSE, and MAE for representative regression
  datasets.

Options
-------

The ``learn/3`` predicate accepts the following options:

- **``maximum_depth/1``**: Maximum depth allowed for the induced
  regression tree. Lower values yield smaller trees; higher values allow
  more detailed partitioning of the training data. The default is
  ``10``.
- **``minimum_samples_leaf/1``**: Minimum number of training examples
  required in a leaf. This option also prevents candidate splits that
  would create child nodes smaller than the requested size. The default
  is ``1``.
- **``minimum_variance_reduction/1``**: Minimum variance-reduction gain
  required for accepting a split. Higher values make the learner more
  conservative by pruning weak splits during induction. The default is
  ``0.0``.
- **``feature_scaling/1``**: Controls z-score standardization of
  continuous attributes before tree induction. Accepted values are
  ``on`` and ``off``. The default is ``off``.
