.. _library_elastic_net_regression:

``elastic_net_regression``
==========================

Elastic net regression regressor supporting continuous and mixed-feature
datasets. The library implements the ``regressor_protocol`` defined in
the ``regression_protocols`` library and learns a linear model using
cyclic coordinate descent with standard coefficient-wise elastic net
regularization.

API documentation
-----------------

Open the
`../../apis/library_index.html#elastic_net_regression <../../apis/library_index.html#elastic_net_regression>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(elastic_net_regression(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(elastic_net_regression(tester)).

The unit test suite covers the default mixed-penalty behavior together
with the boundary cases ``l1_ratio(0.0)`` and ``l1_ratio(1.0)``.

To run the reference timing and fit benchmarks, load the
``tester_performance.lgt`` file:

::

   | ?- logtalk_load(elastic_net_regression(tester_performance)).

Features
--------

- **Continuous and Mixed Features**: Supports continuous attributes and
  categorical attributes encoded using one-hot vectors.
- **Feature Scaling**: Continuous attributes can be standardized using
  z-score scaling.
- **Missing Values**: Missing numeric and categorical values are encoded
  using explicit missing-value indicator features.
- **Mixed Penalty**: Combines coefficient-wise L1 shrinkage with an L2
  penalty controlled by the ``regularization/1`` and ``l1_ratio/1``
  options, including the ridge-like ``l1_ratio(0.0)`` and lasso-like
  ``l1_ratio(1.0)`` endpoints.
- **Standard Encoding Semantics**: Categorical attributes are one-hot
  encoded and each encoded coefficient is regularized independently,
  matching standard elastic-net implementations.
- **Diagnostics Metadata**: Learned regressors record model name,
  target, training example count, optimization stop reason, completed
  iterations, final parameter delta, encoded feature count, and
  effective options, accessible using the shared regression diagnostics
  predicates.
- **Model Export**: Learned regressors can be exported as predicate
  clauses or written to a file.
- **Reference Benchmarks**: Includes a dedicated performance suite
  reporting training time, RMSE, and MAE for both zero-penalty baselines
  and explicit elastic-net runs on sparse, mixed, categorical, and wide
  mixed datasets.

Regressor representation
------------------------

The learned regressor is represented by default as:

- ``elastic_net_regressor(Encoders, Bias, Weights, Diagnostics)``

The exported predicate clauses therefore use the shape:

- ``Functor(Encoders, Bias, Weights, Diagnostics)``

Diagnostics syntax
------------------

The ``diagnostics/2`` predicate returns a list of metadata terms with
the form:

::

   [
       model(elastic_net_regression),
       target(Target),
       training_example_count(TrainingExampleCount),
       options(Options),
       convergence(Status),
       iterations(Iterations),
       final_delta(FinalDelta),
       encoded_feature_count(FeatureCount)
   ]

Where:

- ``model(elastic_net_regression)`` identifies the learning algorithm
  that produced the regressor.
- ``target(Target)`` stores the target attribute name declared by the
  training dataset.
- ``training_example_count(TrainingExampleCount)`` stores the number of
  examples used during training.
- ``options(Options)`` stores the effective learning options after
  merging the user options with the library defaults.
- ``convergence(Status)`` records the optimization stop condition. The
  current values are ``tolerance`` when the maximum Karush-Kuhn-Tucker
  optimality violation across the intercept and all encoded features is
  within the configured tolerance and ``maximum_iterations_exhausted``
  when training stops because the iteration cap is reached.
- ``iterations(Iterations)`` stores the number of coordinate-descent
  sweeps completed during training.
- ``final_delta(FinalDelta)`` stores the maximum Karush-Kuhn-Tucker
  optimality violation measured during the final optimization check.
- ``encoded_feature_count(FeatureCount)`` stores the number of numeric
  features induced by the encoder list, including missing-value
  indicator features.

Use the ``regression_protocols`` ``diagnostic/2`` and
``regressor_options/2`` helper predicates when you only need a single
metadata term or the effective options.

Options
-------

The ``learn/3`` predicate accepts the following options:

- ``maximum_iterations/1``: Maximum number of coordinate-descent sweeps
  to run before stopping even if the tolerance criterion has not been
  met. The default is ``2000``.
- ``tolerance/1``: Convergence threshold for the maximum
  Karush-Kuhn-Tucker optimality violation in a full coordinate-descent
  sweep. Training stops early when both the intercept condition and all
  encoded-feature subgradient conditions are satisfied within this
  value. The default is ``1.0e-7``.
- ``regularization/1``: Overall penalty coefficient applied during
  optimization. Higher values increase shrinkage and can reduce
  overfitting. The default is ``0.01``.
- ``l1_ratio/1``: Fraction of the overall penalty assigned to the L1
  part of the elastic net penalty. The remaining fraction is assigned to
  the L2 part. Accepted values are floats in the interval
  ``[0.0, 1.0]``, where ``0.0`` gives the ridge endpoint and ``1.0``
  gives the lasso endpoint. The default is ``0.5``.
- ``feature_scaling/1``: Controls z-score standardization of continuous
  attributes before training and prediction. Accepted values are
  ``true`` and ``false``. The default is ``true``.
