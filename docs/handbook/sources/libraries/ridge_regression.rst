.. _library_ridge_regression:

``ridge_regression``
====================

Ridge regression regressor supporting continuous and mixed-feature
datasets. The library implements the ``regressor_protocol`` defined in
the ``regression_protocols`` library and learns a linear model using
batch gradient descent with L2 regularization via the shared
linear-model training core in ``regressor_common``.

API documentation
-----------------

Open the
`../../apis/library_index.html#ridge_regression <../../apis/library_index.html#ridge_regression>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(ridge_regression(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(ridge_regression(tester)).

To run the reference timing and fit benchmarks, load the
``tester_performance.lgt`` file:

::

   | ?- logtalk_load(ridge_regression(tester_performance)).

Features
--------

- **Continuous and Mixed Features**: Supports continuous attributes and
  categorical attributes encoded using one-hot vectors.
- **Feature Scaling**: Continuous attributes can be standardized using
  z-score scaling.
- **Missing Values**: Missing numeric and categorical values are encoded
  using explicit missing-value indicator features.
- **Ridge Penalty**: Applies L2 regularization to the learned weights
  using a ridge-specific ``regularization/1`` option.
- **Diagnostics Metadata**: Learned regressors record model name,
  target, training example count, optimization stop reason, completed
  iterations, final parameter delta, encoded feature count, and
  effective options, accessible using the shared regression diagnostics
  predicates.
- **Model Export**: Learned regressors can be exported as predicate
  clauses or written to a file.
- **Reference Benchmarks**: Includes a dedicated performance suite
  reporting training time, RMSE, and MAE for representative regression
  datasets.

Regressor representation
------------------------

The learned regressor is represented by default as:

- ``ridge_regressor(Encoders, Bias, Weights, Diagnostics)``

The exported predicate clauses therefore use the shape:

- ``Functor(Encoders, Bias, Weights, Diagnostics)``

Diagnostics syntax
------------------

The ``diagnostics/2`` predicate returns a list of metadata terms with
the form:

::

   [
       model(ridge_regression),
       target(Target),
       training_example_count(TrainingExampleCount),
       options(Options),
       convergence(Status),
       iterations(Iterations),
       final_delta(FinalDelta),
       encoded_feature_count(FeatureCount)
   ]

Where:

- ``model(ridge_regression)`` identifies the learning algorithm that
  produced the regressor.
- ``target(Target)`` stores the target attribute name declared by the
  training dataset.
- ``training_example_count(TrainingExampleCount)`` stores the number of
  examples used during training.
- ``options(Options)`` stores the effective learning options after
  merging the user options with the library defaults.
- ``convergence(Status)`` records the optimization stop condition. The
  current values are ``tolerance`` when the largest parameter update is
  within the configured tolerance and ``maximum_iterations_exhausted``
  when training stops because the iteration cap is reached.
- ``iterations(Iterations)`` stores the number of batch gradient-descent
  updates completed during training.
- ``final_delta(FinalDelta)`` stores the maximum absolute parameter
  change from the final optimization step.
- ``encoded_feature_count(FeatureCount)`` stores the number of numeric
  features induced by the encoder list, including missing-value
  indicator features.

Use the ``regression_protocols`` ``diagnostic/2`` and
``regressor_options/2`` helper predicates when you only need a single
metadata term or the effective options.

Options
-------

The ``learn/3`` predicate accepts the following options:

- ``learning_rate/1``: Step size used by batch gradient descent when
  updating the bias and weights. Larger values speed up training but can
  overshoot; smaller values are more conservative. The default is
  ``0.05``.
- ``maximum_iterations/1``: Maximum number of gradient-descent
  iterations to run before stopping even if the tolerance criterion has
  not been met. The default is ``2000``.
- ``tolerance/1``: Convergence threshold for the maximum parameter
  update. Training stops early when the largest absolute change in the
  bias or any weight is at or below this value. The default is
  ``1.0e-7``.
- ``regularization/1``: Ridge penalty coefficient applied to the weight
  vector during optimization. Higher values increase shrinkage and can
  reduce overfitting. The default is ``0.01``.
- ``feature_scaling/1``: Controls z-score standardization of continuous
  attributes before training and prediction. Accepted values are
  ``true`` and ``false``. The default is ``true``.
