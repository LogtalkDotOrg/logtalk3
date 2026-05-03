.. _library_gaussian_process_regression:

``gaussian_process_regression``
===============================

Gaussian process regression regressor supporting continuous and
mixed-feature datasets. The library implements the
``regressor_protocol`` defined in the ``regression_protocols`` library
and learns an exact mixed gaussian process using an
automatic-relevance-determination squared-exponential kernel for
continuous encoded features together with a categorical overlap kernel
for categorical attributes.

API documentation
-----------------

Open the
`../../apis/library_index.html#gaussian_process_regression <../../apis/library_index.html#gaussian_process_regression>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(gaussian_process_regression(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(gaussian_process_regression(tester)).

To run the reference timing and fit benchmarks, load the
``tester_performance.lgt`` file:

::

   | ?- logtalk_load(gaussian_process_regression(tester_performance)).

Features
--------

- **Continuous and Mixed Features**: Supports continuous attributes and
  categorical attributes encoded using reference-level dummy coding.
- **Feature Scaling**: Continuous attributes can be standardized using
  z-score scaling.
- **Missing Values**: Missing numeric and categorical values are encoded
  using explicit missing-value indicator features.
- **Exact Bayesian Regression**: Uses exact Gaussian process regression
  with a mixed covariance kernel combining an
  automatic-relevance-determination squared-exponential component for
  continuous encoded features and a field-wise categorical overlap
  component for categorical attributes.
- **Automatic Hyperparameter Selection**: By default performs
  deterministic log-marginal-likelihood optimization of the
  continuous-feature length scales, categorical mismatch penalties,
  signal variance, and noise variance. The ``length_scale/1``,
  ``categorical_penalty/1``, ``signal_variance/1``, and
  ``noise_variance/1`` options also accept ``auto`` when optimization is
  disabled.
- **Uncertainty Quantification**: Exposes posterior predictive Gaussian
  distributions for new instances using the ``predict_distribution/3``
  predicate.
- **Adaptive Stabilization**: Retries covariance factorization with
  progressively larger jitter values and records the effective retry
  count in the learned diagnostics.
- **Memory-Based Representation**: Stores the encoded training rows and
  cached Cholesky factor required for exact posterior prediction.
- **Model Export**: Learned regressors can be exported as predicate
  clauses or written to a file.
- **Reference Benchmarks**: Includes a dedicated performance suite
  reporting training time, RMSE, and MAE for representative regression
  datasets.

Regressor representation
------------------------

The learned regressor is represented by default as:

- ``gaussian_process_regressor(Encoders, TrainingFeatures, TargetMean, Alpha, CholeskyFactor, Kernel, Diagnostics)``

The exported predicate clauses therefore use the shape:

- ``Functor(Encoders, TrainingFeatures, TargetMean, Alpha, CholeskyFactor, Kernel, Diagnostics)``

Prediction API
--------------

The standard ``predict/3`` predicate returns the posterior predictive
mean.

The ``predict_distribution/3`` predicate returns a term with the form:

- ``gaussian(Mean, Variance)``

where ``Variance`` is the posterior predictive variance for observed
targets, including the learned observation noise variance. Tiny negative
values caused by floating-point roundoff are clipped to zero; larger
negative values raise a domain error.

Diagnostics syntax
------------------

The ``diagnostics/2`` predicate returns a list of metadata terms with
the form:

::

   [
       model(gaussian_process_regression),
       target(Target),
       training_example_count(TrainingExampleCount),
       options(Options),
       kernel(squared_exponential),
       length_scales(LengthScales),
       categorical_penalties(CategoricalPenalties),
       signal_variance(SignalVariance),
       noise_variance(NoiseVariance),
       jitter(Jitter),
       continuous_feature_count(ContinuousFeatureCount),
       categorical_feature_count(CategoricalFeatureCount),
       jitter_attempts(JitterAttempts),
       log_marginal_likelihood(LogMarginalLikelihood),
       convergence(Convergence),
       iterations(Iterations),
       final_delta(FinalDelta),
       encoded_feature_count(FeatureCount)
   ]

Where:

- ``model(gaussian_process_regression)`` identifies the learning
  algorithm that produced the regressor.
- ``target(Target)`` stores the target attribute name declared by the
  training dataset.
- ``training_example_count(TrainingExampleCount)`` stores the number of
  examples used during training.
- ``options(Options)`` stores the effective learning options after
  merging the user options with the library defaults.
- ``kernel(squared_exponential)`` records the covariance-kernel family
  used by the learned model.
- ``length_scales(LengthScales)`` stores the learned per-feature
  squared-exponential length scales for the continuous encoded feature
  dimensions.
- ``categorical_penalties(CategoricalPenalties)`` stores the learned
  mismatch penalties for the categorical attributes.
- ``signal_variance(SignalVariance)`` stores the learned latent-process
  marginal variance.
- ``noise_variance(NoiseVariance)`` stores the learned observation noise
  variance.
- ``jitter(Jitter)`` stores the effective diagonal jitter used to
  stabilize the covariance factorization.
- ``continuous_feature_count(ContinuousFeatureCount)`` stores the number
  of continuous encoded feature dimensions used by the
  squared-exponential kernel.
- ``categorical_feature_count(CategoricalFeatureCount)`` stores the
  number of categorical attributes handled by the overlap-kernel
  component.
- ``jitter_attempts(JitterAttempts)`` stores the number of adaptive
  jitter retries required by the final covariance factorization.
- ``log_marginal_likelihood(LogMarginalLikelihood)`` stores the final
  training log marginal likelihood.
- ``convergence(Convergence)`` records the hyperparameter-search stop
  reason.
- ``iterations(Iterations)`` stores the number of hyperparameter-search
  sweeps performed.
- ``final_delta(FinalDelta)`` stores the last log-marginal-likelihood
  improvement magnitude.
- ``encoded_feature_count(FeatureCount)`` stores the number of numeric
  features induced by the encoder list, including missing-value
  indicator features.

Use the ``regression_protocols`` ``diagnostic/2`` and
``regressor_options/2`` helper predicates when you only need a single
metadata term or the effective options.

Options
-------

The ``learn/3`` predicate accepts the following options:

- ``kernel/1``: Kernel family. The current accepted value is
  ``squared_exponential``. The default is ``squared_exponential``.
- ``feature_scaling/1``: Controls z-score standardization of continuous
  attributes before training and prediction. Accepted values are
  ``true`` and ``false``. The default is ``true``.
- ``optimize_hyperparameters/1``: Controls deterministic
  log-marginal-likelihood optimization of the kernel hyperparameters.
  Accepted values are ``true`` and ``false``. The default is ``true``.
- ``length_scale/1``: Initial or fixed squared-exponential length-scale
  specification for the continuous encoded feature dimensions. Accepted
  values are ``auto``, a positive float that is broadcast to every
  continuous encoded feature, or a list of positive floats with one
  value per continuous encoded feature. The default is ``auto``.
- ``categorical_penalty/1``: Initial or fixed categorical
  mismatch-penalty specification. Accepted values are ``auto``, a
  positive float that is broadcast to every categorical attribute, or a
  list of positive floats with one value per categorical attribute. The
  default is ``auto``.
- ``signal_variance/1``: Initial or fixed latent-process variance.
  Accepted values are ``auto`` or a positive float. The default is
  ``auto``.
- ``noise_variance/1``: Initial or fixed observation noise variance.
  Accepted values are ``auto`` or a non-negative float. The default is
  ``auto``.
- ``jitter/1``: Positive diagonal stabilization jitter added to the
  covariance matrix before factorization. When factorization fails, the
  implementation retries with progressively larger jitter values until
  it succeeds or exhausts the retry budget. The default is ``1.0e-8``.
- ``maximum_iterations/1``: Maximum number of hyperparameter-search
  sweeps when optimization is enabled. The default is ``25``.
- ``tolerance/1``: Minimum log-marginal-likelihood improvement required
  to continue hyperparameter optimization. The default is ``1.0e-6``.
