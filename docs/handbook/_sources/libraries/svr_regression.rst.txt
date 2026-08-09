.. _library_svr_regression:

``svr_regression``
==================

Support vector regression (SVR) using an epsilon-insensitive dual margin
model with linear, polynomial, and radial basis function kernels.
Training uses a per-example dual subgradient descent loop
(Pegasos-style), the same optimization pattern used by
``kernel_svm_classifier``, with the one-sided hinge-loss condition
replaced by the two-sided epsilon-insensitive tube condition that
defines SVR.

The library implements the ``regressor_protocol`` defined in the
``regression_protocols`` library. It provides predicates for learning a
regressor from a dataset object, using it to make predictions, and
exporting the learned model as a list of predicate clauses or to a file.

Datasets are represented as objects implementing the
``regression_dataset_protocol`` protocol.

API documentation
-----------------

Open the
`../../apis/library_index.html#svr-regression <../../apis/library_index.html#svr-regression>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(svr_regression(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(svr_regression(tester)).

To run the performance benchmark suite, load the
``tester_performance.lgt`` file:

\| ?- logtalk_load(svr_regression(tester_performance)).

Features
--------

- **Multiple Kernels**: Supports ``linear``,
  ``polynomial(Degree, Gamma, Coef0)``, and ``rbf(Gamma)`` kernels,
  selected via the ``kernel/1`` option, matching
  ``kernel_svm_classifier``'s kernel specifications.
- **Epsilon-Insensitive Loss**: Predictions within ``epsilon`` of the
  target incur no penalty; only residuals outside that tube drive
  coefficient updates, following Vapnik's epsilon-SVR formulation.
- **Mixed Features**: Reuses ``regressor_common``'s shared tabular
  encoders for continuous and categorical attributes, including
  missing-value indicators.
- **Regularized Training**: Supports configurable learning-rate
  scheduling, tolerance, and L2 regularization, applied as per-example
  weight decay in the same style as ``kernel_svm_classifier``.
- **Diagnostics Metadata**: Learned regressors record model name,
  target, training example count, kernel, optimization stop reason,
  completed iterations, final parameter delta, encoded feature count,
  and effective options, accessible using the shared regression
  diagnostics predicates.
- **Model Export**: Learned regressors can be exported as predicate
  clauses or written to a file.

A note on convergence and the default options
---------------------------------------------

Unlike ``lasso_regression``'s coordinate descent, which converges to a
precise KKT-optimality fixed point, this library's dual subgradient
descent loop converges, like any fixed- or decaying-step-size stochastic
method, to a neighborhood of the optimum rather than to it exactly;
``final_delta`` in the diagnostics will generally not reach very small
values even after many epochs, and ``convergence`` will typically read
``maximum_iterations_exhausted`` rather than ``tolerance`` unless
``maximum_iterations`` is set generously.

This matters more for SVR than it does for ``kernel_svm_classifier``: a
classifier's hinge loss only needs the decision boundary on the right
side of each example, so a somewhat noisy fit is often good enough, but
a regressor is scored on how numerically close its predictions are,
which is directly sensitive to how tightly training has converged.

Empirically, training with the constant learning-rate schedule
(``learning_schedule(constant)``) plateaus quickly and stays noisy even
with many more epochs. Using
``learning_schedule(inverse_scaling(Power))`` instead — which decays the
step size over epochs, satisfying the standard Robbins-Monro conditions
for stochastic-approximation convergence — reduced prediction error by
roughly an order of magnitude in testing against the same dataset and
epoch budget. For this reason ``inverse_scaling(0.5)`` is the library
default (rather than ``kernel_svm_classifier``'s
``learning_schedule(constant)`` default), paired with a larger default
``maximum_iterations`` (``200`` rather than ``kernel_svm_classifier``'s
``25``). When tighter fits are needed, raising ``maximum_iterations``
well into the hundreds or low thousands, optionally combined with a
smaller ``epsilon`` and a larger initial ``learning_rate`` to compensate
for the decaying schedule, converges substantially further; how far
depends on the dataset.

Because ``epsilon`` is compared directly against residuals in the target
variable's own units, it is scale-dependent: an ``epsilon`` of ``0.1``
is a tight tolerance for a target ranging in the thousands and a loose
one for a target ranging between 0 and 1. Choose it relative to the
acceptable prediction error for the problem at hand, not by leaving it
at the default without considering the target's scale. Continuous
*features* are standardized via ``feature_scaling/1`` as usual, but the
target itself is never rescaled, so predictions and ``epsilon`` stay in
the target's original units.

Options
-------

The ``learn/3`` predicate supports these options:

- ``kernel/1`` - kernel function to use (default: ``linear``)
- ``epsilon/1`` - half-width of the epsilon-insensitive tube, in the
  target variable's units; residuals within this distance of zero incur
  no loss (default: ``0.1``)
- ``learning_rate/1`` - base learning rate for the dual optimization
  loop (default: ``0.5``)
- ``learning_schedule/1`` - learning-rate schedule, either ``constant``
  or ``inverse_scaling(Power)`` (default: ``inverse_scaling(0.5)``)
- ``maximum_iterations/1`` - maximum number of optimization epochs
  (default: ``200``)
- ``tolerance/1`` - convergence threshold for the maximum parameter
  update in an epoch (default: ``1.0e-5``)
- ``l2_regularization/1`` - L2 penalty factor applied as per-example
  weight decay during optimization (default: ``0.001``)
- ``feature_scaling/1`` - whether to standardize continuous attributes
  before encoding (default: ``true``)

Usage
-----

Learning a regressor
~~~~~~~~~~~~~~~~~~~~

::

   | ?- svr_regression::learn(dataset, Regressor).

   | ?- svr_regression::learn(dataset, Regressor, [kernel(rbf(0.5)), maximum_iterations(500)]).

Making predictions
~~~~~~~~~~~~~~~~~~

::

   | ?- svr_regression::learn(dataset, Regressor),
        svr_regression::predict(Regressor, [x1-2.0, x2-3.0], Prediction).

Exporting the regressor
~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- svr_regression::learn(dataset, Regressor),
        svr_regression::export_to_clauses(dataset, Regressor, predict_target, Clauses).

   | ?- svr_regression::learn(dataset, Regressor),
        svr_regression::export_to_file(dataset, Regressor, predict_target, 'regressor.pl').

Regressor representation
------------------------

The learned regressor is represented as a compound term with the form:

::

   svr_regressor(Encoders, Kernel, TrainingRows, Bias, Coefficients, Diagnostics)

Where:

- ``Encoders``: list of continuous scaling descriptors and categorical
  value encoders
- ``Kernel``: selected kernel specification
- ``TrainingRows``: encoded feature vectors for the training examples
- ``Bias``: the model's scalar bias (intercept) term
- ``Coefficients``: list of one dual coefficient per training row
- ``Diagnostics``: training metadata including the effective options

When exported using ``export_to_clauses/4`` or ``export_to_file/4``,
this regressor term is serialized directly as the single argument of the
generated predicate clause so that the exported model can be loaded and
reused as-is.

Diagnostics syntax
------------------

The ``diagnostics/2`` predicate returns a list of metadata terms with
the form:

::

   [
       model(svr_regression),
       target(Target),
       training_example_count(TrainingExampleCount),
       options(Options),
       convergence(Status),
       iterations(Iterations),
       final_delta(FinalDelta),
       kernel(Kernel),
       encoded_feature_count(FeatureCount)
   ]

Where ``convergence(Status)`` is ``tolerance`` when the maximum
parameter update in a full epoch fell within the configured tolerance,
and ``maximum_iterations_exhausted`` when training stopped because the
epoch cap was reached — see the convergence note above for why the
latter is the common case with the default options.
``final_delta(FinalDelta)`` stores the maximum parameter change observed
during the final epoch. The remaining fields follow the same conventions
as ``lasso_regression``'s diagnostics, with ``kernel(Kernel)`` added to
record the kernel used for training.

Use the ``regression_protocols`` ``diagnostic/2`` and
``regressor_options/2`` helper predicates when only a single metadata
term or the effective options are needed.

References
----------

1. Vapnik, V. (1995). "The Nature of Statistical Learning Theory".
2. Smola, A.J. and Scholkopf, B. (2004). "A Tutorial on Support Vector
   Regression".
3. Shalev-Shwartz, S., Singer, Y., Srebro, N. and Cotter, A. (2011).
   "Pegasos: Primal Estimated Sub-Gradient Solver for SVM".
4. Cortes, C. and Vapnik, V. (1995). "Support-Vector Networks".
