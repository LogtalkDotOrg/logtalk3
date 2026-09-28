.. _library_time_series_regression:

``time_series_regression``
==========================

This library implements univariate time series forecasting using
autoregressive (AR) models fitted by least squares, with an optional
intercept, optional differencing (ARI models), and optional automatic
order selection using information criteria, plus analytic prediction
intervals. It implements the ``forecaster_protocol`` and reuses dataset
validation, diagnostics, export, lag construction, and differencing
support from the ``time_series_protocols`` library. Least-squares
problems are solved using the ``linear_algebra`` library, and prediction
interval quantiles are computed using the ``univariate_distributions``
library.

Datasets are objects implementing the ``time_series_dataset_protocol``
protocol. All observations must be numbers; missing observations are not
supported.

API documentation
-----------------

Open the
`../../apis/library_index.html#time-series-regression <../../apis/library_index.html#time-series-regression>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(time_series_regression(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(time_series_regression(tester)).

The test suite compares fitted coefficients, error sums, information
criteria, forecasts, and prediction interval bounds for a noisy AR(2)
dataset with values computed independently using NumPy least squares
(and, for prediction intervals, SciPy's normal quantile function).

Model
-----

An AR(p) model with differencing order d and an intercept predicts the
d-times differenced series ``y`` as:

::

   y(t) = c + phi(1) * y(t-1) + ... + phi(p) * y(t-p) + e(t)

The intercept ``c`` and the coefficients ``phi(i)`` are estimated by
conditional least squares: the model is fitted to the ``n - d - p``
observations of the differenced series that have ``p`` preceding values.
Least squares is solved using a pivoted orthogonal (QR) method that does
not form the normal equations. Forecasts are computed recursively and,
when differencing is used, integrated back to the scale of the original
series.

Typical usage:

::

   | ?- time_series_regression::learn(my_series, Forecaster, [order(2)]),
        time_series_regression::forecast(Forecaster, 12, Forecasts).

Options
-------

The ``learn/3`` predicate supports the following options:

- ``order(Order)``: autoregressive order, either a positive integer or
  ``auto``. Default is ``1``.
- ``max_order(MaxOrder)``: maximum order considered when ``order(auto)``
  is used. Default is ``10``. Only valid with ``order(auto)``.
- ``selection_criterion(Criterion)``: criterion used by ``order(auto)``;
  one of ``aic``, ``aicc``, or ``bic``. Default is ``aicc``. Only valid
  with ``order(auto)``.
- ``intercept(Boolean)``: whether to estimate an intercept. Default is
  ``true``.
- ``differencing(Differencing)``: number of times the series is
  differenced before fitting, a non-negative integer. Default is ``0``.
- ``retain_residuals(Boolean)``: whether to keep the one-step training
  residuals in the learned forecaster diagnostics. Default is ``false``.

Passing ``max_order/1`` or ``selection_criterion/1`` together with an
explicit integer order raises a
``domain_error(time_series_regression_option, Option)`` error.

The minimum series length is ``Differencing + 2 * Order + Intercept``,
where ``Intercept`` is ``1`` or ``0``, and
``Differencing + Intercept + 4`` for ``order(auto)``; otherwise
``learn/3`` throws a ``domain_error(series_length, Dataset)`` error.

Automatic order selection
-------------------------

With ``order(auto)``, orders ``1`` up to ``max_order/1`` (capped so that
every candidate has more observations than parameters, plus one) are
compared using the selected information criterion:

- ``aic``: ``n * ln(SSE/n) + 2k``
- ``aicc``: ``aic + 2k(k+1)/(n-k-1)``
- ``bic``: ``n * ln(SSE/n) + k * ln(n)``

where ``k`` is the number of estimated regression coefficients and ``n``
and ``SSE`` are the sample size and sum of squared errors. To make the
criteria comparable, all candidate orders are fitted to the same sample,
which is the one available to the largest candidate order. The selected
order is then refitted using all the observations available to it. Ties
are broken in favor of the smaller order. The candidate scores are
recorded in the ``order_selection(Criterion, Candidates)`` diagnostic,
where ``Candidates`` is a list of ``Order-Score`` pairs.

Rank-deficient designs
----------------------

When the design matrix is rank deficient (for example, when fitting an
intercept to a constant or differenced linear-trend series), the
least-squares solution is not unique. The solver returns a basic
solution with the dependent columns dropped. Forecasts are still valid
least-squares forecasts, but the individual coefficient values should
not be interpreted. The ``design_rank/1`` diagnostic reports the
numerical rank of the design matrix.

Immutable online updates
------------------------

The ``update/3-4`` predicates return a new forecaster after appending
one observation to the series while keeping the fitted intercept and
coefficients unchanged. The original forecaster is not modified. The
one-step prediction error for the new observation is added to the
``sum_squared_error/1``, ``mean_squared_error/1``, and
``scored_count/1`` diagnostics (and to the retained residuals, if
enabled). The information criteria diagnostics keep describing the
original fit. No update options are currently defined, so the
``Options`` argument of ``update/4`` must be an empty list.

Forecaster representation
-------------------------

A learned forecaster is represented as a term with the format:

::

   time_series_regression_forecaster(Model, State, Parameters, Diagnostics)

where ``Model`` is ``ar(Order, Differencing)``, ``State`` is
``ar_state(Window, Levels)``, and ``Parameters`` is
``ar_parameters(Intercept, Coefficients)``. The window holds the last
``Order`` values of the differenced series, most recent first. The
levels list holds the last value of the series at each differencing
level, starting with the original series. The intercept is ``0.0`` when
``intercept(false)`` is used.

The diagnostics list includes the ``model/1``,
``training_series_length/1``, and ``options/1`` terms common to all
forecasters plus ``order/1``, ``differencing/1``, ``intercept/1``,
``parameter_count/1``, ``scored_count/1``, ``design_rank/1``,
``sum_squared_error/1``, ``mean_squared_error/1``, ``aic/1``, ``aicc/1``
(``none`` when undefined), ``bic/1``, ``update_count/1``, and
``residuals/1`` (``none`` unless retained) terms, and
``order_selection/2`` when ``order(auto)`` is used.

Prediction intervals
--------------------

``forecast_interval/5`` computes analytic prediction interval bounds for
the next ``Horizon`` forecasts:

::

   time_series_regression::forecast_interval(Forecaster, Horizon, Lower, Upper, Options)

The bounds assume independent, identically distributed, zero-mean
Gaussian innovations and treat the fitted intercept and coefficients as
known. The ``h``-step forecast error variance is the residual variance
times the sum of the squared ``psi(0)..psi(h-1)`` weights of the model
(the AR polynomial combined with the differencing polynomial, when
``differencing/1`` is positive), and the bounds are the point forecast
plus or minus the standard normal quantile for the requested confidence
times the forecast error standard deviation. The residual variance is
``sum_squared_error/1`` divided by the residual degrees of freedom
(``scored_count/1`` minus ``design_rank/1``). A zero ``Horizon`` returns
two empty lists.

``Options`` is validated independently from ``learn/3`` options (a
``domain_error(option, Option)`` is raised for an unsupported option)
and supports:

- ``confidence(Level)``: central interval coverage, a number in the open
  interval ``]0.0, 1.0[`` (default: ``0.95``).
- ``method(normal)``: the only supported interval method (default, and
  currently the only accepted value).

When the residual degrees of freedom are not positive (an exactly
determined fit, where ``scored_count/1`` equals ``design_rank/1``), a
positive ``Horizon`` raises a
``domain_error(residual_degrees_of_freedom, Forecaster)`` error, since
the residual variance is then undefined.

The standard normal quantile is computed by the
``univariate_distributions`` library.

Limitations
-----------

- Missing observations are not supported.
- Prediction intervals are the analytic normal-theory approximation
  described above. They do not account for uncertainty in the estimated
  intercept and coefficients (only in future innovations), which
  understates interval width, particularly for short training series or
  high orders; no bootstrap or simulation-based alternative is provided.
