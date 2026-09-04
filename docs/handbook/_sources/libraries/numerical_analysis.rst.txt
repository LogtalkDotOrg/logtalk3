.. _library_numerical_analysis:

``numerical_analysis``
======================

Portable numerical-analysis algorithms using objects as function
callbacks. The library provides scalar root finding, finite-interval
scalar quadrature, one-dimensional interpolation, and non-stiff
initial-value ODE solvers.

Available algorithms:

- **Bisection** - robust bracketed root finder.
- **Brent-Dekker** - recommended derivative-free bracketed root finder.
- **Secant** - derivative-free open root finder.
- **Newton** - derivative-based open root finder.
- **Adaptive Simpson** - finite-interval quadrature with absolute and
  relative error control.
- **Gauss-Legendre** - fixed-order quadrature using 2, 4, 8, or 16
  nodes.
- **Piecewise linear** - stable local interpolation.
- **Barycentric polynomial** - global polynomial interpolation with
  precomputed weights.
- **Cubic spline** - natural or clamped cubic splines with first- and
  second-derivative evaluation.
- **Euler** - first-order fixed-step ODE solver.
- **RK4** - classical fourth-order fixed-step ODE solver.
- **RK45** - recommended adaptive Dormand-Prince 5(4) ODE solver.

API documentation
-----------------

Open the
`../../apis/library_index.html#numerical-analysis <../../apis/library_index.html#numerical-analysis>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(numerical_analysis(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(numerical_analysis(tester)).

Function callbacks
------------------

A function object implements ``univariate_function_protocol`` by
defining ``evaluate/2``. Newton's method additionally requires
``derivative/2``:

::

   :- object(example_function,
       implements(univariate_function_protocol)).

       evaluate(X, Value) :-
           Value is X * X - 2.0.

       derivative(X, Derivative) :-
           Derivative is 2.0 * X.

   :- end_object.

Root finding
------------

Root finders provide ``find_root/2-4``. Bisection and Brent accept a
``bracket(Lower, Upper)`` initial specification, secant accepts
``guesses(First, Second)``, and Newton accepts ``guess(Initial)``:

::

   | ?- bisection_root_finder(example_function)::find_root(
   |        bracket(0.0, 2.0), Root).

Common options are ``tol_x(Tolerance)``, ``tol_f(Tolerance)``, and
``max_iterations(Iterations)``. The four-argument variant returns
statistics including iteration and function-evaluation counts, the final
function value, ``converged(Boolean)``, and
``termination_reason(Reason)``. Newton statistics report derivative
evaluations separately.

Quadrature
----------

Quadrature objects provide ``integrate/3-5``:

::

   | ?- adaptive_simpson_quadrature(example_function)::integrate(
   |        0.0, 2.0, Integral).

Options are ``tol_abs(Tolerance)``, ``tol_rel(Tolerance)``, and
``max_subdivisions(Subdivisions)`` for adaptive Simpson. Gauss-Legendre
accepts ``order(Order)``, where the supported orders are 2, 4, 8, and
16. Zero-width intervals return ``0.0`` without evaluating the callback.
Reversed bounds negate the result.

Interpolation
-------------

Interpolators provide ``fit/2-3`` and ``evaluate/3``. Points are
represented by ``X-Y`` pairs. Input points are sorted by ``X``; at least
two numeric points with distinct abscissas are required. Evaluation
outside the fitted closed domain raises a domain error.

Piecewise-linear and barycentric interpolation have no options. Cubic
splines accept ``boundary(natural)`` (the default) or
``boundary(clamped(FirstDerivative, LastDerivative))``. The
``cubic_spline_interpolator::derivative/4`` predicate evaluates
derivative orders one and two. Fitted models are implementation-specific
opaque terms.

ODE systems and solvers
-----------------------

An ODE system object implements ``ode_system_protocol`` by defining
``derivative(Time, State, Derivative)``. States and derivatives are
non-empty numeric lists of the same length. Scalar equations use
one-element lists.

ODE solvers provide ``solve/4-6``. A trajectory is an ordered list of
``Time-State`` pairs that includes both endpoints. Forward and backward
integration are supported; equal initial and final times return a
singleton trajectory without evaluating the system.

Euler and RK4 accept ``step_size(Step)`` and ``max_steps(MaxSteps)``.
RK45 accepts ``initial_step(Step)``, ``min_step(Step)``,
``max_step(Step)``, ``tol_abs(Tolerance)``, ``tol_rel(Tolerance)``,
``safety_factor(Factor)``, and ``max_steps(MaxSteps)``. Step options are
positive magnitudes; the solver derives direction from the time
interval. Statistics report accepted and rejected steps, derivative
evaluations, final step size, convergence, and the termination reason.

Convergence
-----------

Iteration, subdivision, step-budget, or minimum-step exhaustion is
reported as a numerical outcome. The best available result is returned
with ``converged(false)`` and an explanatory termination reason. Invalid
inputs, options, brackets, fitted models, or callback results raise
standard errors.

Limitations
-----------

Quadrature is scalar and limited to finite intervals. Interpolators do
not extrapolate. The ODE solvers are explicit methods for non-stiff
systems and do not provide event detection, dense output, stiffness
detection, or implicit integration.

Numerical results remain subject to each backend's floating-point
arithmetic.
