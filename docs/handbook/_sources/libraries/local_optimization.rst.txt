.. _library_local_optimization:

``local_optimization``
======================

Classical local optimization methods for continuous problems. The
library is intended both for standalone use and as a local polisher
after a global meta-heuristic (PSO, differential evolution, genetic
algorithm, simulated annealing, tabu search, ant colony, ...).

Available solvers:

- **Nelder-Mead** - derivative-free downhill simplex
- **Gradient descent** - steepest descent with Armijo or fixed line
  search
- **Conjugate gradient** - Fletcher–Reeves or Polak–Ribière with
  restarts
- **Barzilai-Borwein** - adaptive-step gradient method (BB1 / BB2 /
  alternate)
- **BFGS** - dense quasi-Newton with Armijo line search
- **L-BFGS** - limited-memory quasi-Newton with Armijo line search
- **L-BFGS-B** - bound-constrained L-BFGS (projected gradient + free
  set)
- **Trust-region Newton-CG** - full Newton with a Steihaug-CG subproblem

All solvers share the same problem protocol and the same ``run/2-4``
API. For problems with general equality or inequality constraints, see
the companion
```constrained_optimization`` <../constrained_optimization/NOTES.md>`__
library.

API documentation
-----------------

Open the
`../../docs/library_index.html#local-optimization <../../docs/library_index.html#local-optimization>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(local_optimization(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(local_optimization(tester)).

Architecture
------------

- ``local_optimization_problem_protocol`` - problem interface (required
  ``initial_point/1`` and ``objective/2``; optional gradient, Hessian,
  bounds, stop condition, and progress).
- ``local_optimization_solver`` - category providing shared option
  handling, bound projection, vector utilities, and progress reporting.
- ``nelder_mead(Problem)`` - Nelder-Mead simplex solver
  (derivative-free).
- ``gradient_descent(Problem)`` - steepest descent (requires
  ``gradient/2``).
- ``barzilai_borwein(Problem)`` - Barzilai-Borwein adaptive step
  (requires ``gradient/2``).
- ``conjugate_gradient(Problem)`` - nonlinear CG (requires
  ``gradient/2``).
- ``bfgs(Problem)`` - dense quasi-Newton (requires ``gradient/2``).
- ``lbfgs(Problem)`` - limited-memory quasi-Newton (requires
  ``gradient/2``).
- ``lbfgs_b(Problem)`` - bound-constrained L-BFGS (requires
  ``gradient/2``; uses ``position_bounds/1`` when defined).
- ``trust_region_newton_cg(Problem)`` - trust-region Newton-CG (requires
  ``gradient/2`` and ``hessian/2``).

Working with ``constrained_optimization``
-----------------------------------------

The ``constrained_optimization_problem_protocol`` extends
``local_optimization_problem_protocol`` with general equality and
inequality constraints and their Jacobians. Thus, a constrained problem
uses the same ``initial_point/1``, ``objective/2``, ``gradient/2``,
``position_bounds/1``, and optional hooks documented here, while adding
its general constraint data. Load the companion library with:

::

   | ?- logtalk_load(constrained_optimization(loader)).

This also loads ``local_optimization``, as several constrained methods
use a local solver for their inner subproblems. The local solver is
selected by name as the second parameter of ``augmented_lagrangian/2``,
``quadratic_penalty/2``, or ``log_barrier/2``:

::

   | ?- augmented_lagrangian(my_problem, lbfgs)::run(Point, Value).

In this composition, the constrained solver manages feasibility,
multipliers, penalties, or barriers, and repeatedly delegates
transformed local subproblems to the selected local solver.
Gradient-based inner solvers require the constrained problem to provide
the corresponding constraint Jacobians. Inner-solver options such as
``max_iterations``, ``tol_x``, ``tol_f``, and ``tol_g`` apply to each
delegated solve; the constrained library provides separate options for
its outer iterations and tolerances.

Because the constrained protocol extends the local protocol, its problem
objects can also be passed directly to a local solver. Doing so ignores
``equality_constraints/2`` and ``inequality_constraints/2``; only box
constraints from ``position_bounds/1`` are enforced. Direct local
polishing is therefore appropriate only when those general constraints
are absent or when temporary constraint violation is explicitly
acceptable. Otherwise, use a constrained solver and select the local
method as its inner solver.

Defining a problem
------------------

A problem object must implement ``local_optimization_problem_protocol``
by defining at least:

- ``initial_point(-Point)`` - non-empty list of numbers used as the
  starting point.
- ``objective(+Point, -Value)`` - numeric objective value. Solvers
  minimize by default; use ``objective(maximize)`` to maximize.

Optionally a problem may also define:

- ``gradient(+Point, -Gradient)`` - **required** by all gradient-based
  solvers (gradient descent, Barzilai-Borwein, conjugate gradient, BFGS,
  L-BFGS, L-BFGS-B, trust-region Newton-CG). When missing those solvers
  raise an existence error.
- ``hessian(+Point, -Hessian)`` - second-order information, as a dense
  ``N x N`` matrix (list of rows). **Required** by trust-region
  Newton-CG; optional and unused by every other solver. When missing,
  trust-region Newton-CG raises an existence error.
- ``position_bounds(-Bounds)`` - list of ``Lower-Upper`` pairs (box
  constraints). When present, trial points are projected onto the box.
- ``stop_condition(+Iteration, +BestPoint, +BestValue)`` - early
  termination.
- ``progress(+Iteration, +BestPoint, +BestValue, +Measure, +Evaluations)``
  - periodic progress callback.

Solvers
-------

Nelder-Mead
~~~~~~~~~~~

Derivative-free simplex method. Maintains a simplex of ``N+1`` vertices
in ``N`` dimensions. Each iteration reflects the worst vertex through
the centroid of the remaining vertices and may expand, contract, or
shrink.

Standard coefficients (overridable):

- reflection ``Alpha = 1``
- expansion ``Gamma = 2``
- contraction ``Rho = 0.5``
- shrink ``Sigma = 0.5``

With ``adaptive(true)``, the Gao-Han dimension-dependent coefficients
are used instead: ``Alpha = 1``, ``Gamma = 1 + 2/N``,
``Rho = 0.75 - 1/(2*N)``, and ``Sigma = 1 - 1/N``. These coefficients
override the four corresponding coefficient options.

The initial simplex is built from ``initial_point/1`` by perturbing each
coordinate. The relative step size is controlled by ``initial_step(S)``
(default ``0.05``).

Gradient descent
~~~~~~~~~~~~~~~~

Steepest descent. The search direction is the negative gradient of the
objective for minimization and the positive gradient for maximization.
Requires ``gradient/2``.

Line search options:

- ``line_search(armijo)`` (default) - backtracking Armijo sufficient
  decrease
- ``line_search(fixed)`` - constant step size given by ``step_size(S)``

When bounds are present the method becomes projected gradient descent.

Barzilai-Borwein
~~~~~~~~~~~~~~~~

Adaptive-step gradient method. After each accepted step it forms
``s = x_new - x_old`` and ``y = g_new - g_old`` and chooses the next
step length from one of:

- ``formula(bb1)`` - long step ``alpha = (s.s) / (s.y)``
- ``formula(bb2)`` - short step ``alpha = (s.y) / (y.y)``
- ``formula(alternate)`` (default) - switches between BB1 and BB2 each
  iteration

Requires ``gradient/2``. The first step uses ``step_size(S)`` (default
``1.0``). When the denominator is near zero, the computed step is
non-positive, or the step falls outside ``[step_min, step_max]``, the
previous accepted step is reused.

Line search options:

- ``line_search(none)`` (default) - accept the pure BB step
- ``line_search(armijo)`` - optional backtracking Armijo on top of the
  BB step length

When bounds are present, trial points are projected onto the box after
each step.

Conjugate gradient
~~~~~~~~~~~~~~~~~~

Nonlinear conjugate gradient with Fletcher–Reeves or Polak–Ribière
conjugacy coefficients. Requires ``gradient/2``.

- ``beta(polak_ribiere)`` (default) - uses the standard PR+ truncation
  ``max(Beta, 0)``
- ``beta(fletcher_reeves)`` - classical FR formula

The direction is reset to steepest descent every ``restart(N)``
iterations (default: problem dimension) and whenever the new direction
is not sufficiently downhill (or uphill for maximization). Line search
is backtracking Armijo.

BFGS
~~~~

Dense quasi-Newton method. Maintains an approximation to the inverse
Hessian, updated after every accepted step with the standard BFGS
rank-two formula; the approximation starts at the identity matrix (so
the first step is plain steepest descent). Requires ``gradient/2``.

Internally, maximization is handled by minimizing the negated objective
and gradient, so the quasi-Newton direction, curvature test, and Armijo
condition are always expressed in minimization form - this sidesteps the
sign-handling pitfalls of an explicit minimize/maximize branch in the
line search.

The inverse-Hessian update is skipped, keeping the previous
approximation, whenever the curvature condition ``y.s > 0`` is not
comfortably satisfied, to preserve positive definiteness. An optional
``restart(N)`` periodically resets the approximation to the identity
matrix, mirroring ``conjugate_gradient``'s direction restarts (off by
default).

Line search is backtracking Armijo, with the same options and defaults
as gradient descent and conjugate gradient.

L-BFGS
~~~~~~

Limited-memory quasi-Newton method. Instead of a dense inverse-Hessian
matrix, only the last ``memory_size(M)`` step/gradient-difference pairs
are kept, and the search direction is recovered from them with the
standard two-loop recursion - ``O(M*n)`` time and memory per iteration
instead of ``bfgs(_)``'s ``O(n^2)``. Requires ``gradient/2``.

Uses the same phi-space (always-minimize) formulation as ``bfgs(_)`` for
maximization, the same curvature safeguard (a pair is dropped rather
than risking a non-descent direction), and the same optional periodic
``restart(N)`` - here clearing the pair history instead of resetting a
matrix. With an empty history the search direction is plain steepest
descent, so the first step (and every step right after a restart)
matches ``bfgs(_)``'s first step exactly. With a longer history, the
two-loop recursion also rescales the initial direction by a factor
``gamma_k``, computed from the most recent pair as ``(s.y) / (y.y)``, on
every iteration - a standard conditioning heuristic - so, unlike the
first step, later steps are not expected to exactly retrace
``bfgs(_)``'s trajectory even with a large ``memory_size``.

Line search is backtracking Armijo, with the same options and defaults
as the other gradient-based solvers.

L-BFGS-B
~~~~~~~~

Bound-constrained L-BFGS with a **level-B approximate generalized Cauchy
point** (first-segment quadratic minimization along the projected
gradient path). Unlike plain ``lbfgs(_)``, which only clamps trial
points after an unconstrained step, this solver:

1. Builds an **approximate GCP** ``x^c``: breakpoints of
   ``x(t)=P(x-tg)``, minimize a quadratic model of the limited-memory
   BFGS Hessian on the first segment ``[0,t_1]``, with curvature from
   the most recent pair (``gamma = (s.y)/(y.y)``)
2. Identifies the **free set** at ``x^c``
3. Computes an L-BFGS direction via the two-loop recursion and **masks**
   components outside the free set or that would leave the box
4. Limits the Armijo step to the largest **feasible** step along that
   direction
5. Stops on the **projected gradient** norm

Requires ``gradient/2``. When ``position_bounds/1`` is absent it behaves
like unconstrained ``lbfgs(_)``. Prefer this solver whenever box
constraints are present; prefer plain ``lbfgs(_)`` for purely
unconstrained problems.

Options match ``lbfgs(_)`` (``memory_size``, ``restart``, Armijo
parameters).

Full multi-segment Byrd-Lu-Nocedal-Zhu Cauchy search and free-subspace
minimization of the quadratic model are not implemented (possible future
refinement).

Trust-region Newton-CG
~~~~~~~~~~~~~~~~~~~~~~

Full Newton's method with a Steihaug-CG (truncated conjugate gradient)
trust-region subproblem. Requires both ``gradient/2`` and ``hessian/2``,
the latter as a dense ``N x N`` matrix (list of rows).

Unlike every other gradient-based solver here, there is no line search.
At each outer iteration, the step is obtained by approximately
minimizing the local quadratic model (built from the gradient and
Hessian at the current point) within a ball of radius ``trust_radius``,
using Steihaug-CG: plain conjugate gradient on the model, stopped early
either by a direction of non-positive curvature or by reaching the
ball's boundary, in which case the step is extended to the boundary
along the current CG direction. Whenever an unconstrained Newton step
lies inside the trust region, Steihaug-CG recovers it directly, so
convergence is quadratic near a well-behaved minimum (typically far
fewer iterations than the quasi-Newton solvers above, at the cost of
requiring an explicit Hessian).

The trust-region radius itself is grown or shrunk each iteration based
on how well the quadratic model predicted the actual objective change
(the ratio ``rho`` of actual to predicted reduction, Nocedal and Wright
Algorithm 4.1): ``rho < 0.25`` shrinks the radius, ``rho > 0.75`` with
the step at the boundary grows it (up to ``trust_radius_max``), and a
step is accepted only when ``rho > eta``. A rejected step still counts
as a completed iteration (only the radius shrinks; gradient and Hessian
are not re-evaluated).

Uses the same phi-space (always-minimize) formulation as ``bfgs(_)`` and
``lbfgs(_)`` for maximization, so the subproblem and acceptance test are
always expressed in minimization form.

Common options
--------------

Inherited from the solver category and available to every solver:

- ``initial_point(Point)``- override problem defined initial point (no
  default).
- ``objective(minimize|maximize)`` - optimization direction (default:
  ``minimize``).
- ``target_value(Value)`` - stop when the best value reaches or passes
  the target; use ``none`` to disable (default: ``none``).
- ``max_iterations(N)`` - iteration limit (default: ``1000``).
- ``tol_x(T)`` - step-size / simplex-size tolerance (default:
  ``1.0e-8``).
- ``tol_f(T)`` - objective-change tolerance (default: ``1.0e-8``).
- ``tol_g(T)`` - gradient-norm tolerance; used by gradient-based solvers
  (default: ``1.0e-6``).
- ``updates(N)`` - number of progress reports; ``0`` disables reporting
  (default: ``0``).

Solver-specific options
-----------------------

.. _nelder-mead-1:

Nelder-Mead
~~~~~~~~~~~

- ``reflection(Alpha)`` - default ``1.0``
- ``expansion(Gamma)`` - default ``2.0``
- ``contraction(Rho)`` - default ``0.5``
- ``shrink(Sigma)`` - default ``0.5``
- ``initial_step(S)`` - relative initial simplex step (default ``0.05``)
- ``adaptive(false|true)`` - use Gao-Han dimension-dependent
  coefficients (default ``false``)

.. _gradient-descent-1:

Gradient descent
~~~~~~~~~~~~~~~~

- ``line_search(armijo|fixed)`` - default ``armijo``
- ``step_size(S)`` - initial / fixed step (default ``1.0``)
- ``armijo_c(C)`` - sufficient-decrease constant (default ``1.0e-4``)
- ``armijo_tau(T)`` - backtracking factor (default ``0.5``)
- ``armijo_max_backtracks(N)`` - default ``20``

.. _barzilai-borwein-1:

Barzilai-Borwein
~~~~~~~~~~~~~~~~

- ``formula(bb1|bb2|alternate)`` - default ``alternate``
- ``step_size(S)`` - initial / fallback step (default ``1.0``)
- ``step_min(S)`` - lower clamp for the BB step (default ``1.0e-10``)
- ``step_max(S)`` - upper clamp for the BB step (default ``1.0e10``)
- ``line_search(none|armijo)`` - default ``none``
- ``armijo_c(C)``, ``armijo_tau(T)``, ``armijo_max_backtracks(N)`` -
  used only when ``line_search(armijo)`` is selected

.. _conjugate-gradient-1:

Conjugate gradient
~~~~~~~~~~~~~~~~~~

- ``beta(polak_ribiere|fletcher_reeves)`` - default ``polak_ribiere``
- ``restart(dimension|N)`` - reset interval; ``dimension`` means every
  ``N`` iterations (default ``dimension``)
- ``step_size(S)``, ``armijo_c(C)``, ``armijo_tau(T)``,
  ``armijo_max_backtracks(N)`` - same meaning as for gradient descent

.. _bfgs-1:

BFGS
~~~~

- ``restart(none|dimension|N)`` - periodic inverse-Hessian reset
  interval; ``none`` disables periodic resets (default), ``dimension``
  means every ``N`` iterations
- ``step_size(S)``, ``armijo_c(C)``, ``armijo_tau(T)``,
  ``armijo_max_backtracks(N)`` - same meaning as for gradient descent

.. _l-bfgs-1:

L-BFGS
~~~~~~

- ``memory_size(M)`` - number of step/gradient-difference pairs kept
  (default ``10``)
- ``restart(none|dimension|N)`` - periodic pair-history reset interval;
  ``none`` disables periodic resets (default), ``dimension`` means every
  ``N`` iterations
- ``step_size(S)``, ``armijo_c(C)``, ``armijo_tau(T)``,
  ``armijo_max_backtracks(N)`` - same meaning as for gradient descent

.. _l-bfgs-b-1:

L-BFGS-B
~~~~~~~~

Same options as L-BFGS (``memory_size``, ``restart``, ``step_size``,
Armijo parameters).

.. _trust-region-newton-cg-1:

Trust-region Newton-CG
~~~~~~~~~~~~~~~~~~~~~~

- ``trust_radius_initial(R)`` - starting trust-region radius (default
  ``1.0``)
- ``trust_radius_max(R)`` - upper bound on the trust-region radius
  (default ``100.0``); ``trust_radius_initial`` must not exceed it
- ``eta(Eta)`` - minimum actual-to-predicted reduction ratio for a step
  to be accepted, in ``[0, 0.25)`` (default ``0.15``)
- ``cg_tol(T)`` - relative residual tolerance that stops the inner
  Steihaug-CG early (default ``0.1``)
- ``cg_max_iterations(dimension|N)`` - cap on inner Steihaug-CG
  iterations per outer step; ``dimension`` means ``N`` (default
  ``dimension``)

Run statistics
--------------

The ``run/4`` predicate returns a list of statistics. Fields common to
all solvers:

- ``iterations(N)`` - completed iterations
- ``evaluations(E)`` - objective function evaluations
- ``final_value(V)`` - best objective value found

Nelder-Mead additionally reports:

- ``final_simplex_size(S)`` - maximum edge length of the final simplex

Gradient descent, Barzilai-Borwein, conjugate gradient, BFGS, L-BFGS,
L-BFGS-B, and trust-region Newton-CG additionally report:

- ``gradient_evaluations(G)`` - number of gradient evaluations
- ``final_gradient_norm(N)`` - Euclidean norm of the final gradient
  (projected gradient norm for L-BFGS-B)

Trust-region Newton-CG additionally reports:

- ``hessian_evaluations(H)`` - number of Hessian evaluations

Limitations
-----------

- Continuous numeric vectors only.
- Box constraints only. Use the ``constrained_optimization`` library for
  equality and inequality constraints.
- Single starting point (no multi-start wrapper yet).
- Projected steps after a conjugate-gradient update can weaken
  conjugacy; a pure bound-constrained CG formulation is not yet
  implemented.
- Projected steps after a BFGS or unconstrained L-BFGS update can weaken
  the quasi-Newton model; use ``lbfgs_b(_)`` for box constraints.
- L-BFGS-B uses a **level-B approximate GCP** (first segment only) plus
  free-set masking and feasible-step limiting. Full multi-segment BLNZ
  Cauchy search and quadratic subspace minimization on free variables
  remain possible future refinements.
- Trust-region Newton-CG requires a dense ``N x N`` Hessian; there is no
  Hessian-free (matrix-vector-product-only) variant, and a projected
  step can weaken agreement between the model and the actual objective
  change, which can trigger more trust-region shrinkage than an
  unconstrained problem would.

Usage
-----

Defining a problem (derivative-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   :- object(rosenbrock,
       implements(local_optimization_problem_protocol)).

       initial_point([-1.2, 1.0]).

       objective([X, Y], Value) :-
           Value is 100*(Y - X*X)^2 + (1 - X)^2.

   :- end_object.

Defining a problem with analytic gradient
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   :- object(sphere,
       implements(local_optimization_problem_protocol)).

       initial_point([3.0, 4.0]).

       objective([X, Y], Value) :-
           Value is X*X + Y*Y.

       gradient([X, Y], [GX, GY]) :-
           GX is 2*X,
           GY is 2*Y.

   :- end_object.

Defining a problem with an analytic Hessian (for trust-region Newton-CG)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   :- object(sphere,
       implements(local_optimization_problem_protocol)).

       initial_point([3.0, 4.0]).

       objective([X, Y], Value) :-
           Value is X*X + Y*Y.

       gradient([X, Y], [GX, GY]) :-
           GX is 2*X,
           GY is 2*Y.

       hessian(_Point, [[2.0, 0.0], [0.0, 2.0]]).

   :- end_object.

.. _barzilai-borwein-2:

Barzilai-Borwein
~~~~~~~~~~~~~~~~

::

   | ?- barzilai_borwein(sphere)::run(Point, Value, Statistics, [
           formula(alternate),
           max_iterations(200),
           tol_g(1.0e-8)
       ]).

   | ?- barzilai_borwein(rosenbrock)::run(Point, Value, [
           formula(bb2),
           line_search(armijo),
           max_iterations(500)
       ]).

.. _nelder-mead-2:

Nelder-Mead
~~~~~~~~~~~

::

   | ?- nelder_mead(rosenbrock)::run(Point, Value).

   | ?- nelder_mead(rosenbrock)::run(
            Point, Value, Statistics,
            [max_iterations(500), tol_x(1.0e-10), tol_f(1.0e-12)]
        ).

.. _gradient-descent-2:

Gradient descent
~~~~~~~~~~~~~~~~

::

   | ?- gradient_descent(sphere)::run(Point, Value).

   | ?- gradient_descent(sphere)::run(
            Point, Value, Statistics,
            [max_iterations(200), tol_g(1.0e-8), line_search(armijo)]
        ).

   | ?- gradient_descent(sphere)::run(
            Point, Value,
            [line_search(fixed), step_size(0.1), max_iterations(100)]
        ).

.. _conjugate-gradient-2:

Conjugate gradient
~~~~~~~~~~~~~~~~~~

::

   | ?- conjugate_gradient(rosenbrock)::run(Point, Value).

   | ?- conjugate_gradient(rosenbrock)::run(
            Point, Value, Statistics,
            [beta(polak_ribiere), max_iterations(500), tol_g(1.0e-8)]
        ).

   | ?- conjugate_gradient(sphere)::run(
            Point, Value,
            [beta(fletcher_reeves), restart(10), max_iterations(100)]
        ).

.. _bfgs-2:

BFGS
~~~~

::

   | ?- bfgs(rosenbrock)::run(Point, Value).

   | ?- bfgs(rosenbrock)::run(
            Point, Value, Statistics,
            [max_iterations(200), tol_g(1.0e-10)]
        ).

   | ?- bfgs(sphere)::run(
            Point, Value,
            [restart(dimension), max_iterations(100)]
        ).

.. _l-bfgs-2:

L-BFGS
~~~~~~

::

   | ?- lbfgs(rosenbrock)::run(Point, Value).

   | ?- lbfgs(rosenbrock)::run(
            Point, Value, Statistics,
            [memory_size(20), max_iterations(200), tol_g(1.0e-10)]
        ).

   | ?- lbfgs(sphere)::run(
            Point, Value,
            [memory_size(5), restart(dimension), max_iterations(100)]
        ).

L-BFGS-B (box constraints)
~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- lbfgs_b(bounded_sphere)::run(Point, Value).

   | ?- lbfgs_b(bounded_sphere)::run(
            Point, Value, Statistics,
            [memory_size(10), max_iterations(200), tol_g(1.0e-10)]
        ).

.. _trust-region-newton-cg-2:

Trust-region Newton-CG
~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- trust_region_newton_cg(rosenbrock)::run(Point, Value).

   | ?- trust_region_newton_cg(rosenbrock)::run(
            Point, Value, Statistics,
            [max_iterations(100), tol_g(1.0e-10)]
        ).

   | ?- trust_region_newton_cg(sphere)::run(
            Point, Value,
            [trust_radius_initial(0.5), cg_max_iterations(2)]
        ).

Maximization
~~~~~~~~~~~~

::

   | ?- gradient_descent(negative_sphere)::run(
            Point, Value,
            [objective(maximize), max_iterations(200), tol_g(1.0e-8)]
        ).

Box-constrained problem
~~~~~~~~~~~~~~~~~~~~~~~

::

   :- object(bounded_sphere,
       implements(local_optimization_problem_protocol)).

       initial_point([0.8, 0.8]).

       position_bounds([(-1.0)-1.0, (-1.0)-1.0]).

       objective([X, Y], Value) :-
           Value is X*X + Y*Y.

       gradient([X, Y], [GX, GY]) :-
           GX is 2*X,
           GY is 2*Y.

   :- end_object.

   | ?- gradient_descent(bounded_sphere)::run(Point, Value).
   % Point is projected onto [-1,1]^2

Using as a local polisher after a global search
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- differential_evolution(my_problem)::run(Rough, _, [max_generations(100)]),
        nelder_mead(my_problem)::run(Refined, Value, [initial_point(Rough), max_iterations(200)]).

   | ?- conjugate_gradient(my_problem)::run(
            Refined, Value,
            [max_iterations(200), tol_g(1.0e-10)]
        ).
