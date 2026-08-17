.. _library_local_optimization:

``local_optimization``
======================

Classical local optimization methods for continuous problems. The
library is intended both for standalone use and as a local polisher
after a global metaheuristic (PSO, differential evolution, genetic
algorithm, simulated annealing, tabu search, ant colony, \\ldots).

Available solvers:

- **Nelder–Mead** — derivative-free downhill simplex
- **Gradient descent** — steepest descent with Armijo or fixed line
  search
- **Conjugate gradient** — Fletcher–Reeves or Polak–Ribière with
  restarts

All solvers share the same problem protocol and the same ``run/2-4``
API. BFGS / L-BFGS are planned.

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

- ``local_optimization_problem_protocol`` — problem interface (required
  ``initial_point/1`` and ``objective/2``; optional gradient, Hessian,
  bounds, stop condition, and progress).
- ``local_optimization_solver`` — category providing shared option
  handling, bound projection, vector utilities, and progress reporting.
- ``nelder_mead(Problem)`` — Nelder–Mead simplex solver
  (derivative-free).
- ``gradient_descent(Problem)`` — steepest descent (requires
  ``gradient/2``).
- ``conjugate_gradient(Problem)`` — nonlinear CG (requires
  ``gradient/2``).

Defining a problem
------------------

A problem object must implement ``local_optimization_problem_protocol``
by defining at least:

- ``initial_point(-Point)`` — non-empty list of numbers used as the
  starting point.
- ``objective(+Point, -Value)`` — numeric objective value. Solvers
  minimize by default; use ``objective(maximize)`` to maximize.

Optionally a problem may also define:

- ``gradient(+Point, -Gradient)`` — **required** by gradient descent and
  conjugate gradient. When missing those solvers raise an existence
  error.
- ``hessian(+Point, -Hessian)`` — optional second-order information (for
  future Newton / verification use).
- ``position_bounds(-Bounds)`` — list of ``Lower-Upper`` pairs (box
  constraints). When present, trial points are projected onto the box.
- ``stop_condition(+Iteration, +BestPoint, +BestValue)`` — early
  termination.
- ``progress(+Iteration, +BestPoint, +BestValue, +Measure, +Evaluations)``
  — periodic progress callback.

Solvers
-------

Nelder–Mead
~~~~~~~~~~~

Derivative-free simplex method. Maintains a simplex of (n+1) vertices in
(n) dimensions. Each iteration reflects the worst vertex through the
centroid of the remaining vertices and may expand, contract, or shrink.

Standard coefficients (overridable):

- reflection (\\alpha = 1)
- expansion (\\gamma = 2)
- contraction (\\rho = 0.5)
- shrink (\\sigma = 0.5)

The initial simplex is built from ``initial_point/1`` by perturbing each
coordinate. The relative step size is controlled by ``initial_step(S)``
(default ``0.05``).

Gradient descent
~~~~~~~~~~~~~~~~

Steepest descent. The search direction is (-\\nabla f) for minimization
and (+\\nabla f) for maximization. Requires ``gradient/2``.

Line search options:

- ``line_search(armijo)`` (default) — backtracking Armijo sufficient
  decrease
- ``line_search(fixed)`` — constant step size given by ``step_size(S)``

When bounds are present the method becomes projected gradient descent.

Conjugate gradient
~~~~~~~~~~~~~~~~~~

Nonlinear conjugate gradient with Fletcher–Reeves or Polak–Ribière
conjugacy coefficients. Requires ``gradient/2``.

- ``beta(polak_ribiere)`` (default) — uses the standard PR+ truncation
  (\\max(\\beta, 0))
- ``beta(fletcher_reeves)`` — classical FR formula

The direction is reset to steepest descent every ``restart(N)``
iterations (default: problem dimension) and whenever the new direction
is not sufficiently downhill (or uphill for maximization). Line search
is backtracking Armijo.

Common options
--------------

Inherited from the solver category and available to every solver:

- ``objective(minimize|maximize)`` — optimization direction (default:
  ``minimize``).
- ``target_value(Value)`` — stop when the best value reaches or passes
  the target; use ``none`` to disable (default: ``none``).
- ``max_iterations(N)`` — iteration limit (default: ``1000``).
- ``tol_x(T)`` — step-size / simplex-size tolerance (default:
  ``1.0e-8``).
- ``tol_f(T)`` — objective-change tolerance (default: ``1.0e-8``).
- ``tol_g(T)`` — gradient-norm tolerance; used by gradient-based solvers
  (default: ``1.0e-6``).
- ``updates(N)`` — number of progress reports; ``0`` disables reporting
  (default: ``0``).

Solver-specific options
-----------------------

.. _neldermead-1:

Nelder–Mead
~~~~~~~~~~~

- ``reflection(Alpha)`` — default ``1.0``
- ``expansion(Gamma)`` — default ``2.0``
- ``contraction(Rho)`` — default ``0.5``
- ``shrink(Sigma)`` — default ``0.5``
- ``initial_step(S)`` — relative initial simplex step (default ``0.05``)
- ``adaptive(false|true)`` — reserved for a future Gao–Han variant
  (default ``false``)

.. _gradient-descent-1:

Gradient descent
~~~~~~~~~~~~~~~~

- ``line_search(armijo|fixed)`` — default ``armijo``
- ``step_size(S)`` — initial / fixed step (default ``1.0``)
- ``armijo_c(C)`` — sufficient-decrease constant (default ``1.0e-4``)
- ``armijo_tau(T)`` — backtracking factor (default ``0.5``)
- ``armijo_max_backtracks(N)`` — default ``20``

.. _conjugate-gradient-1:

Conjugate gradient
~~~~~~~~~~~~~~~~~~

- ``beta(polak_ribiere|fletcher_reeves)`` — default ``polak_ribiere``
- ``restart(dimension|N)`` — reset interval; ``dimension`` means every
  (n) iterations (default ``dimension``)
- ``step_size(S)``, ``armijo_c(C)``, ``armijo_tau(T)``,
  ``armijo_max_backtracks(N)`` — same meaning as for gradient descent

Run statistics
--------------

The ``run/4`` predicate returns a list of statistics. Fields common to
all solvers:

- ``iterations(N)`` — completed iterations
- ``evaluations(E)`` — objective function evaluations
- ``final_value(V)`` — best objective value found

Nelder–Mead additionally reports:

- ``final_simplex_size(S)`` — maximum edge length of the final simplex

Gradient descent and conjugate gradient additionally report:

- ``gradient_evaluations(G)`` — number of gradient evaluations
- ``final_gradient_norm(N)`` — Euclidean norm of the final gradient

Limitations
-----------

- Continuous numeric vectors only.
- Box constraints only (projection / clamping); no general equality or
  inequality constraint handling.
- Single starting point (no multi-start wrapper yet).
- Nelder–Mead adaptive (Gao–Han) coefficients are stubbed but not
  active.
- Projected steps after a conjugate-gradient update can weaken
  conjugacy; a pure bound-constrained CG formulation is not yet
  implemented.
- BFGS / L-BFGS are not yet available.

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

.. _neldermead-2:

Nelder–Mead
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
        % refine with a local solver (problem must expose Rough as
        % initial_point/1, or use a small adapter object)
        nelder_mead(my_problem)::run(Refined, Value, [max_iterations(200)]).

   | ?- conjugate_gradient(my_problem)::run(
            Refined, Value,
            [max_iterations(200), tol_g(1.0e-10)]
        ).

Planned solvers
---------------

- BFGS (dense quasi-Newton)
- L-BFGS (limited-memory quasi-Newton)

Both will implement the same ``run/2-4`` API and reuse
``local_optimization_problem_protocol`` and
``local_optimization_solver``.
