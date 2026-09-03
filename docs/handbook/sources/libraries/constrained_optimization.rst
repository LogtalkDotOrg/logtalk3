.. _library_constrained_optimization:

``constrained_optimization``
============================

This library complements ``local_optimization`` (which only handles box
constraints via ``position_bounds/1``) with general equality and
inequality constraints.

API documentation
-----------------

Open the
`../../docs/library_index.html#constrained-optimization <../../docs/library_index.html#constrained-optimization>`__
link in a web browser.

Loading
-------

To load all entities currently in this library, load the ``loader.lgt``
file:

::

   | ?- logtalk_load(constrained_optimization(loader)).

Testing
-------

To test this library's predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(constrained_optimization(tester)).

Solvers
-------

``qp_active_set``
~~~~~~~~~~~~~~~~~

``qp_active_set`` is a dense primal active-set solver for convex
quadratic programs, following Nocedal and Wright, section 16.5. It
repeatedly solves the KKT system for the current working set. A nonzero
step is taken either fully or only as far as the nearest newly binding
inactive inequality. At a zero step, the first active inequality with a
negative multiplier in row order is dropped, following Bland's rule.
Equalities are always active and are never dropped.

The initial feasible point is found heuristically. The solver starts
from the minimum-norm point satisfying the equalities, or the origin
when there are no equalities, then repeatedly adds violated inequalities
to that system. It verifies feasibility both after this phase and before
returning. If no feasible point is found, the active-set iteration limit
is reached, or a working-set KKT system is singular, ``solve/8`` fails
rather than returning an infeasible or non-optimal point.

Linear systems use ``linear_algebra::solve_linear_system/3``; the
phase-1 minimum-norm point uses ``linear_algebra::pseudo_inverse/2``.
The solver is intended for small dense QPs and is also used internally
by ``sqp_active_set(_)``.

Current limitation: phase 1 is not a complete two-phase or elastic-mode
method and can fail to find a feasible point even when one exists.

``sqp_active_set(Problem)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This Sequential Quadratic Programming solver builds and solves a
``qp_active_set`` subproblem at each iteration. It maintains a
damped-BFGS approximation to the Hessian of the Lagrangian. The secant
pair uses Lagrangian gradients and Powell damping; multipliers for box
constraints are omitted because those constraints are linear.

Globalization uses the L1 exact-penalty merit function
``f(x) + mu*(||g(x)||_1 + ||max(0,h(x))||_1)`` with Armijo backtracking.
The penalty weight is increased as required by the current QP
multipliers and is never decreased. A failed QP solve or exhausted line
search causes ``run/4`` to fail.

Bounds from ``position_bounds/1`` are converted into exact linear
inequality rows on the QP step. Convergence requires both a sufficiently
small QP step and acceptable constraint violation. Statistics include
``final_step_norm/1``, ``final_constraint_violation/1``, and
``termination_reason/1``; possible reasons are ``converged``,
``target_reached``, ``stop_condition``, and ``max_iterations``.

This solver is generally the best choice for problems close to a
quadratic objective with linear constraints. Current limitations: there
is no feasibility-restoration phase when a nonlinear constraint
linearization produces an infeasible QP, and the QP working set is not
warm-started between iterations or runs.

``augmented_lagrangian(Problem, InnerSolver)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This method of multipliers delegates each outer subproblem to an
existing ``local_optimization`` solver such as ``bfgs`` or ``lbfgs``.
Equality constraints use the Hestenes-Powell term
``lambda.g(x) + (rho/2)*||g(x)||^2``. Inequalities use the smooth
Rockafellar-Bertsekas term based on ``max(0, lambda_j + rho*h_j(x))``.
After each inner solve, equality multipliers are updated by
``lambda + rho*g(x)`` and inequality multipliers by
``max(0, lambda + rho*h(x))``.

The penalty parameter grows only when the new constraint violation fails
to decrease by the configured factor. Bounds are forwarded unchanged to
the inner solver instead of being penalized. The inner solver always
minimizes because the subproblem already applies the sign required for
the requested objective direction.

``max_iterations``, ``tol_x``, ``tol_f``, and ``tol_g`` apply to each
inner solve; ``max_outer_iterations`` and ``outer_tolerance`` control
the outer loop. ``updates(N)`` controls outer ``progress/5`` callbacks,
while ``inner_updates(N)`` controls delegated ``inner_progress/6``
callbacks. Inner callbacks use the stage ``outer(K)`` and report the
transformed subproblem objective and convergence measure. Both options
default to zero, disabling their respective callbacks. Statistics
include ``final_violation/1`` and ``termination_reason/1``, with
``converged``, ``target_reached``, or ``max_iterations`` as possible
reasons.

This is the usual default among the delegated outer-loop methods.
Current limitation: inner-solver state such as a BFGS approximation is
rebuilt for every outer iteration.

``quadratic_penalty(Problem, InnerSolver)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This solver delegates minimization of
``f(x) + (rho/2)*(||g(x)||^2 + ||max(0,h(x))||^2)`` to an existing
``local_optimization`` solver. It has the same outer-loop and bound
handling as ``augmented_lagrangian(_, _)``, but maintains no multiplier
estimates. The penalty parameter grows when constraint violation does
not decrease by the configured factor.

The inner and outer option scopes and termination reasons are the same
as for ``augmented_lagrangian(_, _)``, including the independent outer
and inner progress controls. Statistics include ``final_violation/1``
and ``termination_reason/1``.

The method is simple and can suit small, well-conditioned problems, but
``augmented_lagrangian(_, _)`` is normally preferable. Current
limitation: exact feasibility is reached only as ``rho`` tends to
infinity. Large penalties increasingly ill-condition the inner problem,
so an insufficient outer budget or unsuitable penalty schedule can leave
a material residual.

``log_barrier(Problem, InnerSolver)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This primal interior-point method handles inequalities using
``-mu*sum(log(-h_j(x)))`` and handles equalities using a quadratic
penalty. Each outer iteration delegates the resulting subproblem to an
existing ``local_optimization`` solver, then unconditionally shrinks
``mu`` and grows the equality penalty only when equality violation is
not decreasing fast enough. Bounds are forwarded to the inner solver.

If the initial point is not strictly inequality-feasible, the solver
first minimizes a squared-violation phase-1 subproblem. If that
heuristic does not produce a strictly feasible point, ``run/4`` raises
``domain_error(strictly_feasible_initial_point, Point)``. During the
main solve, trial points outside the barrier domain use a smooth
quadratic fallback so an inner line search cannot trigger an arithmetic
exception. Outer and inner progress use the same independent controls as
the other delegated solvers. Main inner solves use the stage
``outer(K)``; phase-1 callbacks use ``phase1``.

This solver is useful when the objective is defined only in the interior
of the feasible region. Statistics report ``final_mu/1`` and
``final_equality_violation/1`` separately, plus ``termination_reason/1``
with ``converged``, ``target_reached``, or ``max_iterations``.

Current limitation: phase 1 is heuristic and may fail on a feasible
nonlinear problem. The inner solver is also not domain-aware; the smooth
fallback protects trial evaluations but is not a replacement for a
feasibility-preserving line search.

``primal_dual_interior_point(Problem)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This infeasible-start method implements Mehrotra's predictor-corrector
(Nocedal and Wright, chapter 19, algorithm 19.3). At each iteration it
solves an affine-scaling predictor system, derives the cubic centering
parameter, then solves a corrector system with the centering and
second-order correction terms. Slack and inequality multiplier steps use
the fraction-to-boundary rule. A damped-BFGS approximation represents
the Hessian of the Lagrangian, and an L1 merit function globalizes the
step.

Slack variables allow the problem's initial point to violate equality,
inequality, and box constraints. Bounds are folded into the same reduced
KKT system as constant-Jacobian inequalities. The centering quantity
``sigma*mu`` is distinct from the merit-function penalty and is
recomputed from the predictor step every iteration.

Statistics report ``final_stationarity_residual/1``,
``final_primal_infeasibility/1``, ``final_complementarity_gap/1``, and
``termination_reason/1``. Reasons are ``converged``, ``target_reached``,
``stop_condition``, ``max_iterations``, or ``kkt_singular``.

Current limitations: KKT solves are dense and have no scaling or
iterative refinement; an accurate point can therefore be returned with
``termination_reason(kkt_singular)`` before every requested residual
tolerance is met. The merit line search retains a safeguarded fallback
because strict rejection of all non-descent or non-Armijo trials is not
robust on the supported nonlinear and maximization cases. Like the other
nonlinear solvers, this is a local method and may converge to any
reachable KKT point rather than a global optimum.

Architecture
------------

- ``constrained_optimization_solver`` - a category providing shared
  numeric, objective-direction, list, inner-solver construction,
  penalty-update, and validation auxiliary predicates used by the
  solvers and their internal problem wrappers. Imported alongside
  ``local_optimization_solver(_Problem_)`` wherever both are needed, via
  ``imports([constrained_optimization_solver, local_optimization_solver(_Problem_)])``;
  called via ``^^``, like any other category predicate.
- ``constrained_optimization_problem_protocol`` - extends
  ``local_optimization_problem_protocol`` with
  ``equality_constraints/2``, ``equality_jacobian/2``,
  ``inequality_constraints/2``, and ``inequality_jacobian/2``. Box
  constraints continue to be expressed via the inherited
  ``position_bounds/1``. Implemented by problems passed to any of the
  five problem-object solvers.
- ``qp_solver_protocol`` - protocol for QP subroutines operating on
  plain matrices/vectors
  (``minimize 0.5*x^T*H*x + c^T*x s.t. Aeq*x = beq, Aineq*x =< bineq``),
  as opposed to the problem-object-based protocols above. Implemented by
  ``qp_active_set``.
- The three solvers that delegate to an inner local solver use internal
  problem wrappers: ``sub_problem/6`` for
  ``augmented_lagrangian(_, _)``, ``penalty_sub_problem/4`` for
  ``quadratic_penalty(_, _)``, and ``barrier_sub_problem/5`` plus
  ``phase1_sub_problem/2`` for ``log_barrier(_, _)``. The wrappers
  expose the transformed objective and constraints expected by the
  selected inner solver.

Defining a QP directly
----------------------

``qp_active_set`` does not use a problem object; ``solve/8`` takes the
problem in standard form directly:

::

   minimize    0.5 * x^T * H * x + c^T * x
   subject to  Aeq * x = beq
               Aineq * x =< bineq

- ``H`` - dense ``N x N`` matrix (list of rows), symmetric positive
  semi-definite over the feasible set.
- ``C`` - length-``N`` vector.
- ``Aeq``, ``Beq`` - equality constraint matrix (``Meq x N``, possibly
  ``[]``) and right-hand side (length ``Meq``).
- ``Aineq``, ``Bineq`` - inequality constraint matrix (``Mineq x N``,
  possibly ``[]``, rows in the form ``row.x =< b``) and right-hand side
  (length ``Mineq``).

``solve/8`` fails (rather than raising an error) when no feasible point
is found, the problem is unbounded below, or the current working set
becomes numerically singular; it raises a ``domain_error/2`` when the
input matrices/vectors have inconsistent dimensions.

::

   | ?- qp_active_set::solve(
            [[1.0,0.0],[0.0,1.0]], [0.0,0.0],
            [], [],
            [[-1.0,-1.0]], [-2.0],
            X, Lambda
        ).
   X = [1.0, 1.0],
   Lambda = [1.0].

   % minimize 0.5*(x1^2+x2^2) subject to x1+x2 >= 2: the closest point
   % on that line to the origin.

Solving a nonlinear constrained problem
---------------------------------------

All five nonlinear solvers take a problem object implementing
``constrained_optimization_problem_protocol``, in the same way
``bfgs(_)``/``lbfgs(_)`` take a ``local_optimization_problem_protocol``
object:

::

   | ?- sqp_active_set(my_problem)::run(BestPoint, BestValue).

   | ?- augmented_lagrangian(my_problem, bfgs)::run(BestPoint, BestValue).

``sqp_active_set(_)`` and ``primal_dual_interior_point(_)`` require the
corresponding Jacobian whenever a constraint predicate is defined. The
three delegated solvers require Jacobians when their selected inner
solver uses gradients. A missing required Jacobian raises
``existence_error/2`` rather than silently falling back to finite
differences.

Library-wide limitations
------------------------

All solvers use dense matrices. Problem sizes are therefore expected to
remain within the range where dense KKT systems and dense inner-solver
Hessian approximations are practical. An ``initial_point(Point)`` option
can warm-start the point, but no solver preserves internal state such as
a QP working set, dual and slack variables, or an inner solver's Hessian
approximation between runs.

Usage
-----

Solving a QP directly
~~~~~~~~~~~~~~~~~~~~~

::

   | ?- qp_active_set::solve(
            [[1.0,0.0,0.0],[0.0,1.0,0.0],[0.0,0.0,1.0]], [0.0,0.0,-1.0],
            [[1.0,1.0,1.0]], [3.0],
            [[-1.0,0.0,0.0],[0.0,-1.0,0.0],[0.0,0.0,1.0]], [0.0,0.0,1.0],
            X, _Lambda
        ).
   X = [1.0, 1.0, 1.0].

   % minimize 0.5*(x1^2+x2^2+x3^2) - x3
   % subject to x1+x2+x3 = 3, x1 >= 0, x2 >= 0, x3 =< 1

Solving the same problem with SQP
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   :- object(my_problem,
       implements(constrained_optimization_problem_protocol)).

       initial_point([0.0, 0.0, 0.0]).
       objective(X, V) :- X = [X1,X2,X3], V is 0.5*(X1*X1+X2*X2+X3*X3) - X3.
       gradient(X, [X1,X2,G3]) :- X = [_,_,X3], G3 is X3 - 1.0.
       equality_constraints(X, [G]) :- X = [X1,X2,X3], G is X1+X2+X3-3.0.
       equality_jacobian(_, [[1.0,1.0,1.0]]).
       inequality_constraints(X, [H1,H2,H3]) :- X = [X1,X2,X3],
           H1 is -X1, H2 is -X2, H3 is X3-1.0.
       inequality_jacobian(_, [[-1.0,0.0,0.0],[0.0,-1.0,0.0],[0.0,0.0,1.0]]).

   :- end_object.

   | ?- sqp_active_set(my_problem)::run(X, V).
   X = [1.0, 1.0, 1.0].

Solving it with augmented_lagrangian instead
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- augmented_lagrangian(my_problem, bfgs)::run(X, V).
   X = [1.0, 1.0, 1.0].

Solving it with quadratic_penalty instead
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- quadratic_penalty(my_problem, bfgs)::run(X, V).
   X = [1.0, 1.0, 1.0].

Solving it with log_barrier instead
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``initial_point/1`` need not be strictly feasible - a heuristic phase 1
runs first if it is not, but supplying one directly, when convenient,
skips that extra work:

::

   | ?- log_barrier(my_problem, bfgs)::run(X, V).
   X = [1.0, 1.0, 1.0].

Solving it with primal_dual_interior_point instead
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

No inner solver to name, and initial_point/1 need not be feasible at all
- [0.0, 0.0, 0.0] (which violates x1 >= 0 exactly, on the boundary, and
doesn't satisfy the equality either) works fine:

::

   | ?- primal_dual_interior_point(my_problem)::run(X, V).
   X = [1.0, 1.0, 1.0].

Warm-starting from a previous solution
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Every solver in this library accepts an ``initial_point(Point)`` option
(from ``local_optimization_solver(_Problem_)``) that overrides
``Problem``'s own ``initial_point/1`` - useful when re-solving a problem
that changed only slightly from one already solved:

::

   | ?- sqp_active_set(my_problem)::run(X, V, _Statistics, [initial_point([1.0, 1.0, 1.0])]).
   X = [1.0, 1.0, 1.0].

This overrides only the *starting point*, not any solver-internal state
(a QP working set, dual variables, or an inner solver's Hessian
approximation).
