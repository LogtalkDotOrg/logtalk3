.. _library_linear_programming:

``linear_programming``
======================

The ``linear_programming`` library provides immutable construction and
solving of small linear and mixed-integer linear programs. The
``simplex`` backend is a portable dense two-phase tableau implementation
supporting Bland's and Dantzig's pivot rules. The
``milp_branch_and_bound`` backend provides deterministic depth-first
branch-and-bound over ``simplex`` LP relaxations.

The model API supports continuous, integer, and binary variable
declarations. The ``simplex`` backend solves continuous models only and
throws a ``domain_error(simplex_variable_type, Variable-Type)`` error
for other variable types. Use ``milp_branch_and_bound`` to solve models
containing integer or binary variables.

API documentation
-----------------

Open the
`../../apis/library_index.html#linear-programming <../../apis/library_index.html#linear-programming>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(linear_programming(loader)).

Testing
-------

To test this library, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(linear_programming(tester)).

Model representation
--------------------

Variables are identified by unique ground terms. Linear expressions are
lists of ``Coefficient*Variable`` terms. Repeated variables are combined
and zero coefficients are removed. Arithmetic expression trees such as
``[2*x + 3*y]`` are not accepted; write ``[2*x, 3*y]`` instead.

Continuous and integer variables default to bounds ``0`` and ``inf``.
Binary variables default to bounds ``0`` and ``1``. Explicit bounds are
numbers, ``-inf`` for an absent lower bound, or ``inf`` for an absent
upper bound.

Example
-------

::

   | ?- simplex::new_problem(P0),
        simplex::variable(x, continuous, P0, P1),
        simplex::variable(y, continuous, P1, P2),
        simplex::constraint([1*x, 2*y], =<, 14, P2, P3),
        simplex::constraint([3*x, -1*y], >=, 0, P3, P4),
        simplex::objective([3*x, 4*y], maximize, P4, P),
        simplex::solve(P, Result),
        simplex::status(Result, Status),
        simplex::variable_value(Result, x, X),
        simplex::variable_value(Result, y, Y),
        simplex::objective_value(Result, Value).
   Status = optimal,
   X = 14.0,
   Y = 0,
   Value = 42.0.

Dense matrix conversion
-----------------------

The ``problem_from_matrices/8`` predicate converts a dense standard-form
input to the same immutable model. Equality rows are represented by
``Aeq`` and ``Beq``; inequality rows are represented by ``Aineq`` and
``Bineq`` and always mean ``Aineq*x =< Bineq``. Variables are named by
one-based column indices.

::

   | ?- simplex::problem_from_matrices(
            [3,4], maximize,
            [], [],
            [[1,2],[-3,1]], [14,0],
            [0-inf,0-inf], Problem
        ),
        simplex::solve(Problem, Result).

Results and errors
------------------

Solver statuses are ``optimal``, ``infeasible``, ``unbounded``,
``iteration_limit``, ``node_limit``, and ``numerical_error``. Only an
optimal result contains variable values and an objective value. The
corresponding inspector predicates fail for other statuses. Malformed
problems and unsupported backend capabilities throw errors instead of
being reported as mathematical solver statuses.

The supported options are ``max_iterations(PositiveInteger)``,
defaulting to 10000, ``tolerance(PositiveNumber)``, defaulting to
``1.0e-9``, and ``pivot_rule(Rule)``, where ``Rule`` is ``bland`` (the
default) or ``dantzig``. Bland's rule selects the first eligible
entering column. Dantzig's rule selects the eligible column with the
most negative reduced cost, breaking ties by the lowest column index.
The iteration limit is shared by both simplex phases.

Mixed-integer solving
---------------------

The ``milp_branch_and_bound`` backend supports mixed continuous,
integer, and binary models. Integer and binary variables must have
finite bounds and each integer domain must contain at least one integer.
The solver branches on the first fractional discrete variable in
declaration order and explores the lower branch first. It proves
optimality by exhausting or bounding the search tree; it does not use
cutting planes or primal heuristics.

The supported options are ``max_nodes(PositiveInteger)``, defaulting to
10000, ``integrality_tolerance(PositiveNumber)``, defaulting to
``1.0e-9``, ``simplex_max_iterations(PositiveInteger)``, defaulting to
10000, and ``simplex_tolerance(PositiveNumber)``, defaulting to
``1.0e-9``, and ``simplex_pivot_rule(Rule)``, defaulting to ``bland``,
which selects the pivot rule for all LP relaxations.

::

   | ?- milp_branch_and_bound::new_problem(P0),
        milp_branch_and_bound::variable(x, binary, P0, P1),
        milp_branch_and_bound::variable(y, binary, P1, P2),
        milp_branch_and_bound::constraint([2*x, 2*y], =<, 3, P2, P3),
        milp_branch_and_bound::objective([1*x, 1*y], maximize, P3, P),
        milp_branch_and_bound::solve(P, Result).

Limitations
-----------

Both backends use dense lists and are intended for small problems. The
library does not implement presolve beyond bound and standard-form
normalization, sparse storage, cutting planes, dual values, reduced
costs, basis export, warm starts, time limits, callbacks, or parallel
search.
