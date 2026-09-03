________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________


`linear_programming`
====================

The `linear_programming` library provides immutable construction and solving
of small linear programs. The initial `simplex` backend is a portable dense
two-phase tableau implementation using Bland's pivot rule.

The model API supports continuous, integer, and binary variable declarations
so that future MILP backends can consume the same models. The `simplex`
backend currently solves continuous models only and throws a
`domain_error(simplex_variable_type, Variable-Type)` error for other variable
types.


API documentation
-----------------

Open the [../../apis/library_index.html#linear-programming](../../apis/library_index.html#linear-programming)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

    | ?- logtalk_load(linear_programming(loader)).


Testing
-------

To test this library, load the `tester.lgt` file:

    | ?- logtalk_load(linear_programming(tester)).


Model representation
--------------------

Variables are identified by unique ground terms. Linear expressions are lists
of `Coefficient*Variable` terms. Repeated variables are combined and zero
coefficients are removed. Arithmetic expression trees such as
`[2*x + 3*y]` are not accepted; write `[2*x, 3*y]` instead.

Continuous and integer variables default to bounds `0` and `inf`. Binary
variables default to bounds `0` and `1`. Explicit bounds are numbers, `-inf`
for an absent lower bound, or `inf` for an absent upper bound.


Example
-------

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

The `problem_from_matrices/8` predicate converts a dense standard-form input
to the same immutable model. Equality rows are represented by `Aeq` and
`Beq`; inequality rows are represented by `Aineq` and `Bineq` and always mean
`Aineq*x =< Bineq`. Variables are named by one-based column indices.

    | ?- simplex::problem_from_matrices(
             [3,4], maximize,
             [], [],
             [[1,2],[-3,1]], [14,0],
             [0-inf,0-inf], Problem
         ),
         simplex::solve(Problem, Result).


Results and errors
------------------

Solver statuses are `optimal`, `infeasible`, `unbounded`, `iteration_limit`,
and `numerical_error`. Only an optimal result contains variable values and an
objective value. The corresponding inspector predicates fail for other
statuses. Malformed problems and unsupported backend capabilities throw
errors instead of being reported as mathematical solver statuses.

The supported options are `max_iterations(PositiveInteger)`, defaulting to
10000, and `tolerance(PositiveNumber)`, defaulting to `1.0e-9`. The iteration
limit is shared by both simplex phases.


Limitations
-----------

The initial backend uses dense lists and is intended for small problems. It
does not implement MILP search, presolve beyond bound and standard-form
normalization, sparse storage, dual values, reduced costs, basis export, warm
starts, time limits, callbacks, or alternative pivot rules.
