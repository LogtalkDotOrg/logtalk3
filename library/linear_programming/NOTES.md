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
of small linear and mixed-integer linear programs. The `simplex` backend is a
portable dense two-phase tableau implementation supporting Bland's and
Dantzig's pivot rules. The
`milp_branch_and_bound` backend provides deterministic depth-first
branch-and-bound over `simplex` LP relaxations.

The model API supports continuous, integer, and binary variable declarations.
The `simplex` backend solves continuous models only and throws a
`domain_error(simplex_variable_type, Variable-Type)` error for other variable
types. Use `milp_branch_and_bound` to solve models containing integer or binary
variables.

The `glpk`, `cbc`, `highs`, and `scip` backends solve continuous and
mixed-integer models by running the corresponding external solver's
command-line tool (see the "External solvers" section). Unlike the
`milp_branch_and_bound` backend, they can solve models with unbounded integer
variables, and their production LP/MIP algorithms can handle larger and harder
problems than the portable reference solvers. Limits on problem size and model
features are determined by the selected external solver. These backends use
the `process` library and thus are currently only supported when using the
ECLiPSe, GNU Prolog, SICStus Prolog, Shumway Prolog, SWI-Prolog, Trealla
Prolog, and XVM backends.


API documentation
-----------------

Open the [../../apis/library_index.html#linear-programming](../../apis/library_index.html#linear-programming)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

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


Continuous solving
------------------

The `simplex` backend solves continuous linear programs using a portable dense
two-phase tableau implementation. The supported options are:

- `max_iterations(PositiveInteger)` - Maximum number of iterations shared by
    both simplex phases (default: `10000`).
- `tolerance(PositiveNumber)` - Numerical tolerance (default: `1.0e-9`).
- `pivot_rule(Rule)` - Pivot rule, either `bland` (the default), which selects
    the first eligible entering column, or `dantzig`, which selects the eligible
    column with the most negative reduced cost, breaking ties by the lowest
    column index.

    | ?- simplex::(
            new_problem(P0),
            variable(x, continuous, P0, P1),
            variable(y, continuous, P1, P2),
            constraint([1*x, 2*y], =<, 14, P2, P3),
            constraint([3*x, -1*y], >=, 0, P3, P4),
            objective([3*x, 4*y], maximize, P4, P),
            solve(P, Result),
            status(Result, Status),
            variable_value(Result, x, X),
            variable_value(Result, y, Y),
            objective_value(Result, Value)
         ).
    Status = optimal,
    X = 14.0,
    Y = 0,
    Value = 42.0.


External solvers
----------------

The `glpk`, `cbc`, `highs`, and `scip` backends implement the same protocol as
the `simplex` and `milp_branch_and_bound` backends but delegate the solving to
an external solver command-line tool. They follow the design of the similar
libraries (e.g., `sat`): the model is written as a CPLEX LP format file, the
solver is run in an isolated directory (with the problem, solution, and output
files removed afterwards), and its output is decoded into the usual result
term. Variables of any type can be used, and integer and binary variables are
supported by all four backends.

| Backend | Solver                                         | Default executable | Notes |
|---------|------------------------------------------------|--------------------|-------|
| `cbc`   | [COIN-OR CBC](https://github.com/coin-or/Cbc)  | `cbc`              | - |
| `glpk`  | [GLPK](https://www.gnu.org/software/glpk/)     | `glpsol`           | `time_limit/1` is rounded up to whole seconds |
| `highs` | [HiGHS](https://highs.dev/)                    | `highs`            | `mip_gap/1` is rejected (not supported by the CLI) |
| `scip`  | [SCIP](https://www.scipopt.org/)               | `scip`             | - |

The solver executables must be installed and available in the system path (or
their location given using the `executable/1` option). Tests of these backends
are conditional on the solver executables being available.

These backends accept the following options:

- `executable(Path)`: solver executable (default: the solver default
  executable resolved using the system path)
- `time_limit(Seconds)`: time limit (default: `0`, i.e., no limit)
- `mip_gap(Gap)`: relative MIP gap (default: `none`, i.e., the solver default)

Example:

	| ?- simplex::new_problem(P0),
	     simplex::variable(x, continuous, P0, P1),
	     simplex::variable(y, continuous, P1, P2),
	     simplex::constraint([1*x, 2*y], =<, 14, P2, P3),
	     simplex::constraint([3*x, -1*y], >=, 0, P3, P4),
	     simplex::constraint([1*x, -1*y], =<, 2, P4, P5),
	     simplex::objective([3*x, 4*y], maximize, P5, P),
	     highs::solve(P, Result),
	     highs::objective_value(Result, Objective).
	Objective = 34

The result statuses for these backends include, besides `optimal`,
`infeasible`, and `unbounded`:

- `feasible`: a solution that is not proven optimal (e.g., after reaching the
  time limit); the `objective_value/2` and `variable_value/3` predicates
  succeed for results with this status
- `infeasible_or_unbounded`: reported by solvers that cannot tell which is the
  case
- `time_limit`: the time limit was reached without finding a solution

The result statistics are `solver/1`, `solver_status/1` (the status text
reported by the solver), `process_status/1`, and `raw_status/1`. A reported
solution must be present and must be consistent with the model before a
result with status `optimal` or `feasible` is constructed. Variables omitted
from a valid solution section have the value zero, and values of integer and
binary variables are rounded to the closest integer when within `1.0e-6` of
it. Values are numbers as reported by the solver (not necessarily floats) and
are subject to the solver feasibility and optimality tolerances.

Notes:

- The LP format uses generated variable names (`x1`, `x2`, ...), thus any
  ground terms can be used as variable names in the model.
- The solvers write their solution to a file in the isolated directory (when
  they cannot print it to the standard output) and are run with that directory
  as the working directory on POSIX systems, as some of them (e.g., HiGHS)
  write log files to the working directory.
- The `lp_format::generate/2` predicate can be used to generate the LP source
  of a model, e.g., to use it with a solver that is not supported.


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

Solver statuses are `optimal`, `feasible`, `infeasible`, `unbounded`,
`infeasible_or_unbounded`, `iteration_limit`, `node_limit`, `time_limit`, and
`numerical_error`. Only `optimal` and `feasible` results contain variable
values and an objective value. The corresponding inspector predicates fail
for other statuses. Malformed problems and unsupported backend capabilities
throw errors instead of being reported as mathematical solver statuses.


Mixed-integer solving
---------------------

The `milp_branch_and_bound` backend supports mixed continuous, integer, and
binary models. Integer and binary variables must have finite bounds and each
integer domain must contain at least one integer. The solver proves optimality
by exhausting or bounding the search tree; it does not use cutting planes or
primal heuristics. A limit or numerical termination returns the best validated
incumbent as a `feasible` result and records the original termination reason as
`stop_status/1` in the statistics. Without a validated incumbent, the limit or
error status is returned directly.

The supported options are:

- `max_nodes(PositiveInteger)` - Maximum number of search nodes (default:
    `10000`).
- `integrality_tolerance(PositiveNumber)` - Integrality tolerance (default:
    `1.0e-9`).
- `simplex_max_iterations(PositiveInteger)` - Maximum simplex iterations per
    LP relaxation (default: `10000`).
- `simplex_tolerance(PositiveNumber)` - Simplex numerical tolerance (default:
    `1.0e-9`).
- `simplex_pivot_rule(Rule)` - Simplex pivot rule for all LP relaxations
    (default: `bland`).
- `branching_rule(Rule)` - Branching rule, either `first_fractional` (the
    default), which selects the first fractional discrete variable in
    declaration order, or `most_fractional`, which selects the variable whose
    value is farthest from its nearest integer, breaking ties by declaration
    order.
- `branch_order(Order)` - Child traversal order, either `lower_first` (the
    default) or `upper_first`.

    | ?- milp_branch_and_bound::
            new_problem(P0),
            variable(x, binary, P0, P1),
            variable(y, binary, P1, P2),
            constraint([2*x, 2*y], =<, 3, P2, P3),
            objective([1*x, 1*y], maximize, P3, P),
            solve(P, Result
         ).


Limitations
-----------

Both portable backends use dense lists and are intended for small problems.

The portable backends do not currently implement presolve beyond bound and
standard-form normalization, sparse storage, cutting planes, warm starts, time
limits, callbacks, or parallel search. They also do not expose dual values,
reduced costs, or basis data.

External solver capabilities and options remain limited to the features
supported by the common CLI interface described above.
