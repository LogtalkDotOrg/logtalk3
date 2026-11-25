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


`wmc`
=====

This library provides weighted model counting (WMC) and plain model counting
(also known as #SAT, the problem of counting the satisfying assignments of a
formula) using either external model counter command-line tools or a reference
counter written in Logtalk. It follows the design of the `sat` and `max_sat`
libraries: problems are written as ordered, ground statements with
term-valued variables (e.g., `queen(Row, Column)`), serialized to DIMACS CNF
source with weights using the Model Counting Competition format, counted by
a solver process, and the output is decoded into a collected result.

The weighted model count of a formula is the sum, over all its models, of the
product of the weights of the literals true in the model. With all weights
equal to 1, the count is the number of models.

The supported solvers, selected using the `solver(Name)` option, are:

- `reference`: a counter written in Logtalk (DPLL with unit propagation and
  connected component decomposition); no external dependency; suitable for
  small problems and as a test oracle; this is the default solver
- `ganak`: [Ganak](https://github.com/meelgroup/ganak), which computes exact
  integer counts and exact rational weighted counts (default executable:
  `ganak`); pre-built binaries for Linux, macOS, and Windows are published
  in the project releases (tested with verison 2.9.0)
- `sharpsat_td`: [SharpSAT-TD](https://github.com/Laakeri/sharpsat-td),
  which computes exact integer counts and weighted counts with arbitrary
  precision floats (default executable: `sharpSAT`); it must be compiled
  from source and its `flow_cutter_pace17` helper binary must be installed
  in the same directory as `sharpSAT`; running it requires a POSIX shell

Other counters that follow the Model Counting Competition output format
(`s` status line and `c s exact PRECISION KIND VALUE` result line) can be
supported by extending the `wmc` object.


API documentation
-----------------

Open the [../../apis/library_index.html#wmc](../../apis/library_index.html#wmc)
file in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(wmc(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(wmc(tester)).

Tests that require a solver are conditional on the solver executable being
available in the system path. When using GNU Prolog, some of the tests are
skipped due to the small default global stack (set the `GLOBALSZ`
environment variable to a larger value, e.g., 65536, to run them all).


Problems
--------

A problem is a list of statements or an object implementing the
`wmc_problem_protocol` protocol, whose facts are compiled to
`wmc_statement/2` clauses by the `wmc_expansion` hook (as with the `sat`
library, use `hook(wmc_expansion)` when compiling the object). Statements
are:

- clauses: a disjunction of literals (`+Term`, `-Term`, or a bare `Term`,
  equivalent to `+Term`); the constant `false` is the empty clause
- `weight(Literal, Weight)`: the weight of a literal, a non-negative
  integer or float; the weight of a literal is 1 unless specified, and it
  can be given at most once
- `probability(Term, P)`: shorthand for `weight(+Term, P)` and
  `weight(-Term, 1-P)`, with `P` between 0 and 1
- `comment(Text)` and `raw(Text)`

Variables that occur only in `weight/2` and `probability/2` statements are
free variables: they are not constrained by any clause but still contribute
the sum of their two weights to the count.

Disjunctions used as arguments must be parenthesized to be portable across
backends. The functors `weight/2`, `probability/2`, `comment/1`, and `raw/1`
and the constant `false` cannot be used as bare propositional terms.

As an example, consider an object definition of the following problem:
probability of an alarm in the classical burglary and earthquake network,
where the alarm sounds if there is a burglary or an earthquake. The weighted
model count with evidence (the alarm sounded) is the probability of the
evidence:

	:- object(alarm,
		implements(wmc_problem_protocol)).

		-burglary ; alarm.
		-earthquake ; alarm.
		burglary ; earthquake ; -alarm.
		alarm.
		probability(burglary, 0.1).
		probability(earthquake, 0.2).

	:- end_object.

Assuming the object saved in an `alarm.lgt` file:

    | ?- logtalk_load(alarm, hook(wmc_expansion)).
    ...

We can then solve the problem and print the result:

	| ?- wmc::solve(object(alarm), Result),
	     wmc::value(Result, Value).
	Value = 0.28

Marginal and conditional probabilities are ratios of counts: count the
problem with and without the evidence (or the query) and divide.

A second example, using a term representation for the problem of counting
the independent sets of a path with three vertices :

	| ?- wmc::solve(terms([(-in(a) ; -in(b)), (-in(b) ; -in(c))]), Result),
	     wmc::print_result(Result).
	s SATISFIABLE
	c s exact int 5


Sources
-------

The `solve/2-3` and `program/1-3` predicates accept the following sources:

- `object(Object)`: an object implementing `wmc_problem_protocol`
- `terms(Statements)`: a list of statements
- `codes(Codes)`: CNF source
- `file(Path)`: a CNF file; its contents are read and passed to the solver
  as a file named `problem.cnf`

For `codes/1` and `file/1` sources, variable terms are recovered from
`c v Number Term` marker comments, which are embedded by default in the
output of `program/1-3` (use `embed_variables(false)` to omit them).


Options
-------

- `solver(Solver)`: `reference` (default), `ganak`, or `sharpsat_td`
- `executable(Path)`: solver executable (default: the solver default
  executable, resolved using the system path); ignored by `reference`
- `time_limit(Seconds)`: time limit for external counters, enforced using
  the `timeout` command (GNU coreutils; not available on Windows), as
  neither Ganak nor SharpSAT-TD has a time limit option; zero (the default)
  means unlimited; ignored by `reference`
- `decomposition_time(Seconds)`: time spent computing a tree decomposition
  by SharpSAT-TD (default: 1); SharpSAT-TD always spends this time, so
  larger problems benefit from larger values
- `embed_variables(Boolean)`: embed variable marker comments in
  `program/1-3` output (default: `true`)

Weighted problems (with `weight/2` or `probability/2` statements, or `c p
weight` lines in `codes/1` and `file/1` sources) are solved using the
weighted counting mode of the external counters; other problems use the
unweighted mode, which produces exact integer counts.


Results
-------

The `solve/2-3` predicates return a `wmc_result(Status, Value, Properties)`
term where `Status` is one of:

- `counted`: `Value` is the weighted model count
- `unsatisfiable`: the count is zero
- `unknown`: no count (e.g., the time limit was reached); `Value` is `none`

`Value` is an integer for unweighted counts and a float for weighted counts.
Properties include `number_type/1`, `representation/1` (e.g., `arb-frac`),
`value_text/1` (the count as printed by the counter), `exact_fraction/1`
(when the counter reports a fraction), `log10_estimate/1`, `solver/1`,
`termination/1` (`completed`, `resource_limit`, or `interrupted`),
`variables/1`, `variable_count/1`, `diagnostics/1`, `source_map/1`,
`raw_status/1`, `process_status/1`, and `raw_output/1`.


Notes and limitations
---------------------

- Weighted counts are floats, thus limited to double precision and subject
  to its range. For exact rational counts, use `result_property/2` to get
  the `exact_fraction/1` property reported by Ganak (when it fits in the
  integers of the backend) or the `value_text/1` property.
- Backends with bounded integers (e.g., GNU Prolog) approximate integer
  counts that exceed their integer range using floats.
- The `reference` counter reports an `unsatisfiable` status whenever the
  count is zero, including problems that are satisfiable but have zero
  weight.
- The `reference` counter ignores the `time_limit/1` option.
- The process launcher scripts are adapted from the `sat` library ones.
