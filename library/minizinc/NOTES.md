________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>  
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


`minizinc`
==========

The `minizinc` library provides access to MiniZinc CLI solving, MiniZinc
source generation, an opt-in object expansion hook, and collected result
inspection. MiniZinc is a single frontend that delegates solving to a backend
solver.

This library interacts with the `minizinc` CLI using the `process` library.
Therefore, it currently supports ECLiPSe, GNU Prolog, SICStus Prolog,
Shumway Prolog, SWI-Prolog, Trealla Prolog, and XVM.


Requirements
------------

Solving requires the `minizinc` executable, available on the system `PATH`
or selected using the `executable(Path)` option. This library was developed
and tested against MiniZinc 2.10.1, available from its
[official website](https://www.minizinc.org/). The selected backend solver
(`solver(Id)` option) must itself be configured in the MiniZinc installation;
use `minizinc --solvers` to list the solvers your installation can use.

On POSIX systems, solving also requires `/bin/sh`, with `mktemp` available
on the system `PATH`. On Windows, solving requires PowerShell 7.3 or later
(`pwsh` on the system `PATH`).

The bundled `minizinc_process.sh` and `minizinc_process.ps1` launcher scripts
must remain in the `scripts` subdirectory of the library.


API documentation
-----------------

Open the [../../apis/library_index.html#minizinc](../../apis/library_index.html#minizinc)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(minizinc(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(minizinc(tester)).

The tests include source-generation goldens, JSON-stream decoding, expansion
isolation, source/result accessors, and solving. Solver integration has been
tested with MiniZinc 2.10.1 (Gecode and COIN-BC/CBC backends). Solver tests
are conditional on the availability of the `minizinc` executable. Lifecycle
checks verify removal of owned directories after successful execution and
after solver syntax errors.

Problem fixtures are written as Logtalk objects implementing
`minizinc_problem_protocol`. The tester loads them with
`hook(minizinc_expansion)` and solves them using `object(Object)`, checking
conversion into MiniZinc source and complete CLI solving:

- `test_files/queens.lgt` defines `minizinc_queens`, an eight-queens model
  using an array, `alldifferent`, and two array comprehensions for the
  diagonal constraints, following the
  [official MiniZinc queens pattern](https://docs.minizinc.dev/en/stable/command_line.html).
- `test_files/knapsack.lgt` defines `minizinc_knapsack`, a 0/1 knapsack
  optimization problem with an externally supplied `capacity` parameter,
  checking both the `data(Path)` and `json_data(Pairs)` options.
- `test_files/shift_schedule.lgt` defines `minizinc_shift_schedule`,
  assigning workers to shifts, checking named and anonymous enum items and
  an enum-typed array index set.

The `test_files/*.jsonl` fixtures are fixed `--json-stream` message captures
(solution, status, warning, and error messages) used to test
`minizinc_output::decode/5` directly, without invoking MiniZinc.


Problem sources
---------------

A MiniZinc problem can be represented in several ways:

- `file(Path)`: `Path` must be an atom identifying a native MiniZinc file.
- `codes(Codes)`: `Codes` must be a list of character codes representing native
  MiniZinc source.
- `terms(Statements)`: `Statements` must be an ordered list of canonical ground
  terms representing the problem.
- `object(Object)`: `Object` must be an object containing the problem and
  implementing `minizinc_problem_protocol`.

The main library predicates take a ground problem source as an input argument.


Usage
-----

Load the library, then compile an object defining a MiniZinc problem using
the `minizinc_expansion` hook object. For example:

	| ?- logtalk_load(minizinc(loader)),
	     logtalk_load(minizinc('test_files/queens'), [hook(minizinc_expansion)]).

To inspect the generated MiniZinc source without running MiniZinc, use the
`minizinc::program/1` predicate. For example:

	| ?- minizinc::program(object(minizinc_queens)).

Use the `minizinc::solve/3` predicate to solve the problem. For example, to
run MiniZinc with the Gecode backend:

	| ?- minizinc::solve(object(minizinc_queens), Result, [solver(gecode)]).

In alternative, a Logtalk application can instead dynamically build a list
of canonical statements and solve them through the `terms(Statements)`
source, without defining an object or loading the hook. An example using a
trivial single-variable model:

	| ?- minizinc::solve(terms([
	         decl(var(bool), x, none),
	         solve(satisfy)
	     ]), Result).

The `program/2` predicate returns MiniZinc character codes without invoking
MiniZinc.

The `print_program/1` predicate validates and prints a character-code list to
the current output stream, preserving its layout without adding a newline.

The `program/1` predicate generates and prints MiniZinc source, equivalent to
calling `program/2` followed by `print_program/1`.

The `minizinc_program::serialize/2` predicate serializes canonical
statements. The `minizinc_program::serialize/4` predicate also accepts a
parallel list of origins and returns a source map. Origins are
`source(File, Start, End)` or `unknown`. Map entries are
`line(Start, End, Origin)`, identifying inclusive, one-based generated line
intervals.

See the next section for full details on the solver options.


Solving
-------

The public source and result predicates are declared in the
`minizinc_protocol` protocol. The object data-provider interface is declared
in the `minizinc_problem_protocol` protocol. The serializer, expansion hook,
and JSON-stream decoder document their interfaces in their respective source
files.

The `solve/2` predicate solves a source with default options. The `solve/3`
predicate accepts an explicit final options list. Both predicates return a
ground collected result after subprocess completion and resource cleanup.

The supported options are:

- `solver(Id)` selects the backend solver by name, identifier, or tag; the
  default is `gecode`. The identifier is passed through to MiniZinc
  unchanged; an unknown identifier surfaces as a decoded `error` message
  (`minizinc_result(error, [], Properties)`), not a pre-validated rejection.
- `executable(Path)` selects a single executable, not a shell command line.
  When omitted, the executable name is `minizinc`, resolved through PATH.
  Existing executable paths are used directly.
- `time_limit(Milliseconds)` selects a nonnegative, frontend-level,
  solver-agnostic overall time limit; the default is `0`, meaning unlimited.
  This maps to MiniZinc's `--time-limit`, not a backend-specific flag.
- `all_solutions(Boolean)` selects whether to report all solutions for
  satisfaction problems, or all improving solutions for optimization
  problems; the default is `false`.
- `num_solutions(Count)` selects a nonnegative solution count limit for
  satisfaction problems; the default is `0`, meaning no limit is passed.
- `random_seed(Seed)` selects a nonnegative random seed or `auto`; the
  default is `auto`, omitting the CLI setting.
- `free_search(Boolean)` instructs the solver to ignore search annotations;
  the default is `false`.
- `parallel(Count)` selects a positive thread count or `auto`; the default
  is `auto`, omitting the CLI setting.
- `statistics(Boolean)` selects whether MiniZinc reports solver statistics;
  the default is `false`.
- `data(Path)` supplies an existing `.dzn` or `.json` data file, passed to
  MiniZinc as an additional positional argument; the default is `none`.
- `json_data(Pairs)` supplies `Name-Term` pairs as inline parameter data,
  using canonical ground value terms (`int/1`, `float/1`, `bool/1`,
  `string/1`, `array/1`, `array2d/1`, `set/1`); the default is `[]`. Data
  values cannot reference other identifiers or call operations, matching
  MiniZinc's own data-file restrictions.

Shared `options` semantics apply: repeated options are accepted and the first
occurrence wins.

Generated MiniZinc source and solver output use UTF-8. Existing native source
files are left untouched. Temporary input and output files created for
solving are removed before returning the result.

Shell dependencies that cannot be resolved raise an existence error. Launcher
failures raise `minizinc_transport(Stage, Status, Details)` errors. A
process status inconsistent with the absence of any decoded message raises
`minizinc_solver_error(Formal, Status, Diagnostics, SourceMap)`, retaining
the captured diagnostics and object source map. A well-formed solver-reported
`error` message is a successful, collected result (`Status = error`), not a
thrown exception.


Canonical representation
-------------------------

The serializer accepts these statement constructors:

- `include(Path)` emits an include item; `Path` is an atom (without quotes).
- `decl(TypeInst, Name, Expr)` emits a scalar parameter or variable
  declaration; `Expr` is `none` when the value is supplied externally (via
  `data/1` or `json_data/1`).
- `array_decl(TypeInst, IndexSets, Name, Expr)` emits an array declaration;
  `IndexSets` is a list of one or two dimensions, each a `range(Lo, Hi)` term
  or an `id(Name)` reference to a declared enum.
- `constraint(Expr)` emits a constraint item.
- `solve(Goal)` emits the solve item; `Goal` is `satisfy`, `minimize(Expr)`,
  or `maximize(Expr)`.
- `enum(Name, Cases)` emits an enum item; `Cases` is `cases(Atoms)` (named
  cases), `anon(SetExpr)` (anonymous enum, `SetExpr` is the integer-set
  argument of the `_(...)` constructor), or `declared` (forward declaration,
  cases supplied externally).
- `raw(Source)` preserves trusted native MiniZinc text with a separating
  newline; `Source` is an atom, a list of character codes, a list of
  one-character atoms, or a backend-native string, normalized into character
  codes.

`TypeInst` is `par(Base)` or `var(Base)`. `Base` is `bool`, `int`, `float`,
`string`, `range(Lo, Hi)`, `set(Base)`, or `id(Name)` (a reference to a
previously declared enum).

Expressions use `int(Integer)`, `float(Float)`, `bool(Bool)`,
`string(Codes)`, `id(Name)` (an identifier reference, also used for enum
case names), `anonymous` (the `_` anonymous decision variable), `range(Lo,
Hi)`, `set(Elements)`, `array(Elements)` (a one-dimensional literal),
`array2d(Rows)` (a two-dimensional literal), `index(ArrayExpr, Indices)`
(array access), `op(Operator, Left, Right)` and `op(Operator, Operand)`
(MiniZinc's own operator atoms, e.g. `'/\\'`, `'\\/'`, `->`, `<-`, `<->`,
`xor`, `union`, `diff`, `symdiff`, `subset`, `in`, `++`, `+`, `-`, `*`, `/`,
`div`, `mod`, `^`, `not`), `call(Name, Args)` (functions, predicates, and
global constraints, undifferentiated), `comprehension(Kind, Expr,
Generators)` with `Kind` of `array`, `set`, or `call(Name)` (the
generator-call sugar used for `forall`, `exists`, `sum`, and similar),
`if_then_else(CondThenPairs, Else)` (`Else` is an expression or `none`,
matching MiniZinc's own optional `else` branch), and `raw(Source)` (an
expression-level escape hatch).

Generators are `generator(Names, Collection, Where)`; `Names` is a list of
atoms, including `'_'` for an anonymous generator variable; `Where` is a
filter expression or `none`.

The `minizinc::solve/3` predicate's `json_data(Pairs)` option values use a
restricted subset of the same vocabulary: `int/1`, `float/1`, `bool/1`,
`string/1`, `array/1`, `array2d/1`, and `set/1` only, matching MiniZinc's own
restriction that data cannot call operations or reference other
identifiers.


Object syntax
-------------

Compile a MiniZinc container with `hook(minizinc_expansion)` and declare that
it implements `minizinc_problem_protocol`. The hook recognizes these
directives:

- `include(Path)`.
- `par(Base, Name)` and `par(Base, Name, Value)`.
- `var(Base, Name)` and `var(Base, Name, Value)`.
- `array_par(Base, IndexSets, Name)` and
  `array_par(Base, IndexSets, Name, Value)`.
- `array_var(Base, IndexSets, Name)` and
  `array_var(Base, IndexSets, Name, Value)`.
- `constraint(Expr)`.
- `solve(Goal)`.
- `enum(Name)` (forward declaration), `enum(Name, Atoms)` (named cases, a
  plain list), and `enum(Name, anon(SetExpr))` (anonymous enum).
- `raw(Source)` (same text-normalization pattern as other process
  libraries' raw escape hatches: an atom, a list of character codes, a list
  of one-character atoms, or a backend-native string).

Each directive shares the functor and arity of its canonical statement
constructor, except `par/2-3` and `var/2-3`, which map to `decl/3`, and
`array_par/3-4` and `array_var/3-4`, which map to `array_decl/4`.

For example:

	:- object(minizinc_queens,
		implements(minizinc_problem_protocol)).

		:- include('alldifferent.mzn').

		:- par(int, n, int(8)).

		:- array_var(range(int(1), id(n)), [range(int(1), id(n))], q).

		:- constraint(call(alldifferent, [id(q)])).
		:- constraint(call(alldifferent, [
			comprehension(array, op('+', index(id(q), [id(i)]), id(i)), [
				generator([i], range(int(1), id(n)), none)
			])
		])).

		:- solve(satisfy).

	:- end_object.

The hook translates this object into the canonical statements:

	include('alldifferent.mzn')
	decl(par(int), n, int(8))
	array_decl(var(range(int(1), id(n))), [range(int(1), id(n))], q, none)
	constraint(call(alldifferent, [id(q)]))
	constraint(call(alldifferent, [
		comprehension(array, op('+', index(id(q), [id(i)]), id(i)), [
			generator([i], range(int(1), id(n)), none)
		])
	]))
	solve(satisfy)

Only recognized directives are translated into canonical statements; every
other term, including ordinary helper clauses and unrelated directives, is
left unchanged, so a problem object can also declare auxiliary runtime
predicates.


Raw MiniZinc source
--------------------

Raw MiniZinc is native MiniZinc source text supplied without translation
into the canonical representation. Use it for syntax not represented by the
generator, such as `let` expressions, tuples, records, option types, or
user-defined predicates and functions.

The `codes(Codes)` source envelope passes a complete program as character
codes. The `file(Path)` source envelope passes an existing MiniZinc file
directly to MiniZinc. Neither form requires the `minizinc_expansion` hook.

The `raw/1` directive embeds raw source among translated clauses in
an object compiled with `hook(minizinc_expansion)`. The canonical `raw(Source)`
statement provides the same escape hatch within a `terms(Statements)` source
or an object's `minizinc_statement/2` data, accepting the same normalized
argument forms as the directive.


Collected results
------------------

The `minizinc_output::decode/5` predicate consumes a complete
`--json-stream` output, raw and normalized process statuses, and diagnostic
character codes. Its final argument is a ground collected result with this
representation:

	minizinc_result(Status, Solutions, Properties)

`Status` is `satisfiable`, `unsatisfiable`, `unknown`, or `error`. Collected
solutions use this representation:

	solution(Values, Objective, Time)

`Values` is a list of `Name-Value` pairs, one per identifier present in
MiniZinc's default JSON output (every global parameter and variable, since
no explicit `output` item is generated). `Objective` is the decoded
`_objective` key's value when present (the library always passes
`--output-objective`), or `none`. `Time` is the solution message's elapsed
time in milliseconds, or `none`.

Decoded values use plain Prolog terms: numbers, `true`/`false`, atoms for
strings, lists for arrays (nested lists for multi-dimensional arrays),
`set(Elements)` for sets (an element may itself be a `range(Low, High)`
term), `enum(Case)` for enumerated values, and `enum_constructor(Ctor,
Value)` for enum constructor values.

The `model/2` predicate enumerates collected solution records. The
`result_property/2` predicate enumerates `status(Status)` and stored
properties:

- `solver_status(Status)` retains the lowercased JSON status label
  (`all_solutions`, `optimal_solution`, `unsatisfiable`, `unbounded`,
  `unsat_or_unbounded`, `unknown`, `error`, or `none` when no status message
  was produced). `raw_status(Status)` and `process_status(Status)` retain
  subprocess metadata.
- `optimality(Value)` is `proven` when `solver_status` is
  `optimal_solution`, else `unproven`.
- `enumeration_complete(Value)` is `true`, `false`, or `unknown`.
- `time(Time)` retains the final status message's elapsed time, or `none`.
- `statistics(List)` retains every `statistics` message's JSON object, in
  arrival order, not merged.
- `warnings(List)` and `errors(List)` retain decoded `warning(Message,
  Location)` and `error(What, Message, Location)` terms; `Location` is
  `location(File, FirstLine, FirstColumn, LastLine, LastColumn)` or `none`.
- `diagnostics(Codes)` and `raw_json(Messages)` preserve the captured
  stderr text and every parsed JSON-stream message, in order.

A satisfy goal that stops after its first solution without `-a` produces no
terminal status message; in that case `Status` is `satisfiable` whenever at
least one solution was collected, and `unknown` otherwise. This mirrors
MiniZinc's own behavior: finding a solution is evidence of satisfiability
even when the search was not proven complete.

The accessors never start or restart a subprocess on backtracking.

The `print_result/1` predicate prints a collected result to the current
output stream using MiniZinc's own native textual conventions: each solution
is printed as `Name = Value;` lines followed by a `----------` separator,
and the final status line is `==========`, `=====UNSATISFIABLE=====`,
`=====UNBOUNDED=====`, `=====UNSATorUNBOUNDED=====`, `=====UNKNOWN=====`, or
`=====ERROR=====`. Printing uses only the collected data and never invokes
MiniZinc.


Limitations
-----------

The implementation integrates only with the `minizinc` CLI. It does not use
the C API or a compiled helper. Protocol-critical JSON and output-format
arguments (`--json-stream`, `--output-mode json`, `--output-time`,
`--output-objective`) are fixed; arbitrary CLI argument passthrough beyond
the documented options is not supported. Solver stdin is closed, so there is
no interactive input channel.

The canonical representation supports a solid, documented subset of the
MiniZinc language: scalar and array parameter/variable declarations,
constraints, the solve item, named/anonymous/forward-declared enums,
comprehensions, if-then-else, and generic operator/function calls. It does
not cover `let` expressions, tuples, records, option types, type-inst
synonyms, user-defined predicates/functions/tests, annotation items, custom
`output` items, or enum-constructor combination (`++`) and inverse
constructors. Native syntax outside this representation requires `file/1`,
`codes/1`, or `raw/1`.

Results and diagnostics are collected in memory, not streamed live.
Exhaustive model enumeration can require substantial memory.

The `time_limit/1` option is MiniZinc's own frontend-level time limit, not a
hard end-to-end timeout independent of MiniZinc. Cleanup is not guaranteed
after non-catchable aborts or failed OS cleanup operations.

Compatibility with MiniZinc versions other than 2.10.1 has not been
runtime-verified. The HiGHS backend requires a working `libhighs` plugin
installation separate from MiniZinc itself; this library does not provide or
verify backend solver installations.
