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


`smt`
=====

The `smt` library provides external CLI solving, SMT-LIB 2 source
generation, an opt-in object expansion hook, and collected SMT solver
result inspection across multiple solver backends.

This library interacts with the solver CLI using the `process` library.
Therefore, it currently supports ECLiPSe, GNU Prolog, SICStus Prolog,
Shumway Prolog, SWI-Prolog, Trealla Prolog, and XVM.

A source is compiled into a single SMT-LIB 2 script that the selected
solver runs to completion in one process invocation. The script may
contain more than one `check-sat`/`check-sat-assuming` command, combined
with `push`/`pop`, so a single `solve/2-3` call supports incremental
solving; see the "Incremental solving" section below.


Requirements
------------

Solving requires one of the supported solver executables, available on
the system `PATH` or selected using the `executable(Path)` option. The
`solver(Solver)` option selects which solver is invoked and the default
executable resolved for it:

- `z3` (the default) runs the `z3` executable. Z3 5.3.3 or a later version
	is expected, available from its
	[official repository](https://github.com/Z3Prover/z3).
- `cvc5` runs the `cvc5` executable. cvc5 1.3.0 or a later version is
	expected, available from its
	[official repository](https://github.com/cvc5/cvc5).
- `bitwuzla` runs the `bitwuzla` executable. Bitwuzla 0.9.1 or a later
	version is expected, available from its
	[official repository](https://github.com/bitwuzla/bitwuzla). Bitwuzla
	rejects plain linear-arithmetic logics (for example `QF_LIA`); it targets
	bit-vectors, floating-point, arrays, and related combined logics (for
	example `QF_BV`).
- `yices2` runs the `yices-smt2` executable (the SMT-LIB 2 front end for
	Yices 2). Yices 2.7.0 or a later version is expected, available from its
	[official repository](https://github.com/SRI-CSL/yices2).

Source generation does not require any executable.

On POSIX systems, solving also requires `/bin/sh`, with `mktemp` available on the system `PATH`.
On Windows, solving requires PowerShell 7.3 or later (`pwsh` on the system `PATH`).

The bundled `smt_process.sh` and `smt_process.ps1` launcher scripts must
remain alongside the library source files.


API documentation
-----------------

Open the [../../apis/library_index.html#smt](../../apis/library_index.html#smt)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(smt(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(smt(tester)).

The tests include source-generation goldens, expansion isolation, S-expression
output decoding, result accessors, and solving. Solver integration has been
tested with Z3 5.3.3, cvc5 1.3.0, Bitwuzla 0.9.1, and Yices 2.7.0 (the
`yices-smt2` executable). Solver tests are conditional on each solver's
availability and are run for all four supported solvers. A deterministic
shell fixture generates 128 KiB on each output channel to check file-backed
capture beyond typical pipe capacity. Lifecycle checks verify removal of
owned directories after successful execution, child failure, and solver
errors.

Problem fixtures are written as Logtalk objects implementing
`smt_problem_protocol`. The tester loads them with `hook(smt_expansion)`
and solves them using `object(Object)`:

- `test_files/test_objects.lgt` defines small satisfiable, unsatisfiable,
	unsat-core, push/pop, raw, and comment fixtures, plus an unrelated object
	left untouched by the hook. The solved fixtures use `QF_BV`, since it is
	the only logic confirmed to work identically across all four solvers
	(Bitwuzla does not support plain linear arithmetic).
- `test_files/trivial.smt2` and `test_files/malformed.smt2` are native
	SMT-LIB 2 fixtures used to exercise the `file/1` source and the
	`smt_solver_error` path.


Usage
-----

Load the library, then compile an object defining an SMT-LIB 2 problem using
the `smt_expansion` hook object. For example:

	| ?- logtalk_load(smt(loader)),
	     logtalk_load(smt('test_files/test_objects'), [hook(smt_expansion)]).

To inspect the generated SMT-LIB 2 source without running a solver, use the
`smt::program/1` predicate. For example:

	| ?- smt::program(object(smt_trivial_sat)).

Use the `smt::solve/3` predicate to solve the problem. For example, to solve
the `smt_trivial_sat` object, collecting the list of per-`check-sat` results
in `Results` (generating the source separately is not required):

	| ?- smt::solve(object(smt_trivial_sat), Results).

In alternative, a Logtalk application can instead dynamically build a list
of canonical statements and solve them through the `terms(Statements)`
source, without defining an object or loading the hook. An example using a
trivial single-constant problem:

	| ?- smt::solve(terms([
	         declare_const(x, int),
	         assert(>(x, 0)),
	         check_sat
	     ]), Results).

See the next section for full details on the solver features and options.


Solving
-------

The public source and result predicates are declared in the `smt_protocol`
protocol. The object data-provider interface is declared in the
`smt_problem_protocol` protocol. The serializer, output decoder, and
expansion hook document their interfaces in their respective source files.

The `solve/2` predicate solves a problem with default options. The `solve/3`
predicate accepts a list of options. Both predicates return a ground list of
collected results, one per `check-sat`/`check-sat-assuming` command in the
problem definiton, in order, after subprocess completion and resource cleanup;
UNSATISFIABLE is a successful result, not predicate failure.

The supported options are:

- `solver(Solver)` selects which solver is invoked: `z3` (the default),
	`cvc5`, `bitwuzla`, or `yices2`. The selected solver determines the
	default `executable/1` value and how `time_limit/1` is translated into a
	CLI argument.
- `executable(Path)` selects a single executable, not a shell command line.
	The default depends on `solver/1`: `z3` for `z3`, `cvc5` for `cvc5`,
	`bitwuzla` for `bitwuzla`, and `yices-smt2` for `yices2`. Existing
	executable paths are used directly; otherwise the executable is resolved
	through the system PATH.
- `time_limit(Seconds)` selects a nonnegative solver time limit in seconds;
	the default is `0`, meaning unlimited. For Z3 this is `-T:` (seconds); for
	cvc5 and Bitwuzla it is translated into milliseconds (`--tlimit=` and
	`--time-limit=`, respectively); for Yices 2 it is `--timeout=` (seconds).
	cvc5 1.3.0 has been observed to abort the process (`SIGABRT`) instead of
	reporting an `unknown` result when a tight `--tlimit` expires on a hard
	problem; `solve/2-3` surfaces this as an `smt_solver_error` rather than
	silently returning an empty result list.

cvc5 and Yices 2 reject `push`/`pop` unless run in incremental mode, so
`solve/2-3` always passes `--incremental` for these two backends; this has
no effect on non-incremental scripts. Z3 and Bitwuzla support `push`/`pop`
without any extra flag.

Shared `options` semantics apply: repeated options are accepted and the first
occurrence wins.

For a compiled `reachability` object, the `solve/2` predicate returns the
collected results using the default solver (Z3):

	| ?- smt::solve(object(reachability), Results).

To solve the same object with cvc5, Bitwuzla, or Yices 2 instead:

	| ?- smt::solve(object(reachability), Results, [solver(cvc5)]).
	| ?- smt::solve(object(reachability), Results, [solver(bitwuzla)]).
	| ?- smt::solve(object(reachability), Results, [solver(yices2)]).

Generated SMT-LIB 2 source and solver output use UTF-8. Existing native
source files are left untouched. Temporary input and output files created
for solving are removed before returning the result.

Shell dependencies that cannot be resolved raise an existence error. Launcher
failures raise `smt_transport(Stage, Status, Details)` errors. Malformed
solver output, an `(error ...)` solver response, fatal solver exit statuses,
and inconsistent solver metadata raise
`smt_solver_error(Formal, Status, Diagnostics, SourceMap)` errors, retaining
the captured diagnostics and object source map.


Sources
-------

The `program/2` predicate returns SMT-LIB 2 character codes without invoking
a solver. Its first argument is a ground source envelope:

- `object(Object)` enumerates the object's ordered `smt_statement/2` clauses.
  The object must conform to `smt_problem_protocol`.
- `terms(Statements)` serializes an ordered list of canonical, ground statements.
- `codes(Codes)` validates and returns native SMT-LIB 2 character codes unchanged.
- `file(Path)` reads the named native SMT-LIB 2 file; `Path` must be an atom.

The `print_program/1` predicate validates and prints a character-code list to
the current output stream, preserving its layout without adding a newline.
It does not parse or validate the SMT-LIB 2 syntax and does not invoke a
solver.

The `program/1` predicate generates and prints SMT-LIB 2 source, equivalent
to calling `program/2` followed by `print_program/1`. It accepts the same
source envelopes.


Statements
----------

The `terms(Statements)` source and an object implementing
`smt_problem_protocol` (loaded with `hook(smt_expansion)`) share the same
canonical, ground statement vocabulary, serialized by the `smt_program`
object. Unlike the `asp`, `minizinc`, `sat`, and `souffle` libraries, a
problem object writes these statement constructors directly as ordinary
ground facts; the hook only additionally recognizes the `:- raw(Source)`
and `:- comment(Source)` directives, mapping to the canonical `raw(Source)`
and `comment(Source)` statements listed below:

- `set_logic(Logic)` and `set_option(Keyword, Value)` emit `set-logic` and
	`set-option` commands; `Keyword` uses an underscore in place of the
	SMT-LIB 2 keyword's dash (for example `produce_models` for
	`:produce-models`).
- `declare_sort(Name, Arity)`, `define_sort(Name, Params, Definition)`,
	`declare_const(Name, Sort)`, `declare_fun(Name, ArgSorts, Sort)`, and
	`define_fun(Name, Params, Sort, Body)` emit the corresponding declaration
	and definition commands. `Params` in `define_sort/3` is a list of sort
	parameter names; `Params` in `define_fun/4` is a list of `Name-Sort` pairs.
- `assert(Term)` and `assert_named(Name, Term)` emit `assert` commands;
	`assert_named/2` wraps the term with a `:named` annotation, as required
	to identify it in a later `get-unsat-core` response.
- `push(Count)` and `pop(Count)` emit `push`/`pop` commands.
- `check_sat` and `check_sat_assuming(Literals)` emit `check-sat` commands.
- `get_model`, `get_unsat_core`, and `get_value(Terms)` emit the
	corresponding response-producing commands. When the statement list
	contains `get_model` or (`get_unsat_core` or any `assert_named/2`
	statement) and the caller has not already supplied the corresponding
	`set_option/2` statement, `serialize/2` and `serialize/4` automatically
	prepend `(set-option :produce-models true)` and/or
	`(set-option :produce-unsat-cores true)`, before any other statement, as
	required by solvers that reject these options once `set-logic` has run.
- `comment(Source)` emits a `;`-prefixed comment line, and `raw(Source)` emits
	trusted verbatim SMT-LIB 2 text, both without further validation beyond
	rejecting an embedded newline for `comment/1`. `Source` is an
	atom, a list of character codes, a list of one-character atoms, or a
	backend-native string, normalized into character codes.

Sorts are either an atom (`int`, `bool`, `real`, or a declared sort name),
`bv(Width)` for `(_ BitVec Width)`, `array(Index, Element)` for
`(Array Index Element)`, or a compound term for a parametric declared sort.

Terms mirror SMT-LIB 2 S-expressions structurally: a Logtalk compound term
`f(Arg1, ..., ArgN)` serializes to `(f Arg1 ... ArgN)`, covering core
connectives (`and/N`, `or/N`, `not/1`, `=>/2`, `xor/2`, `ite/3`, `=/2`,
`distinct/N`), arithmetic (`+/N`, `-/1,2`, `*/N`, `//2`, `mod/2`, `</2`,
`<=/2`, `>/2`, `>=/2`, `abs/1`), bit-vector operators (`bvadd/2`, `bvsub/2`,
`bvmul/2`, `bvudiv/2`, `bvurem/2`, `bvand/2`, `bvor/2`, `bvxor/2`, `bvnot/1`,
`bvneg/1`, `bvshl/2`, `bvlshr/2`, `bvashr/2`, `bvult/2`, `bvule/2`,
`bvugt/2`, `bvuge/2`, `bvslt/2`, `bvsle/2`, `concat/2`), array operators
(`select/2`, `store/3`), and any declared function symbol applied to its
arguments. `forall(Bindings, Body)` and `exists(Bindings, Body)`, with
`Bindings` a list of `Name-Sort` pairs, emit quantified terms.
`extract(I, J, Term)`, `zero_extend(N, Term)`, and `sign_extend(N, Term)`
emit the indexed bit-vector operators. Integers and floats are literal
numerals and decimals; a negative number serializes as `(- N)`, as required
by SMT-LIB 2 numeral syntax. `bv(Value, Width)` is a bit-vector literal,
serialized using `#b` binary notation. `string(Text)` is a string literal.
`true` and `false` are the Boolean literals.

This covers a solid common core; theories, functions, or syntax outside this
vocabulary are not generated and require the `raw/1` escape hatch.

The `smt::program/2` predicate returns the SMT-LIB 2 source for these
canonical statements:

	| ?- smt::program(terms([
	         declare_const(x, int), assert(>(x, 0)), check_sat
	     ]), Codes).


Results
-------

A collected result is `smt_result(Status, Model, Properties)`, where `Status`
is `satisfiable`, `unsatisfiable`, or `unknown`; `Model` is `none` or
`bindings([binding(Symbol, Value), ...])`; and `Properties` is a list
including `solver/1`, `raw_status/1`, `process_status/1`, `termination/1`,
`diagnostics/1`, `source_map/1`, `raw_output/1`, `core/1` (`none` or a list
of unsat-core names), and `values/1` (a list of `value(Term, Value)` pairs
from `get-value` responses).

A parsed `Value` is a number, `true`/`false`, a plain symbol atom,
`bv(Value, Width)` for a bit-vector literal, or a compound term mirroring
the solver's reported S-expression. Deeply nested qualified or indexed
identifiers that are not bit-vector literals are represented as an
`apply(Head, Args)` wrapper rather than failing to decode.

Solver output is classified by shape rather than by positional correlation
with the originating statements: a bare `sat`/`unsat`/`unknown` (or Z3's
`timeout`, treated as `unknown`) starts a new result; a model, unsat core,
or `get-value` response is attached to the most recent result. This lets
`solve/2-3` decode every source kind, including a native `file/1` or
`codes/1` source that was never expanded from the canonical statement
vocabulary.

The `status/2`, `model_binding/2`, `model_value/3`, `unsat_core_name/2`, and
`result_property/2` predicates inspect a single collected result without
starting a process. The `print_result/1` and `print_results/1` predicates
print one result, or a list of results separated by a blank line.

The `capability/2` predicate enumerates an advisory, non-blocking
`Solver`-`Feature` table (`quantifiers`, `strings`, `linear_arithmetic`,
`non_linear_arithmetic`, `unsat_cores`, `incremental`, `bit_vectors`,
`arrays`, `floating_point`, and `algebraic_datatypes`); a solver call may
still succeed, fail, or report `unknown` for a feature outside this table,
and conversely may still fail for a feature listed in it.


Incremental solving
--------------------

Unlike the `sat` library, `smt` supports incremental solving within a single
`solve/2-3` call: a source may contain `push`/`pop` commands and more than
one `check-sat`/`check-sat-assuming` command, run by the solver as one
sequential batch script in one process invocation, rather than through a
persistent interactive session. `solve/2-3` returns one collected result per
`check-sat`/`check-sat-assuming` command, in script order.


Limitations
-----------

- Each `solve/2-3` call starts and tears down one solver process; there is no
	persistent session shared across calls, so process start-up cost is paid
	every time, and no state (declarations, assertions, a `push`/`pop` stack)
	survives between separate calls.
- The term and sort vocabulary covers a solid common core (see the
	"Statements" section); quantified algebraic datatypes, strings and
	sequences, non-standard arithmetic (for example transcendental
	functions), optimization or soft constraints, proof production, and
	interpolation are not generated and require the `raw/1` escape hatch.
- `capability/2` is a fixed, advisory table checked against four specific
	solver versions; it is not derived from the selected executable at run
	time, so it can drift from the actual installed version's features in
	either direction.
- Bitwuzla rejects plain linear-arithmetic logics (for example `QF_LIA`)
	outright; shared test fixtures and cross-solver examples use `QF_BV`
	instead. Quantifier support, non-linear arithmetic, and string theories
	otherwise vary significantly across the four backends; an unsupported
	combination typically surfaces as a solver `unknown` result or an
	`(error ...)` response rather than a clean library-level rejection.
- `time_limit/1` is translated into a different underlying CLI flag and
	measurement unit per backend (seconds for Z3 and Yices 2, milliseconds
	for cvc5 and Bitwuzla), so a given value is not directly comparable
	across solvers. cvc5 1.3.0 has been observed to abort the process
	(`SIGABRT`) on a tight `--tlimit` instead of reporting `unknown`;
	`solve/2-3` surfaces this as an error rather than hanging or silently
	returning no results, but the underlying solver crash is not fixed by
	the library.
- A collected model with zero bindings and the absence of a model are both
	represented as `none`; the two cases are not distinguished.
- Parsed values for deeply nested qualified or indexed identifiers (other
	than bit-vector literals) are represented generically as an
	`apply(Head, Args)` wrapper rather than a precise theory-specific term.
- The Windows launcher (`smt_process.ps1`) follows the same design as the
	`sat` library's, but has not been exercised against the four SMT solvers
	on Windows in this library's own test runs.

