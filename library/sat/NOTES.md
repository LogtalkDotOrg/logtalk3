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


`sat`
=====

The `sat` library provides external CLI solving, DIMACS CNF source
generation, an opt-in object expansion hook, and collected SAT solver
result inspection.

This library interacts with the solver CLI using the `process` library.
Therefore, it currently supports ECLiPSe, GNU Prolog, SICStus Prolog,
Shumway Prolog, SWI-Prolog, Trealla Prolog, and XVM.


Requirements
------------

Solving requires one of the supported solver executables, available on PATH
or selected using the `executable(Path)` option. The `solver(Solver)` option
selects which solver is invoked and the default executable resolved for it:

- `kissat` (the default) runs the `kissat` executable. Kissat 4.0.4 or a
	later version is expected, available from its
	[official repository](https://github.com/arminbiere/kissat).
- `cadical` runs the `cadical` executable. CaDiCaL 3.0.1 or a later version
	is expected, available from its
	[official repository](https://github.com/arminbiere/cadical).
- `glucose` runs the `glucose-syrup` executable. Glucose 4.2.1 or a later
	version is expected, available from its
	[official repository](https://github.com/audemard/glucose).
- `z3` runs the `z3` executable. Z3 5.3.3 or a later
	version is expected, available from its
	[official repository](https://github.com/Z3Prover/z3).

Source generation does not require any executable.

On POSIX systems, solving also requires `/bin/sh`, with `mktemp` available on PATH.
On Windows, solving requires PowerShell 7.3 or later (`pwsh` on PATH).

The bundled `sat_process.sh` and `sat_process.ps1` launcher scripts must
remain alongside the library source files.


API documentation
-----------------

Open the [../../apis/library_index.html#sat](../../apis/library_index.html#sat)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(sat(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(sat(tester)).

The tests include source-generation goldens, expansion isolation, output
decoding, result accessors, and solving. Solver integration has been tested
with Kissat 4.0.4, CaDiCaL 3.0.1, Glucose 4.2.1 (the `glucose-syrup`
executable), and Z3 5.3.3. Solver tests are conditional on each solver's
availability and are run for all four supported solvers. A deterministic
shell fixture generates 128 KiB on each output channel to check file-backed
capture beyond typical pipe capacity. Lifecycle checks verify removal of
owned directories after successful execution, child failure, and solver
errors.

Problem fixtures are written as Logtalk objects implementing
`sat_problem_protocol`. The tester loads them with `hook(sat_expansion)`
and solves them using `object(Object)`, checking conversion into DIMACS CNF
source and complete CLI solving rather than passing original native CNF files:

- `test_files/queens.lgt` defines `sat_queens`, a four-queens CNF encoding
  using `queen(Row, Column)` terms as the propositional variable identity, with
  explicit at-least-one and at-most-one clauses for every row, column, and
  diagonal.
- `test_files/test_objects.lgt` defines small satisfiable, unsatisfiable, raw,
  and comment fixtures, plus an unrelated object left untouched by the hook.
- `test_files/pigeon6.cnf` is a native DIMACS pigeonhole fixture (7 pigeons,
  6 holes, unsatisfiable), used to exercise the `file/1` source and, for
  Kissat, CaDiCaL, and Z3, the `conflicts/1` resource limit.


Usage
-----

Load the library, then compile an object defining a CNF problem using
the `sat_expansion` expansion hook. For example:

	| ?- logtalk_load(sat(loader)),
	     logtalk_load(sat('test_files/queens'), [hook(sat_expansion)]).

To inspect the generated DIMACS source without running a solver, use the
`sat::program/1` predicate. For example:

	| ?- sat::program(object(sat_queens)).

Use the `sat::solve/3` predicate to solve the CNF problem. For example, to
solve the four-queens object, collecting the witness assignment in `Result`
(generating the source separately is not required):

	| ?- sat::solve(object(sat_queens), Result).

See the next section for full details on the solver features and options.


Solving
-------

The public source and result predicates are declared in
[sat_protocol.lgt](sat_protocol.lgt). The object data-provider interface
is declared in [sat_problem_protocol.lgt](sat_problem_protocol.lgt).
The serializer, output decoder, and expansion hook document their interfaces
in their respective source files.

The `solve/2` predicate solves a source with default options. The `solve/3`
predicate accepts an explicit final options list. Both predicates return a
ground collected result after subprocess completion and resource cleanup;
UNSATISFIABLE is a successful result, not predicate failure.

The supported options are:

- `solver(Solver)` selects which solver is invoked: `kissat` (the default),
	`cadical`, `glucose`, or `z3`. The selected solver determines the default
	`executable/1` value and how the other options are translated into CLI
	arguments.
- `executable(Path)` selects a single executable, not a shell command line.
	The default depends on `solver/1`: `kissat` for `kissat`, `cadical` for
	`cadical`, `glucose-syrup` for `glucose`, and `z3` for `z3`. Existing
    executable paths are used directly; otherwise the executable is resolved
    through the system PATH.
- `time_limit(Seconds)` selects a nonnegative solver time limit in seconds;
	the default is `0`, meaning unlimited. For Kissat and CaDiCaL this is a
	solver conflict-driven time limit (`--time=`/`-t`); for Glucose it is a
	process CPU time limit (`-cpu-lim=`); for Z3 it is a hard wall-clock
	timeout (`-T:`), so it is not directly comparable across solvers. Kissat
	rejects an explicit zero time limit, so `0` is instead the library's
	unlimited sentinel and omits the underlying argument for all four solvers.
- `conflicts(Limit)` and `decisions(Limit)` select nonnegative solver
	conflict and decision budgets for Kissat (`--conflicts=`/`--decisions=`),
	CaDiCaL (`-c`/`-d`), and, for `conflicts/1` only, Z3 (`sat.max_conflicts=`);
	the default for both is `0`, meaning unlimited, using the same omission
	convention as `time_limit/1`. An explicit zero budget cannot be requested
	through this option. Glucose exposes no equivalent counters: both options
	are validated but have no effect when `solver(glucose)` is selected. Z3
	exposes no decision counter: `decisions/1` is validated but has no effect
	when `solver(z3)` is selected.
- `seed(Seed)` selects a nonnegative random seed; the default is `0`. Glucose
	rejects an explicit zero seed, so `seed(0)` (the shared default) omits the
	underlying `-rnd-seed` argument for Glucose instead of passing it. Z3
	accepts an explicit zero seed and always passes `sat.random_seed=`.
- `witness(Boolean)` selects whether the solver reports a satisfying
	assignment; the default is `true`. For Kissat and CaDiCaL, `false` passes
	`-n`; for Glucose, witness reporting is off by default and `true` passes
	`-model`. Z3 always reports a satisfying assignment in DIMACS mode, so
	`witness/1` is validated but has no effect when `solver(z3)` is selected.
	For the other three solvers, the collected result has no model when the
	witness is suppressed, even when the status is satisfiable.

Shared `options` semantics apply: repeated options are accepted and the first
occurrence wins.

For a compiled `reachability` object, the `solve/2` predicate returns the
collected result using the default solver (Kissat):

	| ?- sat::solve(object(reachability), Result).

To solve the same object with CaDiCaL, Glucose, or Z3 instead:

	| ?- sat::solve(object(reachability), Result, [solver(cadical)]).
	| ?- sat::solve(object(reachability), Result, [solver(glucose)]).
	| ?- sat::solve(object(reachability), Result, [solver(z3)]).

Generated DIMACS source and solver output use UTF-8, though DIMACS CNF itself
is plain ASCII. Existing native source files are left untouched. Temporary
input and output files created for solving are removed before returning the
result.

Shell dependencies that cannot be resolved raise an existence error. Launcher
failures raise `sat_transport(Stage, Status, Details)` errors. Malformed
solver output, fatal solver exit statuses, and inconsistent solver metadata
raise `sat_solver_error(Formal, Status, Diagnostics, SourceMap)` errors,
retaining the captured diagnostics and object source map.


Sources
-------

The `program/2` predicate returns DIMACS CNF character codes without invoking
a solver. Its first argument is a ground source envelope:

- `object(Object)` enumerates the object's ordered `cnf_statement/2` clauses.
  The object must conform to `sat_problem_protocol`.
- `terms(Statements)` serializes an ordered list of canonical, ground statements.
- `codes(Codes)` validates and returns native DIMACS character codes unchanged.
- `file(Path)` reads the named native DIMACS file; `Path` must be an atom.

The `print_program/1` predicate validates and prints a character-code list to
the current output stream, preserving its layout without adding a newline.
It does not parse or validate the DIMACS syntax and does not invoke a solver.

The `program/1` predicate generates and prints DIMACS source, equivalent to
calling `program/2` followed by `print_program/1`. It accepts the same source
envelopes.

CNF variables are opaque ground Prolog terms, not raw DIMACS integers; the
serializer assigns sequential numbers by first occurrence across clause
statements and exposes the resulting table through `sat_program::variables/2`.
Invalid structures raise documented errors.

The `sat_program::serialize/2` predicate serializes canonical statements.
The `sat_program::serialize/4` predicate also accepts a parallel list of
origins and returns a source map; the generated `p cnf` header occupies
line 1, and the source map covers the remaining statement lines. Origins are
`source(File, Start, End)` or `unknown`. Map entries are
`line(Start, End, Origin)`, identifying inclusive, one-based generated line
intervals.


Canonical representation
-------------------------

The serializer accepts these statement constructors:

- `clause(Literals)` emits one DIMACS clause line. `Literals` is an ordered
  list of `pos(Term)` or `neg(Term)`, `Term` being any ground Prolog term used
  as the variable identity; an empty list emits the empty clause (`0`), which
  is immediately unsatisfiable.
- `comment(Codes)` emits one `c ...` comment line; `Codes` must not contain a
  newline.
- `raw(Codes)` preserves trusted native DIMACS text with a separating newline.
  Raw fragments must not redeclare the `p cnf` header, since the library
  always generates exactly one, computed from the declared clauses.

The `sat::program/2` predicate returns the DIMACS source for this canonical
clause:

	| ?- sat::program(terms([clause([pos(p), neg(q)])]), Codes).


Object syntax
-------------

Compile a CNF container with `hook(sat_expansion)` and declare that it
implements `sat_problem_protocol`. For example:

	:- object(reachability,
		implements(sat_problem_protocol)).

		clause([pos(a), neg(b)]).
		clause([neg(a), pos(b), pos(c)]).

	:- end_object.

The hook translates clauses into ordered `cnf_statement/2` data, preserving
clause structure. An ordinary ground fact becomes a unit positive clause; for
example, `edge(a, b).` becomes `clause([pos(edge(a, b))])`. A `clause(Literals)`
fact lists several literals directly, each a bare `Term` (positive), `pos(Term)`,
or `neg(Term)`. CNF containers accept only bodyless facts; rules (clauses with
a body) are rejected, since propositional CNF has no grounding or inference
step of its own. Keep ordinary runtime helpers in separate provider objects
instead of mixing them into CNF containers.

Equivalent directives include `comment/1` and `cnf_raw/1`. Native DIMACS
syntax that is not valid Logtalk syntax belongs in `file/1`, `codes/1`, or
`cnf_raw/1`.


Raw DIMACS source
------------------

Raw DIMACS is solver source text supplied without translation into the
canonical representation. The `codes(Codes)` source envelope passes a
complete program as character codes. For example, convert an atom containing
the source into codes and solve it:

	| ?- atom_codes('p cnf 2 2\n1 2 0\n-1 2 0\n', Codes),
	     sat::solve(codes(Codes), Result).

The `file(Path)` source envelope passes an existing DIMACS file directly to
the solver. For a file named `problem.cnf` containing the program above:

	| ?- sat::solve(file('problem.cnf'), Result).

Neither form requires the `sat_expansion` hook, nor supplies a variable
table: witness literals in the collected result use the plain DIMACS integer
as the term identity instead of a declared symbol.

The `cnf_raw/1` directive embeds raw source among translated clauses in an
object compiled with `hook(sat_expansion)`. Its argument accepts an atom,
a list of character codes, a list of one-character atoms, or a backend-native
string. Use an atom or a double-quoted text for readable source: the hook
handles these representations and normalizes the text into character codes
without changing the backend's `double_quotes` flag. The argument is literal
text, not an executable goal.

The canonical `raw(Codes)` statement provides the same escape hatch within a
`terms(Statements)` source or an object's `cnf_statement/2` data.


Collected results
------------------

The `sat_output::decode/8` predicate consumes the selected solver identifier,
complete captured stdout, raw and normalized process statuses, diagnostic
character codes, a source map, and an optional variable table. It returns a
ground collected result with this representation:

	sat_result(Status, Model, Properties)

`Status` is `satisfiable`, `unsatisfiable`, or `unknown`. `Model` is
`assignment(Literals)` when a witness was reported, or `none` when the result
is not satisfiable or the witness was suppressed with `witness(false)`.
`Literals` is an ordered list of `pos(Term)` or `neg(Term)`, `Term` being
either the declared variable term or, when no variable table is available,
the plain DIMACS integer.

The `literal/2` predicate enumerates the collected model literals. The
`result_property/2` predicate enumerates `status(Status)` and stored
properties:

- `solver(Solver)` retains the selected solver identifier (`kissat`,
  `cadical`, `glucose`, or `z3`); `raw_status(Status)` and
  `process_status(Status)` retain subprocess metadata.
- `termination(Reason)` distinguishes `completed`, `resource_limit`, and
  `interrupted`. None of the three solvers report which configured limit
  (time, conflicts, or decisions) was responsible for an unproven result;
  Glucose has no graceful resource-limit result at all (see Limitations).
- `variables(Variables)` retains the `Term-Number` table, or `none`.
- `variable_count(Count)` retains the declared variable count, the witness
  length when no table is available, or `unknown`.
- `diagnostics(Codes)`, `source_map(Map)`, and `raw_output(Codes)` preserve
  captured stderr, source locations, and the complete captured stdout.

The accessors never start or restart a subprocess on backtracking.

The `print_result/1` predicate prints a collected result to the current
output stream using the shared `s`/`v` status and witness line conventions
followed by Kissat, CaDiCaL, Glucose (with `witness(true)`), and Z3. For
example, solve a program and print its result:

	| ?- atom_codes('p cnf 1 1\n1 0\n', Codes),
	     sat::solve(codes(Codes), Result),
	     sat::print_result(Result).

Printing uses only the collected data and never invokes a solver.


Limitations
-----------

The implementation integrates only with the Kissat, CaDiCaL, Glucose, and Z3
CLIs. It does not use a foreign or incremental API, and does not support
proof logging. Each solver's extensive tuning options are not exposed; only
the solving-limit and witness options documented above are supported. Solver
stdin is closed, so there is no interactive input channel.

The four solvers are not fully interchangeable:

- CaDiCaL reports an unproven, resource-limited result with a bare
	`c UNKNOWN` comment line instead of Kissat's, Glucose's, and Z3's
	`s UNKNOWN` status line; the decoder accepts both, reporting
	`unknown`/`resource_limit` either way.
- Glucose exposes no conflict or decision budget: `conflicts/1` and
	`decisions/1` are validated but silently have no effect when
	`solver(glucose)` is selected. Reaching Glucose's CPU time limit
	(`time_limit/1`) does not produce a graceful `unknown` result either;
	Glucose prints `*** INTERRUPTED ***` and exits with status 1, which this
	library reports as a `sat_solver_error`, the same as any other abnormal
	exit.
- Glucose's witness defaults to off; `witness(true)` (the shared default)
	passes `-model`, the opposite polarity of Kissat and CaDiCaL, where
	`witness(false)` passes `-n`.
- Glucose rejects an explicit zero random seed, so `seed(0)` (the shared
	default) omits the underlying `-rnd-seed` argument instead of passing it,
	unlike Kissat, CaDiCaL, and Z3, which treat seed `0` as a normal explicit
	value.
- Z3 exits with status `0` regardless of whether the result is satisfiable,
	unsatisfiable, or unknown, unlike the SAT competition exit code convention
	(`10`/`20`/`0`) followed by the other three solvers. The decoder validates
	Z3's exit status solver-specifically, reading `completed` versus
	`resource_limit` from the status label instead of the exit code.
- Z3 exposes no decision counter: `decisions/1` is validated but has no
	effect when `solver(z3)` is selected.
- Z3 always reports a satisfying assignment for a satisfiable result in
	DIMACS mode: `witness/1` is validated but has no effect when `solver(z3)`
	is selected.
- Z3's DIMACS parser is lenient and does not reliably raise an error for
	malformed clause content; it only fails, with a nonzero exit status and no
	`s` status line, when the input file itself cannot be opened.

The canonical representation supports ground propositional clauses only;
there is no grounding, quantification, or clausification of arbitrary
propositional formulas. Native syntax outside this representation requires
`file/1`, `codes/1`, or `cnf_raw/1`.

Source inspection does not parse, validate DIMACS semantics beyond statement
structure, or resolve includes (DIMACS CNF has none). Results and diagnostics
are collected in memory, not streamed live.

The `time_limit/1` option is a solver time limit, not a hard end-to-end
timeout. Cleanup is not guaranteed after non-catchable aborts or failed OS
cleanup operations. An allocation interrupted before its returned path can be
recovered may leave an unidentified directory; paths with uncertain ownership
are not deleted. On Windows, directory permissions inherit the temporary
base's ACL, so use a protected, user-owned temporary base.

Compatibility with supported Kissat, CaDiCaL, Glucose, and Z3 versions
other than 4.0.4, 3.0.1, 4.2.1, and 5.3.3, respectively, has not been
runtime-verified.
