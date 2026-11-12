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


`problog`
=========

The `problog` library provides exact probabilistic inference using the
[ProbLog](https://dtai.cs.kuleuven.be/problog/) CLI, source generation,
opt-in expansion of problem objects, and collected query probabilities.


Requirements
------------

Inference requires ProbLog 2.3.0 (`problog` on PATH, or an executable selected
using `executable/1`). Optional knowledge compilation backends may require
additional dependencies; errors are reported rather than silently selecting
another backend.

The library uses `process`, supporting ECLiPSe, GNU Prolog, SICStus Prolog,
Shumway Prolog, SWI-Prolog, Trealla Prolog, and XVM. POSIX execution requires
`/bin/sh` and `mktemp`; Windows requires PowerShell 7.3 or later (`pwsh`).
Keep the bundled launcher scripts in the `scripts` subdirectory.


API documentation
-----------------

Open [../../apis/library_index.html#problog](../../apis/library_index.html#problog)
in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(problog(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(problog(tester)).

CLI tests are conditional on finding `problog`. Source-generation and results
decoding tests do not require it.

The test fixtures adapt examples from
[ProbLog's sample directory](https://github.com/ML-KULeuven/problog/tree/master/test/sample):
`coins.lgt` checks `some_heads = 0.8`, and `coins_evidence.lgt` checks
`some_heads = 0.6` after conditioning on evidence. `annotated.lgt` exercises
annotated disjunctions and variable sharing. `diagnostics.lgt` uses ProbLog's
`debugprint/1` built-in to check that the known diagnostic message is captured
without depending on the overall output length or formatting. Native file-input
tests use library-specific fixtures.


Problem sources
---------------

A ProbLog problem can be represented in several ways:

- `file(Path)`: the argument must be the path of a native ProbLog file.
- `codes(Codes)`: a list of charactert codes suppliing a native source.
- `terms(Statements)`: a list of canonical ground terms representing the problem.
- `object(Object)`: the argument must be an object containing the problem.

The main library predicates take a problem source as an input argument.


Usage
-----

To solve a ProbLog problem, use the `solve/2` or the `solve/3` predicates. The
`solve/2` predicate uses default options while the `solve/3` predicate uses
the given options (see "Options" section below for details). For example:

	| ?- problog::solve(
            terms(
                [probabilistic_fact(0.5, function(heads, [])),
	            query(function(heads, []))
	        ]),
            Result
         ),
         problog::probability(Result, heads, Probability).

    Probability = 0.5
    yes

All declared queries are collected automatically, including ground instances
of variable queries. No Souffle-style output column specification is needed.
Zero-probability queries are retained. A successful model without queries
returns an empty collection.

Other useful predicates are `program(Source, Codes)`, which generates a codes
representation without invoking ProbLog; `program(Source)`, which prints the
program represented by the source argument; `print_program(Codes)`, which prints
a program represented as a list of character codes (no extra newline is added);
and `print_result(Result)`, which prints collected queries and probabilities.

An example of using the native syntax in an object:

	:- object(coins,
        implements(problog_problem_protocol)).

	    0.5::heads1.
	    0.6::heads2.
	
        someHeads :- heads1.
	    someHeads :- heads2.
	
        query(someHeads).

	:- end_object.

Load the file defining the object using the `problog_expansion` hook:

    | ?- logtalk_load(File, [hook(problog_expansion)]).
    
After, solve the problem using e.g. the `solve/2` predicate:

    | ?- problog::solve(object(coins), Result).
    
    Result = ...
    
The hook also recognizes probabilistic rules, annotated disjunctions, evidence,
deterministic rules, supported directives, plus `comment(Source)` and
`raw(Source)` directives. The hook applies only to objects that implement
`problog_problem_protocol`.


Statement vocabulary
---------------------

The `terms/1` source and expanded objects share a ground canonical AST.
Valid statement constructors are: `fact(Head)`, `rule(Head, Body)`,
`probabilistic_fact(P, Head)`, `probabilistic_rule(P, Head, Body)`,
`annotated_disjunction(Alternatives, Body)`, `query(Goal)`,
`evidence(Goal, Bool)`, `directive(Goal)`, `comment(Codes)`, and `raw(Codes)`.
An annotated disjunction uses `P-Head` alternatives whose probabilities sum
to at most one; a residual probability mass is allowed. Use `true` as the
body for an unconditional annotated disjunction.

Heads and positive goals use `function(Name, Arguments)`, including zero-arity
predicates as `function(Name, [])`. Goal constructors additionally include
`true`, `conjunction(Goals)`, `disjunction(Goals)`, `negation(Goal)`, and
`comparison(Operator, Left, Right)`. Comparisons support `=`, `==`, `\=`,
`\==`, `<`, `=<`, `>`, and `>=`; other built-ins use `function/2`.

Term constructors are `atom(Atom)`, `number(Number)`, `variable(Name)`,
`anonymous`, nested `function(Name, Arguments)`, `list(Elements)` for proper
lists, `list(Elements, Tail)` for improper lists, and
`arithmetic(Operator, Arguments)`. Arithmetic supports binary `+`, `-`, `*`,
`/`, `//`, `mod`, `**`, and unary `+`/`-`. Use the same `variable(Name)`
within a statement for sharing, and `anonymous` for independent `_` variables.

The expansion hook translates ordinary clause bodies and variable sharing
into these constructors. It recognizes `query(Goal)` and `evidence(Goal)`
or `evidence(Goal, Bool)` as facts or supported directive forms. Source
origins are `unknown` or `source(File, Start, End)`; the serializer's
`serialize/4` returns `line(Start, End, Origin)` mappings.


Options
-------

Repeated options use the first occurrence, following `options` semantics.

| Option | Default | Meaning |
|---|---|---|
| `executable(Path)` | `problog` | One executable, not a shell command line |
| `knowledge(Backend)` | `auto` | Backend selection: `auto`, `sdd`, `sddx`, `bdd`, `nnf`, `ddnnf`, `fsdd`, or `fbdd` |
| `timeout(Seconds)` | `none` | Positive integer inference time limit |
| `compile_timeout(Seconds)` | `none` | Positive integer compilation time limit |
| `logspace(Bool)` | `true` | Log-space or normal-space evaluation |
| `propagate_evidence(Bool)` | `true` | Enable or disable evidence propagation |
| `libraries(Paths)` | `[]` | Additional ProbLog library search paths |
| `arguments(Atoms)` | `[]` | Arguments exposed by ProbLog's `cmd_args/1` |

The `auto` option value uses ProbLog's installed default exact backend. No
approximate `kbest`, symbolic, convergence, alternative-mode, arbitrary CLI,
or output-format options are exposed. A time limit is a solver error, not a
partial successful probability collection.


Results and errors
------------------

The `probability(Result, Query, Probability)` predicate enumerates ground
ProbLog queries, represented as native Prolog terms, together with finite
probabilities in `[0,1]`. Ordering follows the CLI output and is not
guaranteed to be sorted. The `result_property/2` predicate can be used to
enumerate `raw_status/1`, `process_status/1`, and `diagnostics/1` (stderr
character codes) result properties. Internally, results have the form:

	problog_result([probability(Query, Probability), ...], Properties)

The library reads the complete CLI `--format prolog` output as data, never as
executable clauses. It handles quoted/nested/list terms and normalizes Python
integer-mantissa scientific notation for portability.

Nonzero exits, inconsistent evidence, undefined predicates, and malformed
output raise `problog_solver_error(Formal, Status, Diagnostics, SourceMap)`
exceptions. The formal error retains captured stdout too, since ProbLog can
print error messages there. Object source maps associate generated line
ranges with original source locations. Launcher errors result in
`problog_transport(Stage, Status, Details)` exceptions.

Generated source and captures use UTF-8. Only owned temporary files and
directories are removed, before returning results. Native files and the
caller's working directory are preserved. Generated models reside in an
owned temporary directory: use absolute imports or `libraries/1` rather
than assuming a generated model lives beside a caller's source file.


Limitations
-----------

Sampling, learning, symbolic evaluation, MPE/MAP, decision theory, and
explanation modes are not currently implemented.

All sources, including raw text and native files, are trusted programs:
ProbLog can perform native I/O and imports. The subprocess boundary is not
a sandbox. Query representations are subject to ProbLog's Prolog output
format; exotic constants whose printed form is ambiguous should be avoided.
