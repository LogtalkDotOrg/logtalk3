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


`mutation_testing`
==================

This tool provides mutation testing support for loaded Logtalk entities.
It's still under development and breaking changes should be expected.


API documentation
-----------------

This tool API documentation is available at:

[../../apis/library_index.html#mutation-testing](../../apis/library_index.html#mutation-testing)


Loading
-------

This tool can be loaded using the query:

	| ?- logtalk_load(mutation_testing(loader)).


Testing
-------

To test this tool, load the `tester.lgt` file:

	| ?- logtalk_load(mutation_testing(tester)).


Features
--------

- Deterministic mutant discovery per entity predicate/non-terminal and mutator.
- Configurable mutator sets and campaign size limits.
- Deterministic sampled execution (`sampling(all|count(N)|rate(R))` plus `seed/1`).
- Mutation score computation (killed versus survived mutants).
- Mutation generation guided by code coverage stats.
- Threshold gating suitable for CI/CD checks.
- Exporting of mutation campaign reports in plain text and JSON formats.


Default mutators
----------------

The default mutators are:

- `fail_insertion`  
	Inserts failure in the selected predicate/non-terminal rule bodies.

- `body_goal_negation`  
	Negates the selected predicate/non-terminal clause behavior.

- `relational_operator_replacement`  
	Replaces a relational operator (e.g. `>`, `<`, `@=<`, `==`) with a complementary alternative.

- `arithmetic_operator_replacement`  
	Replaces an arithmetic operator in selected expressions.

- `truth_literal_flip`  
	Flips truth literals (`true` <-> `fail`) in selected goals.

- `head_arguments_mutation`  
	Mutates one compile-time bound head argument using `type::mutation/3`.

- `head_arguments_reordering`  
	Reorders head arguments (swapping the first two arguments).

- `clauses_reordering`  
	Reorders predicate/non-terminal clauses/rules by swapping a clause with its successor.


Usage
-----

Mutation testing **requires** an existing set of tests for the code being
mutated. These tests are used to verify if they are able to kill the
generated mutants or if they are too weak allowing the mutants to survive.

By default, the tool looks for the tests driver file (usually, `tester.lgt`)
in the directory of the entity, library, or directory being tested. If the
tests driver file is not found there, the tool looks into the startup
directory of the Logtalk process.

Both the name of the tests driver file and its directory can be set using
the `tester_file_name/1` and `tester_directory/1` options if the defaults
don't work in your particular case.

In this tool documentation, **campaign predicates** refers to the following
public predicates:

- `entity/1-2`
- `predicate/2-3`
- `library/1-2`
- `directory/1-2`

These predicates run mutants and report results in one step.

Run mutation testing for one entity using defaults:

	| ?- mutation_testing::entity(my_object).

List generated mutants for one entity:

	| ?- mutation_testing::entity_mutants(my_object, Mutants).

Run with custom options:

	| ?- mutation_testing::entity(my_object, [
		mutators([fail_insertion, body_goal_negation]),
		max_mutations_per_mutator(5),
		sampling(count(100)),
		seed(20260303),
		max_mutators(100),
		threshold(60.0),
		verbose(true),
        print_mutation(true)
	]).

Run campaigns for all loaded entities from a specific library:

	| ?- mutation_testing::library(core).

Run campaigns for all loaded entities from a specific directory:

	| ?- mutation_testing::directory('/path/to/sources').

Pretty-print a report term using the default text format:

	| ?- mutation_testing::report_entity(my_object, Report, []),
	     mutation_testing::format_report(Report).

Suppress report formatting output for campaign predicates:

	| ?- mutation_testing::entity(my_object, [format(none)]).

Execution uses `lgtunit` test sets only. When using `timeout(Timeout)`, timeout
handling is best-effort.

When running mutation campaigns inside another `lgtunit` test run, per-mutant
`killed` versus `survived` classification may be affected by nested test-run
message and event handling. Campaign summary accounting remains deterministic.


Limitations
-----------

- Mutation points are currently mostly clause-level (one occurrence per clause per mutator), except `head_arguments_mutation` which creates one occurrence per compile-time bound head argument with supported type; internal expression-level candidate enumeration is not implemented.
- Equivalent mutant detection and duplicate mutant pruning are not implemented.
- Built-in mutation apply/revert currently targets loaded source entities (objects/categories), not protocols.
- Nested `lgtunit` runs may affect per-mutant `killed` vs `survived` status classification.


Options
-------

- `include_entities(Entities)`  
	List of loaded entities to include (default `[]` meaning all).

- `exclude_entities(Entities)`  
	List of loaded entities to exclude (default `[]`).

- `max_mutators(Max)`  
	Maximum number of discovered mutators to use when `mutators/1` is not explicitly provided (`all` or positive integer; default `all`).

- `max_mutations_per_mutator(Max)`  
	Maximum number of mutations generated per mutator and predicate/non-terminal (`all` or positive integer; default `5`).

- `mutators(Mutators)`  
	Names of the mutators to use. When set to `[]` (default), mutators are auto-discovered and optionally limited by `max_mutators/1`.

- `sampling(Sampling)`  
	Mutant selection strategy (`all`, `count(N)`, or `rate(R)` where `0.0 =< R =< 1.0`; default `all`). See below for full details.

- `seed(Seed)`  
	Pseudo-random generator seed for reproducible sampling (default `123456789`; ignored when `sampling(all)`).

- `timeout(Timeout)`  
	Maximum time in seconds for each mutant execution (positive number; default `300`). Timeout handling is best-effort.

- `threshold(Threshold)`  
	Minimum mutation score in range `0.0..100.0` (default `0.0`).

- `verbose(Boolean)`  
	Print per-mutant results (default `false`).

- `format(Format)`
	Controls report formatting output (`none`, `text`, or `json`; default `text`).
	When set to `text` or `json`, a report file is generated automatically for campaign predicates.
	The `json` format implements the Stryker Mutation Testing Framework report format:
	https://stryker-mutator.io/

- `report_file_name(FileName)`
	Report output file base name or path without extension (atom; default `mutation_test_report`).
	The extension is inferred from `format/1` (`text` -> `.txt`, `json` -> `.json`).
	When not absolute, the file is saved in the tests driver directory.

- `print_mutation(Boolean)`  
	When `true`, prints original and mutated terms with source location for mutators. This option is only effective when `verbose(true)` (default `false`).

- `tester_file_name(Tester)`  
	Name of the tests driver file for the code being tested (default `tester.lgt`).

- `tester_directory(Directory)`  
	Full path to the directory containing the tests driver file for the code being tested (no default).

Sampling semantics
------------------

The `sampling(Sampling)` option controls **which generated mutants are actually
executed** in a mutation campaign. It is a post-generation selection step:

1. Generate candidate mutants for the selected entities/predicates/mutators.
2. Apply `sampling(...)` to select a subset (or all).
3. Execute only the selected mutants.

Because of this design, `sampling(...)` affects campaign predicates
(`entity/2`, `predicate/3`, `library/2`, `directory/2`).

The `format(Format)` option affects only campaign predicates that execute and
report in one step (`entity/2`, `predicate/3`, `library/2`, `directory/2`).
It does not affect the `*_mutants` predicates or the `report_*` predicates,
which only compute and return terms.

For campaign predicates, a report file is written automatically unless
`format(none)` is used.

For `report_library/3` and `report_directory/3`, call `format_report/2-3`
explicitly if you want to persist the computed report term. A single
`format_report(..., Report)` call always generates a single output document
(including JSON).

The `mutants/2-3` predicates are still useful for inspecting the generated
deterministic mutant list itself; they are not execution/reporting predicates.

### Sampling modes

- `sampling(all)`
	Execute all generated mutants.

- `sampling(count(N))`
	Shuffle the generated mutant list and execute up to `N` mutants.
	If `N` is greater than the total number of generated mutants, all mutants
	are executed.

- `sampling(rate(R))`
	Compute `round(R * TotalGeneratedMutants)`, shuffle the mutant list, and
	execute that many mutants.
	If the computed value is greater than the total number of generated mutants,
	all mutants are executed.

The randomization used by `count/1` and `rate/1` is controlled by the `seed/1`
option, allowing reproducible selection.

### Sampling usage in practice

- **Control campaign cost**
	Run a representative subset when the full mutant set is large.

- **Trade off speed vs. confidence**
	Use smaller samples for quick feedback and larger/full runs for stronger
	confidence before release.

- **Enable reproducible sampling**
	With a fixed `seed/1`, repeated runs with the same generated mutant set and
	same sampling options select the same subset. When all mutants are executed,
    (using `sampling(all)`) changing the seed has no effect on selection.

- **Complement other limits**
	`max_mutators/1` and `max_mutations_per_mutator/1` constrain mutant
	generation; `sampling/1` controls execution selection after generation.


Defining new mutators
---------------------

Define a new mutator as parametric object implementing the `expanding` protocol
and importing the `mutator_common` category:

	:- object(my_mutator(_Entity_, _Predicate_, _ClauseIndex_, _Occurrence_, _PrintMutation_),
		implements(expanding),
        imports(mutator_common)).

        % mutate code when loading it using this object as a hook object
	    term_expansion(Term, Mutation) :-
            ...

        % compute a term mutation; generate multiple mutations by backtracking
    	mutation(Term, Mutation) :-
            ...

        % reset any required internal state
    	reset :-
            ...

    :- end_object.

New mutators are found by dynamically looking for objects that conform to
the `mutator_protocol` protocol (which is implemented by the imported
`mutator_common` category).

For implementation examples, see the default mutator hooks:

- [mutators/fail_insertion_hook.lgt](mutators/fail_insertion_hook.lgt)
- [mutators/body_goal_negation_hook.lgt](mutators/body_goal_negation_hook.lgt)
- [mutators/relational_operator_replacement_hook.lgt](mutators/relational_operator_replacement_hook.lgt)
- [mutators/arithmetic_operator_replacement_hook.lgt](mutators/arithmetic_operator_replacement_hook.lgt)
- [mutators/truth_literal_flip_hook.lgt](mutators/truth_literal_flip_hook.lgt)
- [mutators/head_arguments_mutation_hook.lgt](mutators/head_arguments_mutation_hook.lgt)
- [mutators/head_arguments_reordering_hook.lgt](mutators/head_arguments_reordering_hook.lgt)
- [mutators/clauses_reordering_hook.lgt](mutators/clauses_reordering_hook.lgt)
