________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

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


`lgtunit`
=========

The `lgtunit` tool provides testing support for Logtalk. It can also be used
for testing plain Prolog code and Prolog module code.

This tool is inspired by the xUnit frameworks architecture and by the works of
Joachim Schimpf (ECLiPSe library `test_util`) and Jan Wielemaker (SWI-Prolog
`plunit` package).

Tests are defined in objects, which represent a _test set_ or _test suite_.
In simple cases, we usually define a single object containing the tests. But
it is also possible to use parametric test objects or multiple objects
defining parametrizable tests or test subsets for testing more complex units
and facilitate tests maintenance. Parametric test objects are specially
useful to test multiple implementations of the same protocol using a single
set of tests by passing the implementation object as a parameter value.


Main files
----------

The `lgtunit.lgt` source file implements a framework for defining and running
unit tests in Logtalk. The `lgtunit_messages.lgt` source file defines the
default translations for the messages printed when running unit tests. These
messages can be intercepted to customize output, e.g. to make it less verbose,
or for integration with e.g. GUI IDEs and continuous integration servers.

Other files part of this tool provide support for alternative output formats
of test results and are discussed below.


API documentation
-----------------

This tool API documentation is available at:

[../../docs/library_index.html#lgtunit](../../docs/library_index.html#lgtunit)


Loading
-------

This tool can be loaded using the query:

	| ?- logtalk_load(lgtunit(loader)).


Writing and loading tests
-------------------------

In order to write your own unit tests, define objects extending the `lgtunit`
object. You may start by copying the `tests-sample.lgt` file (at the root of
the Logtalk distribution) to a `tests.lgt` file in your project directory and
edit it to add your tests:

	:- object(tests,
		extends(lgtunit)).

		% test definitions
		...

	:- end_object.

The section on [*test dialects*](#test-dialects) below describes in detail
how to write tests. See the `tests` top directory for examples of actual unit
tests. Other sources of examples are the `library` and `examples` directories.

The tests must be term-expanded by the `lgtunit` object by compiling the source
files defining the test objects using the option `hook(lgtunit)`. For example:

	| ?- logtalk_load(tests, [hook(lgtunit)]).

As the term-expansion mechanism applies to all the contents of a source file,
the source files defining the test objects should preferably not contain entities
other than the test objects. Additional code necessary for the tests should go to
separate files.

The `tester-sample.lgt` file (at the root of the Logtalk distribution)
exemplifies how to compile and load `lgtunit` tool, the source code under
testing, the unit tests, and for automatically run all the tests after loading.
You may copy this file to a `tester.lgt` file in your project directory and edit
it to load your project and tests files.

Debugged test sets should preferably be compiled in optimal mode, specially
when containing deterministic tests and when using the utility benchmarking
predicates.


Running unit tests
------------------

Assuming that your test object is named `tests`, after compiling and loading its
source file, you can run the tests by typing:

	| ?- tests::run.

Usually, this goal is called automatically from an `initialization/1` directive
in a `tester.lgt` loader file. You can also run a single test (or a list of tests)
using the `run/1` predicate:

	| ?- tests::run(test_identifier).

When testing complex _units_, it is often desirable to split the tests between
several test objects or using parametric test objects to be able to run the same
tests using different parameters (e.g. different data sets). In this case, you
can run all test subsets using the goal:

	| ?- lgtunit::run_test_sets([...])

where the `run_test_sets/1` predicate argument is a list of test object
identifiers. This predicate makes possible to get a single code coverage
report that takes into account all the tests.

It's also possible to automatically run loaded tests when using the `make`
tool by calling the goal that runs the tests from a definition of the hook
predicate `logtalk_make_target_action/1`. For example, by adding to the
tests `tester.lgt` driver file the following code:

	% integrate the tests with logtalk_make/1
	:- multifile(logtalk_make_target_action/1).
	:- dynamic(logtalk_make_target_action/1).

	logtalk_make_target_action(check) :-
		tests::run.

Alternatively, you can define the predicate `make/1` inside the test set
object. For example:

	make(check).

This clause will cause all tests to be run when calling the `logtalk_make/1`
predicate with the target `check` (or its top-level shortcut, `{?}`). The
other possible target is `all` (with top-level shortcut `{*}`).

Note that you can have multiple test driver files. For example, one driver
file that runs the tests collecting code coverage data and a quicker driver
file that skips code coverage and compiles the code to be tested in optimized
mode.


Parametric test objects
-----------------------

Parameterized unit tests can be easily defined by using parametric test
objects. A typical example is testing multiple implementations of the
same protocol. In this case, we can use a parameter to pass the specific
implementation being tested. For example, assume that we want to run the
same set of tests for the library `randomp` protocol. We can write:

	:- object(tests(_RandomObject_),
	    extends(lgtunit)).
	
		:- uses(_RandomObject_, [
		    random/1, between/3, member/2,
			...
		]).

		test(between_3_in_interval) :-
			between(1, 10, Random),
			1 =< Random, Random =< 10.

		...
	
	:- end_object.

We can then test a specific implementation by instantiating the parameter.
For example:

	| ?- tests(fast_random)::run.

Or use the `lgtunit::run_test_sets/1` predicate to test all the implementations:

	| ?- lgtunit::run_test_sets([
			tests(backend_random),
			tests(fast_random),
			tests(random)
		]).


Test dialects
-------------

Multiple test _dialects_ are supported by default. See the next section on how
to define your own test dialects. In all dialects, a callable term, usually an
atom, is used to uniquely identify a test. This simplifies reporting failed tests
and running tests selectively. An error message is printed if duplicated test
identifiers are found. These errors must be corrected otherwise the reported
test results can be misleading. Ideally, tests should have descriptive names
that clearly state the purpose of the test and what is being tested.

Unit tests can be written using any of the following predefined dialects:

	test(Test) :- Goal.

This is the most simple dialect, allowing the specification of tests that
are expected to succeed. The argument of the `test/1` predicate is the test
identifier, which must be unique. A more versatile dialect is:

	succeeds(Test) :- Goal.
	deterministic(Test) :- Goal.
	fails(Test) :- Goal.
	throws(Test, Ball) :- Goal.
	throws(Test, Balls) :- Goal.

This is a straightforward dialect. For `succeeds/1` tests, `Goal` is
expected to succeed. For `deterministic/1` tests, `Goal` is expected to
succeed once without leaving a choice-point. For `fails/1` tests, `Goal`
is expected to fail. For `throws/2` tests, `Goal` is expected to throw
the exception term `Ball` or one of the exception terms in the list
`Balls`. The specified exception must subsume the generated exception
for the test to succeed.

An alternative test dialect that can be used with more expressive power is:

	test(Test, Outcome) :- Goal.

The possible values of the outcome argument are:

- `true`  
	the test is expected to succeed

- `true(Assertion)`  
	the test is expected to succeed and satisfy the `Assertion` goal

- `deterministic`  
	the test is expected to succeed once without leaving a choice-point

- `deterministic(Assertion)`  
	the test is expected to succeed once without leaving a choice-point and satisfy the `Assertion` goal

- `fail`  
	the test is expected to fail

- `false`  
	the test is expected to fail

- `error(Error)`  
	the test is expected to throw the exception term `error(Error, _)`

- `errors(Errors)`  
	the test is expected to throw an exception term `error(Error, _)` where `Error` is an element of the list `Errors`

- `ball(Ball)`  
	the test is expected to throw the exception term `Ball`

- `balls(Balls)`  
	the test is expected to throw an exception term `Ball` where `Ball` is an element of the list `Balls`

In the case of the `true(Assertion)` and `deterministic(Assertion)` outcomes,
a message that includes the assertion goal is printed for assertion failures
and errors to help to debug failed unit tests. Note that this message is only
printed when the test goal succeeds as its failure will prevent the assertion
goal from being called. This allows distinguishing between test goal failure
and assertion failure.

Some tests may require individual condition, setup, or cleanup goals. In this
case, the following alternative test dialect can be used:

	test(Test, Outcome, Options) :- Goal.

The currently supported options are (non-recognized options are ignored):

- `condition(Goal)`  
	condition for deciding if the test should be run or skipped (default goal is `true`)

- `setup(Goal)`  
	setup goal for the test (default goal is `true`)

- `cleanup(Goal)`  
	cleanup goal for the test (default goal is `true`)

- `note(Term)`  
	annotation to print (between parenthesis by default) after the test result (default is `''`); the annotation term can share variables with the test goal, which can be used to pass additional information about the test result

Also supported is QuickCheck testing where random tests are automatically
generated and run given a predicate mode template with type information for
each argument (see the section below for more details):

	quick_check(Test, Template, Options).
	quick_check(Test, Template).

The valid options are the same as for the `test/3` dialect plus a `n/1` option
to specify the number of random tests that will be generated/run (default is
100) and a `s/1` option to specify the maximum number of shrink operations
(default is 64).

For examples of how to write unit tests, check the `tests` folder or the
`testing` example in the `examples` folder in the Logtalk distribution.
Most of the provided examples also include unit tests, some of them with
code coverage.


User-defined test dialects
--------------------------

Additional test dialects can be easily defined by extending the `lgtunit`
object and by term-expanding the new dialect into one of the default dialects.
As an example, suppose that you want a dialect where you can simply write a
file with clauses using the format:

	test_identifier :-
		test_goal.

First, we define an expansion for this file into a test object:

	:- object(simple_dialect,
		implements(expanding)).
	
		term_expansion(begin_of_file, [(:- object(tests,extends(lgtunit)))]).
		term_expansion((Head :- Body), [test(Head) :- Body]).
		term_expansion(end_of_file, [(:- end_object)]).
	
	:- end_object.

Then we can use this hook object to expand and run tests written in this
dialect by using a `tester.lgt` driver file with contents such as:

	:- initialization((
		set_logtalk_flag(report, warnings),
		logtalk_load(lgtunit(loader)),
		logtalk_load(library(hook_flows_loader)),
		logtalk_load(simple_dialect),
		logtalk_load(tests, [hook(hook_pipeline([simple_dialect,lgtunit]))]),
		tests::run
	)).

The hook pipeline first applies our `simple_dialect` expansion followed by
the default `lgtunit` expansion. This solution allows other hook objects
(e.g. required by the code being tested) to also be used by updating the
pipeline.


QuickCheck
----------

QuickCheck was originally developed for Haskell. Implementations for several
other programming languages soon followed. The idea is to express properties
that predicates must comply with and automatically generate tests for those
properties. The `lgtunit` tool supports both `quick_check/2-3` test dialects,
as described above, and `quick_check/1-3` public predicates for interactive
use:

	quick_check(Template, Result, Options).
	quick_check(Template, Options).
	quick_check(Template).

The `quick_check/3` predicate returns results in reified form:

- `passed`,
- `failed(Goal)` with Goal being the random test that failed
- `error(Error, Template)` or `error(Error, Goal)`

The other two predicates print the test results. The template can be a `::/2`,
`<</2`, or `:/2` qualified callable term. When the template is an unqualified
callable term, it will be used to construct a goal to be called in the context
of the *sender* using the `<</2` debugging control construct. A simple example
by passing a template that will trigger a failed test (as the `random::random/1`
predicate always returns non-negative floats):

	| ?- lgtunit::quick_check(random::random(-negative_float)).
	*     quick check test failure (at test 1 after 1 shrink):
	*       random::random(0.09230089279334841)
	no

Another example using a Prolog module predicate:

	| ?- lgtunit::quick_check(
			pairs:pairs_keys_values(
				+list(pair(atom,integer)),
				-list(atom),
				-list(integer)
			)
		).
	% 100 random tests passed
	yes

Properties are expressed using predicates. The QuickCheck test dialects and
predicates take as argument the mode template for a property, generate random
values for each input argument based on the type information, and check each
output argument. For common types, the implementation tries first common edge
cases (e.g. empty atom, empty list, or zero) before generating arbitrary
values. When the output arguments check fails, the QuickCheck implementation
tries (by default) up to 64 shrink operations of the counter-example to report
a simpler case to help debugging the failed test. Edge cases, generating of
arbitrary terms, and shrinking terms make use of the library `arbitrary`
category via the `type` object (both entities can be extended by the user by
defining clauses for multifile predicates).

The mode template syntax is the same used in the `info/2` predicate directives
with an additional notation, `{}/1`, for passing argument values as-is instead
of generating random values for these arguments. For example, assume that we
want to verify the `type::valid/2` predicate, which takes as first argument a
type. Randomly generating random types would be cumbersome at best but the
main problem is that we need to generate random values for the second argument
according to the first argument. Using the `{}/1` notation we can solve this
problem for any specific type, e.g. integer, by writing:

	| ?- lgtunit::quick_check(type::valid({integer}, +integer)).

We can also test all (ground, i.e. non-parametric) types with arbitrary
value generators by writing:

	| ?- forall(
			(type::type(Type), ground(Type), type::arbitrary(Type)),
			lgtunit::quick_check(type::valid({Type}, +Type))
		 ).

You can find the list of the basic supported types for using in the template
in the API documentation for the library entities `type` and `arbitrary`. Note
that other library entities, including third-party or your own, can contribute
with additional type definitions as both `type` and `arbitrary` entities are
user extensible by defining clauses for their multifile predicates.

An optional argument, `n/1`, allows the specification of the number of random
tests that will be generated and run. The maximum number of shrink operations
can be specified using the option `s/1`.

The user can define new types to use in the property mode templates to use
with its QuickCheck tests by defining clauses for the `arbitrary` library
category multifile predicates.

Note that is possible to complement the random tests performed by QuickCheck
by defining a surrogate predicate that calls the predicate being tested and
performs additional checks on the generated random values.


Skipping tests
--------------

A test object can define the `condition/0` predicate (which defaults to
`true`) to test if some necessary condition for running the tests holds. The
tests are skipped if the call to this predicate fails or generates an error.

Individual tests that for some reason should be unconditionally skipped can
have the test clause head prefixed with the `(-)/1` operator. For example:

	- test(not_yet_ready) :-
		...

The number of skipped tests is reported together with the numbers of passed
and failed tests. To skip a test depending on some condition, use the `test/3`
dialect and the `condition/1` option. For example:

	test(test_id, true, [condition(current_prolog_flag(bounded,true))) :-
		...

The conditional compilation directives can also be used in alternative but note
that in this case there will be no report on the number of skipped tests.


Checking test goal results
--------------------------

Checking test goal results can be performed using the `test/2-3` dialect `true/1`
and `deterministic/1` assertions. For example:

	test(compare_3_order_less, deterministic(Order == (<))) :-
		compare(Order, 1, 2).

For the other test dialects, checking test goal results can be performed by calling
the `assertion/1-2` utility predicates or by writing the checking goals directly in
the test body. For example:

	test(compare_3_order_less) :-
		compare(Order, 1, 2),
		^^assertion(Order == (<)).

or:

	succeeds(compare_3_order_less) :-
		compare(Order, 1, 2),
		Order == (<).

Using assertions is preferable as it facilitates debugging by printing the
unexpected results when the tests fail.

Ground results can be compared using the standard `==/2` term equality
built-in predicate. Non-ground results can be compared using the `variant/2`
predicate provided by `lgtunit`. The standard `subsumes_term/2` built-in
predicate can be used when testing a compound term structure while
abstracting some of its arguments. Floating-point numbers can be compared
using the `=~=/2`, `approximately_equal/3`, `essentially_equal/3`, and
`tolerance_equal/4` predicates provided by `lgtunit`. Using the `=/2` term
unification built-in predicate is almost always an error as it would mask
test goals failing to bind output arguments.


Testing local predicates
------------------------

The `(<<)/2` debugging control construct can be used to access and test object
local predicates (i.e. predicates without a scope directive). In this case,
make sure that the `context_switching_calls` compiler flag is set to `allow`
for those objects. This is seldom required, however, as local predicates are
usually auxiliary predicates called by public predicates and thus tested when
testing those public predicates. The code coverage support can pinpoint any
local predicate clause that is not being exercised by the tests.


Testing non-deterministic predicates
------------------------------------

For testing non-deterministic predicates (with a finite and manageable number
of solutions), you can wrap the test goal using the standard `findall/3`
predicate to collect all solutions and check against the list of expected
solutions. When the expected solutions are a set, use in alternative the
standard `setof/3` predicate.


Testing generators
------------------

To test all solutions of a predicate that acts as a *generator*, we can use
the `forall/2` predicate as the test goal with the `assertion/2` predicate
called to report details on any solution that fails the test. For example:

	:- uses(lgtunit, [assertion/2]).
	...

	test(test_solution_generator) :-
		forall(
			generator(X, Y, Z),
			assertion(generator(X,Y,Z), test(X,Y,Z))
		).


Testing input/output predicates
-------------------------------

Extensive support for testing input/output predicates is provided, based on
similar support found on the Prolog conformance testing framework written by
Péter Szabó and Péter Szeredi.

Two sets of predicates are provided, one for testing text input/output and
one for testing binary input/output. In both cases, temporary files (possibly
referenced by a user-defined alias) are used. The predicates allow setting,
checking, and cleaning text/binary input/output.

As an example of testing an input predicate, consider the standard `get_char/1`
predicate. This predicate reads a single character (atom) from the current
input stream. Some test for basic functionality could be:

	test(get_char_1_01, true(Char == 'q')) :-
		^^set_text_input('qwerty'),
		get_char(Char).

	test(get_char_1_02, true(Assertion)) :-
		^^set_text_input('qwerty'),
		get_char(_Char),
		^^text_input_assertion('werty', Assertion).

As you can see in the above example, the testing pattern consist on setting
the input for the predicate being tested, calling it, and then checking the
results. It is also possible to work with streams other than the current
input/output streams by using the `lgtunit` predicate variants that take a
stream as argument. For example, when testing the standard `get_char/2`
predicate, we could write:

	test(get_char_2_01, true(Char == 'q')) :-
		^^set_text_input(my_alias, 'qwerty'),
		get_char(my_alias, Char).

	test(get_char_2_02, true(Assertion)) :-
		^^set_text_input(my_alias, 'qwerty'),
		get_char(my_alias, _Char),
		^^text_input_assertion(my_alias, 'werty', Assertion).

Testing output predicates follows the same pattern by using instead the
`set_text_output/1-2` and `text_output_assertion/2-3` predicates. For
testing binary input/output predicates, equivalent testing predicates
are provided. There is also a small set of helper predicates for dealing
with stream handles and stream positions. For testing with files instead
of streams, testing predicates are provided that allow creating text and
binary files with given contents and check text and binary files for
expected contents.

For more practical examples, check the included tests for Prolog conformance
of standard input/output predicates.


Suppressing tested predicates output
------------------------------------

Sometimes predicates being tested output text or binary data that at best
clutters testing logs and at worse can interfere with parsing of test
logs. If that output itself is not under testing, you can suppress it by
using the goals `^^suppress_text_output` or `^^suppress_binary_output` at
the beginning of the tests.


Tests with timeout limits
-------------------------

There's no portable way to call a goal with a timeout limit. However, some
backend Prolog compilers provide this functionality:

- B-Prolog: `time_out/3` predicate
- ECLiPSe: `timeout/3` and `timeout/7` library predicates
- SICStus Prolog: `time_out/3` library predicate
- SWI-Prolog: `call_with_time_limit/2` library predicate
- YAP: `time_out/3` library predicate

Logtalk provides a `timeout` portability library implementing a simple
abstraction for those backend Prolog compilers.

The `logtalk_tester` automation script accepts a timeout option that can be
used to set a limit per test set.


Setup and cleanup goals
-----------------------

A test object can define `setup/0` and `cleanup/0` goals. The `setup/0`
predicate is called, when defined, before running the object unit tests. The
`cleanup/0` predicate is called, when defined, after running all the object
unit tests. The tests are skipped when the setup goal fails or throws an error.

Per test setup and cleanup goals can be defined using the `test/3` dialect and
the `setup/1` and `cleanup/1` options. The test is skipped when the setup goal
fails or throws an error. Note that a broken test cleanup goal doesn't affect
the test but may adversely affect any following tests.


Test annotations
----------------

It's possible to define per unit and per test annotations to be printed after
the test results or when tests are skipped. This is particularly useful when
some units or some unit tests may be run while still being developed.
Annotations can be used to pass additional information to a user reviewing
test results. By intercepting the unit test framework message printing calls
(using the `message_hook/4` hook predicate), test automation scripts and
integrating tools can also access these annotations.

Units can define a global annotation using the predicate `note/1`. To define
per test annotations, use the `test/3` dialect and the `note/1` option. For
example, you can inform why a test is being skipped by writing:

	- test(foo_1, true, [note('Waiting for Deep Thought answer')]) :-
		...

Annotations are written, by default, between parenthesis after and in the
same line as the test results.


Debugging failed tests
----------------------

Debugging of failed unit tests is usually easy if you use assertions as the
reason for the assertion failures is printed out. Thus, use preferably the
`test/2-3` dialects with `true(Assertion)` or `deterministic(Assertion)`
outcomes. If a test checks multiple assertions, you can use the predicate
`assertion/2` in the test body.

In order to debug failed unit tests, start by compiling the unit test objects
and the code being tested in debug mode. Load the debugger and trace the test
that you want to debug. For example, assuming your tests are defined in a
`tests` object and that the identifier of test to be debugged is `test_foo`:

	| ?- logtalk_load(debugger(loader)).
	...

	| ?- debugger::trace.
	...

	| ?- tests::run(test_foo).
	...

You can also compile the code and the tests in debug mode but without using
the `hook/1` compiler option for the tests compilation. Assuming that the
`context_switching_calls` flag is set to `allow`, you can then use the `<</2`
debugging control construct to debug the tests. For example, assuming that
the identifier of test to be debugged is `test_foo` and that you used the
`test/1` dialect:

	| ?- logtalk_load(debugger(loader)).
	...

	| ?- debugger::trace.
	...

	| ?- tests<<test(test_foo).
	...

In the more complicated cases, it may be worth to define `loader_debug.lgt`
and `tester_debug.lgt` files that load code and tests in debug mode and also
load the debugger.


Code coverage
-------------

If you want entity predicate clause coverage information to be collected
and printed, you will need to compile the entities that you're testing
using the flags `debug(on)` and `source_data(on)`. Be aware, however,
that compiling in debug mode results in a performance penalty.

A single test object may include tests for one or more entities (objects,
protocols, and categories). The entities being tested by a unit test object
for which code coverage information should be collected must be declared using
the `cover/1` predicate. For example, to collect code coverage data for the
objects `foo` and `bar` include the two clauses:

	cover(foo).
	cover(bar).

In the printed predicate clause coverage information, you may get a total
number of clauses smaller than the covered clauses. This results from the
use of dynamic predicates with clauses asserted at runtime. You may easily
identify dynamic predicates in the results as their clauses often have an
initial count equal to zero.

The list of indexes of the covered predicate clauses can be quite long.
Some backend Prolog compilers provide a flag or a predicate to control
the depth of printed terms that can be useful:

- CxProlog: `write_depth/2` predicate
- ECLiPSe: `print_depth` flag
- SICStus Prolog: `toplevel_print_options` flag
- SWI-Prolog 7.1.10 or earlier: `toplevel_print_options` flag
- SWI-Prolog 7.1.11 or later: `answer_write_options` flag
- XSB: `set_file_write_depth/1` predicate
- YAP: `write_depth/2-3` predicates


Automating running tests
------------------------

You can use the `scripts/logtalk_tester.sh` Bash shell script for automating
running unit tests. See the `scripts/NOTES.md` file for details. On POSIX
systems, assuming Logtalk was installed using one of the provided installers
or installation scripts, there is also a `man` page for the script:

	$ man logtalk_tester

An HTML version of this man page can be found at:

https://logtalk.org/man/logtalk_tester.html

Additional advice on testing and on automating testing using continuous
integration servers can be found at:

https://logtalk.org/testing.html


Utility predicates
------------------

The `lgtunit` tool provides several public utility predicates to simplify
writing unit tests:

- `variant(Term1, Term2)`  
	to check when two terms are a variant of each other (e.g. to check
	expected test results against actual results when they contain variables)

- `assertion(Goal)`  
	to generate an exception in case the goal argument fails or throws an error

- `assertion(Name, Goal)`  
	to generate an exception in case the goal argument fails or throws an error
	(the first argument allows assertion failures to be distinguished when using
	multiple assertions)

- `approximately_equal(Number1, Number2, Epsilon)`  
	for number approximate equality

- `essentially_equal(Number1, Number2, Epsilon)`  
	for number essential equality

- `tolerance_equal(Number1, Number2, RelativeTolerance, AbsoluteTolerance)`  
	for number equality within tolerances

- `Number1 =~= Number2`  
	for number (or list of numbers) close equality (usually floating-point numbers)

- `benchmark(Goal, Time)`  
	for timing a goal

- `benchmark_reified(Goal, Time, Result)`  
	reified version of `benchmark/2`

- `benchmark(Goal, Repetitions, Time)`  
	for finding the average time to prove a goal

- `benchmark(Goal, Repetitions, Clock, Time)`  
	for finding the average time to prove a goal using a cpu or a wall clock

- `deterministic(Goal)`  
	for checking that a predicate succeeds without	leaving a choice-point

- `deterministic(Goal, Deterministic)`  
	reified version of the `deterministic/1` predicate

The `assertion/1-2` predicates can be used in the body of tests where using
two or more assertions is convenient or in the body of tests written using
the `test/1`, `succeeds/1`, and `deterministic/1` dialects to help
differentiate between the test goal and checking the test goal results and
to provide more informative test failure messages.

When the assertion is a call to local predicate of the tests object, you must
call `assertion/1-2` using an implicit or explicit message instead of a using
_super_ call. To use an implicit message, add the following directive to the
tests object:

	:- uses(lgtunit, [assertion/1, assertion/2]).

The reason this is required is that the `assertion/1-2` predicates are
declared as meta-predicates and thus assertion goals are called in the
context of the _sender_, which would be the `lgtunit` object in the case
of a `^^/2` call (as it preserves both _self_ and _sender_ and the tests
are internally run by a message sent from the `lgtunit` object to the
tests object).

As the `benchmark/2-3` predicates are meta-predicates, turning on the
`optimize` compiler flag is advised to avoid runtime compilation of the
meta-argument, which would add an overhead to the timing results. But
this advice conflicts with collecting code coverage data, which requires
compilation in debug mode. The solution is to use separate test objects
for benchmarking and for code coverage.

Consult the `lgtunit` object documentation for more details on these
predicates.


Exporting test results in xUnit XML format
------------------------------------------

To output test results in the xUnit XML format, simply load the
`xunit_output.lgt` file before running the tests. This file defines
an object, `xunit_output`, that intercepts and rewrites unit test
execution messages, converting them to the xUnit XML format.

To export the test results to a file using the xUnit XML format, simply
load the `xunit_report.lgt` file before running the tests. A file named
`xunit_report.xml` will be created in the same directory as the object
defining the tests. When running a set of test suites as a single unified
suite (using the `run_test_sets/1` predicate), the single xUnit report is
created in the directory of the first test suite object in the set.

There are several third-party xUnit report converters that can generate
HTML files for easy browsing. For example:

- http://allure.qatools.ru
  (supports multiple reports)
- https://github.com/Zir0-93/xunit-to-html
  (supports multiple test sets in a single report)
- https://www.npmjs.com/package/xunit-viewer
- https://github.com/JatechUK/NUnit-HTML-Report-Generator
- https://plugins.jenkins.io/xunit


Exporting test results in the TAP output format
-----------------------------------------------

To output test results in the TAP (Test Anything Protocol) format, simply
load the `tap_output.lgt` file before running the tests. This file defines
an object, `tap_output`, that intercepts and rewrites unit test execution
messages, converting them to the TAP output format.

To export the test results to a file using the TAP (Test Anything Protocol)
output format, load instead the `tap_report.lgt` file before running the
tests. A file named `tap_report.txt` will be created in the same directory
as the object defining the tests.

When using the `test/3` dialect with the TAP format, a `note/1` option
whose argument is an atom starting with a `TODO` or `todo` word results
in a test report with a TAP TODO directive.

When running a set of test suites as a single unified suite, the single
TAP report is created in the directory of the first test suite object in
the set.


Exporting code coverage results in XML format
---------------------------------------------

To export code coverage results in XML format, load the `coverage_report.lgt`
file before running the tests. A file named `coverage_report.xml` will be
created in the same directory as the object defining the tests.

The XML file can be opened in most web browsers (with the notorious exception
of Google Chrome) by copying to the same directory the `coverage_report.dtd`
and `coverage_report.xsl` files found in the `tools/lgtunit` directory (when
using the `logtalk_tester` script, these two files are copied automatically).
In alternative, an XSLT processor can be used to generate an XHTML file instead
of relying on a web browser for the transformation. For example, using the
popular `xsltproc` processor:

	$ xsltproc -o coverage_report.html coverage_report.xml

The coverage report can include links to the source code when hosted on
Bitbucket, GitHub, or GitLab. This requires passing the base URL as the
value for the `url` XSLT parameter. The exact syntax depends on the XSLT
processor, however. For example:

	$ xsltproc \
	  --stringparam url https://github.com/LogtalkDotOrg/logtalk3/blob/master \
	  -o coverage_report.html coverage_report.xml

Note that the base URL should preferably be a permanent link (i.e. it should
include the commit SHA1) so that the links to source code files and lines
remain valid if the source code is later updated. It's also necessary to
suppress the local path prefix in the generated `coverage_report.xml` file.
For example:

	$ logtalk_tester -c xml -s $HOME/logtalk/

Alternatively, you can pass the local path prefix to be suppressed to the
XSLT processor (note that the `logtalk_tester` script suppresses the `$HOME`
prefix by default):

	$ xsltproc \
	  --stringparam prefix logtalk/ \
	  --stringparam url https://github.com/LogtalkDotOrg/logtalk3/blob/master \
	  -o coverage_report.html coverage_report.xml

If you are using Bitbucket, GitHub, or GitLab hosted in your own servers, the
`url` parameter may not contain a `bitbucket`, `github`, or `gitlab` string.
In this case, you can use the XSLT parameter `host` to indicate which service
are you running.


Known issues
------------

Parameter variables (`_VariableName_`) cannot currently be used in the
definition of test options (e.g. `condition/1`) when using the `test/3`
dialect. Use in alternative the `pgr/2` built-in execution context
predicate.

Deterministic unit tests are currently not available when using Lean Prolog
or Quintus Prolog as backend compilers do the lack of a required built-in
support that cannot be sensibly defined in Prolog.

Code coverage is only available when testing Logtalk code. But Prolog modules
can often be compiled as Logtalk objects and plain Prolog code may be wrapped
in a Logtalk object by using `include/1` directives. These two workarounds may
thus allow generating code coverage data also for Prolog code by defining tests
that use the `<</2` debugging control construct to call the Prolog predicates.


Other notes
-----------

All source files are indented using tabs (a common setting is a tab width
equivalent to 4 spaces).
