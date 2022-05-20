%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(lgtunit,
	implements(expanding)).

	% avoid a catch-22 due to the local definition
	% of the logtalk::trace_event/2 predicate
	:- set_logtalk_flag(debug, off).

	:- info([
		version is 11:1:1,
		author is 'Paulo Moura',
		date is 2022-05-20,
		comment is 'A unit test framework supporting predicate clause coverage, determinism testing, input/output testing, property-based testing, and multiple test dialects.',
		remarks is [
			'Usage' - 'Define test objects as extensions of the ``lgtunit`` object and compile their source files using the compiler option ``hook(lgtunit)``.',
			'Portability' - 'Deterministic unit tests are currently not available when using Quintus Prolog as the backend compiler.',
			'Known issues' - 'Parameter variables cannot currently be used in the definition of test options.'
		]
	]).

	:- public(cover/1).
	:- mode(cover(?entity_identifier), zero_or_more).
	:- info(cover/1, [
		comment is 'Declares entities being tested for which code coverage information should be collected.',
		argnames is ['Entity']
	]).

	:- public(run/0).
	:- mode(run, one).
	:- info(run/0, [
		comment is 'Runs the unit tests, writing the results to the current output stream.'
	]).

	:- public(run/1).
	:- mode(run(++callable), zero_or_one).
	:- mode(run(++list(callable)), zero_or_one).
	:- info(run/1, [
		comment is 'Runs a unit test or a list of unit tests, writing the results to the current output stream. Runs the global setup and cleanup steps when defined, failing if either step fails. Also fails if the test does not exist.',
		argnames is ['Tests']
	]).

	:- public(run/2).
	:- mode(run(+atom, +atom), one).
	:- info(run/2, [
		comment is 'Runs the unit tests, writing the results to the specified file. Mode can be either ``write`` (to create a new file) or ``append`` (to add results to an existing file).',
		argnames is ['File', 'Mode']
	]).

	:- public(run_test_sets/1).
	:- mode(run_test_sets(+list(object_identifier)), zero_or_one).
	:- info(run_test_sets/1, [
		comment is 'Runs two or more test sets as a unified set generating a single code coverage report if one is requested. Fails if the list does not contains at least two test objects.',
		argnames is ['TestObjects']
	]).

	:- public(test/1).
	:- mode(test(?callable), zero_or_more).
	:- info(test/1, [
		comment is 'Enumerates, by backtracking, the identifiers of all defined unit tests.',
		argnames is ['Identifier']
	]).

	:- public(number_of_tests/1).
	:- mode(number_of_tests(?integer), zero_or_one).
	:- info(number_of_tests/1, [
		comment is 'Number of defined unit tests.',
		argnames is ['NumerOfTests']
	]).

	:- public(deterministic/1).
	:- meta_predicate(deterministic(0)).
	:- mode(deterministic(+callable), zero_or_one).
	:- info(deterministic/1, [
		comment is 'True if the goal succeeds once without leaving choice-points.',
		argnames is ['Goal']
	]).

	:- public(deterministic/2).
	:- meta_predicate(deterministic(0, *)).
	:- mode(deterministic(+callable, --atom), zero_or_one).
	:- info(deterministic/2, [
		comment is 'Reified version of the ``deterministic/1`` predicate. True if the goal succeeds. Returns a boolean value (``true`` or ``false``) indicating if the goal succeeded without leaving choice-points.',
		argnames is ['Goal', 'Deterministic']
	]).

	:- public(assertion/1).
	:- meta_predicate(assertion('::')).
	:- mode(assertion(@callable), one).
	:- info(assertion/1, [
		comment is 'True if the assertion goal succeeds. Throws an error using the assertion goal as argument if the assertion goal throws an error or fails.',
		argnames is ['Assertion'],
		exceptions is [
			'Assertion goal fails' - assertion_failure('Assertion'),
			'Assertion goal throws Error' - assertion_error('Assertion', 'Error')
		]
	]).

	:- public(assertion/2).
	:- meta_predicate(assertion(*, 0)).
	:- mode(assertion(+nonvar, @callable), one).
	:- info(assertion/2, [
		comment is 'True if the assertion goal succeeds. Throws an error using the description as argument if the assertion goal throws an error or fails. The description argument helps to distinguish between different assertions in the same test body.',
		argnames is ['Description', 'Assertion'],
		exceptions is [
			'Assertion goal fails' - assertion_failure('Description'),
			'Assertion goal throws Error' - assertion_error('Description', 'Error')
		]
	]).

	:- public(quick_check/3).
	:- mode(quick_check(@callable, -callable, ++list(compound)), one).
	:- meta_predicate(quick_check(::, *, ::)).
	:- info(quick_check/3, [
		comment is 'Reified version of the ``quick_check/2`` predicate. Reports ``passed(Seed,Discarded,Labels)``, ``failed(Goal,Seed)``, ``error(Error,Goal,Seed)``, or ``error(Error,Culprit)``. ``Goal`` is the failed test. ``Seed`` is the starting seed.',
		argnames is ['Template', 'Result', 'Options'],
		remarks is [
			'``Seed`` argument' - 'Can be used to re-run the same exact sequence of pseudo-random tests by using the ``rs/1`` option after changes to the code being tested.',
			'``Discarded`` argument' - 'Number of generated tests that were discarded for failing to comply a pre-condition specified using the ``pc/1`` option.',
			'``Labels`` argument' - 'List of pairs ``Label-N`` where ``N`` is the number of generated tests that are classified as ``Label`` by a closure specified using the ``l/1`` option.'
		]
	]).

	:- public(quick_check/2).
	:- mode(quick_check(@callable, ++list(compound)), zero_or_one).
	:- meta_predicate(quick_check(::, ::)).
	:- info(quick_check/2, [
		comment is 'Generates and runs random tests for a predicate given its mode template and a set of options. Fails when a generated test fails printing the test.',
		argnames is ['Template', 'Options'],
		remarks is [
			'Number of tests' - 'Use the ``n(NumberOfTests)`` option to specifiy the number of random tests. Default is 100.',
			'Maximum number of shrink operations' - 'Use the ``s(MaxShrinks)`` option to specifiy the number of shrink operations when a counter example is found. Default is 64.',
			'Type edge cases' - 'Use the ``ec(Boolean)`` option to specifiy if type edge cases are tested (before generating random tests). Default is ``true``.',
			'Starting seed' - 'Use the ``rs(Seed)`` option to specifiy the random generator starting seed to be used when generating tests. No default. Seeds should be regarded as opaque terms.',
			'Test generation filtering' - 'Use the ``pc/1`` option to specifiy a pre-condition closure for filtering generated tests (extended with the test arguments; no default).',
			'Generated tests classification' - 'Use the ``l/1`` option to specifiy a label closure for classifying the generated tests (extended with the test arguments plus the labels argument; no default). The labelling predicate can return a single test label or a list of test labels.',
			'Verbose test generation' - 'Use the ``v(Boolean)`` option to specifiy verbose reporting of generated random tests. Default is ``false``.'
		]
	]).

	:- public(quick_check/1).
	:- mode(quick_check(@callable), zero_or_one).
	:- info(quick_check/1, [
		comment is 'Generates and runs random tests using default options for a predicate given its mode template. Fails when a generated test fails printing the test.',
		argnames is ['Template']
	]).

	:- public(benchmark/2).
	:- meta_predicate(benchmark(0, *)).
	:- mode(benchmark(+callable, -float), one).
	:- info(benchmark/2, [
		comment is 'Benchmarks a goal and returns the total execution time in seconds. Uses CPU clock. Goals that may throw an exception should be wrapped by the ``catch/3`` control construct.',
		argnames is ['Goal', 'Time']
	]).

	:- public(benchmark_reified/3).
	:- meta_predicate(benchmark_reified(0, *, *)).
	:- mode(benchmark_reified(+callable, -float, -callable), one).
	:- info(benchmark_reified/3, [
		comment is 'Benchmarks a goal and returns the total execution time in seconds plus its result (``success``, ``failure``, or ``error(Error))``. Uses CPU clock.',
		argnames is ['Goal', 'Time', 'Result']
	]).

	:- public(benchmark/3).
	:- meta_predicate(benchmark(0, *, *)).
	:- mode(benchmark(@callable, +positive_integer, -float), one).
	:- info(benchmark/3, [
		comment is 'Benchmarks a goal by repeating it the specified number of times and returning the total execution time in seconds. Uses CPU clock. Goals that may throw an exception should be wrapped by the ``catch/3`` control construct.',
		argnames is ['Goal', 'Repetitions', 'Time']
	]).

	:- public(benchmark/4).
	:- meta_predicate(benchmark(0, *, *, *)).
	:- mode(benchmark(@callable, +positive_integer, +atom, -float), one).
	:- info(benchmark/4, [
		comment is 'Benchmarks a goal by repeating it the specified number of times and returning the total execution time in seconds using the given clock (``cpu`` or ``wall``). Goals that may throw an exception should be wrapped by the ``catch/3`` control construct.',
		argnames is ['Goal', 'Repetitions', 'Clock', 'Time']
	]).

	:- public(variant/2).
	:- mode(variant(@term, @term), zero_or_one).
	:- info(variant/2, [
		comment is 'True when the two arguments are a variant of each other. I.e. if is possible to rename the term variables to make them identical. Useful for checking expected test results that contain variables.',
		argnames is ['Term1', 'Term2']
	]).

	:- public(approximately_equal/3).
	:- mode(approximately_equal(+number, +number, +number), zero_or_one).
	:- info(approximately_equal/3, [
		comment is 'Compares two numbers for approximate equality given an epsilon value using the de facto standard formula ``abs(Number1 - Number2) =< max(abs(Number1), abs(Number2)) * Epsilon``. Type-checked.',
		argnames is ['Number1', 'Number2', 'Epsilon']
	]).

	:- public(essentially_equal/3).
	:- mode(essentially_equal(+number, +number, +number), zero_or_one).
	:- info(essentially_equal/3, [
		comment is 'Compares two numbers for essential equality given an epsilon value using the de facto standard formula ``abs(Number1 - Number2) =< min(abs(Number1), abs(Number2)) * Epsilon``. Type-checked.',
		argnames is ['Number1', 'Number2', 'Epsilon']
	]).

	:- public(tolerance_equal/4).
	:- mode(tolerance_equal(+number, +number, +number, +number), zero_or_one).
	:- info(tolerance_equal/4, [
		comment is 'Compares two numbers for close equality given relative and absolute tolerances using the de facto standard formula ``abs(Number1 - Number2) =< max(RelativeTolerance * max(abs(Number1), abs(Number2)), AbsoluteTolerance)``. Type-checked.',
		argnames is ['Number1', 'Number2', 'RelativeTolerance', 'AbsoluteTolerance']
	]).

	:- public(op(700, xfx, ('=~='))).
	:- public(('=~=')/2).
	:- mode('=~='(+number, +number), zero_or_one).
	:- mode('=~='(+list(number), +list(number)), zero_or_one).
	:- info(('=~=')/2, [
		comment is 'Compares two numbers (or lists of numbers) for approximate equality using ``100*epsilon`` for the absolute error and, if that fails, ``99.999%`` accuracy for the relative error. But these precision values may not be adequate for all cases. Type-checked.',
		argnames is ['Number1', 'Number2']
	]).

	:- public(epsilon/1).
	:- mode(epsilon(-float), one).
	:- info(epsilon/1, [
		comment is 'Returns the value of epsilon used in the definition of the ``(=~=)/2`` predicate.',
		argnames is ['Epsilon']
	]).

	:- protected(run_tests/0).
	:- mode(run_tests, one).
	:- info(run_tests/0, [
		comment is 'Runs all defined unit tests.'
	]).

	:- protected(run_tests/1).
	:- mode(run_tests(+atom), one).
	:- info(run_tests/1, [
		comment is 'Runs all the tests defined in the given file.',
		argnames is ['File']
	]).

	:- protected(run_test_set/0).
	:- mode(run_test_set, one).
	:- info(run_test_set/0, [
		comment is 'Runs a test set as part of running two or more test sets as a unified set.'
	]).

	:- protected(run_quick_check_tests/5).
	:- mode(run_quick_check_tests(@callable, +list, --nonvar, --number, --list(pair)), one_or_error).
	:- info(run_quick_check_tests/5, [
		comment is 'Runs a list of defined tests using the given options. Returns the starting seed used to generate the random tests, the number of discarded tests, and the test label statistics.',
		argnames is ['Template', 'Options', 'Seed', 'Discarded', 'Labels']
	]).

	:- protected(condition/0).
	:- mode(condition, zero_or_one).
	:- info(condition/0, [
		comment is 'Verifies conditions for running the tests. Defaults to the goal ``true``.'
	]).

	:- protected(setup/0).
	:- mode(setup, zero_or_one).
	:- info(setup/0, [
		comment is 'Setup environment before running the test set. Defaults to the goal ``true``.'
	]).

	:- protected(cleanup/0).
	:- mode(cleanup, zero_or_one).
	:- info(cleanup/0, [
		comment is 'Cleanup environment after running the test set. Defaults to the goal ``true``.'
	]).

	:- protected(make/1).
	:- mode(make(?atom), zero_or_one).
	:- info(make/1, [
		comment is 'Make target for automatically running the test set when calling the ``logtalk_make/1`` built-in predicate. No default. Possible values are ``all`` and ``check``.'
	]).

	:- protected(note/1).
	:- mode(note(?atom), zero_or_one).
	:- info(note/1, [
		comment is 'Note to be printed after the test results. Defaults to the empty atom.'
	]).

	:- protected(suppress_text_output/0).
	:- mode(suppress_text_output, one).
	:- info(suppress_text_output/0, [
		comment is 'Suppresses text output. Useful to avoid irrelevant text output from predicates being tested to clutter the test logs.'
	]).

	:- protected(suppress_binary_output/0).
	:- mode(suppress_binary_output, one).
	:- info(suppress_binary_output/0, [
		comment is 'Suppresses binary output. Useful to avoid irrelevant binary output from predicates being tested to clutter the test logs.'
	]).

	:- protected(set_text_input/3).
	:- mode(set_text_input(+atom, +atom, +list(stream_option)), one).
	:- mode(set_text_input(+atom, +list(atom), +list(stream_option)), one).
	:- info(set_text_input/3, [
		comment is 'Creates a temporary file with the given text contents and opens it for reading referenced by the given alias and using the additional options. If no ``eof_action/1`` option is specified, its value will be the default used by the backend compiler.',
		argnames is ['Alias', 'Contents', 'Options']
	]).

	:- protected(set_text_input/2).
	:- mode(set_text_input(+atom, +atom), one).
	:- mode(set_text_input(+atom, +list(atom)), one).
	:- info(set_text_input/2, [
		comment is 'Creates a temporary file with the given text contents and opens it for reading referenced by the given alias and using the default end-of-file action for the used backend compiler.',
		argnames is ['Alias', 'Contents']
	]).

	:- protected(set_text_input/1).
	:- mode(set_text_input(+atom), one).
	:- mode(set_text_input(+list(atom)), one).
	:- info(set_text_input/1, [
		comment is 'Creates a temporary file with the given text contents, opens it for reading using the default end-of-file action for the used backend compiler, and sets the current input stream to the file.',
		argnames is ['Contents']
	]).

	:- protected(check_text_input/2).
	:- mode(check_text_input(+atom, +atom), zero_or_one).
	:- info(check_text_input/2, [
		comment is 'Checks that the temporary file (referenced by the given alias) being read have the expected text contents.',
		argnames is ['Alias', 'Contents']
	]).

	:- protected(check_text_input/1).
	:- mode(check_text_input(+atom), zero_or_one).
	:- info(check_text_input/1, [
		comment is 'Checks that the temporary file being read have the expected text contents.',
		argnames is ['Contents']
	]).

	:- protected(text_input_assertion/3).
	:- mode(text_input_assertion(+atom, +atom, --callable), one).
	:- info(text_input_assertion/3, [
		comment is 'Returns an assertion for checking that the temporary file (referenced by the given alias) being read have the expected text contents.',
		argnames is ['Alias', 'Contents', 'Assertion']
	]).

	:- protected(text_input_assertion/2).
	:- mode(text_input_assertion(+atom, --callable), one).
	:- info(text_input_assertion/2, [
		comment is 'Returns an assertion for checking that the temporary file being read have the expected text contents.',
		argnames is ['Contents', 'Assertion']
	]).

	:- protected(clean_text_input/0).
	:- mode(clean_text_input, one).
	:- info(clean_text_input/0, [
		comment is 'Cleans the temporary file used when testing text input.'
	]).

	:- protected(set_binary_input/3).
	:- mode(set_binary_input(+atom, +list(byte), +list(stream_option)), one).
	:- info(set_binary_input/3, [
		comment is 'Creates a temporary file with the given binary contents and opens it for reading referenced by the given alias and using the additional options. If no ``eof_action/1`` option is specified, its value will be the default used by the backend compiler.',
		argnames is ['Alias', 'Bytes', 'Options']
	]).

	:- protected(set_binary_input/2).
	:- mode(set_binary_input(+atom, +list(byte)), one).
	:- info(set_binary_input/2, [
		comment is 'Creates a temporary file with the given binary contents and opens it for reading referenced by the given alias and using the default end-of-file action for the used backend compiler.',
		argnames is ['Alias', 'Bytes']
	]).

	:- protected(set_binary_input/1).
	:- mode(set_binary_input(+list(byte)), one).
	:- info(set_binary_input/1, [
		comment is 'Creates a temporary file with the given binary contents, opens it for reading using the default end-of-file action for the used backend compiler, and sets the current input stream to the file.',
		argnames is ['Bytes']
	]).

	:- protected(check_binary_input/2).
	:- mode(check_binary_input(+atom, +list(byte)), zero_or_one).
	:- info(check_binary_input/2, [
		comment is 'Checks that the temporary file (referenced by the given alias) being read have the expected binary contents.',
		argnames is ['Alias', 'Bytes']
	]).

	:- protected(check_binary_input/1).
	:- mode(check_binary_input(+list(byte)), zero_or_one).
	:- info(check_binary_input/1, [
		comment is 'Checks that the temporary file being read have the expected binary contents.',
		argnames is ['Bytes']
	]).

	:- protected(binary_input_assertion/3).
	:- mode(binary_input_assertion(+atom, +list(byte), --callable), one).
	:- info(binary_input_assertion/3, [
		comment is 'Returns an assertion for checking that the temporary file (referenced by the given alias) being read have the expected binary contents.',
		argnames is ['Alias', 'Bytes', 'Assertion']
	]).

	:- protected(binary_input_assertion/2).
	:- mode(binary_input_assertion(+list(byte), --callable), one).
	:- info(binary_input_assertion/2, [
		comment is 'Returns an assertion for checking that the temporary file being read have the expected binary contents.',
		argnames is ['Bytes', 'Assertion']
	]).

	:- protected(clean_binary_input/0).
	:- mode(clean_binary_input, one).
	:- info(clean_binary_input/0, [
		comment is 'Cleans the temporary file used when testing binary input.'
	]).

	:- protected(set_text_output/3).
	:- mode(set_text_output(+atom, +atom, +list(stream_option)), one).
	:- mode(set_text_output(+atom, +list(atom), +list(stream_option)), one).
	:- info(set_text_output/3, [
		comment is 'Creates a temporary file with the given text contents and opens it for writing referenced by the given alias and using the additional options.',
		argnames is ['Alias', 'Contents', 'Options']
	]).

	:- protected(set_text_output/2).
	:- mode(set_text_output(+atom, +atom), one).
	:- mode(set_text_output(+atom, +list(atom)), one).
	:- info(set_text_output/2, [
		comment is 'Creates a temporary file with the given text contents and referenced by the given alias.',
		argnames is ['Alias', 'Contents']
	]).

	:- protected(set_text_output/1).
	:- mode(set_text_output(+atom), one).
	:- mode(set_text_output(+list(atom)), one).
	:- info(set_text_output/1, [
		comment is 'Creates a temporary file with the given text contents and sets the current output stream to the file.',
		argnames is ['Contents']
	]).

	:- protected(check_text_output/3).
	:- mode(check_text_output(+atom, +atom, +list(stream_option)), zero_or_one).
	:- info(check_text_output/3, [
		comment is 'Checks that the temporary file (open with the given options and alias) being written have the expected text contents.',
		argnames is ['Alias', 'Contents', 'Options']
	]).

	:- protected(check_text_output/2).
	:- mode(check_text_output(+atom, +atom), zero_or_one).
	:- info(check_text_output/2, [
		comment is 'Checks that the temporary file (open with default options and alias) being written have the expected text contents.',
		argnames is ['Alias', 'Contents']
	]).

	:- protected(check_text_output/1).
	:- mode(check_text_output(+atom), zero_or_one).
	:- info(check_text_output/1, [
		comment is 'Checks that the temporary file being written have the expected text contents.',
		argnames is ['Contents']
	]).

	:- protected(text_output_assertion/4).
	:- mode(text_output_assertion(+atom, +atom, +list(stream_option), --callable), one).
	:- info(text_output_assertion/4, [
		comment is 'Returns an assertion for checking that the temporary file (open with the given options and alias) being written have the expected text contents.',
		argnames is ['Alias', 'Contents', 'Options', 'Assertion']
	]).

	:- protected(text_output_assertion/3).
	:- mode(text_output_assertion(+atom, +atom, --callable), one).
	:- info(text_output_assertion/3, [
		comment is 'Returns an assertion for checking that the temporary file (open with default options and alias) being written have the expected text contents.',
		argnames is ['Alias', 'Contents', 'Assertion']
	]).

	:- protected(text_output_assertion/2).
	:- mode(text_output_assertion(+atom, --callable), one).
	:- info(text_output_assertion/2, [
		comment is 'Returns an assertion for checking that the temporary file being written have the expected text contents.',
		argnames is ['Contents', 'Assertion']
	]).

	:- protected(text_output_contents/3).
	:- mode(text_output_contents(+atom, -list(character), +list(stream_option)), one).
	:- info(text_output_contents/3, [
		comment is 'Returns the contents of the temporary file (open with the given options and alias) being written.',
		argnames is ['Alias', 'Contents', 'Options']
	]).

	:- protected(text_output_contents/2).
	:- mode(text_output_contents(+atom, -list(character)), one).
	:- info(text_output_contents/2, [
		comment is 'Returns the contents of the temporary file (open with default options and alias) being written.',
		argnames is ['Alias', 'Contents']
	]).

	:- protected(text_output_contents/1).
	:- mode(text_output_contents(-list(character)), one).
	:- info(text_output_contents/1, [
		comment is 'Returns the contents of the temporary file being written.',
		argnames is ['Contents']
	]).

	:- protected(clean_text_output/0).
	:- mode(clean_text_output, one).
	:- info(clean_text_output/0, [
		comment is 'Cleans the temporary file used when testing text output.'
	]).

	:- protected(set_binary_output/3).
	:- mode(set_binary_output(+atom, +list(byte), +list(stream_option)), one).
	:- info(set_binary_output/3, [
		comment is 'Creates a temporary file with the given binary contents and opens it for writing referenced by the given alias and using the additional options.',
		argnames is ['Alias', 'Contents', 'Options']
	]).

	:- protected(set_binary_output/2).
	:- mode(set_binary_output(+atom, +list(byte)), one).
	:- info(set_binary_output/2, [
		comment is 'Creates a temporary file with the given binary contents and opens it for writing referenced with the given alias.',
		argnames is ['Alias', 'Bytes']
	]).

	:- protected(set_binary_output/1).
	:- mode(set_binary_output(+list(byte)), one).
	:- info(set_binary_output/1, [
		comment is 'Creates a temporary file with the given binary contents and sets the current output stream to the file.',
		argnames is ['Bytes']
	]).

	:- protected(check_binary_output/2).
	:- mode(check_binary_output(+atom, +list(byte)), zero_or_one).
	:- info(check_binary_output/2, [
		comment is 'Checks that the temporary file (referenced by the given alias) have the expected binary contents.',
		argnames is ['Alias', 'Bytes']
	]).

	:- protected(check_binary_output/1).
	:- mode(check_binary_output(+list(byte)), zero_or_one).
	:- info(check_binary_output/1, [
		comment is 'Checks that the temporary file have the expected binary contents.',
		argnames is ['Bytes']
	]).

	:- protected(binary_output_assertion/3).
	:- mode(binary_output_assertion(+atom, +list(byte), --callable), one).
	:- info(binary_output_assertion/3, [
		comment is 'Returns an assertion for checking that the temporary file (referenced by the given alias) have the expected binary contents.',
		argnames is ['Alias', 'Bytes', 'Assertion']
	]).

	:- protected(binary_output_assertion/2).
	:- mode(binary_output_assertion(+list(byte), --callable), one).
	:- info(binary_output_assertion/2, [
		comment is 'Returns an assertion for checking that the temporary file have the expected binary contents.',
		argnames is ['Bytes', 'Assertion']
	]).

	:- protected(binary_output_contents/2).
	:- mode(binary_output_contents(+atom, -list(byte)), one).
	:- info(binary_output_contents/2, [
		comment is 'Returns the binary contents of the temporary file (referenced by the given alias) being written.',
		argnames is ['Alias', 'Bytes']
	]).

	:- protected(binary_output_contents/1).
	:- mode(binary_output_contents(-list(byte)), one).
	:- info(binary_output_contents/1, [
		comment is 'Returns the binary contents of the temporary file being written.',
		argnames is ['Bytes']
	]).

	:- protected(clean_binary_output/0).
	:- mode(clean_binary_output, one).
	:- info(clean_binary_output/0, [
		comment is 'Cleans the temporary file used when testing binary output.'
	]).

	:- protected(create_text_file/3).
	:- mode(create_text_file(+atom, +atom, +list(stream_option)), one).
	:- mode(create_text_file(+atom, +list(atom), +list(stream_option)), one).
	:- info(create_text_file/3, [
		comment is 'Creates a text file with the given contents. The file is open for writing using the given options.',
		argnames is ['File', 'Contents', 'Options']
	]).

	:- protected(create_text_file/2).
	:- mode(create_text_file(+atom, +atom), one).
	:- mode(create_text_file(+atom, +list(atom)), one).
	:- info(create_text_file/2, [
		comment is 'Creates a text file with the given contents. The file is open for writing using default options.',
		argnames is ['File', 'Contents']
	]).

	:- protected(create_binary_file/2).
	:- mode(create_binary_file(+atom, +list(byte)), one).
	:- info(create_binary_file/2, [
		comment is 'Creates a binary file with the given contents.',
		argnames is ['File', 'Bytes']
	]).

	:- protected(check_text_file/3).
	:- mode(check_text_file(+atom, +atom, +list(stream_option)), zero_or_one).
	:- info(check_text_file/3, [
		comment is 'Checks that the contents of a text file match the expected contents. The file is open for reading using the given options.',
		argnames is ['File', 'Contents', 'Options']
	]).

	:- protected(check_text_file/2).
	:- mode(check_text_file(+atom, +atom), zero_or_one).
	:- info(check_text_file/2, [
		comment is 'Checks that the contents of a text file (open for reading using default options) match the expected contents.',
		argnames is ['File', 'Contents']
	]).

	:- protected(text_file_assertion/4).
	:- mode(text_file_assertion(+atom, +atom, +list(stream_option), --callable), one).
	:- info(text_file_assertion/4, [
		comment is 'Returns an assertion for checking that the given file have the expected text contents. The file is open for reading using the given options.',
		argnames is ['File', 'Contents', 'Options', 'Assertion']
	]).

	:- protected(text_file_assertion/3).
	:- mode(text_file_assertion(+atom, +atom, --callable), one).
	:- info(text_file_assertion/3, [
		comment is 'Returns an assertion for checking that the given file have the expected text contents. The file is open for reading using default options.',
		argnames is ['File', 'Contents', 'Assertion']
	]).

	:- protected(check_binary_file/2).
	:- mode(check_binary_file(+atom, +list(byte)), zero_or_one).
	:- info(check_binary_file/2, [
		comment is 'Checks the contents of a binary file match the expected contents.',
		argnames is ['File', 'Bytes']
	]).

	:- protected(binary_file_assertion/3).
	:- mode(binary_file_assertion(+atom, +list(byte), --callable), one).
	:- info(binary_file_assertion/3, [
		comment is 'Returns an assertion for checking that the given file have the expected binary contents.',
		argnames is ['Path', 'Bytes', 'Assertion']
	]).

	:- protected(clean_file/1).
	:- mode(clean_file(+atom), one).
	:- info(clean_file/1, [
		comment is 'Closes any existing stream associated with the file and deletes the file if it exists.',
		argnames is ['File']
	]).

	:- protected(closed_input_stream/2).
	:- mode(closed_input_stream(-stream, +list(stream_option)), one).
	:- info(closed_input_stream/2, [
		comment is 'Opens a temporary file with the given options for reading, closes it, and returns its stream handle.',
		argnames is ['Stream', 'Options']
	]).

	:- protected(closed_output_stream/2).
	:- mode(closed_output_stream(-stream, +list(stream_option)), zero_or_one).
	:- info(closed_output_stream/2, [
		comment is 'Opens a temporary file with the given options for writing, closes it, and returns its stream handle.',
		argnames is ['Stream', 'Options']
	]).

	:- protected(stream_position/1).
	:- mode(stream_position(-stream_position), one).
	:- info(stream_position/1, [
		comment is 'Returns a syntactically valid stream position.',
		argnames is ['Position']
	]).

	:- protected(test/2).
	:- mode(test(?callable, ?compound), zero_or_more).
	:- info(test/2, [
		comment is 'Table of defined tests.',
		argnames is ['Identifier', 'Test']
	]).

	:- private(running_test_sets_/0).
	:- dynamic(running_test_sets_/0).
	:- mode(running_test_sets_, zero_or_one).
	:- info(running_test_sets_/0, [
		comment is 'Internal flag used when running two or more test sets as a unified set.'
	]).

	:- private(test/3).
	:- meta_predicate(test(*, *, *)).  % just to slience warnings
	:- mode(test(?callable, ?list(variable), ?nonvar), zero_or_more).
	:- info(test/3, [
		comment is 'Compiled unit tests. The list of variables is used to ensure variable sharing betwen a test with its test options.',
		argnames is ['Identifier', 'Variables', 'Outcome']
	]).

	:- private(auxiliary_predicate_counter_/1).
	:- dynamic(auxiliary_predicate_counter_/1).
	:- mode(auxiliary_predicate_counter_(?integer), one_or_more).
	:- info(auxiliary_predicate_counter_/1, [
		comment is 'Counter for generating unique auxiliary predicate names.',
		argnames is ['Counter']
	]).

	:- private(test_/2).
	:- dynamic(test_/2).
	:- mode(test_(?callable, ?compound), zero_or_more).
	:- info(test_/2, [
		comment is 'Table of compiled tests.',
		argnames is ['Identifier', 'Test']
	]).

	:- private(skipped_/1).
	:- dynamic(skipped_/1).
	:- mode(skipped_(?integer), zero_or_one).
	:- info(skipped_/1, [
		comment is 'Counter for skipped tests.',
		argnames is ['Counter']
	]).

	:- private(passed_/1).
	:- dynamic(passed_/1).
	:- mode(passed_(?integer), zero_or_one).
	:- info(passed_/1, [
		comment is 'Counter for passed tests.',
		argnames is ['Counter']
	]).

	:- private(failed_/1).
	:- dynamic(failed_/1).
	:- mode(failed_(?callable), zero_or_one).
	:- info(failed_/1, [
		comment is 'Counter for failed tests.',
		argnames is ['Counter']
	]).

	:- private(flaky_/1).
	:- dynamic(flaky_/1).
	:- mode(flaky_(?callable), zero_or_one).
	:- info(flaky_/1, [
		comment is 'Counter for failed tests that are marked as flaky.',
		argnames is ['Counter']
	]).

	:- private(fired_/3).
	:- dynamic(fired_/3).
	:- mode(fired_(?entity_identifier, ?predicate_indicator, ?integer), zero_or_more).
	:- info(fired_/3, [
		comment is 'Fired clauses when running the unit tests.',
		argnames is ['Entity', 'Predicate', 'Clause']
	]).

	:- private(covered_/4).
	:- dynamic(covered_/4).
	:- mode(covered_(?entity_identifier, ?callable, ?integer, ?integer), zero_or_more).
	:- info(covered_/4, [
		comment is 'Auxiliary predicate for collecting statistics on clause coverage.',
		argnames is ['Entity', 'Predicate', 'Covered', 'Total']
	]).

	% we use the structured printing mechanism in order to allow unit tests
	% results to be intercepted for alternative reporting by e.g. GUI IDEs
	:- uses(logtalk, [print_message/3]).
	% library support for quick check
	:- uses(type, [check/2, check/3, valid/2, arbitrary/2, shrink/3, edge_case/2, get_seed/1, set_seed/1]).
	% library support for suppressin test output
	:- uses(os, [cpu_time/1, null_device_path/1]).
	% library list predicates
	:- uses(list, [append/3, length/2, member/2, memberchk/2, nth1/3, select/3]).
	% for QuickCheck support
	:- uses(fast_random, [maybe/0]).

	:- if(current_logtalk_flag(prolog_dialect, gnu)).
		% workaround gplc limitation when dealing with multifile predicates
		% that are called from a file but not defined in that file
		:- multifile(type::type/1).
		:- multifile(type::check/2).
	:- endif.

	% by default, run the unit tests
	condition.

	% by default, no test setup is needed
	setup.

	% by default, no test cleanup is needed
	cleanup.

	% by default, no note to be printed after test results
	note('').

	run(File, Mode) :-
		open(File, Mode, Stream, [alias(lgtunit_redirected_output)]),
		logtalk::assertz((
			message_hook(_, Kind, lgtunit, Tokens) :-
				message_prefix_stream(Kind, lgtunit, Prefix, _),
				print_message_tokens(lgtunit_redirected_output, Prefix, Tokens)
		)),
		run,
		logtalk::retractall(message_hook(_, _, lgtunit, _)),
		close(Stream).

	run :-
		% save the current output stream
		current_output(Output),
		retractall(running_test_sets_),
		reset_test_counters,
		reset_coverage_results,
		write_tests_header,
		write_tests_object,
		(	run_condition ->
			(	run_setup ->
				::run_tests,
				run_cleanup,
				write_tests_results,
				write_coverage_results
			;	tests_skipped
			)
		;	tests_skipped
		),
		write_tests_footer,
		% restore the current output stream
		set_output(Output).

	run_test_sets([First, Next| Others]) :-
		retractall(running_test_sets_),
		assertz(running_test_sets_),
		reset_coverage_results,
		write_tests_header,
		run_test_sets_([First, Next| Others]),
		write_coverage_results([First, Next| Others]),
		write_tests_footer,
		retractall(running_test_sets_).

	run_test_sets_([]).
	run_test_sets_([TestSet| TestSets]) :-
		context(Context),
		check(object, TestSet, Context),
		TestSet::run_test_set,
		run_test_sets_(TestSets).

	run_test_set :-
		% save the current output stream
		current_output(Output),
		reset_test_counters,
		write_tests_object,
		(	run_condition ->
			(	run_setup ->
				::run_tests,
				run_cleanup,
				write_tests_results
			;	tests_skipped
			)
		;	tests_skipped
		),
		% restore the current output stream
		set_output(Output).

	run(Test) :-
		atom(Test),
		!,
		run([Test]).
	run(Tests) :-
		reset_test_counters,
		reset_coverage_results,
		run_setup,
		self(Object),
		object_property(Object, file(File)),
		forall(
			member(Test, Tests),
			run_test_from_file(Test, File)
		),
		run_cleanup,
		write_tests_results,
		write_coverage_results.

	run_test_from_file(Test, File) :-
		(	::test(Test, Spec) ->
			run_test(Spec, File)
		;	print_message(warning, lgtunit, 'Unknown test: ~q'+[Test]),
			fail
		).

	run_test(Spec, File) :-
		% save the current input and output streams
		current_input(Input), current_output(Output),
		run_test(Spec, File, Output),
		% restore the current input and output streams
		set_input(Input), set_output(Output).

	run_condition :-
		% expected either success or failure; error means user error
		catch(::condition, Error, (broken_step(condition,Error), fail)),
		!.

	run_setup :-
		% expected success; failure or error means user error
		(	catch(::setup, Error, broken_step(setup,Error)) ->
			(	var(Error) ->
				true
			;	fail
			)
		;	failed_step(setup),
			fail
		).

	run_cleanup :-
		% expected success; failure or error means user error
		(	catch(::cleanup, Error, broken_step(cleanup,Error)) ->
			true
		;	failed_step(cleanup)
		).

	run_tests(File) :-
		forall(::test(_, Spec), run_test(Spec, File)).

	:- meta_predicate(run_test((::), (*), (*))).

	% test/3 dialect
	run_test(succeeds(Test, Variables, Position, Condition, Setup, Cleanup, Note), File, Output) :-
		(	run_test_condition(Test, Condition, File, Position, Note, Output) ->
			(	run_test_setup(Test, Setup, File, Position, Note, Output) ->
				cpu_time(Start),
				(	catch(::test(Test, Variables, true), Error, failed_test(Test, File, Position, error_instead_of_success(Error), Note, Start, Output)) ->
					(	var(Error) ->
						passed_test(Test, File, Position, Note, Start, Output)
					;	true
					)
				;	failed_test(Test, File, Position, failure_instead_of_success, Note, Start, Output)
				),
				run_test_cleanup(Test, Cleanup, File, Position, Output)
			;	true
			)
		;	skipped_test(Test, File, Position, Note, Output)
		).
	run_test(deterministic(Test, Variables, Position, Condition, Setup, Cleanup, Note), File, Output) :-
		(	run_test_condition(Test, Condition, File, Position, Note, Output) ->
			(	run_test_setup(Test, Setup, File, Position, Note, Output) ->
				cpu_time(Start),
				(	catch(::test(Test, Variables, deterministic(Deterministic)), Error, failed_test(Test, File, Position, error_instead_of_success(Error), Note, Start, Output)) ->
					(	var(Error) ->
						(	Deterministic == true ->
							passed_test(Test, File, Position, Note, Start, Output)
						;	non_deterministic_success(Test, File, Position, Note, Start, Output)
						)
					;	true
					)
				;	failed_test(Test, File, Position, failure_instead_of_success, Note, Start, Output)
				),
				run_test_cleanup(Test, Cleanup, File, Position, Output)
			;	true
			)
		;	skipped_test(Test, File, Position, Note, Output)
		).
	run_test(fails(Test, Variables, Position, Condition, Setup, Cleanup, Note), File, Output) :-
		(	run_test_condition(Test, Condition, File, Position, Note, Output) ->
			(	run_test_setup(Test, Setup, File, Position, Note, Output) ->
				cpu_time(Start),
				(	catch(::test(Test, Variables, fail), Error, failed_test(Test, File, Position, error_instead_of_failure(Error), Note, Start, Output)) ->
					(	var(Error) ->
						failed_test(Test, File, Position, success_instead_of_failure, Note, Start, Output)
					;	true
					)
				;	passed_test(Test, File, Position, Note, Start, Output)
				),
				run_test_cleanup(Test, Cleanup, File, Position, Output)
			;	true
			)
		;	skipped_test(Test, File, Position, Note, Output)
		).
	run_test(throws(Test, Variables, PossibleErrors, Position, Condition, Setup, Cleanup, Note), File, Output) :-
		(	run_test_condition(Test, Condition, File, Position, Note, Output) ->
			(	run_test_setup(Test, Setup, File, Position, Note, Output) ->
				cpu_time(Start),
				(	catch(::test(Test, Variables, PossibleErrors), Error, check_error(Test, PossibleErrors, Error, File, Position, Note, Start, Output)) ->
					(	var(Error) ->
						PossibleErrors = [PossibleError| _],
						failed_test(Test, File, Position, success_instead_of_error(PossibleError), Note, Start, Output)
					;	true
					)
				;	PossibleErrors = [PossibleError| _],
					failed_test(Test, File, Position, failure_instead_of_error(PossibleError), Note, Start, Output)
				),
				run_test_cleanup(Test, Cleanup, File, Position, Output)
			;	true
			)
		;	skipped_test(Test, File, Position, Note, Output)
		).
	% quick_check/3 dialect
	run_test(quick_check(Test, Variables, Position, Condition, Setup, Cleanup, Note), File, Output) :-
		(	run_test_condition(Test, Condition, File, Position, Note, Output) ->
			(	run_test_setup(Test, Setup, File, Position, Note, Output) ->
				cpu_time(Start),
				(	catch(::test(Test, Variables, quick_check), Error, failed_test(Test, File, Position, Error, Start, Output)) ->
					(	var(Error) ->
						passed_test(Test, File, Position, Note, Start, Output)
					;	true
					)
				;	failed_test(Test, File, Position, failure_instead_of_success, Note, Start, Output)
				),
				run_test_cleanup(Test, Cleanup, File, Position, Output)
			;	true
			)
		;	skipped_test(Test, File, Position, Note, Output)
		).
	% quick_check/2 dialect
	run_test(quick_check(Test, Variables, Position), File, Output) :-
		cpu_time(Start),
		(	catch(::test(Test, Variables, quick_check), Error, failed_test(Test, File, Position, Error, Start, Output)) ->
			(	var(Error) ->
				passed_test(Test, File, Position, Start, Output)
			;	true
			)
		;	failed_test(Test, File, Position, failure_instead_of_success, Start, Output)
		).
	% other dialects
	run_test(succeeds(Test, Variables, Position), File, Output) :-
		cpu_time(Start),
		(	catch(::test(Test, Variables, true), Error, failed_test(Test, File, Position, error_instead_of_success(Error), Start, Output)) ->
			(	var(Error) ->
				passed_test(Test, File, Position, Start, Output)
			;	true
			)
		;	failed_test(Test, File, Position, failure_instead_of_success, Start, Output)
		).
	run_test(deterministic(Test, Variables, Position), File, Output) :-
		cpu_time(Start),
		(	catch(::test(Test, Variables, deterministic(Deterministic)), Error, failed_test(Test, File, Position, error_instead_of_success(Error), Start, Output)) ->
			(	var(Error) ->
				(	Deterministic == true ->
					passed_test(Test, File, Position, Start, Output)
				;	non_deterministic_success(Test, File, Position, Start, Output)
				)
			;	true
			)
		;	failed_test(Test, File, Position, failure_instead_of_success, Start, Output)
		).
	run_test(fails(Test, Variables, Position), File, Output) :-
		cpu_time(Start),
		(	catch(::test(Test, Variables, fail), Error, failed_test(Test, File, Position, error_instead_of_failure(Error), Start, Output)) ->
			(	var(Error) ->
				failed_test(Test, File, Position, success_instead_of_failure, Start, Output)
			;	true
			)
		;	passed_test(Test, File, Position, Start, Output)
		).
	run_test(throws(Test, Variables, PossibleErrors, Position), File, Output) :-
		cpu_time(Start),
		(	catch(::test(Test, Variables, PossibleErrors), Error, check_error(Test, PossibleErrors, Error, File, Position, Start, Output)) ->
			(	var(Error) ->
				PossibleErrors = [PossibleError| _],
				failed_test(Test, File, Position, success_instead_of_error(PossibleError), Start, Output)
			;	true
			)
		;	PossibleErrors = [PossibleError| _],
			failed_test(Test, File, Position, failure_instead_of_error(PossibleError), Start, Output)
		).

	run_test(skipped(Test, Position, Note), File, Output) :-
		skipped_test(Test, File, Position, Note, Output).

	run_test(skipped(Test, Position), File, Output) :-
		skipped_test(Test, File, Position, Output).

	check_error(Test, PossibleErrors, Error, File, Position, Start, Output) :-
		check_error(Test, PossibleErrors, Error, File, Position, '', Start, Output).

	check_error(Test, [PossibleError| PossibleErrors], Error, File, Position, Note, Start, Output) :-
		(	member(ExpectedError, [PossibleError| PossibleErrors]),
			subsumes_term(ExpectedError, Error) ->
			passed_test(Test, File, Position, Note, Start, Output)
		;	failed_test(Test, File, Position, wrong_error(PossibleError, Error), Note, Start, Output)
		).

	write_tests_header :-
		print_message(silent, lgtunit, tests_started),
		os::date_time(Year, Month, Day, Hours, Minutes, Seconds, _),
		print_message(information, lgtunit, tests_start_date_time(Year, Month, Day, Hours, Minutes, Seconds)).

	write_tests_object :-
		self(Object),
		object_property(Object, file(File, Directory)),
		atom_concat(Directory, File, Path),
		print_message(information, lgtunit, running_tests_from_object_file(Object, Path)),
		::number_of_tests(Total),
		print_message(silent, lgtunit, number_of_tests(Total)).

	write_tests_results :-
		self(Object),
		::skipped_(Skipped),
		::passed_(Passed),
		::failed_(Failed),
		::flaky_(Flaky),
		Total is Skipped + Passed + Failed,
		::note(Note),
		print_message(information, lgtunit, tests_results_summary(Object, Total, Skipped, Passed, Failed, Flaky, Note)),
		print_message(information, lgtunit, completed_tests_from_object(Object)).

	write_tests_footer :-
		os::date_time(Year, Month, Day, Hours, Minutes, Seconds, _),
		print_message(information, lgtunit, tests_end_date_time(Year, Month, Day, Hours, Minutes, Seconds)),
		print_message(silent, lgtunit, tests_ended).

	passed_test(Test, File, Position, Note, Start, Output) :-
		self(Object),
		cpu_time(End),
		Time is End - Start,
		increment_passed_tests_counter,
		% ensure that any redirection of the current output stream by
		% the test itself doesn't affect printing the test results
		current_output(Current), set_output(Output),
		print_message(information, lgtunit, passed_test(Object, Test, File, Position, Note, Time)),
		set_output(Current).

	passed_test(Test, File, Position, Start, Output) :-
		passed_test(Test, File, Position, '', Start, Output).

	non_deterministic_success(Test, File, Position, Note, Start, Output) :-
		self(Object),
		cpu_time(End),
		Time is End - Start,
		increment_failed_tests_counter,
		% ensure that any redirection of the current output stream by
		% the test itself doesn't affect printing the test results
		current_output(Current), set_output(Output),
		print_message(error, lgtunit, non_deterministic_success(Object, Test, File, Position, Note, Time)),
		set_output(Current).

	non_deterministic_success(Test, File, Position, Start, Output) :-
		non_deterministic_success(Test, File, Position, '', Start, Output).

	failed_test(Test, File, Position, Reason, Note, Start, Output) :-
		self(Object),
		cpu_time(End),
		Time is End - Start,
		increment_failed_tests_counter,
		(	atom(Note), sub_atom(Note, _, _, _, flaky) ->
			increment_flaky_tests_counter
		;	true
		),
		% ensure that any redirection of the current output stream by
		% the test itself doesn't affect printing the test results
		current_output(Current), set_output(Output),
		print_message(error, lgtunit, failed_test(Object, Test, File, Position, Reason, Note, Time)),
		set_output(Current).

	failed_test(Test, File, Position, Reason, Start, Output) :-
		failed_test(Test, File, Position, Reason, '', Start, Output).

	skipped_test(Test, File, Position, Note, Output) :-
		self(Object),
		increment_skipped_tests_counter,
		% ensure that any redirection of the current output stream by
		% the test itself doesn't affect printing the test results
		current_output(Current), set_output(Output),
		print_message(information, lgtunit, skipped_test(Object, Test, File, Position, Note)),
		set_output(Current).

	skipped_test(Test, File, Position, Output) :-
		skipped_test(Test, File, Position, '', Output).

	tests_skipped :-
		self(Object),
		object_property(Object, file(File)),
		::note(Note),
		print_message(information, lgtunit, tests_skipped(Object, File, Note)).

	:- meta_predicate(run_test_condition(*, *, *, *, *, *)).

	run_test_condition(Test, Condition, File, Position, Note, Output) :-
		option_goal(Condition, Goal),
		% expected either success or failure; error means user error
		(	Goal == true ->
			true
		;	catch(Goal, Error, (failed_test(Test,File,Position,step_error(condition,Error),Note,0.0,Output), fail))
		).

	:- meta_predicate(run_test_setup(*, *, *, *, *, *)).

	run_test_setup(Test, Setup, File, Position, Note, Output) :-
		option_goal(Setup, Goal),
		% expected success; failure or error means user error
		(	Goal == true ->
			true
		;	catch(Goal, Error, failed_test(Test,File,Position,step_error(setup,Error),Note,0.0,Output)) ->
			(	var(Error) ->
				true
			;	fail
			)
		;	failed_test(Test, File, Position, step_failure(setup), Note, 0.0, Output),
			fail
		).

	:- meta_predicate(run_test_cleanup(*, *, *, *, *)).

	run_test_cleanup(Test, Cleanup, File, Position, Output) :-
		option_goal(Cleanup, Goal),
		% expected success; failure or error means user error
		(	Goal == true ->
			true
		;	catch(Goal, Error, failed_cleanup(Test,File,Position,error(Error),Output)) ->
			true
		;	failed_cleanup(Test, File, Position, failure, Output)
		).

	option_goal(Option, Goal) :-
		(	control_construct(Option) ->
			Goal = Option
		;	self(Self),
			Goal = Self<<Option
		).

	control_construct(_::_).
	control_construct(::_).
	control_construct(^^_).
	control_construct(_<<_).
	control_construct(':'(_,_)).

	broken_step(Step, Error) :-
		self(Object),
		print_message(error, lgtunit, broken_step(Step, Object, Error)).

	failed_step(Step) :-
		self(Object),
		print_message(error, lgtunit, failed_step(Step, Object)).

	failed_cleanup(Test, File, Position, Reason, Output) :-
		self(Object),
		% ensure that any redirection of the current output stream by
		% the test itself doesn't affect printing the test results
		current_output(Current), set_output(Output),
		print_message(error, lgtunit, failed_cleanup(Object, Test, File, Position, Reason)),
		set_output(Current).

	reset_compilation_counters :-
		retractall(auxiliary_predicate_counter_(_)),
		assertz(auxiliary_predicate_counter_(0)),
		retractall(test_(_, _)).

	increment_skipped_tests_counter :-
		::retract(skipped_(Old)),
		New is Old + 1,
		::asserta(skipped_(New)).

	reset_test_counters :-
		::retractall(passed_(_)),
		::asserta(passed_(0)),
		::retractall(failed_(_)),
		::asserta(failed_(0)),
		::retractall(flaky_(_)),
		::asserta(flaky_(0)),
		::retractall(skipped_(_)),
		::asserta(skipped_(0)).

	increment_passed_tests_counter :-
		::retract(passed_(Old)),
		New is Old + 1,
		::asserta(passed_(New)).

	increment_failed_tests_counter :-
		::retract(failed_(Old)),
		New is Old + 1,
		::asserta(failed_(New)).

	increment_flaky_tests_counter :-
		::retract(flaky_(Old)),
		New is Old + 1,
		::asserta(flaky_(New)).

	% different unit test idioms are supported using the term-expansion mechanism;
	% the unit test objects must be loaded using this object as an hook object

	% ensure the source_data flag is turned on for the test object
	term_expansion(begin_of_file, [begin_of_file, (:- set_logtalk_flag(source_data,on))]).
	term_expansion((:- Directive), Terms) :-
		% delegate to another predicate to take advantage of first-argument indexing
		directive_expansion(Directive, Terms).

	% skipped tests
	term_expansion((- Head), Expansion) :-
		term_expansion((- Head :- true), Expansion).
	term_expansion((- Head :- _), []) :-
		test_idiom_head(Head, Test),
		check_for_valid_test_identifier(Test),
		logtalk_load_context(term_position, Position),
		(	Head = test(Test, _, Options) ->
			parse_test_options(Options, Test, _, _, _, Note),
			assertz(test_(Test, skipped(Test, Position, Note)))
		;	assertz(test_(Test, skipped(Test, Position)))
		).

	% unit test idiom test/3
	term_expansion((test(Test, Outcome0, Options) :- Goal0), [(test(Test, Variables, Outcome) :- Goal)]) :-
		check_for_valid_test_identifier(Test),
		check_for_valid_test_outcome(Test, Outcome0),
		convert_test_outcome(Outcome0, Test, Goal0, Outcome, Goal),
		logtalk_load_context(term_position, Position),
		term_variables(Outcome0+Options, Variables),
		parse_test_options(Options, Test, Condition, Setup, Cleanup, Note),
		(	Outcome == true ->
			assertz(test_(Test, succeeds(Test, Variables, Position, Condition, Setup, Cleanup, Note)))
		;	Outcome = deterministic(_) ->
			assertz(test_(Test, deterministic(Test, Variables, Position, Condition, Setup, Cleanup, Note)))
		;	Outcome == fail ->
			assertz(test_(Test, fails(Test, Variables, Position, Condition, Setup, Cleanup, Note)))
		;	% errors
			assertz(test_(Test, throws(Test, Variables, Outcome, Position, Condition, Setup, Cleanup, Note)))
		).

	% unit test idiom test/2
	term_expansion((test(Test, Outcome0) :- Goal0), [(test(Test, Variables, Outcome) :- Goal)]) :-
		check_for_valid_test_identifier(Test),
		check_for_valid_test_outcome(Test, Outcome0),
		convert_test_outcome(Outcome0, Test, Goal0, Outcome, Goal),
		logtalk_load_context(term_position, Position),
		term_variables(Outcome0, Variables),
		(	Outcome == true ->
			assertz(test_(Test, succeeds(Test, Variables, Position)))
		;	Outcome = deterministic(_) ->
			assertz(test_(Test, deterministic(Test, Variables, Position)))
		;	Outcome == fail ->
			assertz(test_(Test, fails(Test, Variables, Position)))
		;	% errors
			assertz(test_(Test, throws(Test, Variables, Outcome, Position)))
		).

	% unit test idiom test/1
	term_expansion((test(Test)), [test(Test, [], true)]) :-
		check_for_valid_test_identifier(Test),
		logtalk_load_context(term_position, Position),
		assertz(test_(Test, succeeds(Test, [], Position))).
	term_expansion((test(Test) :- Goal), [(test(Test, [], true) :- Goal)]) :-
		check_for_valid_test_identifier(Test),
		logtalk_load_context(term_position, Position),
		assertz(test_(Test, succeeds(Test, [], Position))).

	% unit test idiom succeeds/1 + deterministic/1 + fails/1 + throws/2
	term_expansion((succeeds(Test)), [test(Test, [], true)]) :-
		check_for_valid_test_identifier(Test),
		logtalk_load_context(term_position, Position),
		assertz(test_(Test, succeeds(Test, [], Position))).
	term_expansion((succeeds(Test) :- Goal), [(test(Test, [], true) :- Goal)]) :-
		check_for_valid_test_identifier(Test),
		logtalk_load_context(term_position, Position),
		assertz(test_(Test, succeeds(Test, [], Position))).
	term_expansion((deterministic(Test) :- Goal), [(test(Test, [], deterministic(Deterministic)) :- lgtunit::deterministic(Head,Deterministic))]) :-
		check_for_valid_test_identifier(Test),
		compile_deterministic_test_aux_predicate(Test, Goal, Head),
		logtalk_load_context(term_position, Position),
		assertz(test_(Test, deterministic(Test, [], Position))).
	term_expansion((fails(Test) :- Goal), [(test(Test, [], fail) :- Goal)]) :-
		check_for_valid_test_identifier(Test),
		logtalk_load_context(term_position, Position),
		assertz(test_(Test, fails(Test, [], Position))).
	term_expansion((throws(Test, Balls) :- Goal), [(test(Test, Variables, Errors) :- Goal)]) :-
		check_for_valid_test_identifier(Test),
		(	Balls = [_| _] ->
			Errors = Balls
		;	Errors = [Balls]
		),
		logtalk_load_context(term_position, Position),
		term_variables(Balls, Variables),
		assertz(test_(Test, throws(Test, Variables, Errors, Position))).

	% unit test idiom quick_check/3
	term_expansion(quick_check(Test, Template, Options),  [(test(Test, Variables, quick_check) :- ::run_quick_check_tests(Template, QuickCheckOptions, _, _, _))]) :-
		check_for_valid_test_identifier(Test),
		logtalk_load_context(term_position, Position),
		term_variables(Options, Variables),
		parse_quick_check_idiom_options(Options, Test, Condition, Setup, Cleanup, Note, QuickCheckOptions),
		assertz(test_(Test, quick_check(Test, Variables, Position, Condition, Setup, Cleanup, Note))).

	% unit test idiom quick_check/2
	term_expansion(quick_check(Test, Template),  [(test(Test, [], quick_check) :- ::run_quick_check_tests(Template, QuickCheckOptions, _, _, _))]) :-
		check_for_valid_test_identifier(Test),
		logtalk_load_context(term_position, Position),
		findall(Option, default_quick_check_option(Option), QuickCheckOptions),
		assertz(test_(Test, quick_check(Test, [], Position))).

	% make target for automatically running the tests
	term_expansion(make(Target), [make(Target), (:- initialization({assertz((logtalk_make_target_action(Target) :- Tests::run))}))]) :-
		logtalk_load_context(entity_identifier, Tests),
		(	Target == all ->
			true
		;	Target == check
		).

	% support the deprecated unit/1 predicate which may still be in use in old code
	term_expansion(unit(Entity), [cover(Entity)]).

	% we make sure that context-switching calls are enabled to help the user in
	% debugging failed unit tests
	directive_expansion(
			object(Test, Relation),
			[(:- object(Test, Relation)), (:- set_logtalk_flag(context_switching_calls,allow)), (:- meta_predicate(test(*, *, *)))]) :-
		reset_compilation_counters.
	directive_expansion(
			object(Test, Relation1, Relation2),
			[(:- object(Test, Relation1, Relation2)), (:- set_logtalk_flag(context_switching_calls,allow)), (:- meta_predicate(test(*, *, *)))]) :-
		reset_compilation_counters.
	directive_expansion(
			object(Test, Relation1, Relation2, Relation3),
			[(:- object(Test, Relation1, Relation2, Relation3)), (:- set_logtalk_flag(context_switching_calls,allow)), (:- meta_predicate(test(*, *, *)))]) :-
		reset_compilation_counters.
	directive_expansion(
			object(Test, Relation1, Relation2, Relation3, Relation4),
			[(:- object(Test, Relation1, Relation2, Relation3, Relation4)), (:- set_logtalk_flag(context_switching_calls,allow)), (:- meta_predicate(test(*, *, *)))]) :-
		reset_compilation_counters.
	directive_expansion(
			object(Test, Relation1, Relation2, Relation3, Relation4, Relation5),
			[(:- object(Test, Relation1, Relation2, Relation3, Relation4, Relation5)), (:- set_logtalk_flag(context_switching_calls,allow)), (:- meta_predicate(test(*, *, *)))]) :-
		reset_compilation_counters.

	% the discontiguous/1 directives usually required when using some of the
	% unit tests idioms are no longer necessary after term-expanding them
	directive_expansion(discontiguous(PI), Expansion) :-
		logtalk_load_context(entity_identifier, _),
		ground(PI),
		filter_discontiguous_directive(PI, Filtered),
		(	Filtered == [] ->
			Expansion = []
		;	Expansion = (:- discontiguous(Filtered))
		).

	% collect all unit test identifiers when reaching the end_object/0 directive
	directive_expansion(end_object, Terms) :-
		findall(1, test_(_, _), Ones),
		length(Ones, Total),
		logtalk_load_context(source, File),
		findall(test(Test, Spec), test_(Test, Spec), Terms, [number_of_tests(Total), (run_tests :- ::run_tests(File)), (:- end_object)]).

	filter_discontiguous_directive(PIs, Filtered) :-
		flatten_to_list(PIs, List),
		filter_discontiguous_predicates(List, Filtered).

	flatten_to_list([A| B], [A| B]) :-
		!.
	flatten_to_list([], []) :-
		!.
	flatten_to_list((A, B), [A| BB]) :-
		!,
		flatten_to_list(B, BB).
	flatten_to_list(A, [A]).

	filter_discontiguous_predicates([], []).
	filter_discontiguous_predicates([PI| PIs], Filtered) :-
		filter_discontiguous_predicate(PI, Filtered0),
		append(Filtered0, Filtered1, Filtered),
		filter_discontiguous_predicates(PIs, Filtered1).

	filter_discontiguous_predicate(Functor/Arity, Filtered) :-
		(	ignorable_discontiguous_predicate(Functor/Arity) ->
			Filtered = []
		;	Filtered = [Functor/Arity]
		).

	ignorable_discontiguous_predicate((-)/1).
	ignorable_discontiguous_predicate(test/3).
	ignorable_discontiguous_predicate(test/2).
	ignorable_discontiguous_predicate(test/1).
	ignorable_discontiguous_predicate(succeeds/1).
	ignorable_discontiguous_predicate(deterministic/1).
	ignorable_discontiguous_predicate(fails/1).
	ignorable_discontiguous_predicate(throws/2).
	ignorable_discontiguous_predicate(quick_check/2).
	ignorable_discontiguous_predicate(quick_check/3).

	check_for_valid_test_outcome(Test, Outcome) :-
		(	var(Outcome) ->
			print_message(error, lgtunit, non_instantiated_test_outcome(Test))
		;	valid_test_outcome(Outcome) ->
			true
		;	print_message(error, lgtunit, invalid_test_outcome(Test, Outcome))
		).

	valid_test_outcome(true).
	valid_test_outcome(true(_)).
	valid_test_outcome(deterministic).
	valid_test_outcome(deterministic(_)).
	valid_test_outcome(subsumes(_, _)).
	valid_test_outcome(variant(_, _)).
	valid_test_outcome(exists(_)).
	valid_test_outcome(all(_)).
	valid_test_outcome(fail).
	valid_test_outcome(false).
	valid_test_outcome(error(_)).
	valid_test_outcome(errors(_)).
	valid_test_outcome(ball(_)).
	valid_test_outcome(balls(_)).

	convert_test_outcome(true, _, Goal, true, Goal).
	convert_test_outcome(true(Assertion), Test, Goal, true, (Goal, lgtunit::assertion(Assertion,Assertion))) :-
		lint_check_assertion(Test, Assertion).
	convert_test_outcome(deterministic, Test, Goal, deterministic(Deterministic), lgtunit::deterministic(Head,Deterministic)) :-
		compile_deterministic_test_aux_predicate(Test, Goal, Head).
	convert_test_outcome(deterministic(Assertion), Test, Goal, deterministic(Deterministic), (lgtunit::deterministic(Head,Deterministic), lgtunit::assertion(Assertion,Assertion))) :-
		lint_check_assertion(Test, Assertion),
		compile_deterministic_test_aux_predicate(Test, Goal, Head).
	convert_test_outcome(subsumes(Expected, Result), _, Goal, true, (Goal, lgtunit::assertion(subsumes_term(Expected,Result),subsumes_term(Expected,Result)))).
	convert_test_outcome(variant(Term1, Term2), _, Goal, true, (Goal, lgtunit::assertion(variant(Term1,Term2),lgtunit::variant(Term1,Term2)))).
	convert_test_outcome(exists(Assertion), Test, Goal, true, (Goal, Assertion)) :-
		lint_check_assertion(Test, Assertion).
	convert_test_outcome(all(Assertion), Test, Goal, true, \+ (Goal, \+ lgtunit::assertion(Assertion,Assertion))) :-
		lint_check_assertion(Test, Assertion).
	convert_test_outcome(fail, _, Goal, fail, Goal).
	convert_test_outcome(false, _, Goal, fail, Goal).
	convert_test_outcome(error(Ball), _, Goal, [error(Ball,_)], Goal).
	convert_test_outcome(errors(Balls), _, Goal, Errors, Goal) :-
		map_errors(Balls, Errors).
	convert_test_outcome(ball(Ball), _, Goal, [Ball], Goal).
	convert_test_outcome(balls(Balls), _, Goal, Balls, Goal).

	lint_check_assertion(_, Assertion) :-
		var(Assertion),
		!.
	lint_check_assertion(Test, Term1 = Term2) :-
		once((var(Term1); var(Term2))),
		!,
		% undo numbervars of variables in the message term when priting by using double negation
		\+ \+ print_message(warning, lgtunit, assertion_uses_unification(Test, Term1 = Term2)).
	lint_check_assertion(_, _).

	map_errors([], []).
	map_errors([Ball| Balls], [error(Ball,_)| Errors]) :-
		map_errors(Balls, Errors).

	test_idiom_head(test(Test, _, _), Test).
	test_idiom_head(test(Test, _), Test).
	test_idiom_head(test(Test), Test).
	test_idiom_head(succeeds(Test), Test).
	test_idiom_head(deterministic(Test), Test).
	test_idiom_head(fails(Test), Test).
	test_idiom_head(throws(Test, _), Test).
	test_idiom_head(quick_check(Test, _, _), Test).
	test_idiom_head(quick_check(Test, _), Test).

	check_for_valid_test_identifier(Test) :-
		self(Object),
		(	var(Test) ->
			print_message(error, lgtunit, non_instantiated_test_identifier)
		;	\+ callable(Test) ->
			print_message(error, lgtunit, non_callable_test_identifier(Object, Test))
		;	\+ ground(Test) ->
			print_message(error, lgtunit, non_ground_test_identifier(Object, Test))
		;	test_(Test, _) ->
			print_message(error, lgtunit, repeated_test_identifier(Object, Test))
		;	true
		).

	parse_test_options(Options, Test, Condition, Setup, Cleanup, Note) :-
		% notes can be variables (e.g. a parameter variable or a variable shared with the
		% test clause body);  use a boolean flag to track the presence of a note/1 option
		parse_test_options(Options, Test, Condition, Setup, Cleanup, Note, _NoteFlag).

	parse_test_options([], _, Condition, Setup, Cleanup, Note, NoteFlag) :-
		(	var(Condition) ->
			% no condition/1 option found
			Condition = true
		;	true
		),
		(	var(Setup) ->
			% no setup/1 option found
			Setup = true
		;	true
		),
		(	var(Cleanup) ->
			% no cleanup/1 option found
			Cleanup = true
		;	true
		),
		(	NoteFlag == true ->
			true
		;	% no note/1 option found
			Note = ''
		).
	parse_test_options([Option| Options], Test, Condition, Setup, Cleanup, Note, NoteFlag) :-
		(	Option = condition(Goal) ->
			compile_test_step_aux_predicate(Test, '__condition', Goal, Condition)
		;	Option = setup(Goal) ->
			compile_test_step_aux_predicate(Test, '__setup', Goal, Setup)
		;	Option = cleanup(Goal) ->
			compile_test_step_aux_predicate(Test, '__cleanup', Goal, Cleanup)
		;	Option = note(Note) ->
			NoteFlag = true
		;	% ignore non-recognized options
			true
		),
		parse_test_options(Options, Test, Condition, Setup, Cleanup, Note, NoteFlag).

	compile_test_step_aux_predicate(Test, Step, Goal, Head) :-
		test_name_to_atom_prefix(Test, Prefix),
		atom_concat(Prefix, Step, Functor),
		term_variables(Goal, Variables),
		Head =.. [Functor| Variables],
		logtalk::compile_aux_clauses([(Head :- Goal)]).

	compile_deterministic_test_aux_predicate(Test, Goal, Head) :-
		test_name_to_atom_prefix(Test, Prefix),
		atom_concat(Prefix, '_deterministic', Functor),
		term_variables(Goal, Variables),
		Head =.. [Functor| Variables],
		logtalk::compile_aux_clauses([(Head :- Goal)]).

	test_name_to_atom_prefix(Test, Prefix) :-
		functor(Test, Name, Arity),
		atom_concat(Name, '_', Prefix0),
		number_codes(Arity, ArityCodes),
		atom_codes(ArityAtom, ArityCodes),
		atom_concat(Prefix0, ArityAtom, Prefix1),
		atom_concat(Prefix1, '_', Prefix2),
		retract(auxiliary_predicate_counter_(OldCounter)), !,
		NewCounter is OldCounter + 1,
		assertz(auxiliary_predicate_counter_(NewCounter)),
		number_codes(NewCounter, CounterCodes),
		atom_codes(CounterAtom, CounterCodes),
		atom_concat(Prefix2, CounterAtom, Prefix).

	parse_quick_check_idiom_options(Options, Test, Condition, Setup, Cleanup, Note, QuickCheckOptions) :-
		parse_test_options(Options, Test, Condition, Setup, Cleanup, Note),
		parse_quick_check_options(Options, QuickCheckOptions).

	parse_quick_check_options(Options, [n(NumberOfTests), s(MaxShrinks), ec(EdgeCases), pc(Condition), l(Label), v(Verbose)| Other]) :-
		(	memberchk(n(NumberOfTests), Options),
			integer(NumberOfTests),
			NumberOfTests >= 0 ->
			true
		;	default_quick_check_option(n(NumberOfTests))
		),
		(	memberchk(s(MaxShrinks), Options),
			integer(MaxShrinks),
			MaxShrinks >= 0 ->
			true
		;	default_quick_check_option(s(MaxShrinks))
		),
		(	memberchk(ec(EdgeCases), Options),
			(EdgeCases == true; EdgeCases == false) ->
			true
		;	default_quick_check_option(ec(EdgeCases))
		),
		(	memberchk(pc(Condition), Options),
			callable(Condition) ->
			true
		;	default_quick_check_option(pc(Condition))
		),
		(	memberchk(l(Label), Options),
			callable(Label) ->
			true
		;	default_quick_check_option(l(Label))
		),
		(	memberchk(v(Verbose), Options),
			(Verbose == true; Verbose == false) ->
			true
		;	default_quick_check_option(v(Verbose))
		),
		(	member(rs(Seed), Options) ->
			Other = [rs(Seed)]
		;	Other = []
		).

	% generate and run 100 tests by default
	default_quick_check_option(n(100)).
	% perform a maximum of 64 shrink operations on a counter-example
	default_quick_check_option(s(64)).
	% use edge cases by default
	default_quick_check_option(ec(true)).
	% don't filter generated tests by default (represented internally by the atom "true")
	default_quick_check_option(pc(true)).
	% don't label generated tests by default (represented internally by the atom "true")
	default_quick_check_option(l(true)).
	% don't do a verbose reporting of generated random tests
	default_quick_check_option(v(false)).

	% default number of tests
	number_of_tests(0).

	test(Identifier) :-
		::test(Identifier, _).

	:- if((
		current_logtalk_flag(prolog_dialect, Dialect),
		(Dialect == b; Dialect == qp; Dialect == scryer; Dialect == swi; Dialect == tau; Dialect == trealla; Dialect == yap)
	)).

		% avoid portability warnings
		:- uses(user, [setup_call_cleanup/3]).
		:- meta_predicate(user::setup_call_cleanup(0,0,0)).

		deterministic(Goal) :-
			setup_call_cleanup(true, Goal, Deterministic = true),
			(	var(Deterministic) ->
				!,
				fail
			;	true
			).

		deterministic(Goal, Deterministic) :-
			setup_call_cleanup(true, Goal, Deterministic = true),
			(	var(Deterministic) ->
				Deterministic = false
			;	true
			),
			!.

	:- elif((
		current_logtalk_flag(prolog_dialect, Dialect),
		(Dialect == cx; Dialect == ji; Dialect == sicstus; Dialect == xsb)
	)).

		% avoid portability warnings
		:- uses(user, [call_cleanup/2]).
		:- meta_predicate(user::call_cleanup(0,0)).

		deterministic(Goal) :-
			call_cleanup(Goal, Deterministic = true),
			(	var(Deterministic) ->
				!,
				fail
			;	true
			).

		deterministic(Goal, Deterministic) :-
			call_cleanup(Goal, Deterministic0 = true),
			(	var(Deterministic0) ->
				Deterministic = false
			;	Deterministic = true
			),
			!.

	:- elif(current_logtalk_flag(prolog_dialect, ciao)).

		% avoid portability warnings
		:- uses(user, [call_det/2]).

		deterministic(Goal) :-
			call_det(Goal, Deterministic),
			!,
			Deterministic == true.

		deterministic(Goal, Deterministic) :-
			call_det(Goal, Deterministic),
			!.

	:- elif(current_logtalk_flag(prolog_dialect, gnu)).

		% avoid portability warnings
		:- uses(user, [call_det/2]).
		:- meta_predicate(user::call_det(0,*)).

		deterministic(Goal) :-
			call_det(Goal, Deterministic),
			!,
			Deterministic == true.

		deterministic(Goal, Deterministic) :-
			call_det(Goal, Deterministic),
			!.

	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

		deterministic(Goal) :-
			{sepia_kernel:get_cut(Before)},
			call(Goal),
			{sepia_kernel:get_cut(After)},
			!,
			Before == After.

		deterministic(Goal, Deterministic) :-
			{sepia_kernel:get_cut(Before)},
			call(Goal),
			{sepia_kernel:get_cut(After)},
			!,
			(	Before == After ->
				Deterministic = true
			;	Deterministic = false
			).

	:- elif(current_logtalk_flag(prolog_dialect, lvm)).

		% avoid portability warnings
		:- uses(user, [call_det/2]).
		:- meta_predicate(user::call_det(0,*)).

		deterministic(Goal) :-
			call_det(Goal, Deterministic),
			!,
			Deterministic == true.

		deterministic(Goal, Deterministic) :-
			call_det(Goal, Deterministic),
			!.

	:- else.

		deterministic(_) :-
			resource_error(deterministic/1).

		deterministic(_, _) :-
			resource_error(deterministic/2).

	:- endif.

	assertion(Goal) :-
		assertion(Goal, Goal).

	assertion(Assertion, Goal) :-
		(	\+ \+ catch(Goal, Error, throw(assertion_error(Assertion, Error))) ->
			true
		;	throw(assertion_failure(Assertion))
		).

	approximately_equal(Number1, Number2, Epsilon) :-
		context(Context),
		check(number, Number1, Context),
		check(number, Number2, Context),
		check(number, Epsilon, Context),
		abs(Number1 - Number2) =< max(abs(Number1), abs(Number2)) * Epsilon.

	essentially_equal(Number1, Number2, Epsilon) :-
		context(Context),
		check(number, Number1, Context),
		check(number, Number2, Context),
		check(number, Epsilon, Context),
		abs(Number1 - Number2) =< min(abs(Number1), abs(Number2)) * Epsilon.

	tolerance_equal(Number1, Number2, RelativeTolerance, AbsoluteTolerance) :-
		context(Context),
		check(number, Number1, Context),
		check(number, Number2, Context),
		check(number, RelativeTolerance, Context),
		check(number, AbsoluteTolerance, Context),
		abs(Number1 - Number2) =< max(RelativeTolerance * max(abs(Number1), abs(Number2)), AbsoluteTolerance).

	'=~='(Float1, _) :-
		var(Float1),
		instantiation_error.
	'=~='(_, Float2) :-
		var(Float2),
		instantiation_error.
	'=~='([], []) :-
		!.
	'=~='([Float1| Floats1], [Float2| Floats2]) :-
		!,
		'=~='(Float1, Float2),
		'=~='(Floats1, Floats2).
	'=~='(Float1, Float2) :-
		context(Context),
		% some backend Prolog systems have non-compliant implementations of
		% float operators such as (/)/2 that can produce integer results
		check(number, Float1, Context),
		check(number, Float2, Context),
		(	% first test the absolute error, for meaningful results with numbers very close to zero:
			epsilon(Epsilon), abs(Float1 - Float2) < 100*Epsilon ->
			true
		;	% if that fails, test the relative error (99.999% accuracy):
			abs(Float1 - Float2) < 0.00001 * max(abs(Float1), abs(Float2))
		).

	:- if((
		current_logtalk_flag(prolog_dialect, Dialect),
		(	Dialect == swi; Dialect == yap; Dialect == gnu; Dialect == b; Dialect == cx;
			Dialect == tau; Dialect == lvm; Dialect == scryer; Dialect == trealla
		)
	)).
		epsilon(Epsilon) :-
			Epsilon is epsilon.
	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).
		epsilon(Epsilon) :-
			Epsilon is nexttoward(1.0, 2.0) - 1.0.
	:- else.
		epsilon(0.000000000001).
	:- endif.

	quick_check(Template, Result, Options) :-
		parse_quick_check_options(Options, QuickCheckOptions),
		catch(run_quick_check_tests(Template, QuickCheckOptions, Seed, Discarded, Labels), Error, true),
		(	var(Error) ->
			Result = passed(Seed, Discarded, Labels)
		;	quick_check_error_reified(Error, Result) ->
			true
		;	% should not happen but some Prolog backends have
			% non-standard exception-handling mechanisms
			Result = error(Error, _)
		).

	quick_check_error_reified(quick_check_failed(Goal, _, _, Seed),                       failed(Goal, Seed)).
	quick_check_error_reified(quick_check_error(error(Exception,_), Goal, _, Seed),       error(Exception, Goal, Seed)).
	quick_check_error_reified(quick_check_error(Exception, Goal, _, Seed),                error(Exception, Goal, Seed)).
	quick_check_error_reified(quick_check_error(label_goal_error(error(Exception,_)), Culprit), Result) :-
		quick_check_error_reified(quick_check_error(label_goal_error(Exception), Culprit), Result).
	quick_check_error_reified(quick_check_error(label_goal_error(Exception), Culprit),    error(Exception, Culprit)).
	quick_check_error_reified(quick_check_error(label_goal_failure, Culprit),             error(label_goal_failure, Culprit)).
	quick_check_error_reified(quick_check_error(pre_condition_error(error(Exception,_)), Culprit), Result) :-
		quick_check_error_reified(quick_check_error(pre_condition_error(Exception), Culprit), Result).
	quick_check_error_reified(quick_check_error(pre_condition_error(Exception), Culprit), error(Exception, Culprit)).
	quick_check_error_reified(quick_check_error(pre_condition_always_fails, Culprit),     error(pre_condition_always_fails, Culprit)).
	quick_check_error_reified(quick_check_error(error(Exception, _), Culprit),            error(Exception, Culprit)).
	quick_check_error_reified(quick_check_error(Exception, Culprit),                      error(Exception, Culprit)).

	quick_check(Template, Options) :-
		parse_quick_check_options(Options, QuickCheckOptions),
		memberchk(n(NumberOfTests), QuickCheckOptions),
		catch(run_quick_check_tests(Template, QuickCheckOptions, Seed, Discarded, Labels), Error, true),
		(	var(Error) ->
			print_message(information, lgtunit, quick_check_passed(NumberOfTests, Seed, Discarded, Labels))
		;	print_message(warning, lgtunit, Error),
			fail
		).

	quick_check(Template) :-
		quick_check(Template, []).

	:- meta_predicate(run_quick_check_tests(::, ::, *, *, *)).
	run_quick_check_tests(Template, Options, Seed, Discarded, Labels) :-
		catch(check(callable, Template), Error, throw(quick_check_error(Error,Template))),
		memberchk(n(N), Options),
		memberchk(s(MaxShrinks), Options),
		memberchk(ec(EdgeCases), Options),
		memberchk(pc(ConditionClosure), Options),
		memberchk(l(LabelClosure), Options),
		memberchk(v(Verbose), Options),
		(	member(rs(Seed), Options) ->
			set_seed(Seed)
		;	get_seed(Seed)
		),
		decompose_quick_check_template(Template, Entity, Operator, Predicate),
		Predicate =.. [Name| Types],
		extend_quick_check_closure(ConditionClosure, Condition),
		extend_quick_check_closure(LabelClosure, Label),
		% we pass both the extend closures and the original closures for improved error reporting
		run_quick_check_tests(1, N, Template, Entity, Operator, Name, Types, MaxShrinks, EdgeCases, Condition, Label, Verbose, Seed, 0, Discarded, [], Labels).

	run_quick_check_tests(Test, N, _Template, _Entity, _Operator, _Name, _Types, _MaxShrinks, _EdgeCases, _Condition, _Label, _Verbose, _Seed, _Discarded, _Discarded, _Labels, _Labels) :-
		Test > N,
		!.
	run_quick_check_tests(Test, N, Template, Entity, Operator, Name, Types, MaxShrinks, EdgeCases, Condition, Label, Verbose, Seed, Discarded0, Discarded, Labels0, Labels) :-
		generate_test(Condition, N, Template, Entity, Operator, Name, Types, Arguments, ArgumentsCopy, Test, EdgeCases, Verbose, Discarded0, Discarded1, Goal),
		(	catch(Goal, Error, quick_check_error(Goal, Template, Test, Seed, Error, Template, Verbose)) ->
				(	check_output_arguments(Types, Arguments, ArgumentsCopy) ->
					Next is Test + 1,
					label_test(Label, Arguments, Labels0, Labels1),
					verbose_report_quick_check_test(Verbose, Goal, Template, passed, information),
					run_quick_check_tests(Next, N, Template, Entity, Operator, Name, Types, MaxShrinks, EdgeCases, Condition, Label, Verbose, Seed, Discarded1, Discarded, Labels1, Labels)
				;	verbose_report_quick_check_test(Verbose, Goal, Template, failure, warning),
					shrink_failed_test(Condition, Types, Goal, Template, Test, 0, MaxShrinks, Verbose, Seed)
				)
			;	verbose_report_quick_check_test(Verbose, Goal, Template, failure, warning),
				shrink_failed_test(Condition, Types, Goal, Template, Test, 0, MaxShrinks, Verbose, Seed)
		).

	label_test(true, _, Labels, Labels).
	% no label closure was specified (as represented internally by the atom "true")
	label_test(Closure-Original, Arguments, Labels0, Labels) :-
		append(Arguments, [TestLabels], FullArguments),
		Goal =.. [call, Closure| FullArguments],
		(	catch(Goal, Error, throw(quick_check_error(label_goal_error(Error), Original))) ->
			count_labels(TestLabels, Labels0, Labels)
		;	throw(quick_check_error(label_goal_failure, Original))
		).

	% support label closures returning a single test label or a list of test labels
	count_labels([], Labels, Labels) :-
		!.
	count_labels([Label| Others], Labels0, Labels) :-
		!,
		count_label(Label, Labels0, Labels1),
		count_labels(Others, Labels1, Labels).
	count_labels(Label, Labels0, Labels) :-
		count_label(Label, Labels0, Labels).

	count_label(Label, Labels0, Labels) :-
		% we use a simple list of pairs for saving the label counts
		% as the typical number of labels is small
		(	select(Label-N, Labels0, Others) ->
			M is N + 1,
			Labels = [Label-M| Others]
		;	Labels = [Label-1| Labels0]
		).

	verbose_report_quick_check_test(false, _Goal, _Template, _Status, _Kind).
	verbose_report_quick_check_test(true, Goal, Template, Status, Kind) :-
		verbose_report_quick_check_test(Goal, Template, Status, Kind).

	verbose_report_quick_check_test(Object<<Goal, _<<_, Status, Kind) :-
		!,
		print_message(Kind, lgtunit, verbose_quick_check_test(Status, Object<<Goal)).
	verbose_report_quick_check_test(_<<Goal, _, Status, Kind) :-
		!,
		print_message(Kind, lgtunit, verbose_quick_check_test(Status, Goal)).
	verbose_report_quick_check_test(Goal, _, Status, Kind) :-
		print_message(Kind, lgtunit, verbose_quick_check_test(Status, Goal)).

	% we need to extract the predicate template from the full template argument
	% to correctly extract the type and mode of the predicate arguments that will
	% be randomly generated
	decompose_quick_check_template(Template, Entity, Operator, Predicate) :-
		(	control_construct(Template, Entity, Operator, Predicate) ->
			true
		;	sender(Sender),
			Entity = Sender, Operator = (<<), Predicate = Template
		).

	control_construct(Object::Template, Object, (::), Template).
	control_construct(Object<<Template, Object, (<<), Template).
	control_construct({Template}, user, (<<), Template).
	control_construct(':'(Module,Template), Module, (:), Template).

	% extend the pre-condition and label closures to ensure that they are called
	% in the correct context: note that we cannot simply use a meta-predicate
	% directive as the number of additional arguments for the closure depend on
	% the template, which is only known at runtime; also note that we use the
	% atom "true" internally to represent that no closure was specified
	extend_quick_check_closure(true, true) :-
		!.
	extend_quick_check_closure(Object::Closure, (Object::Closure)-(Object::Closure)) :-
		!.
	extend_quick_check_closure(Object<<Closure, (Object<<Closure)-(Object<<Closure)) :-
		!.
	extend_quick_check_closure({Closure}, {Closure}-{Closure}) :-
		!.
	extend_quick_check_closure(':'(Module,Closure), (':'(Module,Closure))-(':'(Module,Closure))) :-
		!.
	extend_quick_check_closure(Closure, (Sender<<Closure)-Closure) :-
		sender(Sender).

	generate_test(true, _, _Template, Entity, Operator, Name, Types, Arguments, ArgumentsCopy, Test, EdgeCases, _Verbose, Discarded, Discarded, Goal) :-
		% no pre-condition closure was specified (as represented internally by the atom "true")
		generate_arbitrary_arguments(Types, Arguments, ArgumentsCopy, Test, EdgeCases),
		Predicate =.. [Name| Arguments],
		Goal =.. [Operator, Entity, Predicate].
	generate_test(Closure-Original, N, Template, Entity, Operator, Name, Types, Arguments, ArgumentsCopy, Test, EdgeCases, Verbose, Discarded0, Discarded, Goal) :-
		% use a repeat loop to ensure that we stop trying to generate tests that
		% comply with the given pre-condition if the discarded tests exceed the
		% number of tests that we want to run
		repeat(N, 0, R),
			Test1 is Test + R,
			generate_arbitrary_arguments(Types, Arguments, ArgumentsCopy, Test1, EdgeCases),
			Predicate =.. [Name| Arguments],
			Goal =.. [Operator, Entity, Predicate],
			Condition =.. [call, Closure| Arguments],
		(	catch(Condition, Error, throw(quick_check_error(pre_condition_error(Error), Original))) ->
			!
		;	verbose_report_quick_check_test(Verbose, Goal, Template, discarded, information),
			fail
		),
		Discarded is Discarded0 + R.
	generate_test(_-Original, _N, _Template, _Entity, _Operator, _Name, _Types, _Arguments, _ArgumentsCopy, _Test, _EdgeCases, _Verbose, _Discarded0, _Discarded, _Goal) :-
		throw(quick_check_error(pre_condition_always_fails, Original)).

	% return, along the generated arguments, a copy of those arguments so that
	% we can check that the property being tested don't further instantiates
	% '@'(Type) arguments; but as copies for other argument instantiation modes
	% are not required, only '@'(Type) arguments are actually copied
	generate_arbitrary_arguments([], [], [], _, _).
	generate_arbitrary_arguments([Type| Types], [Argument| Arguments], [ArgumentCopy| ArgumentsCopy], Test, EdgeCases) :-
		generate_arbitrary_argument(Type, Argument, ArgumentCopy, Test, EdgeCases),
		generate_arbitrary_arguments(Types, Arguments, ArgumentsCopy, Test, EdgeCases).

	generate_arbitrary_argument('--'(_), _, _, _, _).
	generate_arbitrary_argument('-'(_), _, _, _, _).
	generate_arbitrary_argument('++'(Type), Arbitrary, _, Test, EdgeCases) :-
		(	type_test_edge_case(ground(Type), Test, Arbitrary, EdgeCases) ->
			true
		;	arbitrary(ground(Type), Arbitrary)
		).
	generate_arbitrary_argument('+'(Type), Arbitrary, _, Test, EdgeCases) :-
		(	type_test_edge_case(Type, Test, Arbitrary, EdgeCases) ->
			true
		;	arbitrary(Type, Arbitrary)
		).
	generate_arbitrary_argument('?'(Type), Arbitrary, _, Test, EdgeCases) :-
		(	type_test_edge_case(Type, Test, Arbitrary, EdgeCases) ->
			true
		;	maybe ->
			arbitrary(var, Arbitrary)
		;	arbitrary(Type, Arbitrary)
		).
	generate_arbitrary_argument('@'(Type), Arbitrary, ArbitraryCopy, Test, EdgeCases) :-
		(	type_test_edge_case(Type, Test, Arbitrary, EdgeCases) ->
			true
		;	arbitrary(Type, Arbitrary)
		),
		copy_term(Arbitrary, ArbitraryCopy).
	generate_arbitrary_argument('{}'(Argument), Argument, _, _, _).

	type_test_edge_case(Type, Test, EdgeCase, true) :-
		findall(Term, edge_case(Type, Term), EdgeCases),
		nth1(Test, EdgeCases, EdgeCase).

	check_output_arguments([], [], []).
	check_output_arguments([Type| Types], [Argument| Arguments], [ArgumentCopy| ArgumentsCopy]) :-
		check_output_argument(Type, Argument, ArgumentCopy),
		check_output_arguments(Types, Arguments, ArgumentsCopy).

	check_output_argument('--'(Type), Argument, _) :-
		valid(Type, Argument).
	check_output_argument('-'(Type), Argument, _) :-
		valid(Type, Argument).
	check_output_argument('++'(_), _, _).
	check_output_argument('+'(Type), Argument, _) :-
		valid(Type, Argument).
	check_output_argument('?'(Type), Argument, _) :-
		valid(Type, Argument).
	check_output_argument('@'(_), Argument, ArgumentCopy) :-
		variant(Argument, ArgumentCopy).
	check_output_argument('{}'(_), _, _).

	shrink_failed_test(true, Types, Goal, Template, Test, Count, MaxShrinks, Verbose, Seed) :-
		(	Count < MaxShrinks ->
			(	shrink_goal(Types, Goal, _, Small),
				catch(\+ Small, _, fail) ->
				verbose_report_quick_check_test(Verbose, Small, Template, shrinked, warning),
				Next is Count + 1,
				shrink_failed_test(true, Types, Small, Template, Test, Next, MaxShrinks, Verbose, Seed)
			;	quick_check_failed(Goal, Template, Test, Count, Seed)
			)
		;	quick_check_failed(Goal, Template, Test, Count, Seed)
		).
	shrink_failed_test(Closure-Original, Types, Goal, Template, Test, Count, MaxShrinks, Verbose, Seed) :-
		(	Count < MaxShrinks ->
			(	shrink_goal(Types, Goal, SmallArguments, Small),
				Condition =.. [call, Closure| SmallArguments],
				catch(Condition, Error, throw(quick_check_error(pre_condition_error(Error), Original))),
				catch(\+ Small, _, fail) ->
				verbose_report_quick_check_test(Verbose, Small, Template, shrinked, warning),
				Next is Count + 1,
				shrink_failed_test(Closure-Original, Types, Small, Template, Test, Next, MaxShrinks, Verbose, Seed)
			;	quick_check_failed(Goal, Template, Test, Count, Seed)
			)
		;	quick_check_failed(Goal, Template, Test, Count, Seed)
		).

	shrink_goal(Types, Large, SmallArguments, Small) :-
		copy_term(Large, LargeCopy),
		decompose_quick_check_template(LargeCopy, Entity, Operator, Goal),
		Goal =.. [Functor| LargeArguments],
		shrink_goal_arguments(Types, LargeArguments, SmallArguments),
		SmallGoal =.. [Functor| SmallArguments],
		Goal \== SmallGoal,
		Small =.. [Operator, Entity, SmallGoal].

	shrink_goal_arguments([], [], []).
	shrink_goal_arguments([Type| Types], [LargeArgument| LargeArguments], [SmallArgument| SmallArguments]) :-
		(	extract_input_type(Type, InputType),
			shrink(InputType, LargeArgument, SmallArgument)
		;	SmallArgument = LargeArgument
		),
		shrink_goal_arguments(Types, LargeArguments, SmallArguments).

	extract_input_type('++'(Type), Type).
	extract_input_type('+'(Type), Type).
	extract_input_type('?'(Type), Type).
	extract_input_type('@'(Type), Type).

	% undo the <</2 control construct if added by the tool itself

	quick_check_error(Object<<Goal, _<<_, Test, Seed, Error, Template, Verbose) :-
		verbose_report_quick_check_test(Verbose, Object<<Goal, Template, error, warning),
		throw(quick_check_error(Error, Object<<Goal, Test, Seed)).
	quick_check_error(_<<Goal, _, Test, Seed, Error, Template, Verbose) :-
		verbose_report_quick_check_test(Verbose, Goal, Template, error, warning),
		throw(quick_check_error(Error, Goal, Test, Seed)).
	quick_check_error(Goal, _, Test, Seed, Error, Template, Verbose) :-
		verbose_report_quick_check_test(Verbose, Goal, Template, error, warning),
		throw(quick_check_error(Error, Goal, Test, Seed)).

	quick_check_failed(Object<<Goal, _<<_, Test, Depth, Seed) :-
		throw(quick_check_failed(Object<<Goal, Test, Depth, Seed)).
	quick_check_failed(_<<Goal, _, Test, Depth, Seed) :-
		throw(quick_check_failed(Goal, Test, Depth, Seed)).
	quick_check_failed(Goal, _, Test, Depth, Seed) :-
		throw(quick_check_failed(Goal, Test, Depth, Seed)).

	% definition taken from the SWI-Prolog documentation
	variant(Term1, Term2) :-
		% avoid trouble in any shared variables
		copy_term(Term1, Term1Copy),
		copy_term(Term2, Term2Copy),
		% ground and compare the term copies
		numbervars(Term1Copy, 0, N),
		numbervars(Term2Copy, 0, N),
		Term1Copy == Term2Copy.

	benchmark(Goal, Time) :-
		os::cpu_time(Time0),
		ignore(Goal),
		os::cpu_time(Time1),
		Time is Time1 - Time0.

	benchmark_reified(Goal, Time, Result) :-
		os::cpu_time(Time0),
		(	catch(Goal, Error, true) ->
			(	var(Error) ->
				Result = success
			;	Result = error(Error)
			)
		;	Result = failure
		),
		os::cpu_time(Time1),
		Time is Time1 - Time0.

	benchmark(Goal, Repetitions, Time) :-
		benchmark_(cpu, Goal, Repetitions, Time).

	benchmark(Goal, Repetitions, Clock, Time) :-
		benchmark_(Clock, Goal, Repetitions, Time).

	:- meta_predicate(benchmark_(*, 0, *, *)).
	benchmark_(cpu, Goal, Repetitions, Time) :-
		os::cpu_time(Time0),
		empty_loop(Repetitions),
		os::cpu_time(Time1),
		goal_loop(Goal, Repetitions),
		os::cpu_time(Time2),
		Time is Time2 - 2*Time1 + Time0.
	benchmark_(wall, Goal, Repetitions, Time) :-
		os::wall_time(Time0),
		empty_loop(Repetitions),
		os::wall_time(Time1),
		goal_loop(Goal, Repetitions),
		os::wall_time(Time2),
		Time is Time2 - 2*Time1 + Time0.

	empty_loop(Repetitions) :-
		repeat(Repetitions),
		once(true),
		fail.
	empty_loop(_).

	:- meta_predicate(goal_loop(0, *)).

	goal_loop(Goal, Repetitions) :-
		repeat(Repetitions),
		once(Goal),
		fail.
	goal_loop(_, _).

	repeat(_).
	repeat(N) :-
		N > 1,
		M is N - 1,
		repeat(M).

	repeat(_, R, R).
	repeat(N, R0, R) :-
		N > 1,
		M is N - 1,
		R1 is R0 + 1,
		repeat(M, R1, R).

	% predicate clause coverage support;
	% it requires entities compiled in debug mode

	:- multifile(logtalk::trace_event/2).
	:- dynamic(logtalk::trace_event/2).

	% the Logtalk runtime calls all defined logtalk::trace_event/2 hooks using
	% a failure-driven loop; thus we don't have to worry about handling all
	% events or failing after handling an event to give other hooks a chance
	logtalk::trace_event(fact(Entity, Fact, N, _, _), _) :-
		fired(Entity, Fact, N).
	logtalk::trace_event(rule(Entity, Head, N, _, _), _) :-
		fired(Entity, Head, N).

	fired(Entity, Other::Head, N) :-
		!,
		functor(Head, Functor, Arity),
		(	fired_(Entity, Other::Functor/Arity, N) ->
			true
		;	atom(Entity) ->
			assertz(fired_(Entity, Other::Functor/Arity, N))
		;	functor(Entity, EntityFunctor, EntityArity),
			functor(Template, EntityFunctor, EntityArity),
			assertz(fired_(Template, Other::Functor/Arity, N))
		).
	fired(_, ':'(_,_), _) :-
		% clauses of Prolog module multifile predicates currently have
		% an index of zero with no associated source data information
		!.
	fired(Entity, Head, N) :-
		functor(Head, Functor, Arity),
		(	fired_(Entity, Functor/Arity, N) ->
			true
		;	atom(Entity) ->
			assertz(fired_(Entity, Functor/Arity, N))
		;	functor(Entity, EntityFunctor, EntityArity),
			functor(Template, EntityFunctor, EntityArity),
			assertz(fired_(Template, Functor/Arity, N))
		).

	reset_coverage_results :-
		retractall(fired_(_, _, _)),
		retractall(covered_(_, _, _, _)).

	write_coverage_results :-
		declared_entities(DeclaredEntities),
		(	DeclaredEntities == [] ->
			print_message(information, lgtunit, no_code_coverage_information_collected)
		;	print_message(information, lgtunit, code_coverage_header),
			write_entity_coverage_results(DeclaredEntities),
			fired_entities(TestedEntities),
			write_coverage_results_summary(DeclaredEntities, TestedEntities)
		).

	write_coverage_results(TestSets) :-
		declared_entities(TestSets, DeclaredEntities),
		(	DeclaredEntities == [] ->
			print_message(information, lgtunit, no_code_coverage_information_collected)
		;	print_message(information, lgtunit, code_coverage_header),
			write_entity_coverage_results(DeclaredEntities),
			fired_entities(TestSets, TestedEntities),
			write_coverage_results_summary(DeclaredEntities, TestedEntities)
		).

	% the same entity can be declared as covered by multiple test sets; the
	% following predicates use the entity indicators to avoid any duplicated
	% code coverage results for parametric entities

	declared_entities(Entities) :-
		findall(
			Name/Arity,
			(::cover(Entity), functor(Entity, Name, Arity)),
			Indicators
		),
		entity_indicators_to_templates(Indicators, Entities).

	declared_entities(TestSets, Entities) :-
		findall(
			Name/Arity,
			(member(TestSet, TestSets), TestSet::cover(Entity), functor(Entity, Name, Arity)),
			Indicators
		),
		entity_indicators_to_templates(Indicators, Entities).

	fired_entities(TestSets, Entities) :-
		findall(
			Name/Arity,
			(member(TestSet, TestSets), TestSet::cover(Entity), fired_entity(Entity), functor(Entity, Name, Arity)),
			Indicators
		),
		entity_indicators_to_templates(Indicators, Entities).

	fired_entities(Entities) :-
		findall(
			Name/Arity,
			(::cover(Entity), fired_entity(Entity), functor(Entity, Name, Arity)),
			Indicators
		),
		entity_indicators_to_templates(Indicators, Entities).

	% we consider objects and categories with no clauses also
	% as "fired" in order to reported them as covered
	fired_entity(Entity) :-
		(	fired_(Entity, _, _) ->
			true
		;	current_object(Entity) ->
			object_property(Entity, number_of_user_clauses(0))
		;	current_category(Entity) ->
			category_property(Entity, number_of_user_clauses(0))
		;	fail
		).

	entity_indicators_to_templates(Indicators, Entities) :-
		sort(Indicators, Sorted),
		findall(
			Entity,
			(	member(Name/Arity, Sorted),
				functor(Entity, Name, Arity)
			),
			Entities
		).

	write_entity_coverage_results([]).
	write_entity_coverage_results([Entity| Entities]) :-
		print_message(silent, lgtunit, entity_coverage_starts(Entity)),
		write_entity_coverage_information(Entity),
		print_message(silent, lgtunit, entity_coverage_ends(Entity)),
		write_entity_coverage_results(Entities).

	% ignore coverage declarations for protocols
	write_entity_coverage_information(Entity) :-
		atom(Entity),
		current_protocol(Entity),
		!,
		print_message(warning, lgtunit, no_code_coverage_for_protocols(Entity)).
	% list entity provided multifile predicates that were called
	write_entity_coverage_information(Entity) :-
		setof(N, fired_(Entity, Other::Functor/Arity, N), Ns),
		entity_indicator_number_of_clauses(Entity, Other::Functor/Arity, PredicateIndicator, Total),
		length(Ns, Covered),
		(	Covered =< Total ->
			assertz(covered_(Entity, Other::Functor/Arity, Covered, Total))
		;	% likely a dynamic predicate with clauses asserted at runtime
			assertz(covered_(Entity, Other::Functor/Arity, Covered, Covered))
		),
		(	Total =:= 0 ->
			% predicate with no clauses
			Percentage is 100.0
		;	Percentage is float(Covered * 100 / Total)
		),
		print_message(information, lgtunit, entity_predicate_coverage(Entity, PredicateIndicator, Covered, Total, Percentage, Ns)),
		fail.
	% list object provided multifile predicates that were never called
	write_entity_coverage_information(Entity) :-
		current_object(Entity),
		object_property(Entity, provides(Functor/Arity, Other, _)),
		\+ fired_(Entity, Other::Functor/Arity, _),
		entity_indicator_number_of_clauses(Entity, Other::Functor/Arity, PredicateIndicator, Total),
		assertz(covered_(Entity, Other::Functor/Arity, 0, Total)),
		(	Total =:= 0 ->
			print_message(information, lgtunit, entity_predicate_coverage(Entity, PredicateIndicator, 0, Total, 100.0, []))
		;	print_message(information, lgtunit, entity_predicate_coverage(Entity, PredicateIndicator, 0, Total, 0, []))
		),
		fail.
	% list category provided multifile predicates that were never called
	write_entity_coverage_information(Entity) :-
		current_category(Entity),
		category_property(Entity, provides(Functor/Arity, Other, _)),
		\+ fired_(Entity, Other::Functor/Arity, _),
		entity_indicator_number_of_clauses(Entity, Other::Functor/Arity, PredicateIndicator, Total),
		assertz(covered_(Entity, Other::Functor/Arity, 0, Total)),
		(	Total =:= 0 ->
			print_message(information, lgtunit, entity_predicate_coverage(Entity, PredicateIndicator, 0, Total, 100.0, []))
		;	print_message(information, lgtunit, entity_predicate_coverage(Entity, PredicateIndicator, 0, Total, 0, []))
		),
		fail.
	% list entity own predicates that were called
	write_entity_coverage_information(Entity) :-
		% do not consider dynamic clauses asserted at runtime (which have an index, N, of zero)
		setof(N, (fired_(Entity, Functor/Arity, N), N > 0), Ns),
		entity_indicator_number_of_clauses(Entity, Functor/Arity, PredicateIndicator, Total),
		length(Ns, Covered),
		(	Covered =< Total ->
			assertz(covered_(Entity, Functor/Arity, Covered, Total))
		;	% likely a dynamic predicate with clauses asserted at runtime
			assertz(covered_(Entity, Functor/Arity, Covered, Covered))
		),
		(	Total =:= 0 ->
			% predicate with no clauses
			Percentage is 100.0
		;	Percentage is float(Covered * 100 / Total)
		),
		print_message(information, lgtunit, entity_predicate_coverage(Entity, PredicateIndicator, Covered, Total, Percentage, Ns)),
		fail.
	% list object own predicates that were never called
	write_entity_coverage_information(Entity) :-
		current_object(Entity),
		object_property(Entity, defines(Functor/Arity, Properties)),
		% do not consider dynamic clauses asserted at runtime (which have an index, N, of zero)
		\+ (fired_(Entity, Functor/Arity, N), N > 0),
		properties_indicator_number_of_clauses(Properties, Functor/Arity, PredicateIndicator, Total),
		assertz(covered_(Entity, Functor/Arity, 0, Total)),
		(	Total =:= 0 ->
			print_message(information, lgtunit, entity_predicate_coverage(Entity, PredicateIndicator, 0, Total, 100.0, []))
		;	print_message(information, lgtunit, entity_predicate_coverage(Entity, PredicateIndicator, 0, Total, 0, []))
		),
		fail.
	% list category own predicates that were never called
	write_entity_coverage_information(Entity) :-
		current_category(Entity),
		category_property(Entity, defines(Functor/Arity, Properties)),
		\+ fired_(Entity, Functor/Arity, _),
		properties_indicator_number_of_clauses(Properties, Functor/Arity, PredicateIndicator, Total),
		assertz(covered_(Entity, Functor/Arity, 0, Total)),
		(	Total =:= 0 ->
			print_message(information, lgtunit, entity_predicate_coverage(Entity, PredicateIndicator, 0, Total, 100.0, []))
		;	print_message(information, lgtunit, entity_predicate_coverage(Entity, PredicateIndicator, 0, Total, 0, []))
		),
		fail.
	% print entity summary coverage statistics
	write_entity_coverage_information(Entity) :-
		covered_entity(Entity, Covered, Total),
		!,
		(	Covered =:= 0, Total =:= 0 ->
			% entity with no clauses
			Percentage is 100.0
		;	Total =:= 0 ->
			% entity not covered
			Percentage is 0.0
		;	% at least partially covered entity
			Percentage is float(Covered * 100 / Total)
		),
		print_message(information, lgtunit, entity_coverage(Entity, Covered, Total, Percentage)).
	% unknown entity
	write_entity_coverage_information(Entity) :-
		print_message(warning, lgtunit, unknown_entity_declared_covered(Entity)).

	entity_indicator_number_of_clauses(Entity, Other::Functor/Arity, PredicateIndicator, NumberOfClauses) :-
		!,
		(	current_object(Entity) ->
			object_property(Entity, provides(Functor/Arity, Other, DefinitionProperties))
		;	current_category(Entity),
			category_property(Entity, provides(Functor/Arity, Other, DefinitionProperties))
		),
		(	member(number_of_clauses(NumberOfClauses), DefinitionProperties) ->
			true
		;	NumberOfClauses = 0
		),
		(	current_object(Other) ->
			object_property(Other, declares(Functor/Arity, DeclarationProperties))
		;	current_category(Other),
			category_property(Other, declares(Functor/Arity, DeclarationProperties))
		),
		(	member(non_terminal(NonTerminal), DeclarationProperties) ->
			PredicateIndicator = Other::NonTerminal
		;	PredicateIndicator = Other::Functor/Arity
		).
	entity_indicator_number_of_clauses(Entity, Functor/Arity, PredicateIndicator, NumberOfClauses) :-
		(	current_object(Entity) ->
			object_property(Entity, defines(Functor/Arity, Properties))
		;	current_category(Entity),
			category_property(Entity, defines(Functor/Arity, Properties))
		),
		properties_indicator_number_of_clauses(Properties, Functor/Arity, PredicateIndicator, NumberOfClauses).

	properties_indicator_number_of_clauses(Properties, PredicateIndicator0, PredicateIndicator, NumberOfClauses) :-
		\+ member(auxiliary, Properties),
		(	member(number_of_clauses(NumberOfClauses), Properties) ->
			true
		;	NumberOfClauses = 0
		),
		(	member(non_terminal(PredicateIndicator), Properties) ->
			true
		;	PredicateIndicator = PredicateIndicator0
		).

	write_coverage_results_summary(DeclaredEntities, TestedEntities) :-
		length(DeclaredEntities, TotalEntities),
		length(TestedEntities, CoveredEntities),
		(	TotalEntities =:= 0 ->
			PercentageEntities is 0.0
		;	PercentageEntities is float(CoveredEntities * 100 / TotalEntities)
		),
		covered_entities(DeclaredEntities, CoveredClauses, TotalClauses),
		(	TotalClauses =:= 0 ->
			PercentageClauses is 0.0
		;	PercentageClauses is float(CoveredClauses * 100 / TotalClauses)
		),
		print_message(information, lgtunit, declared_entities_and_clause_numbers(TotalEntities, TotalClauses)),
		print_message(information, lgtunit, covered_entities_numbers(CoveredEntities, TotalEntities, PercentageEntities)),
		print_message(information, lgtunit, covered_clause_numbers(CoveredClauses, TotalClauses, PercentageClauses)).

	covered_entity(Entity, Coverage, Clauses) :-
		\+ \+ covered_(Entity, _, _, _),
		!,
		findall(Covered-Total, covered_(Entity, _, Covered, Total), List),
		sum_coverage(List, Coverage, Clauses).
	covered_entity(Entity, 0, Clauses) :-
		(	current_object(Entity) ->
			object_property(Entity, number_of_user_clauses(Clauses))
		;	current_category(Entity) ->
			category_property(Entity, number_of_user_clauses(Clauses))
		;	fail
		).

	covered_entities(Entities, Coverage, Clauses) :-
		findall(
			Covered-Total,
			(	member(Entity, Entities),
				covered_entity(Entity, Covered, Total)
			),
			List
		),
		sum_coverage(List, Coverage, Clauses).

	sum_coverage(List, Coverage, Clauses) :-
		sum_coverage(List, 0, Coverage, 0, Clauses).

	sum_coverage([], Coverage, Coverage, Clauses, Clauses).
	sum_coverage([Covered-Total| List], Coverage0, Coverage, Clauses0, Clauses) :-
		Coverage1 is Coverage0 + Covered,
		Clauses1 is Clauses0 + Total,
		sum_coverage(List, Coverage1, Coverage, Clauses1, Clauses).

	% support for suppressing output

	suppress_text_output :-
		null_device_path(Path),
		open(Path, write, Stream, [type(text)]),
		set_output(Stream).

	suppress_binary_output :-
		null_device_path(Path),
		open(Path, write, Stream, [type(binary)]),
		set_output(Stream).

	% support for testing input predicates

	set_text_input(Alias, Contents, Options) :-
		clean_file(Alias, 'test_input.text', Path),
		open(Path, write, WriteStream, [type(text)| Options]),
		write_text_contents(WriteStream, Contents),
		close(WriteStream),
		open(Path, read, _, [type(text),alias(Alias)| Options]).

	set_text_input(Alias, Contents) :-
		set_text_input(Alias, Contents, []).

	set_text_input(Contents) :-
		clean_file('test_input.text', Path),
		open(Path, write, WriteStream, [type(text)]),
		write_text_contents(WriteStream, Contents),
		close(WriteStream),
		open(Path, read, ReadStream, [type(text)]),
		set_input(ReadStream).

	check_text_input(Alias, Expected) :-
		get_text_contents(Alias, Expected, Contents),
		clean_text_input,
		Expected == Contents.

	check_text_input(Expected) :-
		current_input(Stream),
		get_text_contents(Stream, Expected, Contents),
		clean_text_input,
		Expected == Contents.

	text_input_assertion(Alias, Expected, Expected == Contents) :-
		get_text_contents(Alias, Expected, Contents),
		clean_text_input.

	text_input_assertion(Expected, Expected == Contents) :-
		current_input(Stream),
		get_text_contents(Stream, Expected, Contents),
		clean_text_input.

	clean_text_input :-
		clean_file('test_input.text', _).

	set_binary_input(Alias, Bytes, Options) :-
		clean_file(Alias, 'test_input.binary', Path),
		open(Path, write, WriteStream, [type(binary)| Options]),
		write_binary_contents(Bytes, WriteStream),
		close(WriteStream),
		open(Path, read, _, [type(binary),alias(Alias)| Options]).

	set_binary_input(Alias, Bytes) :-
		set_binary_input(Alias, Bytes, []).

	set_binary_input(Bytes) :-
		clean_file('test_input.binary', Path),
		open(Path, write, WriteStream, [type(binary)]),
		write_binary_contents(Bytes, WriteStream),
		close(WriteStream),
		open(Path, read, ReadStream, [type(binary)]),
		set_input(ReadStream).

	check_binary_input(Alias, Expected) :-
		get_binary_contents(Alias, Expected, Contents),
		clean_binary_input,
		Expected == Contents.

	check_binary_input(Expected) :-
		current_input(Stream),
		get_binary_contents(Stream, Expected, Contents),
		clean_binary_input,
		Expected == Contents.

	binary_input_assertion(Alias, Expected, Expected == Contents) :-
		get_binary_contents(Alias, Expected, Contents),
		clean_binary_input.

	binary_input_assertion(Expected, Expected == Contents) :-
		current_input(Stream),
		get_binary_contents(Stream, Expected, Contents),
		clean_binary_input.

	clean_binary_input :-
		clean_file('test_input.binary', _).

	% support for testing output predicates

	set_text_output(Alias, Contents, Options) :-
		clean_file(Alias, 'test_output.text', Path),
		open(Path, write, Stream, [type(text),alias(Alias)| Options]),
		write_text_contents(Stream, Contents).

	set_text_output(Alias, Contents) :-
		set_text_output(Alias, Contents, []).

	set_text_output(Contents) :-
		clean_file('test_output.text', Path),
		open(Path, write, Stream, [type(text)]),
		write_text_contents(Stream, Contents),
		set_output(Stream).

	check_text_output(Alias, Expected, Options) :-
		close(Alias),
		os::absolute_file_name('test_output.text', Path),
		open(Path, read, InputStream, Options),
		get_text_contents(InputStream, Expected, Contents),
		clean_text_output,
		Expected == Contents.

	check_text_output(Alias, Expected) :-
		check_text_output(Alias, Expected, []).

	check_text_output(Expected) :-
		current_output(OutputStream),
		close(OutputStream),
		os::absolute_file_name('test_output.text', Path),
		open(Path, read, InputStream),
		get_text_contents(InputStream, Expected, Contents),
		clean_text_output,
		Expected == Contents.

	text_output_assertion(Alias, Expected, Options, Expected == Contents) :-
		close(Alias),
		os::absolute_file_name('test_output.text', Path),
		open(Path, read, InputStream, Options),
		get_text_contents(InputStream, Expected, Contents),
		clean_text_output.

	text_output_assertion(Alias, Expected, Expected == Contents) :-
		text_output_assertion(Alias, Expected, [], Expected == Contents).

	text_output_assertion(Expected, Expected == Contents) :-
		current_output(OutputStream),
		close(OutputStream),
		os::absolute_file_name('test_output.text', Path),
		open(Path, read, InputStream),
		get_text_contents(InputStream, Expected, Contents),
		clean_text_output.

	text_output_contents(Alias, Contents, Options) :-
		close(Alias),
		os::absolute_file_name('test_output.text', Path),
		open(Path, read, InputStream, Options),
		get_text_contents(InputStream, Contents),
		clean_text_output.

	text_output_contents(Alias, Contents) :-
		close(Alias),
		os::absolute_file_name('test_output.text', Path),
		open(Path, read, InputStream),
		get_text_contents(InputStream, Contents),
		clean_text_output.

	text_output_contents(Contents) :-
		current_output(OutputStream),
		close(OutputStream),
		os::absolute_file_name('test_output.text', Path),
		open(Path, read, InputStream),
		get_text_contents(InputStream, Contents),
		clean_text_output.

	clean_text_output :-
		clean_file('test_output.text', _).

	set_binary_output(Alias, Bytes, Options) :-
		clean_file(Alias, 'test_output.binary', Path),
		open(Path, write, Stream, [type(binary), alias(Alias)| Options]),
		write_binary_contents(Bytes, Stream).

	set_binary_output(Alias, Bytes) :-
		set_binary_output(Alias, Bytes, []).

	set_binary_output(Bytes) :-
		clean_file('test_output.binary', Path),
		open(Path, write, Stream, [type(binary)]),
		write_binary_contents(Bytes, Stream),
		set_output(Stream).

	check_binary_output(Alias, Expected) :-
		close(Alias),
		os::absolute_file_name('test_output.binary', Path),
		open(Path, read, InputStream, [type(binary)]),
		get_binary_contents(InputStream, Expected, Contents),
		clean_binary_output,
		Expected == Contents.

	check_binary_output(Expected) :-
		current_output(OutputStream),
		close(OutputStream),
		os::absolute_file_name('test_output.binary', Path),
		open(Path, read, InputStream, [type(binary)]),
		get_binary_contents(InputStream, Expected, Contents),
		clean_binary_output,
		Expected == Contents.

	binary_output_assertion(Alias, Expected, Expected == Contents) :-
		close(Alias),
		os::absolute_file_name('test_output.binary', Path),
		open(Path, read, InputStream, [type(binary)]),
		get_binary_contents(InputStream, Expected, Contents),
		clean_binary_output.

	binary_output_assertion(Expected, Expected == Contents) :-
		current_output(OutputStream),
		close(OutputStream),
		os::absolute_file_name('test_output.binary', Path),
		open(Path, read, InputStream, [type(binary)]),
		get_binary_contents(InputStream, Expected, Contents),
		clean_binary_output.

	binary_output_contents(Alias, Contents) :-
		close(Alias),
		os::absolute_file_name('test_output.binary', Path),
		open(Path, read, InputStream, [type(binary)]),
		get_binary_contents(InputStream, Contents),
		clean_binary_output.

	binary_output_contents(Contents) :-
		current_output(OutputStream),
		close(OutputStream),
		os::absolute_file_name('test_output.binary', Path),
		open(Path, read, InputStream, [type(binary)]),
		get_binary_contents(InputStream, Contents),
		clean_binary_output.

	clean_binary_output :-
		clean_file('test_output.binary', _).

	% other predicates for testing input/output predicates

	create_text_file(File, Contents, Options) :-
		os::absolute_file_name(File, Path),
		open(Path, write, Stream, Options),
		write_text_contents(Stream, Contents),
		close(Stream).

	create_text_file(File, Contents) :-
		create_text_file(File, Contents, []).

	create_binary_file(File, Bytes) :-
		os::absolute_file_name(File, Path),
		open(Path, write, Stream, [type(binary)]),
		write_binary_contents(Bytes, Stream),
		close(Stream).

	check_text_file(File, Expected, Options) :-
		os::absolute_file_name(File, Path),
		open(Path, read, Stream, Options),
		get_text_contents(Stream, Expected, Contents),
		Expected == Contents.

	check_text_file(File, Expected) :-
		check_text_file(File, Expected, []).

	text_file_assertion(File, Expected, Options, Expected == Contents) :-
		os::absolute_file_name(File, Path),
		open(Path, read, Stream, Options),
		get_text_contents(Stream, Expected, Contents).

	text_file_assertion(File, Expected, Assertion) :-
		text_file_assertion(File, Expected, [], Assertion).

	check_binary_file(File, Expected) :-
		os::absolute_file_name(File, Path),
		open(Path, read, Stream, [type(binary)]),
		get_binary_contents(Stream, Expected, Contents),
		Expected == Contents.

	binary_file_assertion(File, Expected, Expected == Contents) :-
		os::absolute_file_name(File, Path),
		open(Path, read, Stream, [type(binary)]),
		get_binary_contents(Stream, Expected, Contents).

	% auxiliary predicates for testing input/output predicates

	clean_file(File) :-
		clean_file(File, _).

	clean_file(File, Path) :-
		os::absolute_file_name(File, Path),
		% the file can be associated with more than one stream
		forall(
			stream_property(Stream, file_name(Path)),
			close(Stream)
		),
		% the file may exist only if some unit test failed
		(	os::file_exists(Path) ->
			os::delete_file(Path)
		;	true
		).

	clean_file(Alias, File, Path) :-
		% the alias may be associated with a stream opened for a different file
		(	stream_property(Stream, alias(Alias)) ->
			close(Stream)
		;	true
		),
		clean_file(File, Path).

	write_text_contents(Stream, Contents) :-
		(	atom(Contents) ->
			write(Stream, Contents)
		;	write_text_contents_list(Contents, Stream)
		).

	write_text_contents_list([], _).
	write_text_contents_list([Atom| Atoms], Stream) :-
		write(Stream, Atom),
		write_text_contents_list(Atoms, Stream).

	get_text_contents(Stream, Expected, Contents) :-
		atom_length(Expected, Length),
		Limit is Length + 1,
		get_chars(Stream, Chars, Limit),
		atom_chars(Contents, Chars).

	get_chars(Stream, Chars, Countdown) :-
		get_char(Stream, Char),
		(	Char == end_of_file ->
			Chars = [],
			close(Stream)
		;	Countdown =< 0 ->
			Chars = []
		;	Chars = [Char| Rest],
			NextCountdown is Countdown - 1,
			get_chars(Stream, Rest, NextCountdown)
		).

	get_text_contents(Stream, Chars) :-
		get_char(Stream, Char),
		(	Char == end_of_file ->
			Chars = [],
			close(Stream)
		;	Chars = [Char| Rest],
			get_text_contents(Stream, Rest)
		).

	write_binary_contents([], _).
	write_binary_contents([Byte| Bytes], Stream) :-
		put_byte(Stream, Byte),
		write_binary_contents(Bytes, Stream).

	get_binary_contents(Stream, Expected, Bytes) :-
		length(Expected, Length),
		Limit is Length + 1,
		get_bytes(Stream, Bytes, Limit).

	get_bytes(Stream, Bytes, Countdown) :-
		get_byte(Stream, Byte),
		(	Byte == -1 ->
			close(Stream),
			Bytes = []
		;	Countdown =< 0 ->
			Bytes = []
		;	Bytes = [Byte| Rest],
			NextCountdown is Countdown - 1,
			get_bytes(Stream, Rest, NextCountdown)
		).

	get_binary_contents(Stream, Bytes) :-
		get_byte(Stream, Byte),
		(	Byte == -1 ->
			close(Stream),
			Bytes = []
		;	Bytes = [Byte| Rest],
			get_binary_contents(Stream, Rest)
		).

	closed_input_stream(ReadStream, Options) :-
		os::absolute_file_name(temporary_file, Path),
		open(Path, write, WriteStream),
		close(WriteStream),
		open(Path, read, ReadStream, Options),
		close(ReadStream),
		os::delete_file(Path).

	closed_output_stream(WriteStream, Options) :-
		os::absolute_file_name(temporary_file, Path),
		open(Path, write, WriteStream, Options),
		close(WriteStream),
		os::delete_file(Path).

	:- if(current_logtalk_flag(prolog_dialect, sicstus)).

		stream_position(Position) :-
			os::absolute_file_name(temporary_file, Path),
			open(Path, write, Stream),
			stream_position(Stream, Position),
			close(Stream),
			os::delete_file(Path).

	:- else.

		stream_position(Position) :-
			os::absolute_file_name(temporary_file, Path),
			open(Path, write, Stream),
			stream_property(Stream, position(Position)),
			close(Stream),
			os::delete_file(Path).

	:- endif.

:- end_object.
