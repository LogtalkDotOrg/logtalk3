%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
	implements(expanding)).		% built-in protocol for term and goal expansion methods

	:- set_logtalk_flag(debug, off).

	:- info([
		version is 2.7,
		author is 'Paulo Moura',
		date is 2015/10/17,
		comment is 'A unit test framework supporting predicate clause coverage, determinism testing, input/output testing, and multiple test dialects.'
	]).

	:- public(cover/1).
	:- mode(cover(?entity_identifier), zero_or_more).
	:- info(cover/1, [
		comment is 'Declares entities being tested for which code coverage information should be collected.',
		argnames is ['Entity']
	]).

	:- public(run/2).
	:- mode(run(+atom, +atom), one).
	:- info(run/2, [
		comment is 'Runs the unit tests, writing the results to the specified file. Mode can be either "write" (to create a new file) or "append" (to add results to an existing file).',
		argnames is ['File', 'Mode']
	]).

	:- public(run/0).
	:- mode(run, one).
	:- info(run/0, [
		comment is 'Runs the unit tests, writing the results to the current output stream.'
	]).

	:- public(deterministic/1).
	:- meta_predicate(deterministic(0)).
	:- mode(deterministic(+callable), zero_or_one).
	:- info(deterministic/1, [
		comment is 'True if a goal succeeds once without leaving choice-points.',
		argnames is ['Goal']
	]).

	:- public(op(700, xfx, ('=~='))).
	:- public(('=~=')/2).
	:- mode('=~='(+float, +float), zero_or_one).
	:- info(('=~=')/2, [
		comment is 'Compares two floats for approximate equality using 100*epsilon for the absolute error and, if that fails, 99.999% accuracy for the relative error. Handy when writing certain unit tests but the default precision values may not be adequate for all cases.',
		argnames is ['Float1', 'Float2']
	]).

	:- protected(run_tests/0).
	:- mode(run_tests, one).
	:- info(run_tests/0, [
		comment is 'Runs all defined unit tests.'
	]).

	:- protected(run_tests/2).
	:- mode(run_tests(+list(callable), +atom), one).
	:- info(run_tests/2, [
		comment is 'Runs a list of defined tests.',
		argnames is ['Tests', 'File']
	]).

	:- protected(condition/0).
	:- mode(condition, zero_or_one).
	:- info(condition/0, [
		comment is 'Verifies conditions for running the tests. Defaults to the goal true.'
	]).

	:- protected(setup/0).
	:- mode(setup, zero_or_one).
	:- info(setup/0, [
		comment is 'Setup environment before running the test set. Defaults to the goal true.'
	]).

	:- protected(cleanup/0).
	:- mode(cleanup, zero_or_one).
	:- info(cleanup/0, [
		comment is 'Cleanup environment after running the test set. Defaults to the goal true.'
	]).

	:- protected(note/1).
	:- mode(note(?atom), zero_or_one).
	:- info(note/1, [
		comment is 'Note to be printed after the test results. Defaults to the empty atom.'
	]).

	:- protected(set_text_input/3).
	:- mode(set_text_input(+atom, +atom, +list(stream_option)), one).
	:- info(set_text_input/3, [
		comment is 'Creates a temporary file with the given text contents and opens it for reading referenced by the given alias and using the additional options. If not eof_action/1 option is specified, its value will be the default used by the backend compiler.',
		argnames is ['Alias', 'Contents', 'Options']
	]).

	:- protected(set_text_input/2).
	:- mode(set_text_input(+atom, +atom), one).
	:- info(set_text_input/2, [
		comment is 'Creates a temporary file with the given text contents and opens it for reading referenced by the given alias and using the default end-of-file action for the used backend compiler.',
		argnames is ['Alias', 'Contents']
	]).

	:- protected(set_text_input/1).
	:- mode(set_text_input(+atom), one).
	:- info(set_text_input/1, [
		comment is 'Creates a temporary file with the given text contents, opens it for reading using the default end-of-file action for the used backend compiler, and sets the current input stream to the file.',
		argnames is ['Contents']
	]).

	:- protected(check_text_input/2).
	:- mode(check_text_input(+atom, +atom), zero_or_one).
	:- info(check_text_input/2, [
		comment is 'Checks that the temporary file being written have the expected text contents.',
		argnames is ['Alias', 'Contents']
	]).

	:- protected(check_text_input/1).
	:- mode(check_text_input(+atom), zero_or_one).
	:- info(check_text_input/1, [
		comment is 'Checks that the temporary file being written have the expected text contents.',
		argnames is ['Contents']
	]).

	:- protected(clean_text_input/0).
	:- mode(clean_text_input, one).
	:- info(clean_text_input/0, [
		comment is 'Cleans the temporary file used when testing text input.'
	]).

	:- protected(set_binary_input/3).
	:- mode(set_binary_input(+atom, +list(byte), +list(stream_option)), one).
	:- info(set_binary_input/3, [
		comment is 'Creates a temporary file with the given binary contents and opens it for reading referenced by the given alias and using the additional options. If not eof_action/1 option is specified, its value will be the default used by the backend compiler.',
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
		comment is 'Checks that the temporary file (referenced with the given alias) have the expected binary contents.',
		argnames is ['Alias', 'Bytes']
	]).

	:- protected(check_binary_input/1).
	:- mode(check_binary_input(+list(byte)), zero_or_one).
	:- info(check_binary_input/1, [
		comment is 'Checks that the temporary file have the expected binary contents.',
		argnames is ['Bytes']
	]).

	:- protected(clean_binary_input/0).
	:- mode(clean_binary_input, one).
	:- info(clean_binary_input/0, [
		comment is 'Cleans the temporary file used when testing binary input.'
	]).

	:- protected(set_text_output/2).
	:- mode(set_text_output(+atom, +atom), one).
	:- info(set_text_output/2, [
		comment is 'Creates a temporary file with the given text contents and referenced with the given alias.',
		argnames is ['Alias', 'Contents']
	]).

	:- protected(set_text_output/1).
	:- mode(set_text_output(+atom), one).
	:- info(set_text_output/1, [
		comment is 'Creates a temporary file with the given text contents and sets the current output stream to the file.',
		argnames is ['Contents']
	]).

	:- protected(check_text_output/2).
	:- mode(check_text_output(+atom, +atom), zero_or_one).
	:- info(check_text_output/2, [
		comment is 'Checks that the temporary file being written have the expected text contents.',
		argnames is ['Alias', 'Contents']
	]).

	:- protected(check_text_output/1).
	:- mode(check_text_output(+atom), zero_or_one).
	:- info(check_text_output/1, [
		comment is 'Checks that the temporary file being written have the expected text contents.',
		argnames is ['Contents']
	]).

	:- protected(clean_text_output/0).
	:- mode(clean_text_output, one).
	:- info(clean_text_output/0, [
		comment is 'Cleans the temporary file used when testing text output.'
	]).

	:- protected(set_binary_output/2).
	:- mode(set_binary_output(+atom, +list(byte)), one).
	:- info(set_binary_output/2, [
		comment is 'Creates a temporary file with the given binary contents and referenced with the given alias.',
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
		comment is 'Checks that the temporary file (referenced with the given alias) have the expected binary contents.',
		argnames is ['Alias', 'Bytes']
	]).

	:- protected(check_binary_output/1).
	:- mode(check_binary_output(+list(byte)), zero_or_one).
	:- info(check_binary_output/1, [
		comment is 'Checks that the temporary file have the expected binary contents.',
		argnames is ['Bytes']
	]).

	:- protected(clean_binary_output/0).
	:- mode(clean_binary_output, one).
	:- info(clean_binary_output/0, [
		comment is 'Cleans the temporary file used when testing binary output.'
	]).

	:- protected(create_text_file/2).
	:- mode(create_text_file(+atom, +atom), one).
	:- info(create_text_file/2, [
		comment is 'Creates a text file with the given contents.',
		argnames is ['File', 'Contents']
	]).

	:- protected(create_binary_file/2).
	:- mode(create_binary_file(+atom, +list(byte)), one).
	:- info(create_binary_file/2, [
		comment is 'Creates a binary file with the given contents.',
		argnames is ['File', 'Bytes']
	]).

	:- protected(check_text_file/2).
	:- mode(check_text_file(+atom, +atom), zero_or_one).
	:- info(check_text_file/2, [
		comment is 'Checks that the contents of a text file match the expected contents.',
		argnames is ['File', 'Contents']
	]).

	:- protected(check_binary_file/2).
	:- mode(check_binary_file(+atom, +list(byte)), zero_or_one).
	:- info(check_binary_file/2, [
		comment is 'Checks the contents of a binary file match the expected contents.',
		argnames is ['File', 'Bytes']
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

	:- private(test/3).
	:- mode(test(?atom, ?list(variable), ?nonvar), zero_or_more).
	:- info(test/3, [
		comment is 'Specifies a unit test.',
		argnames is ['Identifier', 'Variables', 'Outcome']
	]).

	:- private(test_/2).
	:- dynamic(test_/2).
	:- mode(test_(?atom, ?compound), zero_or_more).
	:- info(test_/2, [
		comment is 'Table of defined tests.',
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

	:- private(fired_/3).
	:- dynamic(fired_/3).
	:- mode(fired_(?entity_identifier, ?predicate_indicator, ?integer), zero_or_more).
	:- info(fired_/3, [
		comment is 'Fired clauses when running the unit tests.',
		argnames is ['Entity', 'Predicate', 'Clause']
	]).

	:- private(covered_/2).
	:- dynamic(covered_/2).
	:- mode(covered_(?integer, ?integer), zero_or_more).
	:- info(covered_/2, [
		comment is 'Auxiliary predicate for collecting statistics on clause coverage.',
		argnames is ['Covered', 'Total']
	]).

	% we use the structured printing mechanism in order to allow unit tests
	% results to be intercepted for alternative reporting by e.g. GUI IDEs
	:- uses(logtalk, [
		print_message/3
	]).

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
		current_output(Output),
		reset_test_counters,
		reset_coverage_results,
		write_tests_header,
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
		set_output(Output).

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

	run_tests([], _).
	run_tests([Test| Tests], File) :-
		% save the current input and output streams
		current_input(Input), current_output(Output),
		run_test(Test, File),
		% restore the current input and output streams
		set_input(Input), set_output(Output),
		run_tests(Tests, File).

	% by default, no tests are defined:
	run_tests :-
		run_tests([], _).

	% test/3 dialect
	run_test(succeeds(Test, Variables, Position, Condition, Setup, Cleanup, Note), File) :-
		(	run_test_condition(Test, Condition, File, Position, Note) ->
			(	run_test_setup(Test, Setup, File, Position, Note) ->
				(	catch(::test(Test, Variables, true), Error, failed_test(Test, File, Position, error_instead_of_success(Error), Note)) ->
					(	var(Error) ->
						passed_test(Test, File, Position, Note)
					;	true
					)
				;	failed_test(Test, File, Position, failure_instead_of_success, Note)
				),
				run_test_cleanup(Test, Cleanup, File, Position)
			;	true
			)
		;	skipped_test(Test, File, Position, Note)
		).
	run_test(deterministic(Test, Variables, Position, Condition, Setup, Cleanup, Note), File) :-
		(	run_test_condition(Test, Condition, File, Position, Note) ->
			(	run_test_setup(Test, Setup, File, Position, Note) ->
				(	catch(::test(Test, Variables, deterministic), Error, failed_test(Test, File, Position, error_instead_of_success(Error), Note)) ->
					(	var(Error) ->
						passed_test(Test, File, Position, Note)
					;	true
					)
				;	failed_test(Test, File, Position, failure_instead_of_success, Note)
				),
				run_test_cleanup(Test, Cleanup, File, Position)
			;	true
			)
		;	skipped_test(Test, File, Position, Note)
		).
	run_test(fails(Test, Variables, Position, Condition, Setup, Cleanup, Note), File) :-
		(	run_test_condition(Test, Condition, File, Position, Note) ->
			(	run_test_setup(Test, Setup, File, Position, Note) ->
				(	catch(::test(Test, Variables, fail), Error, failed_test(Test, File, Position, error_instead_of_failure(Error), Note)) ->
					(	var(Error) ->
						failed_test(Test, File, Position, success_instead_of_failure, Note)
					;	true
					)
				;	passed_test(Test, File, Position, Note)
				),
				run_test_cleanup(Test, Cleanup, File, Position)
			;	true
			)
		;	skipped_test(Test, File, Position, Note)
		).
	run_test(throws(Test, Variables, PossibleErrors, Position, Condition, Setup, Cleanup, Note), File) :-
		(	run_test_condition(Test, Condition, File, Position, Note) ->
			(	run_test_setup(Test, Setup, File, Position, Note) ->
				(	catch(::test(Test, Variables, PossibleErrors), Error, check_error(Test, PossibleErrors, Error, File, Position)) ->
					(	var(Error) ->
						failed_test(Test, File, Position, success_instead_of_error, Note)
					;	true
					)
				;	failed_test(Test, File, Position, failure_instead_of_error, Note)
				),
				run_test_cleanup(Test, Cleanup, File, Position)
			;	true
			)
		;	skipped_test(Test, File, Position, Note)
		).
	% other dialects
	run_test(succeeds(Test, Position), File) :-
		(	catch(::test(Test, _, true), Error, failed_test(Test, File, Position, error_instead_of_success(Error))) ->
			(	var(Error) ->
				passed_test(Test, File, Position)
			;	true
			)
		;	failed_test(Test, File, Position, failure_instead_of_success)
		).
	run_test(deterministic(Test, Position), File) :-
		(	catch(::test(Test, _, deterministic), Error, failed_test(Test, File, Position, error_instead_of_success(Error))) ->
			(	var(Error) ->
				passed_test(Test, File, Position)
			;	true
			)
		;	failed_test(Test, File, Position, failure_instead_of_success)
		).
	run_test(fails(Test, Position), File) :-
		(	catch(::test(Test, _, fail), Error, failed_test(Test, File, Position, error_instead_of_failure(Error))) ->
			(	var(Error) ->
				failed_test(Test, File, Position, success_instead_of_failure)
			;	true
			)
		;	passed_test(Test, File, Position)
		).
	run_test(throws(Test, PossibleErrors, Position), File) :-
		(	catch(::test(Test, _, PossibleErrors), Error, check_error(Test, PossibleErrors, Error, File, Position)) ->
			(	var(Error) ->
				failed_test(Test, File, Position, success_instead_of_error)
			;	true
			)
		;	failed_test(Test, File, Position, failure_instead_of_error)
		).

	run_test(skipped(Test, Position, Note), File) :-
		skipped_test(Test, File, Position, Note).

	run_test(skipped(Test, Position), File) :-
		skipped_test(Test, File, Position).

	check_error(Test, [PossibleError| PossibleErrors], Error, File, Position) :-
		(	member(ExpectedError, [PossibleError| PossibleErrors]),
			subsumes_term(ExpectedError, Error) ->
			passed_test(Test, File, Position)
		;	failed_test(Test, File, Position, wrong_error(PossibleError, Error))
		).

	write_tests_header :-
		self(Self),
		date::today(Year, Month, Day),
		time::now(Hours, Minutes, Seconds),
		print_message(information, lgtunit, tests_start_date_time(Year, Month, Day, Hours, Minutes, Seconds)),
		(	object_property(Self, file(File, Directory)) ->
			atom_concat(Directory, File, Path),
			print_message(information, lgtunit, running_tests_from_object_file(Self, Path))
		;	print_message(information, lgtunit, running_tests_from_object(Self))
		).

	write_tests_results :-
		self(Self),
		::skipped_(Skipped),
		::passed_(Passed),
		::failed_(Failed),
		Total is Skipped + Passed + Failed,
		::note(Note),
		print_message(information, lgtunit, tests_results_summary(Total, Skipped, Passed, Failed, Note)),
		print_message(information, lgtunit, completed_tests_from_object(Self)).

	write_tests_footer :-
		date::today(Year, Month, Day),
		time::now(Hours, Minutes, Seconds),
		print_message(information, lgtunit, tests_end_date_time(Year, Month, Day, Hours, Minutes, Seconds)).

	passed_test(Test, File, Position, Note) :-
		increment_passed_tests_counter,
		print_message(information, lgtunit, passed_test(Test, File, Position, Note)).

	passed_test(Test, File, Position) :-
		passed_test(Test, File, Position, '').

	failed_test(Test, File, Position, Reason, Note) :-
		increment_failed_tests_counter,
		print_message(error, lgtunit, failed_test(Test, File, Position, Reason, Note)).

	failed_test(Test, File, Position, Reason) :-
		failed_test(Test, File, Position, Reason, '').

	skipped_test(Test, File, Position, Note) :-
		increment_skipped_tests_counter,
		print_message(information, lgtunit, skipped_test(Test, File, Position, Note)).

	skipped_test(Test, File, Position) :-
		skipped_test(Test, File, Position, '').

	tests_skipped :-
		self(Object),
		::note(Note),
		print_message(information, lgtunit, tests_skipped(Object, Note)).

	run_test_condition(Test, Goal, File, Position, Note) :-
		% expected either success or failure; error means user error 
		(	Goal == true ->
			true
		;	catch({Goal}, Error, (failed_test(Test,File,Position,step_error(condition,Error),Note), fail))
		).

	run_test_setup(Test, Goal, File, Position, Note) :-
		% expected success; failure or error means user error 
		(	Goal == true ->
			true
		;	catch({Goal}, Error, failed_test(Test,File,Position,step_error(setup,Error),Note)) ->
			(	var(Error) ->
				true
			;	fail
			)
		;	failed_test(Test, File, Position, step_failure(setup), Note),
			fail
		).

	run_test_cleanup(Test, Goal, File, Position) :-
		% expected success; failure or error means user error 
		(	Goal == true ->
			true
		;	catch({Goal}, Error, failed_cleanup(Test,File,Position,error(Error))) ->
			true
		;	failed_cleanup(Test, File, Position, failure)
		).

	broken_step(Step, Error) :-
		self(Self),
		print_message(error, lgtunit, broken_step(Step, Self, Error)).

	failed_step(Step) :-
		self(Self),
		print_message(error, lgtunit, failed_step(Step, Self)).

	failed_cleanup(Test, File, Position, Reason) :-
		print_message(error, lgtunit, failed_cleanup(Test, File, Position, Reason)).

	reset_compilation_counters :-
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

	% different unit test idioms are supported using the term-expansion mechanism;
	% the unit test objects must be loaded using this object as an hook object

	term_expansion((:- Directive), Terms) :-
		% delegate to another predicate to take advantage of first-argument indexing
		directive_expansion(Directive, Terms).

	% skipped tests
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
		callable(Outcome0),
		convert_test_outcome(Outcome0, Goal0, Outcome, Goal),
		check_for_valid_test_identifier(Test),
		logtalk_load_context(term_position, Position),
		term_variables(Options, Variables),
		parse_test_options(Options, Test, Condition, Setup, Cleanup, Note),
		(	Outcome == true ->
			assertz(test_(Test, succeeds(Test, Variables, Position, Condition, Setup, Cleanup, Note)))
		;	Outcome == deterministic ->
			assertz(test_(Test, deterministic(Test, Variables, Position, Condition, Setup, Cleanup, Note)))
		;	Outcome == fail ->
			assertz(test_(Test, fails(Test, Variables, Position, Condition, Setup, Cleanup, Note)))
		;	% errors
			assertz(test_(Test, throws(Test, Variables, Outcome, Position, Condition, Setup, Cleanup, Note)))
		).

	% unit test idiom test/2
	term_expansion((test(Test, Outcome0) :- Goal0), [(test(Test, [], Outcome) :- Goal)]) :-
		callable(Outcome0),
		convert_test_outcome(Outcome0, Goal0, Outcome, Goal),
		check_for_valid_test_identifier(Test),
		logtalk_load_context(term_position, Position),
		(	Outcome == true ->
			assertz(test_(Test, succeeds(Test, Position)))
		;	Outcome == deterministic ->
			assertz(test_(Test, deterministic(Test, Position)))
		;	Outcome == fail ->
			assertz(test_(Test, fails(Test, Position)))
		;	% errors
			assertz(test_(Test, throws(Test, Outcome, Position)))
		).

	% unit test idiom test/1
	term_expansion((test(Test)), [test(Test, [], true)]) :-
		check_for_valid_test_identifier(Test),
		logtalk_load_context(term_position, Position),
		assertz(test_(Test, succeeds(Test, Position))).
	term_expansion((test(Test) :- Goal), [(test(Test, [], true) :- Goal)]) :-
		check_for_valid_test_identifier(Test),
		logtalk_load_context(term_position, Position),
		assertz(test_(Test, succeeds(Test, Position))).

	% unit test idiom succeeds/1 + deterministic/1 + fails/1 + throws/2
	term_expansion((succeeds(Test)), [test(Test, [], true)]) :-
		check_for_valid_test_identifier(Test),
		logtalk_load_context(term_position, Position),
		assertz(test_(Test, succeeds(Test, Position))).
	term_expansion((succeeds(Test) :- Goal), [(test(Test, [], true) :- Goal)]) :-
		check_for_valid_test_identifier(Test),
		logtalk_load_context(term_position, Position),
		assertz(test_(Test, succeeds(Test, Position))).
	term_expansion((deterministic(Test) :- Goal), [(test(Test, [], deterministic) :- lgtunit::deterministic(Goal))]) :-
		check_for_valid_test_identifier(Test),
		logtalk_load_context(term_position, Position),
		assertz(test_(Test, deterministic(Test, Position))).
	term_expansion((fails(Test) :- Goal), [(test(Test, [], fail) :- Goal)]) :-
		check_for_valid_test_identifier(Test),
		logtalk_load_context(term_position, Position),
		assertz(test_(Test, fails(Test, Position))).
	term_expansion((throws(Test, Balls) :- Goal), [(test(Test, [], Errors) :- Goal)]) :-
		check_for_valid_test_identifier(Test),
		(	Balls = [_| _] ->
			Errors = Balls
		;	Errors = [Balls]
		),
		logtalk_load_context(term_position, Position),
		assertz(test_(Test, throws(Test, Errors, Position))).

	% support the deprecated unit/1 predicate which may still be in use in old code
	term_expansion(unit(Entity), [cover(Entity)]).

	% we make sure that context-switching calls are enabled to help the user in
	% debugging failed unit tests
	directive_expansion(
			object(Test, Relation),
			[(:- object(Test, Relation)), (:- set_logtalk_flag(context_switching_calls,allow))]) :-
		reset_compilation_counters.
	directive_expansion(
			object(Test, Relation1, Relation2),
			[(:- object(Test, Relation1, Relation2)), (:- set_logtalk_flag(context_switching_calls,allow))]) :-
		reset_compilation_counters.
	directive_expansion(
			object(Test, Relation1, Relation2, Relation3),
			[(:- object(Test, Relation1, Relation2, Relation3)), (:- set_logtalk_flag(context_switching_calls,allow))]) :-
		reset_compilation_counters.
	directive_expansion(
			object(Test, Relation1, Relation2, Relation3, Relation4),
			[(:- object(Test, Relation1, Relation2, Relation3, Relation4)), (:- set_logtalk_flag(context_switching_calls,allow))]) :-
		reset_compilation_counters.
	directive_expansion(
			object(Test, Relation1, Relation2, Relation3, Relation4, Relation5),
			[(:- object(Test, Relation1, Relation2, Relation3, Relation4, Relation5)), (:- set_logtalk_flag(context_switching_calls,allow))]) :-
		reset_compilation_counters.

	% the discontiguous/1 directives usually required when using some of the
	% unit tests idioms are no longer necessary after term-expanding them 
	directive_expansion(discontiguous(PI), Expansion) :-
		ground(PI),
		filter_discontiguous_directive(PI, Filtered),
		(	Filtered == [] ->
			Expansion = []
		;	Expansion = (:- discontiguous(Filtered))
		).

	% collect all unit test identifiers when reching the end_object/0 directive 
	directive_expansion(end_object, [(run_tests :- ::run_tests(Tests, File)), (:- end_object)]) :-
		findall(Test, retract(test_(_, Test)), Tests),
		logtalk_load_context(source, File).

	filter_discontiguous_directive((PI,PIs), Filtered) :-
		filter_discontiguous_directive(PI, FilteredHead),
		filter_discontiguous_directive(PIs, FilteredTail),
		append(FilteredHead, FilteredTail, Filtered).
	filter_discontiguous_directive([], []).
	filter_discontiguous_directive([PI| PIs], Filtered) :-
		filter_discontiguous_directive(PI, FilteredHead),
		filter_discontiguous_directive(PIs, FilteredTail),
		append(FilteredHead, FilteredTail, Filtered).
	filter_discontiguous_directive(Functor/Arity, Filtered) :-
		(	ignorable_discontiguous_predicate(Functor/Arity) ->
			Filtered = []
		;	Filtered = Functor/Arity
		).

	ignorable_discontiguous_predicate((-)/1).
	ignorable_discontiguous_predicate(test/2).
	ignorable_discontiguous_predicate(test/1).
	ignorable_discontiguous_predicate(succeeds/1).
	ignorable_discontiguous_predicate(deterministic/1).
	ignorable_discontiguous_predicate(fails/1).
	ignorable_discontiguous_predicate(throws/2).

	convert_test_outcome(true, Goal, true, Goal).
	convert_test_outcome(true(Test), Goal, true, (Goal, Test)).
	convert_test_outcome(deterministic, Goal, deterministic, lgtunit::deterministic(Goal)).
	convert_test_outcome(deterministic(Test), Goal, deterministic, (lgtunit::deterministic(Goal), Test)).
	convert_test_outcome(fail, Goal, fail, Goal).
	convert_test_outcome(error(Ball), Goal, [error(Ball,_)], Goal).
	convert_test_outcome(errors(Balls), Goal, Errors, Goal) :-
		map_errors(Balls, Errors).
	convert_test_outcome(ball(Ball), Goal, [Ball], Goal).
	convert_test_outcome(balls(Balls), Goal, Balls, Goal).

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

	check_for_valid_test_identifier(Test) :-
		(	var(Test) ->
			print_message(error, lgtunit, non_instantiated_test_identifier)
		;	test_(Test, _) ->
			print_message(error, lgtunit, repeated_test_identifier(Test))
		;	true
		).

	parse_test_options([], _, Condition, Setup, Cleanup, Note) :-
		(var(Condition) -> Condition = true; true),
		(var(Setup) -> Setup = true; true),
		(var(Cleanup) -> Cleanup = true; true),
		(var(Note) -> Note = ''; true).
	parse_test_options([Option| Options], Test, Condition, Setup, Cleanup, Note) :-
		(	Option = condition(Goal) ->
			compile_test_step_aux_predicate(Test, '_condition', Goal, Condition)
		;	Option = setup(Goal) ->
			compile_test_step_aux_predicate(Test, '_setup', Goal, Setup)
		;	Option = cleanup(Goal) ->
			compile_test_step_aux_predicate(Test, '_cleanup', Goal, Cleanup)
		;	Option = note(Note) ->
			true
		;	% ignore non-recognized options
			true
		),
		parse_test_options(Options, Test, Condition, Setup, Cleanup, Note).

	compile_test_step_aux_predicate(Test, Step, Goal, CompiledHead) :-
		atom_concat(Test, Step, Head),
		logtalk_load_context(entity_name, Entity),
		logtalk::compile_predicate_heads(Head, Entity, CompiledHead, _),
		logtalk::compile_aux_clauses([(Head :- Goal)]).

	:- if((	current_logtalk_flag(prolog_dialect, Dialect),
			(Dialect == b; Dialect == qp; Dialect == swi; Dialect == yap)
	)).
		deterministic(Goal) :-
			setup_call_cleanup(true, Goal, Deterministic = true),
			(	nonvar(Deterministic) ->
				!
			;	!,
				fail
			).
	:- elif((	current_logtalk_flag(prolog_dialect, Dialect),
				(Dialect == cx; Dialect == ji; Dialect == sicstus; Dialect == xsb)
	)).
		deterministic(Goal) :-
			call_cleanup(Goal, Deterministic = true),
			(	var(Deterministic) ->
				!,
				fail
			;	!
			).
	:- elif(current_logtalk_flag(prolog_dialect, gnu)).
		deterministic(Goal) :-
			call_det(Goal, Deterministic),
			!,
			Deterministic == true.
	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).
		deterministic(Goal) :-
			{sepia_kernel:get_cut(Before)},
			call(Goal),
			{sepia_kernel:get_cut(After)},
			!,
			Before == After.
	:- else.
		deterministic(_) :-
			throw(error(resource_error, deterministic/1)).
	:- endif.

	'=~='(Float1, Float2) :-
		(	% first test the absolute error, for meaningful results with numbers very close to zero:
			epsilon(Epsilon), abs(Float1 - Float2) < 100*Epsilon ->
			true
		;	% if that fails, test the relative error (protected by a catch/3 to avoid division errors)
			% by using as the divisor the larger float in order to make argument order irrelevant:
			abs(Float1) > abs(Float2) ->
			catch(abs((Float1 - Float2) / Float1) < 0.00001, _, fail)	% 99.999% accuracy
		;	catch(abs((Float1 - Float2) / Float2) < 0.00001, _, fail)
		).

	:- if((	current_logtalk_flag(prolog_dialect, Dialect),
			(Dialect == swi; Dialect == yap; Dialect == gnu; Dialect == b; Dialect == cx)
	)).
		epsilon(Epsilon) :-
			Epsilon is epsilon.
	:- else.
		epsilon(0.000000000001).
	:- endif.

	% predicate clause coverage support;
	% it requires entities compiled in debug mode

	:- multifile(logtalk::trace_event/2).
	:- dynamic(logtalk::trace_event/2).

	% the Logtalk runtime calls all defined logtalk::trace_event/2 hooks using
	% a failure-driven loop; thus we don't have to worry about handling all
	% events or failing after handling an event to give other hooks a chance
	logtalk::trace_event(fact(Entity, Fact, N, _), _) :-
		fired(Entity, Fact, N).
	logtalk::trace_event(rule(Entity, Head, N, _), _) :-
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
		retractall(fired_(_, _, _)).

	write_coverage_results :-
		(	setof(TestedEntity, fired_entity(TestedEntity), TestedEntities) ->
			print_message(information, lgtunit, code_coverage_header),
			write_coverage_results(TestedEntities),
			setof(DeclaredEntity, ::cover(DeclaredEntity), DeclaredEntities),
			write_coverage_results_summary(DeclaredEntities, TestedEntities)
		;	print_message(information, lgtunit, no_code_coverage_information_collected)
		).

	fired_entity(Entity) :-
		::cover(Entity),
		\+ \+ fired_(Entity, _, _).

	write_coverage_results([]).
	write_coverage_results([Entity| Entities]) :-
		write_entity_coverage_information(Entity),
		write_coverage_results(Entities).

	write_entity_coverage_information(Entity) :-
		% do not consider dynamic clauses asserted at runtime (which have an index of zero)
		setof(N, (fired_(Entity, Other::Functor/Arity, N), N > 0), Ns),
		number_of_clauses(Entity, Other::Functor/Arity, Total),
		length(Ns, Covered),
		(	Covered =< Total ->
			assertz(covered_(Covered, Total))
		;	% likely a dynamic predicate with clauses asserted at runtime
			assertz(covered_(Covered, Covered))
		),
		print_message(information, lgtunit, entity_clause_coverage(Entity, Other::Functor/Arity, Covered/Total, Ns)),
		fail.
	write_entity_coverage_information(Entity) :-
		% do not consider dynamic clauses asserted at runtime (which have an index of zero)
		setof(N, (fired_(Entity, Functor/Arity, N), N > 0), Ns),
		number_of_clauses(Entity, Functor/Arity, Total),
		length(Ns, Covered),
		(	Covered =< Total ->
			assertz(covered_(Covered, Total))
		;	% likely a dynamic predicate with clauses asserted at runtime
			assertz(covered_(Covered, Covered))
		),
		print_message(information, lgtunit, entity_clause_coverage(Entity, Functor/Arity, Covered/Total, Ns)),
		fail.
	write_entity_coverage_information(Entity) :-
		current_object(Entity),
		object_property(Entity, defines(Functor/Arity, Properties)),
		% do not consider dynamic clauses asserted at runtime (which have an index of zero)
		\+ (fired_(Entity, Functor/Arity, N), N > 0),
		memberchk(number_of_clauses(Total), Properties),
		\+ memberchk(auxiliary, Properties),
		assertz(covered_(0, Total)),
		print_message(information, lgtunit, entity_clause_coverage(Entity, Functor/Arity, 0/Total, [])),
		fail.
	write_entity_coverage_information(Entity) :-
		current_category(Entity),
		category_property(Entity, defines(Functor/Arity, Properties)),
		% do not consider dynamic clauses asserted at runtime (which have an index of zero)
		\+ (fired_(Entity, Functor/Arity, N), N > 0),
		memberchk(number_of_clauses(Total), Properties),
		\+ memberchk(auxiliary, Properties),
		assertz(covered_(0, Total)),
		print_message(information, lgtunit, entity_clause_coverage(Entity, Functor/Arity, 0/Total, [])),
		fail.
	write_entity_coverage_information(_).

	number_of_clauses(Entities, Total) :-
		entities_number_of_clauses(Entities, 0, Total).

	entities_number_of_clauses([], Total, Total).
	entities_number_of_clauses([Entity| Entities], Total0, Total) :-
		(	current_object(Entity) ->
			object_property(Entity, number_of_user_clauses(EntityTotal))
		;	current_category(Entity) ->
			category_property(Entity, number_of_user_clauses(EntityTotal))
		;	% protocol
			EntityTotal = 0
		),
		Total1 is Total0 + EntityTotal,
		entities_number_of_clauses(Entities, Total1, Total).

	number_of_clauses(Entity, Other::Functor/Arity, Total) :-
		current_object(Entity),
		object_property(Entity, provides(Functor/Arity, Other, Properties)),
		memberchk(number_of_clauses(Total), Properties),
		!.
	number_of_clauses(Entity, Functor/Arity, Total) :-
		current_object(Entity),
		object_property(Entity, defines(Functor/Arity, Properties)),
		memberchk(number_of_clauses(Total), Properties),
		!.
	number_of_clauses(Entity, Other::Functor/Arity, Total) :-
		current_category(Entity),
		category_property(Entity, provides(Functor/Arity, Other, Properties)),
		memberchk(number_of_clauses(Total), Properties),
		!.
	number_of_clauses(Entity, Functor/Arity, Total) :-
		current_category(Entity),
		category_property(Entity, defines(Functor/Arity, Properties)),
		memberchk(number_of_clauses(Total), Properties),
		!.
	number_of_clauses(_, _, 0).

	write_coverage_results_summary(DeclaredEntities, TestedEntities) :-
		length(DeclaredEntities, TotalDeclaredEntities),
		number_of_clauses(DeclaredEntities, TotalClauses),
		length(TestedEntities, TotalTestedEntities),
		%number_of_clauses(TestedEntities, Clauses),
		covered(Coverage, TestedClauses),
		Percentage is Coverage * 100 / TotalClauses,
		print_message(information, lgtunit, declared_entities_and_clause_numbers(TotalDeclaredEntities, TotalClauses)),
		print_message(information, lgtunit, covered_entities_and_clause_numbers(TotalTestedEntities, TestedClauses)),
		print_message(information, lgtunit, covered_clause_numbers(Coverage, TotalClauses, Percentage)).

	covered(Coverage, Clauses) :-
		findall(Covered-Total, retract(covered_(Covered, Total)), List),
		sum_coverage(List, Coverage, Clauses).

	sum_coverage(List, Coverage, Clauses) :-
		sum_coverage(List, 0, Coverage, 0, Clauses).

	sum_coverage([], Coverage, Coverage, Clauses, Clauses).
	sum_coverage([Covered-Total| List], Coverage0, Coverage, Clauses0, Clauses) :-
		Coverage1 is Coverage0 + Covered,
		Clauses1 is Clauses0 + Total,
		sum_coverage(List, Coverage1, Coverage, Clauses1, Clauses).

	% support for testing input predicates

	set_text_input(Alias, Contents, Options) :-
		clean_file(Alias, 'test_input.text', Path),
		open(Path, write, WriteStream, [type(text)]),
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

	check_text_input(Alias, Atom) :-
		get_text_contents(Alias, Contents),
		clean_text_input,
		Atom == Contents.

	check_text_input(Atom) :-
		current_input(Stream),
		get_text_contents(Stream, Contents),
		clean_text_input,
		Atom == Contents.

	clean_text_input :-
		clean_file('test_input.text', _).

	set_binary_input(Alias, Bytes, Options) :-
		clean_file(Alias, 'test_input.binary', Path),
		open(Path, write, WriteStream, [type(binary)]),
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

	check_binary_input(Alias, Bytes) :-
		get_binary_contents(Alias, Contents),
		clean_binary_input,
		Bytes == Contents.

	check_binary_input(Bytes) :-
		current_input(Stream),
		get_binary_contents(Stream, Contents),
		clean_binary_input,
		Bytes == Contents.

	clean_binary_input :-
		clean_file('test_input.binary', _).

	% support for testing output predicates

	set_text_output(Alias, Contents) :-
		clean_file(Alias, 'test_output.text', Path),
		open(Path, write, Stream, [type(text),alias(Alias)]),
		write_text_contents(Stream, Contents).

	set_text_output(Contents) :-
		clean_file('test_output.text', Path),
		open(Path, write, Stream, [type(text)]),
		write_text_contents(Stream, Contents),
		set_output(Stream).

	check_text_output(Alias, Atom) :-
		close(Alias),
		os::expand_path('test_output.text', Path),
		open(Path, read, InputStream),
		get_text_contents(InputStream, Contents),
		clean_text_output,
		Atom == Contents.

	check_text_output(Atom) :-
		current_output(OutputStream),
		close(OutputStream),
		os::expand_path('test_output.text', Path),
		open(Path, read, InputStream),
		get_text_contents(InputStream, Contents),
		clean_text_output,
		Atom == Contents.

	clean_text_output :-
		clean_file('test_output.text', _).

	set_binary_output(Alias, Bytes) :-
		clean_file(Alias, 'test_output.binary', Path),
		open(Path, write, Stream, [type(binary), alias(Alias)]),
		write_binary_contents(Bytes, Stream).

	set_binary_output(Bytes) :-
		clean_file('test_output.binary', Path),
		open(Path, write, Stream, [type(binary)]),
		write_binary_contents(Bytes, Stream),
		set_output(Stream).

	check_binary_output(Alias, Bytes) :-
		close(Alias),
		os::expand_path('test_output.binary', Path),
		open(Path, read, InputStream, [type(binary)]),
		get_binary_contents(InputStream, Contents),
		clean_binary_output,
		Bytes == Contents.

	check_binary_output(Bytes) :-
		current_output(OutputStream),
		close(OutputStream),
		os::expand_path('test_output.binary', Path),
		open(Path, read, InputStream, [type(binary)]),
		get_binary_contents(InputStream, Contents),
		clean_binary_output,
		Bytes == Contents.

	clean_binary_output :-
		clean_file('test_output.binary', _).

	% other predicates for testing input/output predicates

	create_text_file(File, Contents) :-
		os::expand_path(File, Path),
		open(Path, write, Stream),
		write_text_contents(Stream, Contents),
		close(Stream).

	create_binary_file(File, Bytes) :-
		os::expand_path(File, Path),
		open(Path, write, Stream, [type(binary)]),
		write_binary_contents(Bytes, Stream),
		close(Stream).

	check_text_file(File, ExpectedContents) :-
		os::expand_path(File, Path),
		open(Path, read, Stream),
		get_text_contents(Stream, ActualContents),
		ExpectedContents == ActualContents.

	check_binary_file(File, ExpectedContents) :-
		os::expand_path(File, Path),
		open(Path, read, Stream, [type(binary)]),
		get_binary_contents(Stream, ActualContents),
		ExpectedContents == ActualContents.

	% auxiliary predicates for testing input/output predicates

	clean_file(File) :-
		clean_file(File, _).

	clean_file(File, Path) :-
		os::expand_path(File, Path),
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

	get_text_contents(Stream, Atom) :-
		get_chars(Stream, Chars, 1000),
		atom_chars(Atom, Chars).

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

	write_binary_contents([], _).
	write_binary_contents([Byte| Bytes], Stream) :-
		put_byte(Stream, Byte),
		write_binary_contents(Bytes, Stream).

	get_binary_contents(Stream, Bytes) :-
		get_bytes(Stream, Bytes, 1000).

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

	closed_input_stream(ReadStream, Options) :-
		os::expand_path(temporary_file, Path),
		open(Path, write, WriteStream),
		close(WriteStream),
		open(Path, read, ReadStream, Options),
		close(ReadStream),
		os::delete_file(Path).

	closed_output_stream(WriteStream, Options) :-
		os::expand_path(temporary_file, Path),
		open(Path, write, WriteStream, Options),
		close(WriteStream),
		os::delete_file(Path).

	stream_position(Position) :-
		os::expand_path(temporary_file, Path),
		open(Path, write, Stream, [reposition(true)]),
		stream_property(Stream, position(Position)),
		close(Stream),
		os::delete_file(Path).

	% auxiliary predicates; we could use the Logtalk standard library but we
	% prefer to minimize this object dependencies given its testing purpose

	append([], List, List).
	append([Head| Tail], List, [Head| Tail2]) :-
		append(Tail, List, Tail2).

	length(List, Length) :-
		length(List, 0, Length).

	length([], Length, Length).
	length([_| Tail], Acc, Length) :-
		Acc2 is Acc + 1,
		length(Tail, Acc2, Length).

	member(Element, [Element| _]).
	member(Element, [_| List]) :-
		member(Element, List).

	memberchk(Element, [Element| _]) :-
		!.
	memberchk(Element, [_| List]) :-
		memberchk(Element, List).

:- end_object.
