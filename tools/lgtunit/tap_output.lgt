%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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


% define a flag to allow the logtalk_tester script to pass the
% option to suppress the test file and directory path prefix
:- initialization(
	create_logtalk_flag(suppress_path_prefix, '', [type(atom), keep(true)])
).


:- object(tap_output).

	:- info([
		version is 4:0:1,
		author is 'Paulo Moura',
		date is 2024-04-01,
		comment is 'Intercepts unit test execution messages and outputs a report using the TAP format to the current output stream.',
		remarks is [
			'Usage' - 'Simply load this object before running your tests using the goal ``logtalk_load(lgtunit(tap_output))``.'
		]
	]).

	:- private(generating_/0).
	:- dynamic(generating_/0).
	:- mode(generating_, zero_or_one).
	:- info(generating_/0, [
		comment is 'Flag to detect report in progress when processing two or more test sets as a unified set.'
	]).

	:- private(partial_/1).
	:- dynamic(partial_/1).
	:- mode(partial_(?integer), zero_or_more).
	:- info(partial_/1, [
		comment is 'Cache of total of tests per test set.',
		argnames is ['Count']
	]).

	:- private(test_count_/1).
	:- dynamic(test_count_/1).
	:- mode(test_count_(?integer), zero_or_one).
	:- info(test_count_/1, [
		comment is 'Test counter.',
		argnames is ['Count']
	]).

	test_count(N) :-
		retract(test_count_(N0)),
		N is N0 + 1,
		assertz(test_count_(N)).

	% intercept all messages from the "lgtunit" object while running tests

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, _, lgtunit, _) :-
		ignore(message_hook(Message)).

	% start
	message_hook(running_tests_from_object_file(_, _)) :-
		(	generating_ ->
			true
		;	write('TAP version 13'), nl,
			assertz(generating_),
			retractall(partial_(_)),
			retractall(test_count_(_)),
			assertz(test_count_(0))
		).
	% number of tests
	message_hook(number_of_tests(Total)) :-
		number_codes(Total, Codes),
		atom_codes(TotalAtom, Codes),
		write('1..'), write(TotalAtom), nl.
	% test results summary
	message_hook(tests_results_summary(_, Partial, _, _, _, _, _)) :-
		assertz(partial_(Partial)).
	% stop
	message_hook(tests_end_date_time(_, _, _, _, _, _)) :-
		retractall(generating_).
	% broken step
	message_hook(broken_step(condition, _, Error)) :-
		write('Bail out! Test suite condition unexpected error: '), pretty_print_term(Error), nl.
	message_hook(broken_step(setup, _, Error)) :-
		write('Bail out! Test suite setup unexpected error: '), pretty_print_term(Error), nl.
	% failed step
	message_hook(failed_step(setup, _)) :-
		write('Bail out! Test suite setup failed.'), nl.
	% tests skipped
	message_hook(tests_skipped(_, _, Note)) :-
		write('1..0 # skip'),
		(	Note == '' ->
			true
		;	write(' ('), write(Note), write(')')
		),
		nl.
	% passed test
	message_hook(passed_test(Object, Test, _, _, Note, _, _)) :-
		test_count(N),
		write('ok '), write(N), write(' - '), writeq(Test), write(' @ '), writeq(Object),
		write_test_note(passed, Note).
	% failed test
	message_hook(failed_test(Object, Test, _, _, Reason, Note, _, _)) :-
		test_count(N),
		write('not ok '), write(N), write(' - '), writeq(Test), write(' @ '), writeq(Object),
		write_test_note(failed, Note),
		write_failed_reason_message(Reason).
	% skipped test
	message_hook(skipped_test(Object, Test, _, _, Note)) :-
		test_count(N),
		write('ok '), write(N), write(' - '), writeq(Test), write(' @ '), writeq(Object),
		write(' # skip'),
		write_test_note(skipped, Note).
	% code coverage results
	message_hook(covered_clause_numbers(_, _, Percentage)) :-
		write('  ---'), nl,
		write('  coverage: '), write(Percentage), write('%'), nl,
		write('  ...'), nl, nl.
	message_hook(no_code_coverage_information_collected) :-
		write('  ---'), nl,
		write('  coverage: n/a'), nl,
		write('  ...'), nl, nl.

	write_failed_reason_message(Reason) :-
		write('  ---'), nl,
		write_failed_reason_message_data(Reason),
		write('  ...'), nl.

	write_failed_reason_message_data(success_instead_of_failure) :-
		write('  message: "test goal succeeded but should have failed"'), nl.
	write_failed_reason_message_data(success_instead_of_error(ExpectedError)) :-
		write('  message: "test goal succeeded but should have throw an error"'), nl,
		write('  expected: '), pretty_print_term(ExpectedError), nl.
	write_failed_reason_message_data(failure_instead_of_success) :-
		write('  message: "test goal failed but should have succeeded"'), nl.
	write_failed_reason_message_data(failure_instead_of_error(ExpectedError)) :-
		write('  message: "test goal failed but should have throw an error"'), nl,
		write('  expected: '), pretty_print_term(ExpectedError), nl.
	write_failed_reason_message_data(error_instead_of_failure(Error)) :-
		write('  message: "test goal throws an error but should have failed"'), nl,
		write('  got: '), pretty_print_term(Error), nl.
	write_failed_reason_message_data(error_instead_of_success(Error)) :-
		write('  message: "test goal throws an error but should have succeeded"'), nl,
		write('  got: '), pretty_print_term(Error), nl.
	write_failed_reason_message_data(wrong_error(ExpectedError, Error)) :-
		write('  message: "test goal throws the wrong error"'), nl,
		write('  got: '), pretty_print_term(Error), nl,
		write('  expected: '), pretty_print_term(ExpectedError), nl.
	write_failed_reason_message_data(step_error(Step, Error)) :-
		write('  message: "'), write(Step), write(' goal throws an error but should have succeeded"'), nl,
		write('  got: '), pretty_print_term(Error), nl.
	write_failed_reason_message_data(step_failure(Step)) :-
		write('  message: "'), write(Step), write(' goal failed but should have succeeded"'), nl.

	write_test_note(Result, Note) :-
		(	Note == '' ->
			true
		;	(	atom(Note), sub_atom(Note, 0, _, _, 'todo')
			;	atom(Note), sub_atom(Note, 0, _, _, 'TODO')
			),
			% check that we are not already using a SKIP directive
			Result \== skipped ->
			% write note as a TODO directive
			write(' # '), write(Note)
		;	% write note as a comment after the test description
			write(' ('), write(Note), write(')')
		),
		nl.

	pretty_print_term(Term) :-
		\+ \+ (
			numbervars(Term, 0, _),
			write_term(Term, [numbervars(true), quoted(true)])
		).

:- end_object.
