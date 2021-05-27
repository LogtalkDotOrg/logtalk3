%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(minimal_output).

	:- info([
		version is 3:0:0,
		author is 'Paulo Moura',
		date is 2021-05-27,
		comment is 'Intercepts unit test execution messages and outputs a minimal report.',
		remarks is [
			'Usage' - 'Simply load this object before running your tests using the goal ``logtalk_load(lgtunit(minimal_output))``.',
			'Limitations' - 'Cannot be used when the test objects also intercept ``lgtunit`` messages.'
		]
	]).

	% intercept all messages from the "lgtunit" object while running tests

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, _, lgtunit, _) :-
		ignore(message_hook(Message)).

	% test results summary
	message_hook(tests_results_summary(Object, Total, Skipped, Passed, Failed, Flaky, Note)) :-
		nl,
		writeq(Object), write(' suite: '),
		write(Total), write(' tests, '),
		write(Skipped), write(' skipped, '),
		write(Passed), write(' passed, '),
		write(Failed), write(' failed ('), write(Flaky), write(' flaky)'),
		nl,
		(	Note == '' ->
			true
		;	write('('), write(Note), write(')'),
			nl
		).
	% broken step
	message_hook(broken_step(condition, _, Error)) :-
		nl,
		write('! Test suite condition unexpected error: '), pretty_print_term(Error), nl.
	message_hook(broken_step(setup, _, Error)) :-
		nl,
		write('! Test suite setup unexpected error: '), pretty_print_term(Error), nl.
	% failed step
	message_hook(failed_step(setup, _)) :-
		nl,
		write('! Test suite setup failed.'), nl.
	% passed test
	message_hook(passed_test(_, _, _, _, _)) :-
		write('.').
	% failed test
	message_hook(failed_test(Object, Test, _, _, Reason, Note)) :-
		nl,
		write('! '), writeq(Test), write(' @ '), writeq(Object), write(' failed'),
		(	Note == '' ->
			true
		;	write('!   ('), write(Note), write(')')
		),
		nl,
		write_failed_reason_message_data(Reason).
	% skipped test
	message_hook(skipped_test(_, _, _, _, _)) :-
		write('-').
	% code coverage results
	message_hook(covered_clause_numbers(_, _, Percentage)) :-
		write('coverage: '), write(Percentage), write('%'), nl, nl.
	message_hook(no_code_coverage_information_collected) :-
		write('coverage: n/a'), nl, nl.

	write_failed_reason_message_data(success_instead_of_failure) :-
		write('!   test goal succeeded but should have failed'), nl.
	write_failed_reason_message_data(success_instead_of_error(ExpectedError)) :-
		write('!   test goal succeeded but should have throw an error'), nl,
		write('!   expected: '), pretty_print_term(ExpectedError), nl.
	write_failed_reason_message_data(failure_instead_of_success) :-
		write('!   test goal failed but should have succeeded'), nl.
	write_failed_reason_message_data(failure_instead_of_error(ExpectedError)) :-
		write('!   test goal failed but should have throw an error'), nl,
		write('!   expected: '), pretty_print_term(ExpectedError), nl.
	write_failed_reason_message_data(error_instead_of_failure(Error)) :-
		write('!   test goal throws an error but should have failed'), nl,
		write('!   got: '), pretty_print_term(Error), nl.
	write_failed_reason_message_data(error_instead_of_success(Error)) :-
		write('!   test goal throws an error but should have succeeded'), nl,
		write('!   got: '), pretty_print_term(Error), nl.
	write_failed_reason_message_data(wrong_error(ExpectedError, Error)) :-
		write('!   test goal throws the wrong error'), nl,
		write('!   got: '), pretty_print_term(Error), nl,
		write('!   expected: '), pretty_print_term(ExpectedError), nl.
	write_failed_reason_message_data(step_error(Step, Error)) :-
		write('!   '), write(Step), write(' goal throws an error but should have succeeded'), nl,
		write('!   got: '), pretty_print_term(Error), nl.
	write_failed_reason_message_data(step_failure(Step)) :-
		write('!   '), write(Step), write(' goal failed but should have succeeded'), nl.

	pretty_print_term(Term) :-
		\+ \+ (
			numbervars(Term, 0, _),
			write_term(Term, [numbervars(true), quoted(true)])
		).

:- end_object.
