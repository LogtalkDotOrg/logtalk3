%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


:- object(tap_output).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2016/02/23,
		comment is 'Intercepts and replaces unit test execution messages, converting them to the TAP output format.'
	]).

	% intercept all messages from the "lgtunit" object while running tests

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, _, lgtunit, _) :-
		ignore(message_hook(Message)).

	% start
	message_hook(tests_start_date_time(_, _, _, _, _, _)) :-
		write('TAP version 13'), nl.
	% stop
	message_hook(tests_results_summary(Total, _, _, _, _)) :-
		number_codes(Total, Codes),
		atom_codes(TotalAtom, Codes),
		write('1..'), write(TotalAtom), nl.
	% broken step
	message_hook(broken_step(condition, _, Error)) :-
		write('Bail out! Test suite condition unexpected error: '), pretty_print_term(Error), nl.
	message_hook(broken_step(setup, _, Error)) :-
		write('Bail out! Test suite setup unexpected error: '), pretty_print_term(Error), nl.
	% failed step
	message_hook(failed_step(setup, _)) :-
		write('Bail out! Test suite setup failed.'), nl.
	% tests skipped
	message_hook(tests_skipped(_, Note)) :-
		write('1..0 # skip'),
		(	Note == '' ->
			true
		;	write(' ('), write(Note), write(')')
		),
		nl.
	% passed test
	message_hook(passed_test(Test, _, _, Note)) :-
		write('ok - '), write(Test),
		(	Note == '' ->
			true
		;	write(' ('), write(Note), write(')')
		),
		nl.
	% failed test
	message_hook(failed_test(Test, _, _, Reason, Note)) :-
		write('not ok - '), write(Test),
		(	Note == '' ->
			true
		;	write(' ('), write(Note), write(')')
		),
		nl,
		write_failed_reason_message(Reason).
	% skipped test
	message_hook(skipped_test(Test, _, _, Note)) :-
		write('ok - # skip '), write(Test),
		(	Note == '' ->
			true
		;	write(' ('), write(Note), write(')')
		),
		nl.
	% code coverage results
	message_hook(covered_clause_numbers(_, _, Percentage)) :-
		write(' ---'), nl,
		write(' coverage: '), write(Percentage), write('%'), nl,
		write(' ...'), nl, nl.
	message_hook(no_code_coverage_information_collected) :-
		write(' ---'), nl,
		write(' coverage: n/a'), nl,
		write(' ...'), nl, nl.

	write_failed_reason_message(Reason) :-
		write(' ---'), nl,
		write_failed_reason_message_data(Reason),
		write(' ...'), nl.

	write_failed_reason_message_data(success_instead_of_failure) :-
		write(' message: "test goal succeeded but should have failed"'), nl.
	write_failed_reason_message_data(success_instead_of_error) :-
		write(' message: "test goal succeeded but should have throw an error"'), nl.
	write_failed_reason_message_data(failure_instead_of_success) :-
		write(' message: "test goal failed but should have succeeded"'), nl.
	write_failed_reason_message_data(failure_instead_of_error) :-
		write(' message: "test goal failed but should have throw an error"'), nl.
	write_failed_reason_message_data(error_instead_of_failure(Error)) :-
		write(' message: "test goal throws an error but should have failed"'), nl,
		write(' got: '), pretty_print_term(Error), nl.
	write_failed_reason_message_data(error_instead_of_success(Error)) :-
		write(' message: "test goal throws an error but should have succeeded"'), nl,
		write(' got: '), pretty_print_term(Error), nl.
	write_failed_reason_message_data(wrong_error(ExpectedError, Error)) :-
		write(' message: "test goal throws the wrong error"'), nl,
		write(' got: '), pretty_print_term(Error), nl,
		write(' expected: '), pretty_print_term(ExpectedError), nl.
	write_failed_reason_message_data(step_error(Step, Error)) :-
		write(' message: "'), write(Step), write(' goal throws an error but should have succeeded"'), nl,
		write(' got: '), pretty_print_term(Error), nl.
	write_failed_reason_message_data(step_failure(Step)) :-
		write(' message: "'), write(Step), write(' goal failed but should have succeeded"'), nl.

	pretty_print_term(Term) :-
		\+ \+ (
			numbervars(Term, 0, _),
			write_term(Term, [numbervars(true), quoted(true)])
		).

:- end_object.
