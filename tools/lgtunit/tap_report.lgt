%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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
% option to supress the test file and directory path prefix
:- initialization(
	create_logtalk_flag(suppress_path_prefix, '', [type(atom), keep(true)])
).


:- object(tap_report).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2017/12/15,
		comment is 'Intercepts unit test execution messages and generates a tap_report.txt file using the TAP output format in the same directory as the tests object file.'
	]).

	:- private(partial_/1).
	:- dynamic(partial_/1).
	:- mode(partial_(?integer), zero_or_more).
	:- info(partial_/1, [
		comment is 'Cache of total of tests per test set.'
	]).

	% intercept all messages from the "lgtunit" object while running tests

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, _, lgtunit, _) :-
		message_hook(Message),
		% allow default processing of the messages
		fail.

	% start
	message_hook(running_tests_from_object_file(_, File)) :-
		logtalk::loaded_file_property(File, directory(Directory)),
		atom_concat(Directory, 'tap_report.txt', ReportFile),
		(	stream_property(_, alias(tap_report)) ->
			true
		;	open(ReportFile, write, _, [alias(tap_report)]),
			write(tap_report, 'TAP version 13'), nl(tap_report),
			retractall(partial_(_))
		).
	% test results summary
	message_hook(tests_results_summary(_, Partial, _, _, _, _)) :-
		assertz(partial_(Partial)).
	% stop
	message_hook(tests_end_date_time(_, _, _, _, _, _)) :-
		close(tap_report).
	% broken step
	message_hook(broken_step(condition, _, Error)) :-
		write(tap_report, 'Bail out! Test suite condition unexpected error: '), pretty_print_term(Error), nl(tap_report).
	message_hook(broken_step(setup, _, Error)) :-
		write(tap_report, 'Bail out! Test suite setup unexpected error: '), pretty_print_term(Error), nl(tap_report).
	% failed step
	message_hook(failed_step(setup, _)) :-
		write(tap_report, 'Bail out! Test suite setup failed.'), nl(tap_report).
	% tests skipped
	message_hook(tests_skipped(_, Note)) :-
		write(tap_report, '1..0 # skip'),
		(	Note == '' ->
			true
		;	write(tap_report, ' ('), write(tap_report, Note), write(tap_report, ')')
		),
		nl(tap_report).
	% passed test
	message_hook(passed_test(Object, Test, _, _, Note)) :-
		write(tap_report, 'ok '),
		writeq(tap_report, Test), write(tap_report, ' @ '), writeq(tap_report, Object),
		write_test_note(passed, Note).
	% failed test
	message_hook(failed_test(Object, Test, _, _, Reason, Note)) :-
		write(tap_report, 'not ok '),
		writeq(tap_report, Test), write(tap_report, ' @ '), writeq(tap_report, Object),
		write_test_note(failed, Note),
		write_failed_reason_message(Reason).
	% skipped test
	message_hook(skipped_test(Object, Test, _, _, Note)) :-
		write(tap_report, 'ok '),
		writeq(tap_report, Test), write(tap_report, ' @ '), writeq(tap_report, Object),
		write(tap_report, ' # skip'),
		write_test_note(skipped, Note).
	% code coverage results
	message_hook(code_coverage_header) :-
		findall(Partial, partial_(Partial), Partials),
		sum(Partials, Total),
		number_codes(Total, Codes),
		atom_codes(TotalAtom, Codes),
		write(tap_report, '1..'), write(tap_report, TotalAtom), nl(tap_report).
	message_hook(covered_clause_numbers(_, _, Percentage)) :-
		write(tap_report, '  ---'), nl(tap_report),
		write(tap_report, '  coverage: '), write(tap_report, Percentage), write(tap_report, '%'), nl(tap_report),
		write(tap_report, '  ...'), nl(tap_report), nl(tap_report).
	message_hook(no_code_coverage_information_collected) :-
		write(tap_report, '  ---'), nl(tap_report),
		write(tap_report, '  coverage: n/a'), nl(tap_report),
		write(tap_report, '  ...'), nl(tap_report), nl(tap_report).

	write_failed_reason_message(Reason) :-
		write(tap_report, '  ---'), nl(tap_report),
		write_failed_reason_message_data(Reason),
		write(tap_report, '  ...'), nl(tap_report).

	write_failed_reason_message_data(success_instead_of_failure) :-
		write(tap_report, '  message: "test goal succeeded but should have failed"'), nl(tap_report).
	write_failed_reason_message_data(success_instead_of_error) :-
		write(tap_report, '  message: "test goal succeeded but should have throw an error"'), nl(tap_report).
	write_failed_reason_message_data(failure_instead_of_success) :-
		write(tap_report, '  message: "test goal failed but should have succeeded"'), nl(tap_report).
	write_failed_reason_message_data(failure_instead_of_error) :-
		write(tap_report, '  message: "test goal failed but should have throw an error"'), nl(tap_report).
	write_failed_reason_message_data(error_instead_of_failure(Error)) :-
		write(tap_report, '  message: "test goal throws an error but should have failed"'), nl(tap_report),
		write(tap_report, '  got: '), pretty_print_term(Error), nl(tap_report).
	write_failed_reason_message_data(error_instead_of_success(Error)) :-
		write(tap_report, '  message: "test goal throws an error but should have succeeded"'), nl(tap_report),
		write(tap_report, '  got: '), pretty_print_term(Error), nl(tap_report).
	write_failed_reason_message_data(wrong_error(ExpectedError, Error)) :-
		write(tap_report, '  message: "test goal throws the wrong error"'), nl(tap_report),
		write(tap_report, '  got: '), pretty_print_term(Error), nl(tap_report),
		write(tap_report, '  expected: '), pretty_print_term(ExpectedError), nl(tap_report).
	write_failed_reason_message_data(step_error(Step, Error)) :-
		write(tap_report, '  message: "'), write(tap_report, Step), write(tap_report, ' goal throws an error but should have succeeded"'), nl(tap_report),
		write(tap_report, '  got: '), pretty_print_term(Error), nl(tap_report).
	write_failed_reason_message_data(step_failure(Step)) :-
		write(tap_report, '  message: "'), write(tap_report, Step), write(tap_report, ' goal failed but should have succeeded"'), nl(tap_report).

	write_test_note(Result, Note) :-
		(	Note == '' ->
			true
		;	(	atom(Note), sub_atom(Note, 0, _, _, 'todo')
			;	atom(Note), sub_atom(Note, 0, _, _, 'TODO')
			),
			% check that we are not already using a SKIP directive
			Result \== skipped ->
			% write note as a TODO directive
			write(tap_report, ' # '), write(tap_report, Note)
		;	% write note as a comment after the test description
			write(tap_report, ' ('), write(tap_report, Note), write(tap_report, ')')
		),
		nl(tap_report).

	pretty_print_term(Term) :-
		\+ \+ (
			numbervars(Term, 0, _),
			write_term(tap_report, Term, [numbervars(true), quoted(true)])
		).

	sum(List, Sum) :-
		sum(List, 0, Sum).

	sum([], Sum, Sum).
	sum([N| Ns], Sum0, Sum) :-
		Sum1 is Sum0 + N,
		sum(Ns, Sum1, Sum).

:- end_object.
