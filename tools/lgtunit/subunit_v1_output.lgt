%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(subunit_v1_output).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-02,
		comment is 'Intercepts unit test execution messages and outputs a Subunit v1 text stream to the current output stream.',
		remarks is [
			'Usage' - 'Simply load this object before running your tests using the goal ``logtalk_load(lgtunit(subunit_v1_output))``.'
		]
	]).

	% intercept all messages from the "lgtunit" object while running tests

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, _, lgtunit, _) :-
		ignore(message_hook(Message)).

	% passed test
	message_hook(passed_test(Object, Test, _, _, Flaky, _, _, _)) :-
		write_test_completion(Object, Test),
		write_test_tags(Flaky),
		write_test_outcome(success, Object, Test).
	% failed test
	message_hook(failed_test(Object, Test, _, _, Reason, Flaky, _, _, _)) :-
		write_test_completion(Object, Test),
		write_test_tags(Flaky),
		write_test_outcome_with_details(failure, Object, Test, Reason).
	% skipped test
	message_hook(skipped_test(Object, Test, _, _, Flaky, Note)) :-
		write_test_completion(Object, Test),
		write_test_tags(Flaky),
		write_test_outcome_with_note(skip, Object, Test, Note).
	% tests skipped
	message_hook(tests_skipped(Object, _, Note)) :-
		forall(
			Object::test(Test),
			(   write_test_completion(Object, Test),
				write_test_outcome_with_note(skip, Object, Test, Note)
			)
		).
	% broken steps
	message_hook(broken_step(condition, Object, Error)) :-
		write_suite_error(Object, condition, Error).
	message_hook(broken_step(setup, Object, Error)) :-
		write_suite_error(Object, setup, Error).
	message_hook(broken_step(cleanup, Object, Error)) :-
		write_suite_error(Object, cleanup, Error).
	message_hook(failed_step(setup, Object)) :-
		write_suite_failure(Object, setup).
	message_hook(failed_step(cleanup, Object)) :-
		write_suite_failure(Object, cleanup).
	% ignore remaining messages
	message_hook(_).

	write_test_completion(Object, Test) :-
		write_time_directive,
		write('test: '),
		write_test_id(Object, Test),
		nl.

	write_test_outcome(Outcome, Object, Test) :-
		write(Outcome),
		write(': '),
		write_test_id(Object, Test),
		nl.

	write_test_tags(true) :-
		write('tags: flaky'),
		nl.
	write_test_tags(false).

	write_test_outcome_with_note(Outcome, Object, Test, Note) :-
		(   Note == '' ->
			write_test_outcome(Outcome, Object, Test)
		;   write(Outcome),
			write(': '),
			write_test_id(Object, Test),
			write(' ['), nl,
			write(Note), nl,
			write(']'), nl
		).

	write_test_outcome_with_details(Outcome, Object, Test, Reason) :-
		write(Outcome),
		write(': '),
		write_test_id(Object, Test),
		write(' ['), nl,
		write_failed_reason_message(Reason),
		write(']'), nl.

	write_suite_error(Object, Step, Error) :-
		write_time_directive,
		write('test: '),
		writeq(Object),
		write('::'),
		write(Step),
		nl,
		write('error: '),
		writeq(Object),
		write('::'),
		write(Step),
		write(' ['), nl,
		write('broken test suite step error: '),
		pretty_print_term(Error), nl,
		write(']'), nl.

	write_suite_failure(Object, Step) :-
		write_time_directive,
		write('test: '),
		writeq(Object),
		write('::'),
		write(Step),
		nl,
		write('failure: '),
		writeq(Object),
		write('::'),
		write(Step),
		write(' ['), nl,
		write('broken test suite step failure'), nl,
		write(']'), nl.

	write_failed_reason_message(success_instead_of_failure) :-
		write('test goal succeeded but should have failed'), nl.
	write_failed_reason_message(success_instead_of_error(ExpectedError)) :-
		write('test goal succeeded but should have throw an error: '), pretty_print_term(ExpectedError), nl.
	write_failed_reason_message(failure_instead_of_success) :-
		write('test goal failed but should have succeeded'), nl.
	write_failed_reason_message(failure_instead_of_error(ExpectedError)) :-
		write('test goal failed but should have throw an error: '), pretty_print_term(ExpectedError), nl.
	write_failed_reason_message(error_instead_of_failure(Error)) :-
		write('test goal throws an error but should have failed: '), pretty_print_term(Error), nl.
	write_failed_reason_message(error_instead_of_success(Error)) :-
		write('test goal throws an error but should have succeeded: '), pretty_print_term(Error), nl.
	write_failed_reason_message(wrong_error(ExpectedError, Error)) :-
		write('test goal throws the wrong error; got: '), pretty_print_term(Error), nl,
		write('expected: '), pretty_print_term(ExpectedError), nl.

	write_failed_reason_message(quick_check_broken(Why, Error)) :-
		write('broken QuickCheck test; why: '), pretty_print_term(Why), nl,
		write('error: '), pretty_print_term(Error), nl.
	write_failed_reason_message(quick_check_broken(Why)) :-
		write('broken QuickCheck test; why: '), pretty_print_term(Why), nl.

	write_failed_reason_message(step_error(Step, Error)) :-
		write(Step), write(' goal throws an error but should have succeeded: '),
		pretty_print_term(Error), nl.
	write_failed_reason_message(step_failure(Step)) :-
		write(Step), write(' goal failed but should have succeeded'), nl.
	write_failed_reason_message(Reason) :-
		pretty_print_term(Reason), nl.

	write_test_id(Object, Test) :-
		writeq(Test),
		write(' @ '),
		writeq(Object).

	write_time_directive :-
		os::date_time(Year, Month, Day, Hours, Minutes, Seconds, Milliseconds),
		integer_to_padded_atom(Month, 2, MonthAtom),
		integer_to_padded_atom(Day, 2, DayAtom),
		integer_to_padded_atom(Hours, 2, HoursAtom),
		integer_to_padded_atom(Minutes, 2, MinutesAtom),
		integer_to_padded_atom(Seconds, 2, SecondsAtom),
		Microseconds is Milliseconds * 1000,
		integer_to_padded_atom(Microseconds, 6, MicrosecondsAtom),
		write('time: '),
		write(Year), write('-'), write(MonthAtom), write('-'), write(DayAtom),
		write(' '),
		write(HoursAtom), write(':'), write(MinutesAtom), write(':'), write(SecondsAtom),
		write('.'), write(MicrosecondsAtom), write('Z'),
		nl.

	integer_to_padded_atom(Integer, Length, Atom) :-
		number_chars(Integer, Chars),
		length(Chars, NumberLength),
		Padding is Length - NumberLength,
		integer_to_padded_chars(Padding, Chars, PaddedChars),
		atom_chars(Atom, PaddedChars).

	integer_to_padded_chars(0, Chars, Chars) :-
		!.
	integer_to_padded_chars(Padding, Chars, ['0'| PaddedChars]) :-
		Padding > 0,
		NextPadding is Padding - 1,
		integer_to_padded_chars(NextPadding, Chars, PaddedChars).

	pretty_print_term(Term) :-
		\+ \+ (
			numbervars(Term, 0, _),
			write_term(Term, [numbervars(true), quoted(true)])
		).

:- end_object.
