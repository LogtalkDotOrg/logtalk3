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


:- initialization((
	% define a flag to allow the logtalk_tester script to pass the
	% option to suppress the test file and directory path prefix
	create_logtalk_flag(suppress_path_prefix, '', [type(atom), keep(true)]),
	% define a flag to allow the logtalk_tester script to pass the
	% base URL for generating links to test files
	create_logtalk_flag(tests_base_url, '', [type(atom), keep(true)])
)).


:- object(ctrf_output).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-02-27,
		comment is 'Intercepts unit test execution messages and outputs a report using the CTRF JSON format to the current output stream.',
		remarks is [
			'Usage' - 'Simply load this object before running your tests using the goal ``logtalk_load(lgtunit(ctrf_output))``.'
		]
	]).

	:- private(message_cache_/1).
	:- dynamic(message_cache_/1).
	:- mode(message_cache_(?callable), zero_or_more).
	:- info(message_cache_/1, [
		comment is 'Table of messages emitted by the lgtunit tool when running tests.',
		argnames is ['Message']
	]).

	% intercept all messages from the "lgtunit" object while running tests

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, _, lgtunit, _) :-
		message_hook(Message).

	message_hook(tests_start_date_time(Year,Month,Day,Hours,Minutes,Seconds)) :-
		!,
		retractall(message_cache_(_)),
		assertz(message_cache_(tests_start_date_time(Year,Month,Day,Hours,Minutes,Seconds))).
	message_hook(tests_end_date_time(Year,Month,Day,Hours,Minutes,Seconds)) :-
		!,
		assertz(message_cache_(tests_end_date_time(Year,Month,Day,Hours,Minutes,Seconds))),
		generate_json_report.
	% "test" entries
	message_hook(passed_test(Object, Test, File, Position, Flaky, Note, CPUTime, WallTime)) :-
		!,
		assertz(message_cache_(test(Object, Test, passed_test(File, Position, Flaky, Note, CPUTime, WallTime)))).
	message_hook(failed_test(Object, Test, File, Position, Reason, Flaky, Note, CPUTime, WallTime)) :-
		!,
		assertz(message_cache_(test(Object, Test, failed_test(File, Position, Reason, Flaky, Note, CPUTime, WallTime)))).
	message_hook(skipped_test(Object, Test, File, Position, Flaky, Note)) :-
		!,
		assertz(message_cache_(test(Object, Test, skipped_test(File, Position, Flaky, Note)))).
	% catchall clause
	message_hook(Message) :-
		assertz(message_cache_(Message)).

	% generate the JSON report

	generate_json_report :-
		summary_stats(Tests, Passed, Failed, Skipped),
		timestamps(Start, Stop),
		Duration is Stop - Start,
		write('{'), nl,
		write('  "reportFormat": "CTRF",'), nl,
		write('  "specVersion": "1.0.0",'), nl,
		write('  "results": {'), nl,
		write('    "tool": {"name": "lgtunit"},'), nl,
		write('    "summary": {'), nl,
		write('      "tests": '), write(Tests), write(','), nl,
		write('      "passed": '), write(Passed), write(','), nl,
		write('      "failed": '), write(Failed), write(','), nl,
		write('      "skipped": '), write(Skipped), write(','), nl,
		write('      "pending": 0,'), nl,
		write('      "other": 0,'), nl,
		write('      "start": '), write(Start), write(','), nl,
		write('      "stop": '), write(Stop), write(','), nl,
		write('      "duration": '), write(Duration), nl,
		write('    },'), nl,
		write('    "tests": ['), nl,
		write_test_entries,
		write('    ]'), nl,
		write('  }'), nl,
		write('}'), nl.

	summary_stats(Tests, Passed, Failed, Skipped) :-
		findall(Total, message_cache_(tests_results_summary(_, Total, _, _, _, _, _)), Totals),
		findall(P, message_cache_(tests_results_summary(_, _, _, P, _, _, _)), Passeds),
		findall(F, message_cache_(tests_results_summary(_, _, _, _, F, _, _)), Faileds),
		findall(S, message_cache_(tests_results_summary(_, _, S, _, _, _, _)), Skippeds),
		findall(
			SkippedSet,
			( message_cache_(tests_skipped(Object, _, _)), Object::number_of_tests(SkippedSet) ),
			SkippedSets
		),
		sum(Totals, 0, TotalsCount),
		sum(Passeds, 0, Passed),
		sum(Faileds, 0, Failed),
		sum(Skippeds, 0, SkippedCount),
		sum(SkippedSets, 0, SkippedSetCount),
		Skipped is SkippedCount + SkippedSetCount,
		Tests is TotalsCount + SkippedSetCount.

	timestamps(Start, Stop) :-
		message_cache_(tests_start_date_time(Year0, Month0, Day0, Hours0, Minutes0, Seconds0)),
		message_cache_(tests_end_date_time(Year, Month, Day, Hours, Minutes, Seconds)),
		date_time_to_epoch_milliseconds(Year0, Month0, Day0, Hours0, Minutes0, Seconds0, Start),
		date_time_to_epoch_milliseconds(Year, Month, Day, Hours, Minutes, Seconds, Stop).

	write_test_entries :-
		findall(
			Entry,
			( 	message_cache_(tests_skipped(Object, File, Note)),
				Object::test(Name),
				Entry = test(Object, Name, skipped_test(File, 0-0, false, Note))
			; 	message_cache_(test(Object, Name, Test)),
				Entry = test(Object, Name, Test)
			),
			Entries
		),
		write_test_entries(Entries).

	write_test_entries([]).
	write_test_entries([Entry]) :-
		!,
		write('      '),
		write_test_entry(Entry), nl.
	write_test_entries([Entry| Entries]) :-
		write('      '),
		write_test_entry(Entry), write(','), nl,
		write_test_entries(Entries).

	write_test_entry(test(Object, Name, passed_test(File, Position, _Flaky, _Note, _, WallTime))) :-
		test_identity(Object, Name, Suite, TestName),
		test_location(File, Position, Short, Line),
		seconds_to_milliseconds(WallTime, Duration),
		write('{'),
		write_json_key_string('name', TestName), write(', '),
		write_json_key_string_array('suite', [Suite]), write(', '),
		write_json_key_string('status', passed), write(', '),
		write_json_key_number('duration', Duration), write(', '),
		write_json_key_string('filePath', Short), write(', '),
		write_json_key_number('line', Line), write(', '),
		write_json_key_string('message', ''), write(', '),
		write_json_key_string('trace', ''),
		write('}').
	write_test_entry(test(Object, Name, failed_test(File, Position, Reason, _Flaky, _Note, _, WallTime))) :-
		test_identity(Object, Name, Suite, TestName),
		test_location(File, Position, Short, Line),
		seconds_to_milliseconds(WallTime, Duration),
		failed_test_reason(Reason, Message, _Type, Error),
		write('{'),
		write_json_key_string('name', TestName), write(', '),
		write_json_key_string_array('suite', [Suite]), write(', '),
		write_json_key_string('status', failed), write(', '),
		write_json_key_number('duration', Duration), write(', '),
		write_json_key_string('filePath', Short), write(', '),
		write_json_key_number('line', Line), write(', '),
		write_json_key_string('message', Message), write(', '),
		write_json_key_string('trace', Error),
		write('}').
	write_test_entry(test(Object, Name, skipped_test(File, Position, _Flaky, _Note))) :-
		test_identity(Object, Name, Suite, TestName),
		test_location(File, Position, Short, Line),
		write('{'),
		write_json_key_string('name', TestName), write(', '),
		write_json_key_string_array('suite', [Suite]), write(', '),
		write_json_key_string('status', skipped), write(', '),
		write_json_key_number('duration', 0), write(', '),
		write_json_key_string('filePath', Short), write(', '),
		write_json_key_number('line', Line), write(', '),
		write_json_key_string('message', 'Skipped test'), write(', '),
		write_json_key_string('trace', ''),
		write('}').

	test_identity(Object, Name, Suite, TestName) :-
		Suite = Object,
		TestName = Name.

	test_location(File, Position, Short, Line) :-
		suppress_path_prefix(File, Short),
		(	Position = Line-_ ->
			true
		;	Line = 0
		).

	% failed_test_reason(Reason, Description, Type, Error)
	failed_test_reason(non_deterministic_success, 'Non-deterministic success', non_deterministic_success, '').
	failed_test_reason(failure_instead_of_error(Error), 'Failure instead of error', failure_instead_of_error, Error).
	failed_test_reason(failure_instead_of_success, 'Failure instead of success', failure_instead_of_success, '').
	failed_test_reason(error_instead_of_success(Error), 'Error instead of success', error_instead_of_success, Error).
	failed_test_reason(error_instead_of_failure(Error), 'Error instead of failure', error_instead_of_failure, Error).
	failed_test_reason(success_instead_of_error(Error), 'Success instead of error', success_instead_of_error, Error).
	failed_test_reason(success_instead_of_failure, 'Success instead of failure', success_instead_of_failure, '').
	failed_test_reason(wrong_error(_, Error), 'Wrong error', wrong_error, Error).
	failed_test_reason(quick_check_failed(Error, _, _), 'QuickCheck test failed', quick_check_failed, Error).
	failed_test_reason(quick_check_error(Error, _, _), 'QuickCheck test error', quick_check_error, Error).
	failed_test_reason(quick_check_broken(Why, _), 'QuickCheck test broken', quick_check_broken, Why).
	failed_test_reason(quick_check_broken(Why), 'QuickCheck test broken', quick_check_broken, Why).
	failed_test_reason(step_error(_, Error), 'Test step error', step_error, Error).
	failed_test_reason(step_failure(Step), 'Test step failure', step_failure, Step).

	suppress_path_prefix(Path, ShortPath) :-
		% bypass the compiler as the flag is only created after loading this file
		{current_logtalk_flag(suppress_path_prefix, Prefix)},
		(	atom_concat(Prefix, ShortPath, Path) ->
			true
		;	ShortPath = Path
		).

	% date and time auxiliary predicates

	date_time_to_epoch_milliseconds(Year, Month, Day, Hours, Minutes, Seconds, Milliseconds) :-
		julian_day(1970, 1, 1, EpochJulianDay),
		julian_day(Year, Month, Day, JulianDay),
		ElapsedSeconds is
			(JulianDay - EpochJulianDay) * 86400 +
			Hours * 3600 +
			Minutes * 60 +
			Seconds,
		Milliseconds is ElapsedSeconds * 1000.

	julian_day(Year, Month, Day, JulianDay) :-
		A is (14 - Month) // 12,
		Y is Year + 4800 - A,
		M is Month + (12 * A) - 3,
		D is Day + ((153 * M + 2) // 5) + (365 * Y) + (Y // 4),
		JulianDay is D - (Y // 100) + (Y // 400) - 32045.

	seconds_to_milliseconds(Seconds, Milliseconds) :-
		Milliseconds is round(Seconds * 1000).

	% JSON auxiliary predicates

	write_json_key_string(Key, Value) :-
		write_json_string(Key),
		write(': '),
		write_json_string(Value).

	write_json_key_number(Key, Value) :-
		write_json_string(Key),
		write(': '),
		write(Value).

	write_json_key_string_array(Key, Values) :-
		write_json_string(Key),
		write(': ['),
		write_json_string_array_elements(Values),
		write(']').

	write_json_string_array_elements([]).
	write_json_string_array_elements([Value]) :-
		!,
		write_json_string(Value).
	write_json_string_array_elements([Value| Values]) :-
		write_json_string(Value),
		write(', '),
		write_json_string_array_elements(Values).

	write_json_string(Value) :-
		( atom(Value) ->
			Atom = Value
		; numbervars(Value, 0, _),
		  {with_output_to(atom(Atom), write_term(Value, [numbervars(true), quoted(true)]))}
		),
		escape_json_string(Atom, Escaped),
		write('"'), write(Escaped), write('"').

	escape_json_string(Atom, Escaped) :-
		atom_chars(Atom, Chars),
		phrase(escape_json_characters(Chars), EscapedChars),
		atom_chars(Escaped, EscapedChars).

	escape_json_characters([]) -->
		[].
	escape_json_characters(['\\'| Chars]) -->
		['\\', '\\'],
		!,
		escape_json_characters(Chars).
	escape_json_characters(['"'| Chars]) -->
		['\\', '"'],
		!,
		escape_json_characters(Chars).
	escape_json_characters(['\n'| Chars]) -->
		['\\', n],
		!,
		escape_json_characters(Chars).
	escape_json_characters(['\r'| Chars]) -->
		['\\', r],
		!,
		escape_json_characters(Chars).
	escape_json_characters(['\t'| Chars]) -->
		['\\', t],
		!,
		escape_json_characters(Chars).
	escape_json_characters([Char| Chars]) -->
		[Char],
		escape_json_characters(Chars).

	sum([], Sum, Sum).
	sum([X| Xs], Acc, Sum) :-
		Acc2 is Acc + X,
		sum(Xs, Acc2, Sum).

:- end_object.
