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


% define a flag to allow the logtalk_tester script to pass the
% option to suppress the test file and directory path prefix
:- initialization(
	create_logtalk_flag(suppress_path_prefix, '', [type(atom), keep(true)])
).


:- category(lgtunit_messages).

	:- info([
		version is 7:0:0,
		author is 'Paulo Moura',
		date is 2022-08-25,
		comment is 'Logtalk unit test framework default message translations.'
	]).

	:- set_logtalk_flag(debug, off).

	% structured message printing predicates;
	% the main reason to not write directly to an output stream is to allows
	% other tools such as IDEs to intercept and handle unit test results

	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).

	logtalk::message_prefix_stream(Kind, lgtunit, Prefix, Stream) :-
		message_prefix_stream(Kind, Prefix, Stream).

	% Quintus Prolog based prefixes (also used in SICStus Prolog):
	message_prefix_stream(information, '% ',     user_output).
	message_prefix_stream(warning,     '*     ', user_output).
	message_prefix_stream(error,       '!     ', user_output).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(Message, lgtunit) -->
		{numbervars(Message, 0, _)},
		message_tokens(Message).

	% messages for tests handling

	message_tokens(tests_started) -->
		[].

	message_tokens(tests_ended) -->
		[].

	message_tokens(tests_start_date_time(Year, Month, Day, Hours, Minutes, Seconds)) -->
		{	integer_to_padded_atom(Month, MonthAtom),
			integer_to_padded_atom(Day, DayAtom),
			integer_to_padded_atom(Hours, HoursAtom),
			integer_to_padded_atom(Minutes, MinutesAtom),
			integer_to_padded_atom(Seconds, SecondsAtom),
			Args = [Year, MonthAtom, DayAtom, HoursAtom, MinutesAtom, SecondsAtom]
		},
		[nl, 'tests started at ~w-~w-~w, ~w:~w:~w'-Args, nl, nl].

	message_tokens(tests_end_date_time(Year, Month, Day, Hours, Minutes, Seconds)) -->
		{	integer_to_padded_atom(Month, MonthAtom),
			integer_to_padded_atom(Day, DayAtom),
			integer_to_padded_atom(Hours, HoursAtom),
			integer_to_padded_atom(Minutes, MinutesAtom),
			integer_to_padded_atom(Seconds, SecondsAtom),
			Args = [Year, MonthAtom, DayAtom, HoursAtom, MinutesAtom, SecondsAtom]
		},
		['tests ended at ~w-~w-~w, ~w:~w:~w'-Args, nl, nl].

	message_tokens(running_tests_from_object_file(Object, File)) -->
		['running tests from object ~q'-[Object], nl, 'file: ~w'-[File], nl, nl].

	message_tokens(number_of_tests(Total)) -->
		['number of tests: ~q'-[Total], nl].

	message_tokens(completed_tests_from_object(Object)) -->
		['completed tests from object ~q'-[Object], nl, nl].

	message_tokens(tests_skipped(Object, _File, Note)) -->
		(	{Note == ''} ->
			['tests skipped @ ~q'-[Object], nl]
		;	['tests skipped @ ~q (~w)'-[Object, Note], nl]
		).

	% messages for test results

	message_tokens(tests_results_summary(_Object, Total, Skipped, Passed, Failed, Flaky, Note)) -->
		(	{Note == ''} ->
			[nl, '~d tests: ~d skipped, ~d passed, ~d failed (~d flaky)'-[Total, Skipped, Passed, Failed, Flaky], nl]
		;	[nl, '~d tests: ~d skipped, ~d passed, ~d failed (~d flaky; ~w)'-[Total, Skipped, Passed, Failed, Flaky, Note], nl]
		).

	message_tokens(passed_test(_Object, Test, _File, _Position, Note, Time)) -->
		(	{Note == ''} ->
			['~q: success (in ~w seconds)'-[Test, Time], nl]
		;	['~q: success (~w) (in ~w seconds)'-[Test, Note, Time], nl]
		).

	message_tokens(non_deterministic_success(_Object, Test, File, Position, Note, Time)) -->
		(	{Note == ''} ->
			['~q: failure (in ~w seconds)'-[Test, Time], nl]
		;	['~q: failure (~w) (in ~w seconds)'-[Test, Note, Time], nl]
		),
		failed_test_reason(non_deterministic_success),
		file_position(File, Position).

	message_tokens(failed_test(_Object, Test, File, Position, Reason, Note, Time)) -->
		(	{Note == ''} ->
			['~q: failure (in ~w seconds)'-[Test, Time], nl]
		;	['~q: failure (~w) (in ~w seconds)'-[Test, Note, Time], nl]
		),
		failed_test_reason(Reason),
		file_position(File, Position).

	message_tokens(skipped_test(_Object, Test, _File, _Position, Note)) -->
		(	{Note == ''} ->
			['~q: skipped'-[Test], nl]
		;	['~q: skipped (~w)'-[Test, Note], nl]
		).

	message_tokens(verbose_quick_check_test(passed, Goal)) -->
		['Passed:    ~q'-[Goal], nl].
	message_tokens(verbose_quick_check_test(discarded, Goal)) -->
		['Discarded: ~q'-[Goal], nl].
	message_tokens(verbose_quick_check_test(failure, Goal)) -->
		['Failure:   ~q'-[Goal], nl].
	message_tokens(verbose_quick_check_test(error, Goal)) -->
		['Error:     ~q'-[Goal], nl].
	message_tokens(verbose_quick_check_test(shrinked, Goal)) -->
		['Shrinked:  ~q'-[Goal], nl].

	message_tokens(quick_check_progress_bar_start) -->
		[].
	message_tokens(quick_check_progress_bar_tick(Test)) -->
		[at_same_line, '...~d'-[Test], flush].
	message_tokens(quick_check_progress_bar_end) -->
		[at_same_line, nl].

	message_tokens(quick_check_passed(NumberOfTests, Seed, Discarded, Labels)) -->
		['~w random tests passed, ~w discarded'-[NumberOfTests, Discarded], nl],
		['starting seed: ~w'-[Seed], nl],
		quick_check_labels(Labels, NumberOfTests).

	message_tokens(quick_check_failed(Goal, Test, Shrinks, Seed, TestSeed)) -->
		(	{Shrinks == 1} ->
			['quick check test failure (at test ~w after ~w shrink):'-[Test, Shrinks], nl, '  ~q'-[Goal], nl]
		;	['quick check test failure (at test ~w after ~w shrinks):'-[Test, Shrinks], nl, '  ~q'-[Goal], nl]
		),
		['starting seed: ~q'-[Seed], nl],
		['test seed:     ~q'-[TestSeed], nl].

	message_tokens(quick_check_error(error(Error,_), Goal, Test, Seed, TestSeed)) -->
		message_tokens(quick_check_error(Error, Goal, Test, Seed, TestSeed)).
	message_tokens(quick_check_error(Error, _Goal, Test, Seed, TestSeed)) -->
		['quick check test error (at test ~w):'-[Test], nl, '  ~q'-[Error], nl],
		['starting seed: ~q'-[Seed], nl],
		['test seed:     ~q'-[TestSeed], nl].

	message_tokens(quick_check_broken(generate_test_error(Template), Error)) -->
		['quick check test generation error for template: ~q'-[Template], nl, '  ~q'-[Error], nl].
	message_tokens(quick_check_broken(generate_test_failure(Template), Type)) -->
		['quick check test generation failure for template: ~q'-[Template], nl, '  ~q'-[Type], nl].

	message_tokens(quick_check_broken(label_goal_error(Label), Error)) -->
		['quick check error using label closure: ~q'-[Label], nl, '  ~q'-[Error], nl].
	message_tokens(quick_check_broken(label_goal_failure(Label))) -->
		['quick check label closure fails: ~q'-[Label], nl].

	message_tokens(quick_check_broken(pre_condition_error(Condition), Error)) -->
		['quick check error using pre-condition closure: ~q'-[Condition], nl, '  ~q'-[Error], nl].
	message_tokens(quick_check_broken(pre_condition_always_fails(Condition))) -->
		['quick check pre-condition closure fails: ~q'-[Condition], nl].

	message_tokens(quick_check_broken(template_error(Template), Error)) -->
		['quick check error using template: ~q'-[Template], nl, '  ~q'-[Error], nl].

	message_tokens(quick_check_broken(option_error(Option), Error)) -->
		['quick check option error: ~q'-[Option], nl, '  ~q'-[Error], nl].

	message_tokens(failed_cleanup(_Object, Test, File, Position, Reason)) -->
		failed_cleanup_reason(Reason, _Object, Test),
		file_position(File, Position).

	message_tokens(broken_step(Step, Object, Error)) -->
		['broken ~w goal for test object ~q: ~q'-[Step, Object, Error], nl].

	message_tokens(failed_step(Step, Object)) -->
		['failed ~w goal for test object ~q'-[Step, Object], nl].

	% messages for test's clause coverage

	message_tokens(declared_entities_and_clause_numbers(Entities, Clauses)) -->
		entity_tokens(Entities),
		[' declared as covered containing '-[]],
		clause_tokens(Clauses),
		[nl].

	message_tokens(covered_entities_numbers(Covered, Total, Percentage)) -->
		['~d out of '-[Covered]],
		entity_tokens(Total),
		[' covered, ~f% entity coverage'-[Percentage], nl].

	message_tokens(covered_clause_numbers(Covered, Total, Percentage)) -->
		['~d out of '-[Covered]],
		clause_tokens(Total),
		[' covered, ~f% clause coverage'-[Percentage], nl, nl].

	message_tokens(code_coverage_header) -->
		[nl, 'clause coverage ratio and covered clauses per entity predicate'-[], nl, nl].

	message_tokens(entity_coverage_starts(_Entity)) -->
		[].

	message_tokens(entity_predicate_coverage(Entity, Predicate, Covered, Total, _Percentage, Clauses)) -->
		(	{Covered =:= Total} ->
			% all clause are covered
			['~q: ~q - ~w - ~w'-[Entity, Predicate, Covered/Total, '(all)'], nl]
		;	['~q: ~q - ~w - ~w'-[Entity, Predicate, Covered/Total, Clauses], nl]
		).

	message_tokens(entity_coverage(Entity, Covered, Total, Percentage)) -->
		['~q: ~d out of '-[Entity, Covered]],
		clause_tokens(Total),
		[' covered, ~f% coverage'-[Percentage], nl, nl].

	message_tokens(entity_coverage_ends(_Entity)) -->
		[].

	message_tokens(no_code_coverage_information_collected) -->
		['no code coverage information collected'-[], nl].

	message_tokens(no_code_coverage_for_protocols(Entity)) -->
		['code coverage requested for protocol: ~q'-[Entity], nl].

	message_tokens(unknown_entity_declared_covered(Entity)) -->
		['unknown entity declared covered: ~q'-[Entity], nl].

	% messages for test identifier errors

	message_tokens(non_instantiated_test_identifier) -->
		['non-instantiated test identifier found'-[], nl].

	message_tokens(non_callable_test_identifier(_Object, Test)) -->
		['non-callable test identifier found: ~q'-[Test], nl].

	message_tokens(non_ground_test_identifier(_Object, Test)) -->
		['non-ground test identifier found: ~q'-[Test], nl].

	message_tokens(repeated_test_identifier(_Object, Test)) -->
		['repeated test identifier found: ~q'-[Test], nl].

	% messages for test outcome errors

	message_tokens(non_instantiated_test_outcome(Test)) -->
		['non-instantiated test outcome found: ~q'-[Test], nl].

	message_tokens(invalid_test_outcome(Test, Outcome)) -->
		['test ~q outcome is invalid: ~q'-[Test, Outcome], nl].

	% linter warnings

	message_tokens(assertion_uses_unification(Test, Assertion)) -->
		['test ~q assertion uses a unification goal: ~q'-[Test, Assertion], nl].

	% auxiliary grammar rules

	failed_test_reason(success_instead_of_failure) -->
		['  test goal succeeded but should have failed'-[], nl].
	failed_test_reason(success_instead_of_error(ExpectedError)) -->
		['  test goal succeeded but should have thrown an error:'-[], nl],
		['    expected ~q'-[ExpectedError], nl].
	failed_test_reason(failure_instead_of_success) -->
		['  test goal failed but should have succeeded'-[], nl].
	failed_test_reason(failure_instead_of_error(ExpectedError)) -->
		['  test goal failed but should have thrown an error:'-[], nl],
		['    expected ~q'-[ExpectedError], nl].
	failed_test_reason(non_deterministic_success) -->
		['  test goal succeeded non-deterministically'-[], nl].
	failed_test_reason(error_instead_of_failure(Error)) -->
		['  test goal throws an error but should have failed: ~q'-[Error], nl].
	failed_test_reason(error_instead_of_success(assertion_error(Assertion, error(Error,_)))) -->
		['  test assertion throws an error: ~q - ~q'-[Assertion, Error], nl].
	failed_test_reason(error_instead_of_success(assertion_error(Assertion, Error))) -->
		['  test assertion throws an error: ~q - ~q'-[Assertion, Error], nl].
	failed_test_reason(error_instead_of_success(assertion_failure(Assertion))) -->
		['  test assertion failed: ~q'-[Assertion], nl].
	failed_test_reason(error_instead_of_success(Error)) -->
		['  test goal throws an error but should have succeeded: ~q'-[Error], nl].
	failed_test_reason(wrong_error(ExpectedError, Error)) -->
		['  test goal throws the wrong error:'-[], nl],
		['    expected ~q'-[ExpectedError], nl],
		['    but got  ~q'-[Error], nl].

	failed_test_reason(quick_check_failed(Goal, Test, Shrinks, Seed, TestSeed)) -->
		(	{Shrinks == 1} ->
			['  quick check test failure (at test ~w after ~w shrink with starting seed ~q and test seed ~q): ~q'-[Test, Shrinks, Seed, TestSeed, Goal], nl]
		;	['  quick check test failure (at test ~w after ~w shrinks with starting seed ~q and test seed ~q): ~q'-[Test, Shrinks, Seed, TestSeed, Goal], nl]
		).

	failed_test_reason(quick_check_error(error(Error,_), Goal, Test, Seed, TestSeed)) -->
		failed_test_reason(quick_check_error(Error, Goal, Test, Seed, TestSeed)).
	failed_test_reason(quick_check_error(Error, _Goal, Test, Seed, TestSeed)) -->
		['  quick check test error (at test ~w with starting seed ~q and test seed ~q): ~q'-[Test, Seed, TestSeed, Error], nl].
	failed_test_reason(quick_check_error(Error, Culprit)) -->
		['  quick check test error (caused by ~q): ~q'-[Error, Culprit], nl].

	failed_test_reason(step_error(Step, Error)) -->
		['  ~w goal throws an error but should have succeeded: ~q'-[Step, Error], nl].
	failed_test_reason(step_failure(Step)) -->
		['  ~w goal failed but should have succeeded'-[Step], nl].

	failed_test_reason(Unexpected) -->
		['  Unexpected failure (likely backend bug): ~q'-[Unexpected], nl].

	failed_cleanup_reason(error(Error), _Object, Test) -->
		['  test ~q cleanup goal throws an error but should have succeeded: ~q'-[Test, Error], nl].
	failed_cleanup_reason(failure, _Object, Test) -->
		['  test ~q cleanup goal failed but should have succeeded'-[Test], nl].

	file_position(Path, Position) -->
		{suppress_path_prefix(Path, ShortPath)},
		(	{Position = Line-Line} ->
			['  in file ~w at or above line ~w'-[ShortPath, Line], nl]
		;	['  in file ~w between lines ~w'-[ShortPath, Position], nl]
		).

	suppress_path_prefix(Path, ShortPath) :-
		{current_logtalk_flag(suppress_path_prefix, Prefix)},
		(	atom_concat(Prefix, ShortPath, Path) ->
			true
		;	ShortPath = Path
		).

	entity_tokens(Entities) -->
		(	{Entities =:= 1} ->
			['~d entity'-[Entities]]
		;	['~d entities'-[Entities]]
		).

	clause_tokens(Clauses) -->
		(	{Clauses =:= 1} ->
			['~d clause'-[Clauses]]
		;	['~d clauses'-[Clauses]]
		).

	quick_check_labels([], _) -->
		[].
	quick_check_labels([Label-N| Labels], NumberOfTests) -->
		(	{N > 0} ->
			{Percentage is N / NumberOfTests * 100}
		;	{Percentage is 0.0}
		),
		['~w: ~d/~d (~f%)'-[Label, N, NumberOfTests, Percentage], nl],
		quick_check_labels(Labels, NumberOfTests).

	integer_to_padded_atom(Integer, Atom) :-
		number_codes(Integer, Codes),
		(	Integer < 10 ->
			char_code('0', ZeroCode),
			atom_codes(Atom, [ZeroCode| Codes])
		;	atom_codes(Atom, Codes)
		).

:- end_category.
