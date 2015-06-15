%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2015 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- category(lgtunit_messages).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/06/15,
		comment is 'Logtalk unit test framework default message translations.'
	]).

	:- set_logtalk_flag(debug, off).

	% structured message printing predicates;
	% the main reason to not write directly to an output stream is to allows
	% other tools such as IDEs to intercept and handle unit test results

	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).

	% Quintus Prolog based prefixes (also used in SICStus Prolog):
	logtalk::message_prefix_stream(information, lgtunit, '% ', user_output).
	logtalk::message_prefix_stream(warning, lgtunit, '*     ', user_output).
	logtalk::message_prefix_stream(error, lgtunit,   '!     ', user_output).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	% messages for tests handling

	logtalk::message_tokens(tests_start_date_time(Year, Month, Day, Hours, Minutes, Seconds), lgtunit) -->
		[nl, 'tests started at ~w/~w/~w, ~w:~w:~w'-[Year, Month, Day, Hours, Minutes, Seconds], nl].

	logtalk::message_tokens(tests_end_date_time(Year, Month, Day, Hours, Minutes, Seconds), lgtunit) -->
		['tests ended at ~w/~w/~w, ~w:~w:~w'-[Year, Month, Day, Hours, Minutes, Seconds], nl, nl].

	logtalk::message_tokens(running_tests_from_object_file(Object, File), lgtunit) -->
		['running tests from object ~q'-[Object], nl, 'file: ~w'-[File], nl].

	logtalk::message_tokens(running_tests_from_object(Object), lgtunit) -->
		['running tests from object ~q'-[Object], nl].

	logtalk::message_tokens(completed_tests_from_object(Object), lgtunit) -->
		['completed tests from object ~q'-[Object], nl].

	logtalk::message_tokens(tests_skipped(_Object), lgtunit) -->
		['tests skipped'-[], nl].

	% messages for test results

	logtalk::message_tokens(tests_results_summary(Total, Skipped, Passed, Failed), lgtunit) -->
		['~d tests: ~d skipped, ~d passed, ~d failed'-[Total, Skipped, Passed, Failed], nl].

	logtalk::message_tokens(passed_test(Test, _File, _Position), lgtunit) -->
		['~w: success'-[Test], nl].

	logtalk::message_tokens(failed_test(Test, File, Position, Reason), lgtunit) -->
		['~w: failure '-[Test], nl],
		failure_reason(Reason),
		['  in file ~w between lines ~w'-[File, Position], nl].

	logtalk::message_tokens(skipped_test(Test, _File, _Position), lgtunit) -->
		['~w: skipped'-[Test], nl].

	logtalk::message_tokens(broken_step(Step, Object, Error), lgtunit) -->
		['broken ~w for object ~q: ~q'-[Step, Object, Error], nl].

	logtalk::message_tokens(failed_step(Step, Object), lgtunit) -->
		['failed ~w for object ~q'-[Step, Object], nl].

	logtalk::message_tokens(broken_step(Test, Step, Object, Error), lgtunit) -->
		['broken ~w for test ~w in object ~q: ~q'-[Step, Test, Object, Error], nl].

	logtalk::message_tokens(failed_step(Test, Step, Object), lgtunit) -->
		['failed ~w for test ~w in object ~q'-[Step, Test, Object], nl].

	% messages for test's clause coverage

	logtalk::message_tokens(declared_entities_and_clause_numbers(Entities, Clauses), lgtunit) -->
		entity_tokens(Entities),
		[' declared as covered containing '-[]],
		clause_tokens(Clauses),
		[nl].

	logtalk::message_tokens(covered_entities_and_clause_numbers(Entities, Clauses), lgtunit) -->
		entity_tokens(Entities),
		[' covered containing '-[]],
		clause_tokens(Clauses),
		[nl].

	logtalk::message_tokens(covered_clause_numbers(Covered, Total, Percentage), lgtunit) -->
		['~d out of '-[Covered]],
		clause_tokens(Total),
		[' covered, ~f% coverage'-[Percentage], nl].

	logtalk::message_tokens(entity_clause_coverage(Entity, Predicate, Ratio, Covered), lgtunit) -->
		{numbervars(Entity, 0, _)},
		['~q: ~q - ~w - ~w'-[Entity, Predicate, Ratio, Covered], nl].

	logtalk::message_tokens(no_code_coverage_information_collected, lgtunit) -->
		['no code coverage information collected'-[], nl].

	% messages for test identifier errors

	logtalk::message_tokens(non_instantiated_test_identifier, lgtunit) -->
		['non-instantiated test identifier found'-[], nl].

	logtalk::message_tokens(repeated_test_identifier(Test), lgtunit) -->
		['repeated test identifier found: ~w'-[Test], nl].

	% auxiliary grammar rules

	failure_reason(success_instead_of_failure) -->
		['  test goal succeeded but should have failed'-[], nl].
	failure_reason(success_instead_of_error) -->
		['  test goal succeeded but should have throw an error'-[], nl].
	failure_reason(failure_instead_of_success) -->
		['  test goal failed but should have succeeded'-[], nl].
	failure_reason(failure_instead_of_error) -->
		['  test goal failed but should have throw an error'-[], nl].
	failure_reason(error_instead_of_failure(Error)) -->
		['  test goal throws an error but should have failed: ~q'-[Error], nl].
	failure_reason(error_instead_of_success(Error)) -->
		['  test goal throws an error but should have succeeded: ~q'-[Error], nl].
	failure_reason(wrong_error(ExpectedError, Error)) -->
		['  test goal throws the wrong error: expected ~q but got ~q'-[ExpectedError, Error], nl].

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

:- end_category.
