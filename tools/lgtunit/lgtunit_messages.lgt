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

	logtalk::message_tokens(tests_skipped(_Object, Note), lgtunit) -->
		(	{Note == ''} ->
			['tests skipped'-[], nl]
		;	['tests skipped (~w)'-[Note], nl]
		).

	% messages for test results

	logtalk::message_tokens(tests_results_summary(Total, Skipped, Passed, Failed, Note), lgtunit) -->
		(	{Note == ''} ->
			['~d tests: ~d skipped, ~d passed, ~d failed'-[Total, Skipped, Passed, Failed], nl]
		;	['~d tests: ~d skipped, ~d passed, ~d failed (~w)'-[Total, Skipped, Passed, Failed, Note], nl]
		).

	logtalk::message_tokens(passed_test(Test, _File, _Position, Note), lgtunit) -->
		(	{Note == ''} ->
			['~w: success'-[Test], nl]
		;	['~w: success (~w)'-[Test, Note], nl]
		).

	logtalk::message_tokens(failed_test(Test, File, Position, Reason, Note), lgtunit) -->
		(	{Note == ''} ->
			['~w: failure '-[Test], nl]
		;	['~w: failure (~w)'-[Test, Note], nl]
		),
		failed_test_reason(Reason),
		['  in file ~w between lines ~w'-[File, Position], nl].

	logtalk::message_tokens(skipped_test(Test, _File, _Position, Note), lgtunit) -->
		(	{Note == ''} ->
			['~w: skipped'-[Test], nl]
		;	['~w: skipped (~w)'-[Test, Note], nl]
		).

	logtalk::message_tokens(failed_cleanup(Test, File, Position, Reason), lgtunit) -->
		failed_cleanup_reason(Reason, Test),
		['  in file ~w between lines ~w'-[File, Position], nl].

	logtalk::message_tokens(broken_step(Step, Object, Error), lgtunit) -->
		['broken ~w goal for test object ~q: ~q'-[Step, Object, Error], nl].

	logtalk::message_tokens(failed_step(Step, Object), lgtunit) -->
		['failed ~w goal for test object ~q'-[Step, Object], nl].

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

	failed_test_reason(success_instead_of_failure) -->
		['  test goal succeeded but should have failed'-[], nl].
	failed_test_reason(success_instead_of_error) -->
		['  test goal succeeded but should have throw an error'-[], nl].
	failed_test_reason(failure_instead_of_success) -->
		['  test goal failed but should have succeeded'-[], nl].
	failed_test_reason(failure_instead_of_error) -->
		['  test goal failed but should have throw an error'-[], nl].
	failed_test_reason(error_instead_of_failure(Error)) -->
		['  test goal throws an error but should have failed: ~q'-[Error], nl].
	failed_test_reason(error_instead_of_success(Error)) -->
		['  test goal throws an error but should have succeeded: ~q'-[Error], nl].
	failed_test_reason(wrong_error(ExpectedError, Error)) -->
		['  test goal throws the wrong error: expected ~q but got ~q'-[ExpectedError, Error], nl].
	failed_test_reason(step_error(Step, Error)) -->
		['  ~w goal throws an error but should have succeeded: ~q'-[Step, Error], nl].
	failed_test_reason(step_failure(Step)) -->
		['  ~w goal failed but should have succeeded: ~q'-[Step], nl].

	failed_cleanup_reason(error(Error), Test) -->
		['  test ~w cleanup goal throws an error but should have succeeded: ~q'-[Test, Error], nl].
	failed_cleanup_reason(failure, Test) -->
		['  test ~w cleanup goal failed but should have succeeded'-[Test], nl].

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
