%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		version is 2.0,
		author is 'Paulo Moura',
		date is 2017/12/15,
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
		[nl, 'tests started at ~w/~w/~w, ~w:~w:~w'-[Year, Month, Day, Hours, Minutes, Seconds], nl, nl].

	message_tokens(tests_end_date_time(Year, Month, Day, Hours, Minutes, Seconds)) -->
		['tests ended at ~w/~w/~w, ~w:~w:~w'-[Year, Month, Day, Hours, Minutes, Seconds], nl, nl].

	message_tokens(running_tests_from_object_file(Object, File)) -->
		['running tests from object ~q'-[Object], nl, 'file: ~w'-[File], nl, nl].

	message_tokens(running_tests_from_object(Object)) -->
		['running tests from object ~q'-[Object], nl].

	message_tokens(completed_tests_from_object(Object)) -->
		['completed tests from object ~q'-[Object], nl, nl].

	message_tokens(tests_skipped(_Object, Note)) -->
		(	{Note == ''} ->
			['tests skipped'-[], nl]
		;	['tests skipped (~w)'-[Note], nl]
		).

	% messages for test results

	message_tokens(tests_results_summary(_Object, Total, Skipped, Passed, Failed, Note)) -->
		(	{Note == ''} ->
			[nl, '~d tests: ~d skipped, ~d passed, ~d failed'-[Total, Skipped, Passed, Failed], nl]
		;	[nl, '~d tests: ~d skipped, ~d passed, ~d failed (~w)'-[Total, Skipped, Passed, Failed, Note], nl]
		).

	message_tokens(passed_test(_Object, Test, _File, _Position, Note)) -->
		(	{Note == ''} ->
			['~q: success'-[Test], nl]
		;	['~q: success (~w)'-[Test, Note], nl]
		).

	message_tokens(failed_test(_Object, Test, File, Position, Reason, Note)) -->
		(	{Note == ''} ->
			['~q: failure '-[Test], nl]
		;	['~q: failure (~w)'-[Test, Note], nl]
		),
		failed_test_reason(Reason),
		['  in file ~w between lines ~w'-[File, Position], nl].

	message_tokens(skipped_test(_Object, Test, _File, _Position, Note)) -->
		(	{Note == ''} ->
			['~q: skipped'-[Test], nl]
		;	['~q: skipped (~w)'-[Test, Note], nl]
		).

	message_tokens(quick_check_passed(NumberOfTests)) -->
		['~w random tests passed'-[NumberOfTests], nl].

	message_tokens(quick_check_failed(Goal)) -->
		failed_test_reason(quick_check_failed(Goal)).

	message_tokens(failed_cleanup(_Object, Test, File, Position, Reason)) -->
		failed_cleanup_reason(Reason, _Object, Test),
		['  in file ~w between lines ~w'-[File, Position], nl].

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

	% messages for test identifier errors

	message_tokens(non_instantiated_test_identifier) -->
		['non-instantiated test identifier found'-[], nl].

	message_tokens(non_callable_test_identifier(_Object, Test)) -->
		['non-callable test identifier found: ~q'-[Test], nl].

	message_tokens(repeated_test_identifier(_Object, Test)) -->
		['repeated test identifier found: ~q'-[Test], nl].

	% messages for test outcome errors

	message_tokens(non_instantiated_test_outcome(_Object, Test)) -->
		['non-instantiated test outcome found: ~q'-[Test], nl].

	message_tokens(invalid_test_outcome(_Object, Test, Outcome)) -->
		['test ~q outcome is invalid: ~q'-[Test, Outcome], nl].

	% auxiliary grammar rules

	failed_test_reason(success_instead_of_failure) -->
		['  test goal succeeded but should have failed'-[], nl].
	failed_test_reason(success_instead_of_error) -->
		['  test goal succeeded but should have thrown an error'-[], nl].
	failed_test_reason(failure_instead_of_success) -->
		['  test goal failed but should have succeeded'-[], nl].
	failed_test_reason(failure_instead_of_error) -->
		['  test goal failed but should have thrown an error'-[], nl].
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
		['  test goal throws the wrong error: expected ~q but got ~q'-[ExpectedError, Error], nl].

	failed_test_reason(quick_check_failed(Goal)) -->
		['  quick check test failure:'-[], nl, '    ~q'-[Goal], nl].

	failed_test_reason(step_error(Step, Error)) -->
		['  ~w goal throws an error but should have succeeded: ~q'-[Step, Error], nl].
	failed_test_reason(step_failure(Step)) -->
		['  ~w goal failed but should have succeeded: ~q'-[Step], nl].

	failed_cleanup_reason(error(Error), _Object, Test) -->
		['  test ~q cleanup goal throws an error but should have succeeded: ~q'-[Test, Error], nl].
	failed_cleanup_reason(failure, _Object, Test) -->
		['  test ~q cleanup goal failed but should have succeeded'-[Test], nl].

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
