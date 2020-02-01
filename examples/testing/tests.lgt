%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


% the following tests object provides a non-exaustive view of
% the test dialects supported by default by the "lgtunit" tool
%
% see the "lgtunit" tool for full details on the test dialects

:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2019-12-16,
		comment is 'Examples of the default test dialects.'
	]).

	% only run this test suite in Logtalk 3.x; in actual test
	% suites, the condition/1 argument would check if necessary
	% if all the necessary conditions to run the test are true
	condition(current_logtalk_flag(version_data, logtalk(3, _, _, _))).

	% turn off suspicious_calls warnings for the tests
	setup :-
		set_logtalk_flag(suspicious_calls, silent).

	% all tests are nice and clean after themselves; in actual
	% test, you may need to cleanup test side-eefects that would
	% e.g. interfere with re-running the tests
	cleanup.

	% add a note to the test suite run output
	note('Test dialects examples').

	% run the tests with make target "all"
	make(all).

	% the most simple test dialect: test/1

	test(user_object_exists) :-
		current_object(user).

	test(non_existing_object) :-
		\+ current_object(foo42).

	% skip the next test
	- test(object_system_exists) :-
		current_object(system).

	% a test dialect with an explicit expected outcome: test/2

	test(logtalk_object_exists, true) :-
		current_object(logtalk).

	test(logtalk_objecty_is_not_dynamic, fail) :-
		object_property(logtalk, (dynamic)).

	test(logtalk_object_is_built_in, deterministic) :-
		object_property(logtalk, built_in).

	test(logtalk_object_source_file_name, true(Basename == 'logtalk.lgt')) :-
		object_property(logtalk, file(Basename, _)).

	test(logtalk_object_source_file_lines, deterministic(integer(Start))) :-
		object_property(logtalk, lines(Start, _)).

	test(logtalk_object_invalid_property, error(type_error(callable,1))) :-
		Property = 1,  % delay error to runtime
		object_property(logtalk, Property).

	% a test dialect where the main functor expresses the expected outcome

	succeeds(expanding_protocol_exists) :-
		current_protocol(expanding).

	fails(expanding_protocol_is_not_dynamic) :-
		protocol_property(expanding, (dynamic)).

	deterministic(expanding_protocol_is_built_in) :-
		protocol_property(expanding, built_in).

	throws(expanding_protocol_invalid_property, error(type_error(callable,1), _)) :-
		Property = 1,  % delay error to runtime
		protocol_property(expanding, Property).

	% a test dialect supporting individual test options

	test(double_quotes_as_list_of_chars, true, [condition(current_prolog_flag(double_quotes,chars))]) :-
		String = "abc",  % avoid lint warnings
		String == [a,b,c].

	test(dynamicly_created_object_is_dynanic, true, [
			setup(create_object(foo, [], [], [])),
			cleanup(abolish_object(foo))
	]) :-
		object_property(foo, (dynamic)).

	% skip the next test
	- test(protocol_serialization_exists, true, [note('Protocol serialization not yet implemented')]) :-
		protocol_property(serialization).

	% a QuickCheck test dialect

	quick_check(atom_concat_produces_an_atom, atom_concat(+atom, +atom, -atom)).

	quick_check(atom_length_produces_an_integer, atom_length(+atom, -integer), [n(25)]).

:- end_object.
