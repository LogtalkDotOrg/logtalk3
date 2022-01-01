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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:8:0,
		author is 'Paulo Moura',
		date is 2018-07-12,
		comment is 'Unit tests for the current_logtalk_flag/2 built-in predicate.'
	]).

	:- discontiguous(succeeds/1).
	:- discontiguous(fails/1).
	:- discontiguous(throws/2).

	throws(current_logtalk_flag_2_1, error(type_error(atom, 1), logtalk(current_logtalk_flag(1, _), _))) :-
		{current_logtalk_flag(1, _)}.

	throws(current_logtalk_flag_2_2, error(domain_error(flag, non_existing_flag), logtalk(current_logtalk_flag(non_existing_flag, _), _))) :-
		{current_logtalk_flag(non_existing_flag, _)}.

	% lint flags

	succeeds(unknown_entities_flag) :-
		test_flag(unknown_entities, warning, silent).

	succeeds(unknown_predicates_flag) :-
		test_flag(unknown_predicates, warning, silent).

	succeeds(undefined_predicates_flag) :-
		test_flag(undefined_predicates, warning, silent).

	succeeds(missing_directives_flag) :-
		test_flag(missing_directives, warning, silent).

	succeeds(duplicated_directives_flag) :-
		test_flag(duplicated_directives, warning, silent).

	succeeds(trivial_goal_fails_flag) :-
		test_flag(trivial_goal_fails, warning, silent).

	succeeds(always_true_or_false_goals_flag) :-
		test_flag(always_true_or_false_goals, warning, silent).

	succeeds(lambda_variables_flag) :-
		test_flag(lambda_variables, warning, silent).

	succeeds(portability_flag) :-
		test_flag(portability, warning, silent).

	succeeds(redefined_built_ins_flag) :-
		test_flag(redefined_built_ins, warning, silent).

	succeeds(deprecated_flag) :-
		test_flag(deprecated, warning, silent).

	succeeds(singleton_variables_flag) :-
		test_flag(singleton_variables, warning, silent).

	succeeds(underscore_variables_flag) :-
		test_flag(underscore_variables, singletons, dont_care).

	% compilation flags

	succeeds(source_data_flag) :-
		test_flag(source_data, on, off).

	succeeds(clean_flag) :-
		test_flag(clean, on, off).

	succeeds(debug_flag) :-
		test_flag(debug, on, off).

	succeeds(optimize_flag) :-
		test_flag(optimize, on, off).

	succeeds(report_flag) :-
		current_logtalk_flag(report, Value),
		once((Value == on; Value == warnings; Value == off)).

	succeeds(reload_flag) :-
		current_logtalk_flag(reload, Value),
		once((Value == always; Value == changed; Value == skip)).

	succeeds(code_prefix_flag) :-
		current_logtalk_flag(code_prefix, Value),
		atom(Value).

	succeeds(scratch_directory_flag) :-
		current_logtalk_flag(scratch_directory, Value),
		atom(Value).

	% optional features flags

	succeeds(complements_flag) :-
		test_flag(complements, allow, deny).

	succeeds(dynamic_declarations_flag) :-
		test_flag(dynamic_declarations, allow, deny).

	succeeds(context_switching_calls_flag) :-
		test_flag(context_switching_calls, allow, deny).

	succeeds(events_flag) :-
		test_flag(events, allow, deny).

	% backend Prolog compiler flags:

	succeeds(prolog_compiler_flag) :-
		current_logtalk_flag(prolog_compiler, Value),
		type::valid(list, Value).

	succeeds(prolog_loader_flag) :-
		current_logtalk_flag(prolog_loader, Value),
		type::valid(list, Value).

	% read-only flags

	succeeds(prolog_dialect_flag) :-
		current_logtalk_flag(prolog_dialect, Value),
		atom(Value).

	succeeds(prolog_version) :-
		current_logtalk_flag(prolog_version, Value),
		ground(Value),
		callable(Value),
		functor(Value, v, 3).
		
	succeeds(prolog_compatible_version) :-
		current_logtalk_flag(prolog_compatible_version, Value),
		ground(Value),
		callable(Value),
		functor(Value, Operator, 1),
		list::memberchk(Operator, [(@>), (@>=), (@<), (@=<), (==)]).

	succeeds(modules_flag) :-
		current_logtalk_flag(modules, Value),
		once((Value == supported; Value == unsupported)).

	succeeds(coinduction_flag) :-
		current_logtalk_flag(coinduction, Value),
		once((Value == supported; Value == unsupported)).

	succeeds(engines_flag) :-
		current_logtalk_flag(engines, Value),
		once((Value == supported; Value == unsupported)).

	succeeds(threads_flag) :-
		current_logtalk_flag(threads, Value),
		once((Value == supported; Value == unsupported)).

	succeeds(tabling_flag) :-
		current_logtalk_flag(tabling, Value),
		once((Value == supported; Value == unsupported)).

	succeeds(unicode_flag) :-
		current_logtalk_flag(unicode, Value),
		once((Value == full; Value == bmp; Value == unsupported)).

	succeeds(encoding_directive_flag) :-
		current_logtalk_flag(encoding_directive, Value),
		once((Value == full; Value == source; Value == unsupported)).

	% auxiliary predicates

	test_flag(Flag, On, Off) :-
		current_logtalk_flag(Flag, Current),
		set_logtalk_flag(Flag, On),
		current_logtalk_flag(Flag, Value1),
		Value1 == On,
		set_logtalk_flag(Flag, Off),
		current_logtalk_flag(Flag, Value2),
		Value2 == Off,
		set_logtalk_flag(Flag, Current).

:- end_object.
