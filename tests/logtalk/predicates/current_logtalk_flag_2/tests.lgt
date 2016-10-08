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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2013/05/15,
		comment is 'Unit tests for the current_logtalk_flag/2 built-in predicate.'
	]).

	:- discontiguous(succeeds/1).
	:- discontiguous(fails/1).
	:- discontiguous(throws/2).

	throws(current_logtalk_flag_2_1, error(type_error(atom, 1), logtalk(current_logtalk_flag(1, _), _))) :-
		{current_logtalk_flag(1, _)}.

	throws(current_logtalk_flag_2_2, error(domain_error(flag, non_existing_flag), logtalk(current_logtalk_flag(non_existing_flag, _), _))) :-
		{current_logtalk_flag(non_existing_flag, _)}.

	succeeds(unknown_entities_flag) :-
		test_flag(unknown_entities, warning, silent).
	succeeds(unknown_predicates_flag) :-
		test_flag(unknown_predicates, warning, silent).
	succeeds(undefined_predicates_flag) :-
		test_flag(undefined_predicates, warning, silent).
	succeeds(portability_flag) :-
		test_flag(portability, warning, silent).
	succeeds(singleton_variables_flag) :-
		test_flag(singleton_variables, warning, silent).
	succeeds(underscore_variables_flag) :-
		test_flag(underscore_variables, singletons, dont_care).

	succeeds(clean_flag) :-
		test_flag(clean, on, off).
	succeeds(debug_flag) :-
		test_flag(debug, on, off).

	succeeds(complements_flag) :-
		test_flag(complements, allow, deny).
	succeeds(dynamic_declarations_flag) :-
		test_flag(dynamic_declarations, allow, deny).
	succeeds(context_switching_calls_flag) :-
		test_flag(context_switching_calls, allow, deny).
	succeeds(events_flag) :-
		test_flag(events, allow, deny).

	succeeds(modules_flag) :-
		current_logtalk_flag(modules, Value),
		once((Value == supported; Value == unsupported)).
	succeeds(threads_flag) :-
		current_logtalk_flag(threads, Value),
		once((Value == supported; Value == unsupported)).
	succeeds(tabling_flag) :-
		current_logtalk_flag(tabling, Value),
		once((Value == supported; Value == unsupported)).

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
