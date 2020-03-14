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


:- op(200, fy, *).


:- object(print_goal_hook,
	implements(expanding)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-03-14,
		comment is 'Use this object to easily print entity predicate goals before, after, or before and after calling them.',
		remarks is [
			'Usage' - 'Mark a goal to be printed by prefixing it with an operator. Printing uses a ``comment`` message.',
			'To print goal before calling it' - '``- Goal``',
			'To print goal after calling it' - '``+ Goal``',
			'To print goal before and after calling it' - '``* Goal``',
			'Operators' - 'This hook object uses the standard ``-`` and ``+`` prefix operators and also defines a global ``*`` prefix operator with the same type and priority.'
		],
		see_also is [
			backend_adapter_hook, default_workflow_hook,
			grammar_rules_hook, identity_hook, prolog_module_hook(_),
			write_to_stream_hook(_, _), write_to_stream_hook(_)
		]
	]).

	% print goal before calling
	goal_expansion(- Goal, ExpandedGoal) :-
		ExpandedGoal = (logtalk::print_message(comment,core,'~q'+[Goal]), Goal).
	% print goal after calling
	goal_expansion(+ Goal, ExpandedGoal) :-
		ExpandedGoal = (Goal, logtalk::print_message(comment,core,'~q'+[Goal])).
	% print goal before and after calling
	goal_expansion(* Goal, ExpandedGoal) :-
		ExpandedGoal = (logtalk::print_message(comment,core,'~q'+[Goal]), Goal, logtalk::print_message(comment,core,'~q'+[Goal])).

:- end_object.
