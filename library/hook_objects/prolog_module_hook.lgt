%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- object(prolog_module_hook(_Module_),
	implements(expanding)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-02-17,
		comment is 'This hook object applies the expansion rules defined in a Prolog module (e.g., ``user``).',
		parnames is ['Module'],
		see_also is [
			backend_adapter_hook, default_workflow_hook,
			identity_hook, grammar_rules_hook, object_wrapper_hook,
			write_to_stream_hook(_, _), write_to_stream_hook(_),
			print_goal_hook, suppress_goal_hook
		]
	]).

	term_expansion(Term, ExpandedTerm) :-
		(	_Module_ == user ->
			{term_expansion(Term, ExpandedTerm)}
		;	_Module_:term_expansion(Term, ExpandedTerm)
		).

	goal_expansion(Goal, ExpandedGoal) :-
		(	_Module_ == user ->
			{goal_expansion(Goal, ExpandedGoal)}
		;	_Module_:goal_expansion(Goal, ExpandedGoal)
		).

:- end_object.


:- if(current_logtalk_flag(prolog_dialect, gnu)).
	% workaround gplc limitation when dealing with predicates
	% that are called from a file but not defined in that file
	:- multifile(term_expansion/2).
	:- dynamic(term_expansion/2).
	:- multifile(goal_expansion/2).
	:- dynamic(goal_expansion/2).
:- endif.
