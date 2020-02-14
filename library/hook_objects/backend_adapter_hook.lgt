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


:- object(backend_adapter_hook,
	implements(expanding)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-02-14,
		comment is 'This hook object applies the expansion rules defined in the Prolog backend adapter file.',
		see_also is [
			default_workflow_hook, identity_hook,
			grammar_rules_hook, prolog_module_hook(_),
			write_to_stream_hook(_, _), write_to_stream_hook(_)
		]
	]).

	term_expansion(Term, ExpandedTerm) :-
		{'$lgt_pp_hook_term_expansion_'(Term, ExpandedTerm)}.

	goal_expansion(Goal, ExpandedGoal) :-
		{'$lgt_prolog_goal_expansion'(Goal, ExpandedGoal)}.

:- end_object.
