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


:- object(default_workflow_hook,
	implements(expanding)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-02-14,
		comment is 'Use this object as the default hook object to restore the default expansion pipeline semantics used by the compiler.',
		see_also is [
			backend_adapter_hook, identity_hook,
			grammar_rules_hook, prolog_module_hook(_),
			write_to_stream_hook(_, _), write_to_stream_hook(_)
		]
	]).

	% define the expansion predicates to trivially fail to try first
	% any defined file specific hook object followed, if that fails,
	% the backend adapter file expansion rules

	term_expansion(_, _) :-
		fail.

	goal_expansion(_, _) :-
		fail.

:- end_object.
