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


:- object(default_workflow_hook,
	implements(expanding)).

	:- info([
		version is 1:0:1,
		author is 'Paulo Moura',
		date is 2020-03-24,
		comment is 'Use this object as the default hook object to restore the default expansion pipeline semantics used by the compiler.',
		see_also is [
			backend_adapter_hook, identity_hook,
			grammar_rules_hook, prolog_module_hook(_), object_wrapper_hook,
			write_to_stream_hook(_, _), write_to_stream_hook(_),
			print_goal_hook, suppress_goal_hook
		]
	]).

	% by not defining the expansion predicates, these trivially fail;
	% therefore, the compiler will try first any defined file specific
	% hook object followed, if that fails, by the backend adapter file
	% expansion rules

:- end_object.
