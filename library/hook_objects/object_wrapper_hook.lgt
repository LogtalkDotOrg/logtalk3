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


:- object(object_wrapper_hook,
	implements(expanding)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-03-19,
		comment is 'Use this object to wrap the contents of a plain prolog file as an object named after the file.',
		see_also is [
			backend_adapter_hook, default_workflow_hook,
			grammar_rules_hook, prolog_module_hook(_),
			write_to_stream_hook(_, _), write_to_stream_hook(_), print_goal_hook
		]
	]).

	term_expansion(begin_of_file, [begin_of_file, (:- object(Name))]) :-
		logtalk_load_context(basename, Basename),
		os::decompose_file_name(Basename, _, Name, _).

	term_expansion(end_of_file, [(:- end_object), end_of_file]).

:- end_object.
