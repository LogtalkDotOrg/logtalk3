%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(write_to_stream_hook(_Stream_, _Options_),
	implements(expanding)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-02-16,
		comment is 'This hook object writes term-expansion results to a stream using a list of ``write_term/3`` options. The terms are terminated by a period and a new line.',
		parnames is ['Stream', 'Options'],
		see_also is [
			backend_adapter_hook, default_workflow_hook,
			identity_hook, grammar_rules_hook,
			prolog_module_hook(_),  object_wrapper_hook,
			write_to_stream_hook(_),
			print_goal_hook, suppress_goal_hook
		]
	]).

	term_expansion(begin_of_file, begin_of_file) :-
		!.
	term_expansion(end_of_file, end_of_file) :-
		!.
	term_expansion(Term, Term) :-
		write_term(_Stream_, Term, _Options_),
		write(_Stream_, '.'), nl(_Stream_).

:- end_object.


:- object(write_to_stream_hook(Stream),
	extends(write_to_stream_hook(Stream, [quoted(true),ignore_ops(true)]))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-02-16,
		comment is 'This hook object writes term-expansion results to a stream in canonical format. The terms are terminated by a period and a new line.',
		parnames is ['Stream'],
		see_also is [
			backend_adapter_hook, default_workflow_hook,
			identity_hook, grammar_rules_hook,
			prolog_module_hook(_), object_wrapper_hook,
			write_to_stream_hook(_, _),
			print_goal_hook, suppress_goal_hook
		]
	]).

:- end_object.
