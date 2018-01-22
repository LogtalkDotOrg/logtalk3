%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


:- object(dump_trace).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/12/16,
		comment is 'Simple solution for redirecting a debugger trace to a file.'
	]).

	:- public(start_redirect_to_file/2).
	:- meta_predicate(start_redirect_to_file(*, 0)).
	:- mode(start_redirect_to_file(+atom, +callable), zero_or_more).
	:- info(start_redirect_to_file/2, [
		comment is 'Starts redirecting debugger trace messages to a file.',
		argnames is ['File', 'Goal']
	]).

	:- public(stop_redirect_to_file/0).
	:- mode(stop_redirect_to_file, one).
	:- info(stop_redirect_to_file/0, [
		comment is 'Stops redirecting debugger trace messages to a file.'
	]).

	start_redirect_to_file(File, Goal) :-
		open(File, write, _, [alias(debugger_redirected_trace)]),
		logtalk::assertz((
			message_hook(tracing_port(_,_,_,_), Kind, debugger, Tokens) :-
				message_prefix_stream(Kind, debugger, Prefix, _),
				print_message_tokens(debugger_redirected_trace, Prefix, Tokens)
		)),
		logtalk::assertz((
			message_hook(tracing_port(_,_,_,_,_), Kind, debugger, Tokens) :-
				message_prefix_stream(Kind, debugger, Prefix, _),
				print_message_tokens(debugger_redirected_trace, Prefix, Tokens)
		)),
		debugger::trace,
		debugger::leash(none),
		call(Goal).

	stop_redirect_to_file :-
		debugger::reset,
		logtalk::retractall(message_hook(_, _, debugger, _)),
		close(debugger_redirected_trace).

:- end_object.
