%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2015 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
