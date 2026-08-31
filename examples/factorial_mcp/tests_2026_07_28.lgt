%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests_2026_07_28,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-26,
		comment is 'Unit tests for the "factorial_mcp" example under the MCP 2026-07-28 adapter.'
	]).

	:- uses(json_rpc, [
		request/4, is_response/1, is_error_response/1, result/2, write_message/2, read_message/2
	]).

	:- uses(list, [
		member/2
	]).

	cover(factorial_mcp).

	cleanup :-
		^^clean_file('mcp26_factorial_in.tmp'),
		^^clean_file('mcp26_factorial_out.tmp').

	% Discover advertises tools
	test(factorial_mcp26_discover_01, true) :-
		run_2026([discover_request(1)], [Response]),
		is_response(Response),
		\+ is_error_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, complete),
		has_pair(Result, capabilities, Caps),
		has_pair(Caps, tools, _).

	% tools/list includes factorial
	test(factorial_mcp26_tools_list_01, true) :-
		run_2026([tools_list_request(1)], [Response]),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, complete),
		has_pair(Result, tools, Tools),
		member(Tool, Tools),
		has_pair(Tool, name, factorial).

	% auxiliary predicates

	run_2026(Specs, Responses) :-
		^^file_path('mcp26_factorial_in.tmp', InFile),
		^^file_path('mcp26_factorial_out.tmp', OutFile),
		open(InFile, write, Out),
		write_specs(Out, Specs),
		close(Out),
		open(InFile, read, In),
		open(OutFile, write, OutStream),
		mcp_server::start(
			factorial_mcp,
			factorial_mcp,
			In,
			OutStream,
			[spec('2026-07-28'), transport(stdio)]
		),
		close(In),
		close(OutStream),
		open(OutFile, read, InStream),
		read_all(InStream, Responses),
		close(InStream).

	write_specs(_, []).
	write_specs(Stream, [Spec| Specs]) :-
		spec_to_message(Spec, Message),
		write_message(Stream, Message),
		write_specs(Stream, Specs).

	read_all(Stream, Messages) :-
		(	catch(read_message(Stream, Message), _, fail) ->
			Messages = [Message| Rest],
			read_all(Stream, Rest)
		;	Messages = []
		).

	meta_2026(Meta) :-
		Meta = {
			'io.modelcontextprotocol/protocolVersion'-'2026-07-28',
			'io.modelcontextprotocol/clientCapabilities'-{
				tools-{},
				elicitation-{}
			}
		}.

	spec_to_message(discover_request(Id), Message) :-
		meta_2026(Meta),
		request('server/discover', {'_meta'-Meta}, Id, Message).
	spec_to_message(tools_list_request(Id), Message) :-
		meta_2026(Meta),
		request('tools/list', {'_meta'-Meta}, Id, Message).
	spec_to_message(tools_call_request(Name, Args, Id), Message) :-
		meta_2026(Meta),
		request('tools/call', {name-Name, arguments-Args, '_meta'-Meta}, Id, Message).
	spec_to_message(tools_call_with_state(Name, Args, Responses, State, Id), Message) :-
		meta_2026(Meta),
		request('tools/call', {
			name-Name,
			arguments-Args,
			inputResponses-Responses,
			requestState-State,
			'_meta'-Meta
		}, Id, Message).

	has_pair({Pairs}, Key, Value) :-
		curly_member(Key-Value, Pairs).

	curly_member(Pair, (Pair, _)) :-
		!.
	curly_member(Pair, (_, Rest)) :-
		!, curly_member(Pair, Rest).
	curly_member(Pair, Pair).

:- end_object.
