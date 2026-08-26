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


:- object(tests_2025_06_18,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-26,
		comment is 'Unit tests for the "factorial_mcp" example.'
	]).

	:- uses(json_rpc, [
		request/4, is_response/1, result/2, write_message/2, read_message/2
	]).

	cover(factorial_mcp).

	cleanup :-
		^^clean_file('mcp_input.tmp'),
		^^clean_file('mcp_output.tmp').

	test(factorial_mcp_initialize_01, true) :-
		run_mcp_exchange(
			[initialize_request(1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, protocolVersion, '2025-06-18'),
		has_pair(Result, capabilities, Capabilities),
		has_pair(Capabilities, tools, {}),
		has_pair(Result, serverInfo, ServerInfo),
		has_pair(ServerInfo, name, factorial_mcp).

	test(factorial_mcp_tools_list_01, true) :-
		run_mcp_exchange(
			[initialize_request(1), tools_list_request(2)],
			[_, Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, tools, [Tool]),
		has_pair(Tool, name, factorial),
		has_pair(Tool, inputSchema, InputSchema),
		has_pair(InputSchema, type, object).

	% auxiliary predicates

	run_mcp_exchange(RequestSpecs, Responses) :-
		^^file_path('mcp_input.tmp', InputFile),
		^^file_path('mcp_output.tmp', OutputFile),
		open(InputFile, write, InputOutput),
		write_request_specs(InputOutput, RequestSpecs),
		close(InputOutput),
		open(InputFile, read, Input),
		open(OutputFile, write, Output),
		mcp_server::start(factorial_mcp, factorial_mcp, Input, Output),
		close(Input),
		close(Output),
		open(OutputFile, read, ResponseInput),
		read_all_messages(ResponseInput, Responses),
		close(ResponseInput).

	write_request_specs(_, []).
	write_request_specs(Stream, [Spec| Specs]) :-
		spec_to_message(Spec, Message),
		write_message(Stream, Message),
		write_request_specs(Stream, Specs).

	spec_to_message(initialize_request(Id), Message) :-
		request(
			initialize,
			{protocolVersion-'2025-06-18', capabilities-{}, clientInfo-{name-test, version-'1.0'}},
			Id,
			Message
		).
	spec_to_message(tools_list_request(Id), Message) :-
		request('tools/list', {}, Id, Message).
	spec_to_message(tools_call_request(Name, Id), Message) :-
		request('tools/call', {name-Name, arguments-{}}, Id, Message).

	read_all_messages(Stream, Messages) :-
		(	catch(read_message(Stream, Message), _, fail) ->
			Messages = [Message| Rest],
			read_all_messages(Stream, Rest)
		;	Messages = []
		).

	has_pair({Pairs}, Key, Value) :-
		curly_member(Key-Value, Pairs).

	curly_member(Pair, (Pair, _)) :-
		!.
	curly_member(Pair, (_, Rest)) :-
		!,
		curly_member(Pair, Rest).
	curly_member(Pair, Pair).

:- end_object.
