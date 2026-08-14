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
		date is 2026-08-14,
		comment is 'Unit tests for the "birds_mcp" example.'
	]).

	:- uses(json_rpc, [
		request/4, response/3, is_request/1, is_response/1, method/2,
		params/2, result/2, id/2, write_message/2, read_message/2
	]).

	cover(birds_mcp).

	cleanup :-
		^^clean_file('mcp_input.tmp'),
		^^clean_file('mcp_output.tmp').

	test(birds_mcp_initialize_01, true) :-
		run_mcp_exchange(
			[initialize_elicitation_request(1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, protocolVersion, '2025-06-18'),
		has_pair(Result, capabilities, Capabilities),
		has_pair(Capabilities, tools, {}),
		\+ has_pair(Capabilities, elicitation, _),
		has_pair(Result, serverInfo, ServerInfo),
		has_pair(ServerInfo, name, 'birds-expert').

	test(birds_mcp_tools_list_01, true) :-
		run_mcp_exchange(
			[initialize_elicitation_request(1), tools_list_request(2)],
			[_, Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, tools, [Tool]),
		has_pair(Tool, name, identify_bird),
		has_pair(Tool, inputSchema, InputSchema),
		has_pair(InputSchema, type, object).

	test(birds_mcp_elicitation_capability_required_01, true) :-
		run_mcp_exchange(
			[initialize_request(1), tools_call_request(identify_bird, 2)],
			[_, Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, isError, @true).

	test(birds_mcp_elicitation_cancel_01, true) :-
		cancellation_response_specs(1, 64, CancellationResponses),
		RequestSpecs = [
			initialize_elicitation_request(1),
			tools_call_request(identify_bird, 2)
			| CancellationResponses
		],
		run_mcp_exchange(RequestSpecs, Responses),
		request_with_method(Responses, 'elicitation/create', ElicitRequest),
		params(ElicitRequest, ElicitParams),
		has_pair(ElicitParams, requestedSchema, Schema),
		has_pair(Schema, properties, Properties),
		has_pair(Properties, answer, AnswerSchema),
		has_pair(AnswerSchema, enum, [yes, no]),
		response_with_id(Responses, 2, ToolResponse),
		result(ToolResponse, ToolResult),
		has_pair(ToolResult, content, [Content]),
		has_pair(Content, text, 'No bird could be identified from the given characteristics.').

	% auxiliary predicates

	run_mcp_exchange(RequestSpecs, Responses) :-
		^^file_path('mcp_input.tmp', InputFile),
		^^file_path('mcp_output.tmp', OutputFile),
		open(InputFile, write, InputOutput),
		write_request_specs(InputOutput, RequestSpecs),
		close(InputOutput),
		open(InputFile, read, Input),
		open(OutputFile, write, Output),
		mcp_server::start('birds-expert', birds_mcp, Input, Output),
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
	spec_to_message(initialize_elicitation_request(Id), Message) :-
		request(
			initialize,
			{protocolVersion-'2025-06-18', capabilities-{elicitation-{}}, clientInfo-{name-test, version-'1.0'}},
			Id,
			Message
		).
	spec_to_message(tools_list_request(Id), Message) :-
		request('tools/list', {}, Id, Message).
	spec_to_message(tools_call_request(Name, Id), Message) :-
		request('tools/call', {name-Name, arguments-{}}, Id, Message).
	spec_to_message(elicit_cancel_response(Id), Message) :-
		response({action-cancel}, Id, Message).

	cancellation_response_specs(Id, Last, Specs) :-
		(	Id > Last ->
			Specs = []
		;	Specs = [elicit_cancel_response(ElicitId)| Rest],
			elicit_id(Id, ElicitId),
			Next is Id + 1,
			cancellation_response_specs(Next, Last, Rest)
		).

	elicit_id(Id, ElicitId) :-
		number_codes(Id, Codes),
		atom_codes(Atom, Codes),
		atom_concat(elicit_, Atom, ElicitId).

	read_all_messages(Stream, Messages) :-
		(	catch(read_message(Stream, Message), _, fail) ->
			Messages = [Message| Rest],
			read_all_messages(Stream, Rest)
		;	Messages = []
		).

	request_with_method([Message| _], Method, Message) :-
		is_request(Message),
		method(Message, Method),
		!.
	request_with_method([_| Messages], Method, Message) :-
		request_with_method(Messages, Method, Message).

	response_with_id([Message| _], Id, Message) :-
		is_response(Message),
		id(Message, Id),
		!.
	response_with_id([_| Messages], Id, Message) :-
		response_with_id(Messages, Id, Message).

	has_pair({Pairs}, Key, Value) :-
		curly_member(Key-Value, Pairs).

	curly_member(Pair, (Pair, _)) :-
		!.
	curly_member(Pair, (_, Rest)) :-
		!,
		curly_member(Pair, Rest).
	curly_member(Pair, Pair).

:- end_object.
