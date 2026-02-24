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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0:2:0,
		author is 'Paulo Moura',
		date is 2026-02-24,
		comment is 'Unit tests for the "mcp_server" library.'
	]).

	:- uses(json_rpc, [
		request/4, request/3, notification/3, response/3, is_request/1, is_response/1, is_error_response/1,
		method/2, params/2, result/2, id/2, error_code/2, write_message/2, read_message/2
	]).

	:- uses(list, [
		member/2
	]).

	cover(mcp_server).

	cleanup :-
		^^clean_file('mcp_input.tmp'),
		^^clean_file('mcp_output.tmp').

	% ==========================================================================
	% json_rpc Content-Length framing tests
	% ==========================================================================

	test(json_rpc_write_read_message_01, true(json_rpc::is_request(ReadMsg))) :-
		^^file_path('mcp_input.tmp', File),
		open(File, write, Output),
		json_rpc::request(subtract, [42,23], 1, Request),
		json_rpc::write_message(Output, Request),
		close(Output),
		open(File, read, Input),
		json_rpc::read_message(Input, ReadMsg),
		close(Input).

	test(json_rpc_write_read_message_02, true(Method-Id == subtract-1)) :-
		^^file_path('mcp_input.tmp', File),
		open(File, write, Output),
		json_rpc::request(subtract, [42,23], 1, Request),
		json_rpc::write_message(Output, Request),
		close(Output),
		open(File, read, Input),
		json_rpc::read_message(Input, ReadMsg),
		close(Input),
		json_rpc::method(ReadMsg, Method),
		json_rpc::id(ReadMsg, Id).

	test(json_rpc_write_read_framed_multiple_01, true(M1-M2 == subtract-update)) :-
		^^file_path('mcp_input.tmp', File),
		open(File, write, Output),
		json_rpc::request(subtract, [42,23], 1, R1),
		json_rpc::notification(update, [1,2,3], N1),
		json_rpc::write_message(Output, R1),
		json_rpc::write_message(Output, N1),
		close(Output),
		open(File, read, Input),
		json_rpc::read_message(Input, ReadMsg1),
		json_rpc::read_message(Input, ReadMsg2),
		close(Input),
		json_rpc::method(ReadMsg1, M1),
		json_rpc::method(ReadMsg2, M2).

	test(json_rpc_read_framed_eof_01, fail) :-
		^^file_path('mcp_input.tmp', File),
		open(File, write, Output),
		close(Output),
		open(File, read, Input),
		json_rpc::read_message(Input, _),
		close(Input).

	% ==========================================================================
	% MCP server protocol tests
	% ==========================================================================

	test(mcp_server_initialize_01, true) :-
		run_mcp_exchange(
			[initialize_request(1)],
			Responses
		),
		Responses = [Response],
		is_response(Response),
		id(Response, 1),
		result(Response, Result),
		has_pair(Result, protocolVersion, '2025-03-26'),
		has_pair(Result, serverInfo, ServerInfo),
		has_pair(ServerInfo, name, 'test-server').

	test(mcp_server_ping_01, true) :-
		run_mcp_exchange(
			[ping_request(1)],
			[Response]
		),
		is_response(Response),
		id(Response, 1).

	test(mcp_server_tools_list_01, true) :-
		run_mcp_exchange(
			[tools_list_request(1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, tools, Tools),
		Tools = [_| _],
		member(Tool, Tools),
		has_pair(Tool, name, factorial).

	test(mcp_server_tools_call_auto_dispatch_01, true(sub_atom(Text, _, _, _, 'Hello, world!'))) :-
		run_mcp_exchange(
			[tools_call_request(greet, {'Name'-world}, 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, content, [ContentItem| _]),
		has_pair(ContentItem, text, Text).

	test(mcp_server_tools_call_add_01, true) :-
		run_mcp_exchange(
			[tools_call_request(add, {'X'-3, 'Y'-4}, 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, content, [ContentItem| _]),
		has_pair(ContentItem, text, Text),
		atom(Text),
		sub_atom(Text, _, _, _, '7').

	test(mcp_server_tools_call_failure_01, true) :-
		run_mcp_exchange(
			[tools_call_request(always_fails, {'Input'-test}, 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, isError, @true).

	test(mcp_server_unknown_method_01, true) :-
		run_mcp_exchange(
			[unknown_request(1)],
			[Response]
		),
		is_error_response(Response),
		error_code(Response, -32601).

	test(mcp_server_tool_not_found_01, true) :-
		run_mcp_exchange(
			[tools_call_request(nonexistent_tool, {}, 1)],
			[Response]
		),
		is_error_response(Response).

	% ==========================================================================
	% Initialize capability tests
	% ==========================================================================

	% Without elicitation, capabilities should only include tools
	test(mcp_server_initialize_capabilities_01, true) :-
		run_mcp_exchange(
			[initialize_request(1)],
			[Response]
		),
		result(Response, Result),
		has_pair(Result, capabilities, Caps),
		has_pair(Caps, tools, {}),
		\+ has_pair(Caps, elicitation, _).

	% With elicitation, capabilities should include both tools and elicitation
	test(mcp_server_initialize_elicit_capabilities_01, true) :-
		run_mcp_exchange_with(
			test_elicit_tools,
			[initialize_request(1)],
			[Response]
		),
		result(Response, Result),
		has_pair(Result, capabilities, Caps),
		has_pair(Caps, tools, _),
		has_pair(Caps, elicitation, _).

	% ==========================================================================
	% Elicitation tests
	% ==========================================================================

	% Full elicitation round-trip: tool asks for name, client accepts
	test(mcp_server_elicitation_accept_01, true(sub_atom(Text, _, _, _, 'Hello, Alice!'))) :-
		run_mcp_exchange_with(
			test_elicit_tools,
			[initialize_request(1),
			 tools_call_request(ask_name, {}, 2),
			 elicit_accept_response(elicit_1, {name-'Alice'})],
			Responses
		),
		Responses = [_InitResponse, ElicitReq, ToolResponse],
		% The middle message should be the elicitation request from server
		is_request(ElicitReq),
		method(ElicitReq, 'elicitation/create'),
		% The final message should be the tool result
		is_response(ToolResponse),
		id(ToolResponse, 2),
		result(ToolResponse, Result),
		has_pair(Result, content, [ContentItem| _]),
		has_pair(ContentItem, text, Text).

	% Verify the structure of the elicitation/create request
	test(mcp_server_elicitation_request_structure_01, true(Message == 'What is your name?')) :-
		run_mcp_exchange_with(
			test_elicit_tools,
			[initialize_request(1),
			 tools_call_request(ask_name, {}, 2),
			 elicit_accept_response(elicit_1, {name-test})],
			[_, ElicitReq, _]
		),
		is_request(ElicitReq),
		method(ElicitReq, 'elicitation/create'),
		id(ElicitReq, elicit_1),
		params(ElicitReq, Params),
		has_pair(Params, message, Message),
		has_pair(Params, requestedSchema, Schema),
		has_pair(Schema, type, object).

	% Elicitation with decline response
	test(mcp_server_elicitation_decline_01, true(Text == 'User declined.')) :-
		run_mcp_exchange_with(
			test_elicit_tools,
			[initialize_request(1),
			 tools_call_request(ask_name, {}, 2),
			 elicit_decline_response(elicit_1)],
			[_, _, ToolResponse]
		),
		is_response(ToolResponse),
		result(ToolResponse, Result),
		has_pair(Result, content, [ContentItem| _]),
		has_pair(ContentItem, text, Text).

	% Elicitation with cancel response
	test(mcp_server_elicitation_cancel_01, true(Text == 'Cancelled.')) :-
		run_mcp_exchange_with(
			test_elicit_tools,
			[initialize_request(1),
			 tools_call_request(ask_name, {}, 2),
			 elicit_cancel_response(elicit_1)],
			[_, _, ToolResponse]
		),
		is_response(ToolResponse),
		result(ToolResponse, Result),
		has_pair(Result, content, [ContentItem| _]),
		has_pair(ContentItem, text, Text).

	% ==========================================================================
	% Elicitation fallback tests
	% ==========================================================================

	% Non-eliciting tool in elicit-capable app falls back to auto-dispatch
	test(mcp_server_elicit_fallback_auto_dispatch_01, true(sub_atom(Text, _, _, _, hello))) :-
		run_mcp_exchange_with(
			test_elicit_tools,
			[initialize_request(1),
			 tools_call_request(echo, {'Input'-hello}, 2)],
			[_, ToolResponse]
		),
		is_response(ToolResponse),
		result(ToolResponse, Result),
		has_pair(Result, content, [ContentItem| _]),
		has_pair(ContentItem, text, Text).

	% ==========================================================================
	% Result format tests
	% ==========================================================================

	% Multi-content result from results/1 format
	test(mcp_server_results_format_01, true(list::length(Content, 2))) :-
		run_mcp_exchange_with(
			test_result_tools,
			[initialize_request(1),
			 tools_call_request(multi_result, {}, 2)],
			[_, ToolResponse]
		),
		is_response(ToolResponse),
		result(ToolResponse, Result),
		has_pair(Result, content, Content).

	% Tool exception results in isError response
	test(mcp_server_tool_exception_01, true) :-
		run_mcp_exchange_with(
			test_result_tools,
			[initialize_request(1),
			 tools_call_request(throw_error, {}, 2)],
			[_, ToolResponse]
		),
		is_response(ToolResponse),
		result(ToolResponse, Result),
		has_pair(Result, isError, @true).

	% ==========================================================================
	% Sequential tool call tests
	% ==========================================================================

	% Multiple tool calls in a single session
	test(mcp_server_multiple_calls_01, true) :-
		run_mcp_exchange(
			[initialize_request(1),
			 tools_call_request(greet, {'Name'-world}, 2),
			 tools_call_request(add, {'X'-3, 'Y'-4}, 3)],
			[_, GreetResponse, AddResponse]
		),
		is_response(GreetResponse),
		id(GreetResponse, 2),
		result(GreetResponse, GreetResult),
		has_pair(GreetResult, content, [GreetContent| _]),
		has_pair(GreetContent, text, GreetText),
		sub_atom(GreetText, _, _, _, 'Hello, world!'),
		is_response(AddResponse),
		id(AddResponse, 3),
		result(AddResponse, AddResult),
		has_pair(AddResult, content, [AddContent| _]),
		has_pair(AddContent, text, AddText),
		sub_atom(AddText, _, _, _, '7').

	% ==========================================================================
	% Test helpers
	% ==========================================================================

	% Run an MCP exchange using the public start/4 API with file streams
	run_mcp_exchange(RequestSpecs, Responses) :-
		run_mcp_exchange_with(test_tools, RequestSpecs, Responses).

	% Run an MCP exchange with a specific application object
	run_mcp_exchange_with(Application, RequestSpecs, Responses) :-
		^^file_path('mcp_input.tmp', InputFile),
		^^file_path('mcp_output.tmp', OutputFile),
		% Write all requests to input file
		open(InputFile, write, Out),
		write_request_specs(Out, RequestSpecs),
		close(Out),
		% Run server with file streams using the public API
		open(InputFile, read, In),
		open(OutputFile, write, OutStream),
		mcp_server::start('test-server', Application, In, OutStream),
		close(In),
		close(OutStream),
		% Read all responses from output file
		open(OutputFile, read, InStream),
		read_all_framed(InStream, Responses),
		close(InStream).

	% Write request specs to a stream
	write_request_specs(_, []).
	write_request_specs(Stream, [Spec| Specs]) :-
		spec_to_message(Spec, Message),
		write_message(Stream, Message),
		write_request_specs(Stream, Specs).

	% Convert test request specs to JSON-RPC messages
	spec_to_message(initialize_request(Id), Message) :-
		request(
			initialize,
			{protocolVersion-'2025-03-26', capabilities-{}, clientInfo-{name-test, version-'1.0'}},
			Id,
			Message
		).
	spec_to_message(ping_request(Id), Message) :-
		request(ping, Id, Message).
	spec_to_message(tools_list_request(Id), Message) :-
		request('tools/list', {}, Id, Message).
	spec_to_message(tools_call_request(ToolName, Arguments, Id), Message) :-
		request('tools/call', {name-ToolName, arguments-Arguments}, Id, Message).
	spec_to_message(unknown_request(Id), Message) :-
		request(unknown_method, {}, Id, Message).
	spec_to_message(elicit_accept_response(Id, Content), Message) :-
		response({action-accept, content-Content}, Id, Message).
	spec_to_message(elicit_decline_response(Id), Message) :-
		response({action-decline}, Id, Message).
	spec_to_message(elicit_cancel_response(Id), Message) :-
		response({action-cancel}, Id, Message).

	% Read all framed messages from a stream
	read_all_framed(Stream, Messages) :-
		(	catch(read_message(Stream, Message), _, fail) ->
			Messages = [Message| Rest],
			read_all_framed(Stream, Rest)
		;	Messages = []
		).

	% curly-term pair lookup (for test assertions)
	has_pair({Pairs}, Key, Value) :-
		curly_member(Key-Value, Pairs).

	curly_member(Pair, (Pair, _)) :-
		!.
	curly_member(Pair, (_, Rest)) :-
		!,
		curly_member(Pair, Rest).
	curly_member(Pair, Pair).

:- end_object.
