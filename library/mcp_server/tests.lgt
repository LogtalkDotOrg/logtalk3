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
		version is 0:5:0,
		author is 'Paulo Moura',
		date is 2026-02-24,
		comment is 'Unit tests for the "mcp_server" library.'
	]).

	:- uses(json_rpc, [
		request/4, request/3, notification/3, response/3, is_request/1, is_response/1, is_error_response/1,
		method/2, params/2, result/2, id/2, error_code/2, error_data/2, write_message/2, read_message/2
	]).

	:- uses(list, [
		member/2
	]).

	cover(mcp_server).

	cleanup :-
		^^clean_file('mcp_input.tmp'),
		^^clean_file('mcp_output.tmp').

	% json_rpc Content-Length framing tests

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

	% MCP server protocol tests

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

	% Protocol version negotiation tests

	% Client sends a newer version; server should negotiate down to its supported version
	test(mcp_server_initialize_newer_version_01, true) :-
		run_mcp_exchange(
			[initialize_version_request('2026-01-01', 1)],
			[Response]
		),
		is_response(Response),
		\+ is_error_response(Response),
		id(Response, 1),
		result(Response, Result),
		has_pair(Result, protocolVersion, '2025-03-26').

	% Client sends an older unsupported version; server should reject with error -32602
	test(mcp_server_initialize_older_version_01, true) :-
		run_mcp_exchange(
			[initialize_version_request('2024-11-05', 1)],
			[Response]
		),
		is_error_response(Response),
		id(Response, 1),
		error_code(Response, -32602).

	% Rejected initialization should include supported versions in data
	test(mcp_server_initialize_rejected_version_data_01, true) :-
		run_mcp_exchange(
			[initialize_version_request('2024-11-05', 1)],
			[Response]
		),
		is_error_response(Response),
		error_data(Response, Data),
		has_pair(Data, supported, Supported),
		member('2025-03-26', Supported).

	% Client sends the exact supported version; should succeed
	test(mcp_server_initialize_exact_version_01, true) :-
		run_mcp_exchange(
			[initialize_version_request('2025-03-26', 1)],
			[Response]
		),
		is_response(Response),
		\+ is_error_response(Response),
		result(Response, Result),
		has_pair(Result, protocolVersion, '2025-03-26').

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

	% Initialize capability tests

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

	% Elicitation tests

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

	% Elicitation fallback tests

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

	% Result format tests

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

	% Sequential tool call tests

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

	% Prompt capability tests

	% With prompts capability, capabilities should include prompts
	test(mcp_server_initialize_prompt_capabilities_01, true) :-
		run_mcp_exchange_with(
			test_prompt_tools,
			[initialize_request(1)],
			[Response]
		),
		result(Response, Result),
		has_pair(Result, capabilities, Caps),
		has_pair(Caps, tools, _),
		has_pair(Caps, prompts, _).

	% Without prompts capability, capabilities should not include prompts
	test(mcp_server_initialize_no_prompt_capabilities_01, true) :-
		run_mcp_exchange(
			[initialize_request(1)],
			[Response]
		),
		result(Response, Result),
		has_pair(Result, capabilities, Caps),
		\+ has_pair(Caps, prompts, _).

	% Prompts/list tests

	% List prompts from prompt-capable application
	test(mcp_server_prompts_list_01, true) :-
		run_mcp_exchange_with(
			test_prompt_tools,
			[prompts_list_request(1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, prompts, Prompts),
		Prompts = [_| _],
		member(Prompt, Prompts),
		has_pair(Prompt, name, greet_prompt).

	% Prompts list includes all declared prompts
	test(mcp_server_prompts_list_02, true(list::length(Prompts, 3))) :-
		run_mcp_exchange_with(
			test_prompt_tools,
			[prompts_list_request(1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, prompts, Prompts).

	% Prompts list from non-prompt application returns empty list
	test(mcp_server_prompts_list_03, true(Prompts == [])) :-
		run_mcp_exchange(
			[prompts_list_request(1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, prompts, Prompts).

	% Prompt descriptor includes arguments
	test(mcp_server_prompts_list_arguments_01, true) :-
		run_mcp_exchange_with(
			test_prompt_tools,
			[prompts_list_request(1)],
			[Response]
		),
		result(Response, Result),
		has_pair(Result, prompts, Prompts),
		member(Prompt, Prompts),
		has_pair(Prompt, name, greet_prompt),
		has_pair(Prompt, description, 'Generates a greeting prompt'),
		has_pair(Prompt, arguments, Arguments),
		Arguments = [Arg],
		has_pair(Arg, name, name),
		has_pair(Arg, required, @true).

	% Prompt with no arguments has empty arguments list
	test(mcp_server_prompts_list_no_args_01, true(Arguments == [])) :-
		run_mcp_exchange_with(
			test_prompt_tools,
			[prompts_list_request(1)],
			[Response]
		),
		result(Response, Result),
		has_pair(Result, prompts, Prompts),
		member(Prompt, Prompts),
		has_pair(Prompt, name, simple_prompt),
		has_pair(Prompt, arguments, Arguments).

	% Prompts/get tests

	% Get a prompt with arguments
	test(mcp_server_prompts_get_01, true(sub_atom(Text, _, _, _, 'Alice'))) :-
		run_mcp_exchange_with(
			test_prompt_tools,
			[prompts_get_request(greet_prompt, {name-'Alice'}, 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, messages, [Message| _]),
		has_pair(Message, role, user),
		has_pair(Message, content, Content),
		has_pair(Content, type, text),
		has_pair(Content, text, Text).

	% Get a simple prompt without arguments
	test(mcp_server_prompts_get_02, true) :-
		run_mcp_exchange_with(
			test_prompt_tools,
			[prompts_get_request(simple_prompt, {}, 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, description, 'A simple test prompt'),
		has_pair(Result, messages, [Message]),
		has_pair(Message, role, user),
		has_pair(Message, content, Content),
		has_pair(Content, text, 'This is a simple test prompt.').

	% Get a prompt without providing arguments object
	test(mcp_server_prompts_get_03, true) :-
		run_mcp_exchange_with(
			test_prompt_tools,
			[prompts_get_request(simple_prompt, 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, messages, [_]).

	% Get a multi-turn prompt
	test(mcp_server_prompts_get_multi_turn_01, true(list::length(Messages, 2))) :-
		run_mcp_exchange_with(
			test_prompt_tools,
			[prompts_get_request(multi_turn_prompt, {topic-'Logtalk'}, 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, messages, Messages),
		Messages = [Msg1, Msg2],
		has_pair(Msg1, role, user),
		has_pair(Msg2, role, assistant).

	% Prompt not found returns error
	test(mcp_server_prompts_get_not_found_01, true) :-
		run_mcp_exchange_with(
			test_prompt_tools,
			[prompts_get_request(nonexistent_prompt, {}, 1)],
			[Response]
		),
		is_error_response(Response).

	% Combined tools and prompts tests

	% Application with both tools and prompts: both capabilities advertised
	test(mcp_server_tools_and_prompts_capabilities_01, true) :-
		run_mcp_exchange_with(
			test_tools_and_prompts,
			[initialize_request(1)],
			[Response]
		),
		result(Response, Result),
		has_pair(Result, capabilities, Caps),
		has_pair(Caps, tools, _),
		has_pair(Caps, prompts, _).

	% Application with both tools and prompts: tools work
	test(mcp_server_tools_and_prompts_tool_call_01, true(sub_atom(Text, _, _, _, '7'))) :-
		run_mcp_exchange_with(
			test_tools_and_prompts,
			[tools_call_request(add, {'X'-3, 'Y'-4}, 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, content, [ContentItem| _]),
		has_pair(ContentItem, text, Text).

	% Application with both tools and prompts: prompts work
	test(mcp_server_tools_and_prompts_prompt_get_01, true(sub_atom(Text, _, _, _, 'summarize'))) :-
		run_mcp_exchange_with(
			test_tools_and_prompts,
			[prompts_get_request(summarize, {text-'Hello world'}, 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, messages, [Message| _]),
		has_pair(Message, content, Content),
		has_pair(Content, text, Text).

	% Resource capability tests

	% With resources capability, capabilities should include resources
	test(mcp_server_initialize_resource_capabilities_01, true) :-
		run_mcp_exchange_with(
			test_resource_tools,
			[initialize_request(1)],
			[Response]
		),
		result(Response, Result),
		has_pair(Result, capabilities, Caps),
		has_pair(Caps, tools, _),
		has_pair(Caps, resources, _).

	% Without resources capability, capabilities should not include resources
	test(mcp_server_initialize_no_resource_capabilities_01, true) :-
		run_mcp_exchange(
			[initialize_request(1)],
			[Response]
		),
		result(Response, Result),
		has_pair(Result, capabilities, Caps),
		\+ has_pair(Caps, resources, _).

	% Resources/list tests

	% List resources from resource-capable application
	test(mcp_server_resources_list_01, true) :-
		run_mcp_exchange_with(
			test_resource_tools,
			[resources_list_request(1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resources, Resources),
		Resources = [_| _],
		member(Resource, Resources),
		has_pair(Resource, uri, 'logtalk://test/greeting').

	% Resources list includes all declared resources
	test(mcp_server_resources_list_02, true(list::length(Resources, 3))) :-
		run_mcp_exchange_with(
			test_resource_tools,
			[resources_list_request(1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resources, Resources).

	% Resources list from non-resource application returns empty list
	test(mcp_server_resources_list_03, true(Resources == [])) :-
		run_mcp_exchange(
			[resources_list_request(1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resources, Resources).

	% Resource descriptor includes all fields
	test(mcp_server_resources_list_fields_01, true) :-
		run_mcp_exchange_with(
			test_resource_tools,
			[resources_list_request(1)],
			[Response]
		),
		result(Response, Result),
		has_pair(Result, resources, Resources),
		member(Resource, Resources),
		has_pair(Resource, uri, 'logtalk://test/greeting'),
		has_pair(Resource, name, greeting),
		has_pair(Resource, description, 'A greeting message'),
		has_pair(Resource, mimeType, 'text/plain').

	% Resources/read tests

	% Read a text resource
	test(mcp_server_resources_read_01, true(Text == 'Hello from Logtalk!')) :-
		run_mcp_exchange_with(
			test_resource_tools,
			[resources_read_request('logtalk://test/greeting', 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, contents, [ContentItem| _]),
		has_pair(ContentItem, uri, 'logtalk://test/greeting'),
		has_pair(ContentItem, mimeType, 'text/plain'),
		has_pair(ContentItem, text, Text).

	% Read a JSON resource
	test(mcp_server_resources_read_02, true) :-
		run_mcp_exchange_with(
			test_resource_tools,
			[resources_read_request('logtalk://test/config', 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, contents, [ContentItem]),
		has_pair(ContentItem, mimeType, 'application/json'),
		has_pair(ContentItem, text, _).

	% Read a resource with multiple content items
	test(mcp_server_resources_read_multi_01, true(list::length(Contents, 2))) :-
		run_mcp_exchange_with(
			test_resource_tools,
			[resources_read_request('logtalk://test/multi', 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, contents, Contents).

	% Resource not found returns error
	test(mcp_server_resources_read_not_found_01, true) :-
		run_mcp_exchange_with(
			test_resource_tools,
			[resources_read_request('logtalk://test/nonexistent', 1)],
			[Response]
		),
		is_error_response(Response).

	% Combined tools, prompts, and resources tests

	% Application with all capabilities: all advertised
	test(mcp_server_all_capabilities_01, true) :-
		run_mcp_exchange_with(
			test_all_capabilities,
			[initialize_request(1)],
			[Response]
		),
		result(Response, Result),
		has_pair(Result, capabilities, Caps),
		has_pair(Caps, tools, _),
		has_pair(Caps, prompts, _),
		has_pair(Caps, resources, _).

	% Application with all capabilities: resources work
	test(mcp_server_all_capabilities_resource_read_01, true(Text == 'Some test data')) :-
		run_mcp_exchange_with(
			test_all_capabilities,
			[resources_read_request('logtalk://test/data', 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, contents, [ContentItem]),
		has_pair(ContentItem, text, Text).

	% Test auxiliary predicates

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
	spec_to_message(initialize_version_request(Version, Id), Message) :-
		request(
			initialize,
			{protocolVersion-Version, capabilities-{}, clientInfo-{name-test, version-'1.0'}},
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
	spec_to_message(prompts_list_request(Id), Message) :-
		request('prompts/list', {}, Id, Message).
	spec_to_message(prompts_get_request(PromptName, Arguments, Id), Message) :-
		request('prompts/get', {name-PromptName, arguments-Arguments}, Id, Message).
	spec_to_message(prompts_get_request(PromptName, Id), Message) :-
		request('prompts/get', {name-PromptName}, Id, Message).
	spec_to_message(resources_list_request(Id), Message) :-
		request('resources/list', {}, Id, Message).
	spec_to_message(resources_read_request(URI, Id), Message) :-
		request('resources/read', {uri-URI}, Id, Message).

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
