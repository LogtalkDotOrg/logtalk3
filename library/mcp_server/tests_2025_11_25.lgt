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


:- object(tests_2025_11_25,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-31,
		comment is 'Unit tests for MCP 2025-11-25: version negotiation, serverInfo.description, icons (SEP-973), URL elicitation (SEP-1036), enum schema pass-through (SEP-1330).'
	]).

	:- uses(json_rpc, [
		request/4, response/3, is_request/1, is_response/1, result/2, method/2, params/2, write_message/2,
		read_message/2
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	cover(mcp_server_2025_11_25_spec).
	cover(test_tools_2025_11_25).

	setup :-
		mcp_server_2025_11_25_spec::cleanup.

	cleanup :-
		^^clean_file('mcp_2025_11_25_in.tmp'),
		^^clean_file('mcp_2025_11_25_out.tmp'),
		mcp_server_2025_11_25_spec::cleanup.

	% version identity

	test(spec_2025_11_25_identity_01, deterministic(Spec == '2025-11-25')) :-
		mcp_server_2025_11_25_spec::spec(Spec).

	test(spec_2025_11_25_supported_versions_01, deterministic) :-
		mcp_server_2025_11_25_spec::supported_specs(Versions),
		assertion(memberchk('2025-11-25', Versions)),
		assertion(memberchk('2025-06-18', Versions)).

	% initialize negotiates 2025-11-25

	test(spec_2025_11_25_initialize_01, deterministic) :-
		run_exchange(
			[initialize_request('2025-11-25', 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, protocolVersion, '2025-11-25'),
		has_pair(Result, serverInfo, Info),
		has_pair(Info, name, 'test-2025-11-25').

	% client asking for 2025-06-18 is accepted (negotiate down)

	test(spec_2025_11_25_negotiate_down_01, deterministic) :-
		run_exchange(
			[initialize_request('2025-06-18', 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, protocolVersion, '2025-06-18').

	% client asking for newer than both still gets 2025-11-25

	test(spec_2025_11_25_negotiate_newer_01, deterministic) :-
		run_exchange(
			[initialize_request('2026-01-01', 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, protocolVersion, '2025-11-25').

	% serverInfo.description (Implementation.description)

	test(spec_2025_11_25_server_description_01, deterministic(D == 'A test server')) :-
		run_exchange_options(
			[server_description('A test server')],
			[initialize_request('2025-11-25', 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, serverInfo, Info),
		has_pair(Info, description, D).

	% icons on tools (SEP-973)

	test(spec_2025_11_25_tool_icons_01, true) :-
		run_exchange(
			[tools_list_request(1)],
			[Response]
		),
		assertion(is_response(Response)),
		result(Response, Result),
		has_pair(Result, tools, Tools),
		member(Tool, Tools),
		has_pair(Tool, name, echo),
		has_pair(Tool, icons, Icons),
		Icons = [Icon| _],
		has_pair(Icon, src, 'https://example.com/icons/echo.png'),
		has_pair(Icon, mimeType, 'image/png').

	test(spec_2025_11_25_tool_icons_sizes_01, true) :-
		run_exchange(
			[tools_list_request(1)],
			[Response]
		),
		assertion(is_response(Response)),
		result(Response, Result),
		has_pair(Result, tools, Tools),
		member(Tool, Tools),
		has_pair(Tool, name, ask_color),
		has_pair(Tool, icons, [Icon| _]),
		has_pair(Icon, sizes, ['48x48']).

	% icons on prompts

	test(spec_2025_11_25_prompt_icons_01, true) :-
		run_exchange(
			[prompts_list_request(1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, prompts, Prompts),
		member(Prompt, Prompts),
		has_pair(Prompt, name, color_prompt),
		has_pair(Prompt, icons, Icons),
		Icons = [_| _].

	% icons on resources

	test(spec_2025_11_25_resource_icons_01, true) :-
		run_exchange(
			[resources_list_request(1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resources, Resources),
		member(Resource, Resources),
		has_pair(Resource, uri, 'logtalk://test/palette'),
		has_pair(Resource, icons, Icons),
		Icons = [_| _].

	% form elicitation still works; enum schema is passed through (SEP-1330)

	test(spec_2025_11_25_enum_schema_01, true) :-
		run_exchange(
			[
				initialize_elicitation_request(1),
				tools_call_request(ask_color, {}, 2),
				elicit_accept_response(elicit_1, {color-red})
			],
			Responses
		),
		Responses = [_Init, ElicitReq, ToolResponse],
		is_request(ElicitReq),
		method(ElicitReq, 'elicitation/create'),
		params(ElicitReq, Params),
		has_pair(Params, requestedSchema, Schema),
		has_pair(Schema, properties, Props),
		has_pair(Props, color, ColorSchema),
		has_pair(ColorSchema, enum, [red, green, blue]),
		has_pair(ColorSchema, enumNames, ['Red', 'Green', 'Blue']),
		is_response(ToolResponse),
		result(ToolResponse, Result),
		has_pair(Result, content, [Item| _]),
		has_pair(Item, text, Text),
		sub_atom(Text, _, _, _, red).

	% URL-mode elicitation (SEP-1036) — direct API test with file streams

	test(spec_2025_11_25_url_elicitation_01, deterministic) :-
		^^file_path('mcp_2025_11_25_in.tmp', InFile),
		^^file_path('mcp_2025_11_25_out.tmp', OutFile),
		% client will accept
		open(InFile, write, W),
		response({action-accept, content-{}}, elicit_1, Accept),
		write_message(W, Accept),
		close(W),
		open(InFile, read, In),
		open(OutFile, write, Out),
		mcp_server_2025_11_25_spec::prepare(test_tools_2025_11_25, [
			server_name('url-test'),
			stdio_input(In),
			stdio_output(Out)
		]),
		mcp_server_2025_11_25_spec::elicit_url_request(
			In, Out,
			'Open the docs to continue',
			'https://example.com/docs',
			Answer
		),
		close(In),
		close(Out),
		assertion(Answer = accept({})),
		% server wrote elicitation/create with mode url
		open(OutFile, read, R),
		read_message(R, Req),
		close(R),
		is_request(Req),
		method(Req, 'elicitation/create'),
		params(Req, Params),
		has_pair(Params, message, 'Open the docs to continue'),
		has_pair(Params, url, 'https://example.com/docs'),
		has_pair(Params, (mode), url).

	% facade accepts the new version

	test(spec_2025_11_25_facade_valid_01, deterministic) :-
		mcp_server::valid_option(spec('2025-11-25')).

	% auxiliary

	run_exchange(Specs, Responses) :-
		run_exchange_options([], Specs, Responses).

	run_exchange_options(Extra, Specs, Responses) :-
		^^file_path('mcp_2025_11_25_in.tmp', InFile),
		^^file_path('mcp_2025_11_25_out.tmp', OutFile),
		open(InFile, write, W),
		write_specs(Specs, W),
		close(W),
		open(InFile, read, In),
		open(OutFile, write, Out),
		Options0 = [
			spec('2025-11-25'),
			transport(stdio),
			server_name('test-2025-11-25')
			| Extra
		],
		mcp_server::start('test-2025-11-25', test_tools_2025_11_25, In, Out, Options0),
		close(In),
		close(Out),
		open(OutFile, read, R),
		read_all(R, Responses),
		close(R).

	write_specs([], _).
	write_specs([Spec| Specs], Stream) :-
		spec_to_message(Spec, Message),
		write_message(Stream, Message),
		write_specs(Specs, Stream).

	read_all(Stream, Messages) :-
		(	catch(read_message(Stream, Message), _, fail) ->
			Messages = [Message| Rest],
			read_all(Stream, Rest)
		;	Messages = []
		).

	spec_to_message(initialize_request(Version, Id), Message) :-
		request(
			initialize,
			{protocolVersion-Version, capabilities-{}, clientInfo-{name-test, version-'1.0'}},
			Id,
			Message
		).
	spec_to_message(initialize_elicitation_request(Id), Message) :-
		request(
			initialize,
			{protocolVersion-'2025-11-25', capabilities-{elicitation-{}}, clientInfo-{name-test, version-'1.0'}},
			Id,
			Message
		).
	spec_to_message(tools_list_request(Id), Message) :-
		request('tools/list', {}, Id, Message).
	spec_to_message(tools_call_request(Name, Args, Id), Message) :-
		request('tools/call', {name-Name, arguments-Args}, Id, Message).
	spec_to_message(prompts_list_request(Id), Message) :-
		request('prompts/list', {}, Id, Message).
	spec_to_message(resources_list_request(Id), Message) :-
		request('resources/list', {}, Id, Message).
	spec_to_message(elicit_accept_response(Id, Content), Message) :-
		response({action-accept, content-Content}, Id, Message).

	has_pair({Pairs}, Key, Value) :-
		curly_member(Key-Value, Pairs).

	curly_member(Pair, (Pair, _)) :-
		!.
	curly_member(Pair, (_, Rest)) :-
		!,
		curly_member(Pair, Rest).
	curly_member(Pair, Pair).

:- end_object.
