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


:- object(tests_stdio,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-21,
		comment is 'Unit tests for the MCP stdio transport (``mcp_server_stdio_transport``). Spec semantics are covered by ``tests_2025_06_18`` and ``tests_2026_07_28``; this suite focuses on transport selection, framing, and outcome rendering.'
	]).

	:- uses(json_rpc, [
		request/4, request/3, response/3, is_response/1, is_error_response/1,
		method/2, result/2, id/2, error_code/2, write_message/2, read_message/2
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	cover(mcp_server_stdio_transport).
	cover(mcp_server).

	cleanup :-
		^^clean_file('mcp_stdio_in.tmp'),
		^^clean_file('mcp_stdio_out.tmp').

	% adapter identity and selection

	test(stdio_protocol_version_default_01, deterministic(V == '2025-06-18')) :-
		% Before start, default reported version is 2025
		mcp_server_stdio_transport::spec(V).

	test(stdio_facade_transport_option_01, deterministic) :-
		mcp_server::valid_option(transport(stdio)),
		mcp_server::valid_option(spec('2025-06-18')),
		mcp_server::valid_option(spec('2026-07-28')).

	test(stdio_facade_resolve_2025_01, deterministic) :-
		% Explicit matrix options select the stdio adapter
		run_stdio(
			test_tools,
			[spec('2025-06-18'), transport(stdio)],
			[initialize_request(1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, protocolVersion, '2025-06-18').

	test(stdio_facade_resolve_2026_01, deterministic) :-
		run_stdio(
			test_tools_2026,
			[spec('2026-07-28'), transport(stdio)],
			[discover_request(1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, complete),
		has_pair(Result, supportedVersions, Versions),
		memberchk('2026-07-28', Versions).

	% Legacy protocol_adapter aliases still work
	test(stdio_legacy_adapter_2025_01, deterministic) :-
		run_stdio(
			test_tools,
			[protocol_adapter(mcp_server_2025_06_18_adapter)],
			[initialize_request(1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, protocolVersion, '2025-06-18').

	test(stdio_legacy_adapter_2026_01, deterministic) :-
		run_stdio(
			test_tools_2026,
			[protocol_adapter(mcp_server_2026_07_28_adapter)],
			[discover_request(1)],
			[Response]
		),
		is_response(Response),
		\+ is_error_response(Response).

	% framing: one JSON-RPC message per line, sequential exchange

	test(stdio_framing_multiple_messages_01, true) :-
		run_stdio(
			test_tools,
			[spec('2025-06-18'), transport(stdio)],
			[
				initialize_request(1),
				ping_request(2),
				tools_list_request(3)
			],
			Responses
		),
		Responses = [R1, R2, R3],
		is_response(R1), id(R1, 1),
		is_response(R2), id(R2, 2),
		is_response(R3), id(R3, 3).

	test(stdio_framing_2026_multiple_01, true) :-
		run_stdio(
			test_tools_2026,
			[spec('2026-07-28'), transport(stdio)],
			[
				discover_request(1),
				tools_list_2026(2),
				tools_call_2026(echo, {'Input'-hi}, 3)
			],
			Responses
		),
		Responses = [R1, R2, R3],
		forall(
			member(R, Responses),
			(is_response(R), \+ is_error_response(R))
		),
		id(R1, 1), id(R2, 2), id(R3, 3).

	% Empty input (EOF) produces no responses and does not throw
	test(stdio_eof_empty_input_01, deterministic(Responses == [])) :-
		run_stdio(
			test_tools,
			[spec('2025-06-18'), transport(stdio)],
			[],
			Responses
		).

	% outcome rendering (2025 reply/1 path via protocol handler)

	test(stdio_render_reply_2025_01, deterministic(sub_atom(Text, _, _, _, 'Hello, world!'))) :-
		run_stdio(
			test_tools,
			[transport(stdio), spec('2025-06-18')],
			[tools_call_request(greet, {'Name'-world}, 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, content, [Item| _]),
		has_pair(Item, text, Text).

	test(stdio_render_error_2025_01, deterministic) :-
		run_stdio(
			test_tools,
			[transport(stdio), spec('2025-06-18')],
			[unknown_request(1)],
			[Response]
		),
		is_error_response(Response),
		error_code(Response, -32601).

	% outcome rendering (2026 reply/1 and resultType)

	test(stdio_render_reply_2026_01, deterministic) :-
		run_stdio(
			test_tools_2026,
			[transport(stdio), spec('2026-07-28')],
			[tools_call_2026(echo, {'Input'-hello}, 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, complete),
		has_pair(Result, content, [Item| _]),
		has_pair(Item, text, Text),
		sub_atom(Text, _, _, _, hello).

	test(stdio_render_mrtr_2026_01, deterministic) :-
		run_stdio(
			test_tools_2026,
			[transport(stdio), spec('2026-07-28')],
			[tools_call_2026(ask_once, {}, 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, input_required),
		has_pair(Result, requestState, waiting_for_name).

	test(stdio_render_error_2026_01, deterministic) :-
		run_stdio(
			test_tools_2026,
			[transport(stdio), spec('2026-07-28')],
			[bare_request('server/discover', 1)],
			[Response]
		),
		is_error_response(Response),
		error_code(Response, -32602).

	% notify/1 through the facade (no active subscribers is fine)

	test(stdio_notify_without_session_01, deterministic) :-
		mcp_server::notify(tools_list_changed).

	% direct adapter start (without facade matrix)

	test(stdio_adapter_start_direct_01, deterministic) :-
		^^file_path('mcp_stdio_in.tmp', InFile),
		^^file_path('mcp_stdio_out.tmp', OutFile),
		open(InFile, write, W),
		spec_to_message(initialize_request(1), Msg),
		write_message(W, Msg),
		close(W),
		open(InFile, read, In),
		open(OutFile, write, Out),
		mcp_server_stdio_transport::start(
			test_tools, In, Out,
			[spec('2025-06-18'), server_name('direct-stdio')]
		),
		close(In),
		close(Out),
		open(OutFile, read, R),
		read_all(R, [Response]),
		close(R),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, serverInfo, Info),
		has_pair(Info, name, 'direct-stdio').

	% auxiliary predicates

	run_stdio(Application, Options, Specs, Responses) :-
		^^file_path('mcp_stdio_in.tmp', InFile),
		^^file_path('mcp_stdio_out.tmp', OutFile),
		open(InFile, write, W),
		write_specs(Specs, W),
		close(W),
		open(InFile, read, In),
		open(OutFile, write, Out),
		mcp_server::start('test-stdio', Application, In, Out, Options),
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

	meta_2026(Meta) :-
		Meta = {
			'io.modelcontextprotocol/protocolVersion'-'2026-07-28',
			'io.modelcontextprotocol/clientCapabilities'-{
				tools-{},
				prompts-{},
				resources-{},
				subscriptions-{},
				elicitation-{}
			}
		}.

	spec_to_message(initialize_request(Id), Message) :-
		request(
			initialize,
			{protocolVersion-'2025-06-18', capabilities-{}, clientInfo-{name-test, version-'1.0'}},
			Id,
			Message
		).
	spec_to_message(ping_request(Id), Message) :-
		request(ping, Id, Message).
	spec_to_message(tools_list_request(Id), Message) :-
		request('tools/list', {}, Id, Message).
	spec_to_message(tools_call_request(Name, Args, Id), Message) :-
		request('tools/call', {name-Name, arguments-Args}, Id, Message).
	spec_to_message(unknown_request(Id), Message) :-
		request(unknown_method, {}, Id, Message).
	spec_to_message(discover_request(Id), Message) :-
		meta_2026(Meta),
		request('server/discover', {'_meta'-Meta}, Id, Message).
	spec_to_message(bare_request(Method, Id), Message) :-
		request(Method, {}, Id, Message).
	spec_to_message(tools_call_2026(Name, Args, Id), Message) :-
		meta_2026(Meta),
		request('tools/call', {name-Name, arguments-Args, '_meta'-Meta}, Id, Message).
	spec_to_message(tools_list_2026(Id), Message) :-
		meta_2026(Meta),
		request('tools/list', {'_meta'-Meta}, Id, Message).

	has_pair({Pairs}, Key, Value) :-
		curly_member(Key-Value, Pairs).

	curly_member(Pair, (Pair, _)) :-
		!.
	curly_member(Pair, (_, Rest)) :-
		!,
		curly_member(Pair, Rest).
	curly_member(Pair, Pair).

:- end_object.
