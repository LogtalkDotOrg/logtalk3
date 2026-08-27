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
		date is 2026-08-22,
		comment is 'Unit tests for the MCP 2026-07-28 adapter.'
	]).

	:- uses(json_rpc, [
		request/4, is_response/1, is_error_response/1, result/2, error_code/2, write_message/2,
		read_message/2
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	cover(mcp_server).
	cover(mcp_server_2026_07_28_adapter).
	cover(mcp_server_2026_07_28_spec).
	cover(mcp_server_stdio_transport).

	cleanup :-
		^^clean_file('mcp26_in.tmp'),
		^^clean_file('mcp26_out.tmp').

	% facade adapter selection

	test(mcp26_facade_default_adapter_01, deterministic) :-
		% Default is protocol_version + transport matrix (stdio / 2025), not protocol_adapter/1
		mcp_server::default_option(spec('2025-06-18')),
		mcp_server::default_option(transport(stdio)),
		\+ mcp_server::default_option(protocol_adapter(_)).

	test(mcp26_facade_select_2026_01, deterministic) :-
		% Starting with explicit 2026 adapter should not throw domain_error
		% (we only check selection; a full exchange is tested below)
		Options = [protocol_adapter(mcp_server_2026_07_28_adapter)],
		mcp_server::valid_option(protocol_adapter(mcp_server_2026_07_28_adapter)),
		memberchk(protocol_adapter(mcp_server_2026_07_28_adapter), Options).

	% server/discover

	test(mcp26_discover_01, deterministic) :-
		run_2026(
			test_tools_2026,
			[discover_request(1)],
			[Response]
		),
		is_response(Response),
		\+ is_error_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, complete),
		has_pair(Result, supportedVersions, Versions),
		memberchk('2026-07-28', Versions),
		has_pair(Result, capabilities, _),
		has_pair(Result, ttlMs, _),
		has_pair(Result, cacheScope, _).

	test(mcp26_discover_server_info_01, deterministic) :-
		run_2026(
			test_tools_2026,
			[discover_request(1)],
			[Response]
		),
		result(Response, Result),
		has_pair(Result, '_meta', Meta),
		has_pair(Meta, 'io.modelcontextprotocol/serverInfo', ServerInfo),
		has_pair(ServerInfo, name, 'test-server-2026').

	test(mcp26_discover_cache_override_01, deterministic(TTL-Scope == 5000-public)) :-
		run_2026(
			test_tools_2026,
			[discover_request(1)],
			[Response]
		),
		result(Response, Result),
		has_pair(Result, ttlMs, TTL),
		has_pair(Result, cacheScope, Scope).

	% required metadata validation

	test(mcp26_missing_meta_01, deterministic) :-
		run_2026(
			test_tools_2026,
			[bare_request('server/discover', 1)],
			[Response]
		),
		is_error_response(Response),
		error_code(Response, -32602).

	test(mcp26_unsupported_version_01, deterministic) :-
		run_2026(
			test_tools_2026,
			[discover_version_request('2025-06-18', 1)],
			[Response]
		),
		is_error_response(Response),
		error_code(Response, -32022).

	test(mcp26_initialize_rejected_01, deterministic) :-
		run_2026(
			test_tools_2026,
			[initialize_2026_style(1)],
			[Response]
		),
		is_error_response(Response).

	% tools/list and tools/call

	test(mcp26_tools_list_01, deterministic) :-
		run_2026(
			test_tools_2026,
			[tools_list_request(1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, complete),
		has_pair(Result, tools, Tools),
		Tools = [_| _],
		has_pair(Result, ttlMs, 1000),
		has_pair(Result, cacheScope, private).

	test(mcp26_tools_call_complete_01, deterministic(sub_atom(Text, _, _, _, hello))) :-
		run_2026(
			test_tools_2026,
			[tools_call_request(echo, {'Input'-hello}, 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, complete),
		has_pair(Result, content, [Item| _]),
		has_pair(Item, text, Text),
		has_pair(Result, structuredContent, StructuredContent),
		has_pair(StructuredContent, 'Output', hello).

	test(mcp26_tools_call_unknown_01, deterministic) :-
		run_2026(
			test_tools_2026,
			[tools_call_request(no_such_tool, {}, 1)],
			[Response]
		),
		is_error_response(Response),
		error_code(Response, -32602).

	% MRTR: one-round input_required then complete

	test(mcp26_mrtr_first_round_01, deterministic) :-
		run_2026(
			test_tools_2026,
			[tools_call_request(ask_once, {}, 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, input_required),
		has_pair(Result, inputRequests, Requests),
		Requests = {_},
		has_pair(Result, requestState, waiting_for_name),
		% No cache fields on input_required
		\+ has_pair(Result, ttlMs, _),
		\+ has_pair(Result, cacheScope, _).

	test(mcp26_mrtr_second_round_accept_01, deterministic(sub_atom(Text, _, _, _, 'Alice'))) :-
		run_2026(
			test_tools_2026,
			[tools_call_with_responses(
				ask_once, {},
				[{key-name_key, action-accept, content-{name-'Alice'}}],
				waiting_for_name,
				1
			)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, complete),
		has_pair(Result, content, [Item| _]),
		has_pair(Item, text, Text).

	test(mcp26_mrtr_decline_01, deterministic(Text == 'Declined.')) :-
		run_2026(
			test_tools_2026,
			[tools_call_with_responses(
				ask_once, {},
				[{key-name_key, action-decline}],
				waiting_for_name,
				1
			)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, complete),
		has_pair(Result, content, [Item| _]),
		has_pair(Item, text, Text).

	% prompts

	test(mcp26_prompts_list_01, deterministic) :-
		run_2026(
			test_prompts_2026,
			[prompts_list_request(1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, complete),
		has_pair(Result, prompts, Prompts),
		Prompts = [_| _].

	test(mcp26_prompts_get_complete_01, deterministic) :-
		run_2026(
			test_prompts_2026,
			[prompts_get_request(greet_prompt, {name-'Bob'}, 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, complete),
		has_pair(Result, messages, [_| _]).

	test(mcp26_prompts_get_mrtr_01, deterministic) :-
		run_2026(
			test_prompts_2026,
			[prompts_get_request(confirm_prompt, {}, 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, input_required).

	% resources

	test(mcp26_resources_list_01, deterministic) :-
		run_2026(
			test_resources_2026,
			[resources_list_request(1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, complete),
		has_pair(Result, resources, Resources),
		Resources = [_| _],
		has_pair(Result, ttlMs, _),
		has_pair(Result, cacheScope, _).

	test(mcp26_resources_read_01, deterministic(Text == 'Hello 2026')) :-
		run_2026(
			test_resources_2026,
			[resources_read_request('logtalk://test/data', 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, complete),
		has_pair(Result, contents, [Item| _]),
		has_pair(Item, text, Text),
		has_pair(Result, ttlMs, 2000),
		has_pair(Result, cacheScope, private).

	test(mcp26_resources_read_mrtr_01, deterministic) :-
		run_2026(
			test_resources_2026,
			[resources_read_request('logtalk://test/gated', 1)],
			[Response]
		),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, input_required).

	test(mcp26_resources_unknown_01, deterministic) :-
		run_2026(
			test_resources_2026,
			[resources_read_request('logtalk://test/missing', 1)],
			[Response]
		),
		is_error_response(Response),
		error_code(Response, -32602).

	% unknown method

	test(mcp26_unknown_method_01, deterministic) :-
		run_2026(
			test_tools_2026,
			[unknown_method_request(1)],
			[Response]
		),
		is_error_response(Response),
		error_code(Response, -32601).

	% ping

	test(mcp26_ping_01, deterministic) :-
		run_2026(
			test_tools_2026,
			[ping_request(1)],
			[Response]
		),
		assertion(is_response(Response)),
		assertion(\+ is_error_response(Response)).

	% combined capabilities

	test(mcp26_all_capabilities_discover_01, deterministic) :-
		run_2026(
			test_all_2026,
			[discover_request(1)],
			[Response]
		),
		result(Response, Result),
		assertion(has_pair(Result, capabilities, Caps)),
		assertion(has_pair(Caps, tools, _)),
		assertion(has_pair(Caps, prompts, _)),
		assertion(has_pair(Caps, resources, _)),
		assertion(has_pair(Caps, subscriptions, _)).

	% resultType present on every success

	test(mcp26_result_type_always_present_01, deterministic) :-
		run_2026(
			test_tools_2026,
			[tools_list_request(1), tools_call_request(echo, {'Input'-x}, 2)],
			Responses
		),
		forall(
			(member(R, Responses), is_response(R), \+ is_error_response(R)),
			(result(R, Res), has_pair(Res, resultType, _))
		).

	% auxiliary predicates

	% run exchanges against the 2026 adapter

	run_2026(Application, Specs, Responses) :-
		^^file_path('mcp26_in.tmp', InFile),
		^^file_path('mcp26_out.tmp', OutFile),
		open(InFile, write, Out),
		write_specs(Specs, Out),
		close(Out),
		open(InFile, read, In),
		open(OutFile, write, OutStream),
		mcp_server::start(
			'test-server-2026',
			Application,
			In,
			OutStream,
			[protocol_adapter(mcp_server_2026_07_28_adapter)]
		),
		close(In),
		close(OutStream),
		open(OutFile, read, InStream),
		read_all(InStream, Responses),
		close(InStream).

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

	% meta builder for 2026 requests

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

	spec_to_message(discover_request(Id), Message) :-
		meta_2026(Meta),
		request('server/discover', {'_meta'-Meta}, Id, Message).
	spec_to_message(discover_version_request(Version, Id), Message) :-
		Meta = {
			'io.modelcontextprotocol/protocolVersion'-Version,
			'io.modelcontextprotocol/clientCapabilities'-{tools-{}}
		},
		request('server/discover', {'_meta'-Meta}, Id, Message).
	spec_to_message(bare_request(Method, Id), Message) :-
		request(Method, {}, Id, Message).
	spec_to_message(initialize_2026_style(Id), Message) :-
		meta_2026(Meta),
		request(initialize, {'_meta'-Meta}, Id, Message).
	spec_to_message(ping_request(Id), Message) :-
		meta_2026(Meta),
		request(ping, {'_meta'-Meta}, Id, Message).
	spec_to_message(tools_list_request(Id), Message) :-
		meta_2026(Meta),
		request('tools/list', {'_meta'-Meta}, Id, Message).
	spec_to_message(tools_call_request(Name, Args, Id), Message) :-
		meta_2026(Meta),
		request('tools/call', {name-Name, arguments-Args, '_meta'-Meta}, Id, Message).
	spec_to_message(tools_call_with_responses(Name, Args, Responses, State, Id), Message) :-
		meta_2026(Meta),
		request('tools/call', {
			name-Name,
			arguments-Args,
			inputResponses-Responses,
			requestState-State,
			'_meta'-Meta
		}, Id, Message).
	spec_to_message(prompts_list_request(Id), Message) :-
		meta_2026(Meta),
		request('prompts/list', {'_meta'-Meta}, Id, Message).
	spec_to_message(prompts_get_request(Name, Args, Id), Message) :-
		meta_2026(Meta),
		request('prompts/get', {name-Name, arguments-Args, '_meta'-Meta}, Id, Message).
	spec_to_message(resources_list_request(Id), Message) :-
		meta_2026(Meta),
		request('resources/list', {'_meta'-Meta}, Id, Message).
	spec_to_message(resources_read_request(URI, Id), Message) :-
		meta_2026(Meta),
		request('resources/read', {uri-URI, '_meta'-Meta}, Id, Message).
	spec_to_message(unknown_method_request(Id), Message) :-
		meta_2026(Meta),
		request(totally_unknown, {'_meta'-Meta}, Id, Message).

	% curly-term predicates

	has_pair({Pairs}, Key, Value) :-
		curly_member(Key-Value, Pairs).

	curly_member(Pair, (Pair, _)) :-
		!.
	curly_member(Pair, (_, Rest)) :-
		!,
		curly_member(Pair, Rest).
	curly_member(Pair, Pair).

:- end_object.
