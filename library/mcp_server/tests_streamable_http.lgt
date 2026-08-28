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


:- object(tests_streamable_http,
	extends(lgtunit)).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-08-28,
		comment is 'Unit tests for the MCP Streamable HTTP transport (2026-07-28 spec over HTTP). Updated for json_body/1, text_body/1, and http_core response/5.'
	]).

	:- uses(json_rpc, [
		request/4, is_response/1, is_error_response/1, result/2, error_code/2,
		method/2, params/2
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	cover(mcp_server_streamable_http_transport).
	cover(mcp_streamable_http_handler).

	setup :-
		mcp_server_streamable_http_transport::cleanup.

	cleanup :-
		mcp_server_streamable_http_transport::cleanup.

	% protocol / facade

	test(http_spec_version_01, deterministic(Version == '2026-07-28')) :-
		mcp_server_streamable_http_transport::spec(Version).

	test(http_prepare_01, deterministic) :-
		mcp_server_streamable_http_transport::prepare(test_tools_2026, [
			server_name('http-test'),
			http_port(18080)
		]),
		mcp_server_streamable_http_transport::current_options(Options),
		memberchk(application(test_tools_2026), Options),
		memberchk(server_name('http-test'), Options),
		memberchk(http_port(18080), Options).

	test(http_valid_options_01, deterministic) :-
		mcp_server_streamable_http_transport::valid_option(http_port(8080)),
		mcp_server_streamable_http_transport::valid_option(http_bind('127.0.0.1')),
		mcp_server_streamable_http_transport::valid_option(http_path('/mcp')),
		mcp_server_streamable_http_transport::valid_option(http_origin_check(true)).

	% HTTP method / path basics

	test(http_method_not_allowed_01, deterministic(Status == 405)) :-
		with_prepared(test_tools_2026, [],
			mcp_server_streamable_http_transport::handle_mcp_request(
				'GET', [], '', http_response(Status, _, _)
			)
		).

	test(http_invalid_json_01, deterministic(Status == 400)) :-
		with_prepared(test_tools_2026, [],
			mcp_server_streamable_http_transport::handle_mcp_request(
				'POST', default_headers, 'not-json', http_response(Status, _, _)
			)
		).

	% server/discover

	test(http_discover_01, deterministic) :-
		call_json_rpc(
			test_tools_2026,
			discover_request(1),
			HTTPResponse
		),
		http_json_response(HTTPResponse, Response),
		is_response(Response),
		\+ is_error_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, complete),
		has_pair(Result, supportedVersions, Versions),
		memberchk('2026-07-28', Versions),
		has_pair(Result, capabilities, Caps),
		has_pair(Caps, tools, _),
		has_pair(Caps, subscriptions, _).

	test(http_discover_missing_meta_01, deterministic) :-
		call_json_rpc(
			test_tools_2026,
			bare_request('server/discover', 1),
			HTTPResponse
		),
		http_json_response(HTTPResponse, Response),
		is_error_response(Response),
		error_code(Response, -32602).

	test(http_discover_unsupported_version_01, deterministic) :-
		call_json_rpc(
			test_tools_2026,
			discover_version_request('2025-06-18', 1),
			HTTPResponse
		),
		http_json_response(HTTPResponse, Response),
		is_error_response(Response),
		error_code(Response, -32022).

	% tools/list and tools/call

	test(http_tools_list_01, deterministic) :-
		call_json_rpc(
			test_tools_2026,
			tools_list_request(1),
			HTTPResponse
		),
		http_json_response(HTTPResponse, Response),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, complete),
		has_pair(Result, tools, Tools),
		Tools = [_| _],
		has_pair(Result, ttlMs, 1000),
		has_pair(Result, cacheScope, private).

	test(http_tools_call_01, deterministic(sub_atom(Text, _, _, _, hello))) :-
		call_json_rpc(
			test_tools_2026,
			tools_call_request(echo, {'Input'-hello}, 1),
			HTTPResponse
		),
		http_json_response(HTTPResponse, Response),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, complete),
		has_pair(Result, content, [Item| _]),
		has_pair(Item, text, Text).

	test(http_tools_call_unknown_01, deterministic) :-
		call_json_rpc(
			test_tools_2026,
			tools_call_request(no_such_tool, {}, 1),
			HTTPResponse
		),
		http_json_response(HTTPResponse, Response),
		is_error_response(Response),
		error_code(Response, -32602).

	% MRTR

	test(http_mrtr_input_required_01, deterministic) :-
		call_json_rpc(
			test_tools_2026,
			tools_call_request(ask_once, {}, 1),
			HTTPResponse
		),
		http_json_response(HTTPResponse, Response),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, input_required),
		has_pair(Result, inputRequests, [_| _]),
		has_pair(Result, requestState, waiting_for_name).

	test(http_mrtr_complete_01, deterministic(sub_atom(Text, _, _, _, 'Alice'))) :-
		call_json_rpc(
			test_tools_2026,
			tools_call_with_responses(
				ask_once, {},
				[{key-name_key, action-accept, content-{name-'Alice'}}],
				waiting_for_name,
				1
			),
			HTTPResponse
		),
		http_json_response(HTTPResponse, Response),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, complete),
		has_pair(Result, content, [Item| _]),
		has_pair(Item, text, Text).

	% prompts / resources

	test(http_prompts_list_01, deterministic) :-
		call_json_rpc(
			test_prompts_2026,
			prompts_list_request(1),
			HTTPResponse
		),
		http_json_response(HTTPResponse, Response),
		result(Response, Result),
		has_pair(Result, resultType, complete),
		has_pair(Result, prompts, [_| _]).

	test(http_resources_read_01, deterministic(Text == 'Hello 2026')) :-
		call_json_rpc(
			test_resources_2026,
			resources_read_request('logtalk://test/data', 1),
			HTTPResponse
		),
		http_json_response(HTTPResponse, Response),
		result(Response, Result),
		has_pair(Result, resultType, complete),
		has_pair(Result, contents, [Item| _]),
		has_pair(Item, text, Text).

	% progress / SSE body (buffered path — no live stream attached)

	test(http_progress_token_sse_content_type_01, deterministic) :-
		call_json_rpc(
			test_tools_2026,
			tools_call_with_progress(echo, {'Input'-hi}, t1, 1),
			HTTPResponse
		),
		HTTPResponse = http_response(200, Headers, Payload),
		sse_payload_atom(Payload, Body),
		assertion(atom(Body)),
		memberchk('Content-Type'-CT, Headers),
		assertion(sub_atom(CT, _, _, _, 'text/event-stream')).

	test(http_progress_token_sse_has_final_data_01, deterministic) :-
		call_json_rpc(
			test_tools_2026,
			tools_call_with_progress(echo, {'Input'-hi}, t1, 1),
			HTTPResponse
		),
		HTTPResponse = http_response(200, _, Payload),
		sse_payload_atom(Payload, Body),
		assertion(sub_atom(Body, _, _, _, 'data: ')),
		assertion(sub_atom(Body, _, _, _, 'resultType')).

	% notify/1 error isolation

	test(http_notify_no_subscribers_01, deterministic) :-
		with_prepared(test_tools_2026, [],
			mcp_server_streamable_http_transport::notify(tools_list_changed)
		).

	% notify/1 must remain deterministic even when called repeatedly
	test(http_notify_idempotent_01, deterministic) :-
		with_prepared(test_tools_2026, [], (
			mcp_server_streamable_http_transport::notify(tools_list_changed),
			mcp_server_streamable_http_transport::notify(prompts_list_changed),
			mcp_server_streamable_http_transport::notify(resources_list_changed),
			mcp_server_streamable_http_transport::notify(resource_updated('logtalk://x'))
		)).

	% origin check

	test(http_origin_forbidden_01, deterministic(Status == 403)) :-
		Headers = [
			'Origin'-'http://evil.example',
			'MCP-Protocol-Version'-'2026-07-28',
			'Accept'-'application/json, text/event-stream',
			'Content-Type'-'application/json'
		],
		with_prepared(test_tools_2026, [http_origin_check(true)],
			mcp_server_streamable_http_transport::handle_mcp_request(
				'POST', Headers, '{}', http_response(Status, _, _)
			)
		).

	test(http_origin_localhost_ok_01, deterministic) :-
		meta_2026(Meta),
		request('server/discover', {'_meta'-Meta}, 1, Msg),
		json_serialize_term(Msg, Body),
		Headers = [
			'Origin'-'http://localhost:3000',
			'MCP-Protocol-Version'-'2026-07-28',
			'Mcp-Method'-'server/discover',
			'Accept'-'application/json, text/event-stream',
			'Content-Type'-'application/json'
		],
		with_prepared(test_tools_2026, [http_origin_check(true)],
			mcp_server_streamable_http_transport::handle_mcp_request(
				'POST', Headers, Body, HTTPResponse
			)
		),
		http_json_response(HTTPResponse, Response),
		is_response(Response).

	% handler path routing

	test(http_handler_not_found_01, deterministic) :-
		with_prepared(test_tools_2026, [http_path('/mcp')], (
			Request = request('POST', '/other', [], '{}'),
			mcp_streamable_http_handler::handle(Request, Response),
			Response = response(http(1,1), status(404, _), _, _, _)
		)).

	test(http_handler_method_not_allowed_01, deterministic) :-
		with_prepared(test_tools_2026, [http_path('/mcp')], (
			Request = request('GET', '/mcp', [], ''),
			mcp_streamable_http_handler::handle(Request, Response),
			Response = response(http(1,1), status(405, _), _, _, _)
		)).

	% ping was removed in MCP 2026-07-28; expect method not found

	test(http_ping_01, deterministic(Code == -32601)) :-
		call_json_rpc(test_tools_2026, ping_request(1), HTTPResponse),
		http_json_response(HTTPResponse, Response),
		is_error_response(Response),
		error_code(Response, Code).

	% unknown method

	test(http_unknown_method_01, deterministic(Code == -32601)) :-
		call_json_rpc(test_tools_2026, unknown_method_request(1), HTTPResponse),
		http_json_response(HTTPResponse, Response),
		is_error_response(Response),
		error_code(Response, Code).

	% auxiliary predicates

	with_prepared(Application, ExtraOptions, Goal) :-
		catch(
			(	mcp_server_streamable_http_transport::prepare(Application, ExtraOptions),
				call(Goal)
			),
			Error,
			(	mcp_server_streamable_http_transport::cleanup,
				throw(Error)
			)
		),
		mcp_server_streamable_http_transport::cleanup.

	:- meta_predicate(with_prepared(*, *, 0)).

	call_json_rpc(Application, Spec, HTTPResponse) :-
		spec_to_message(Spec, Message),
		json_serialize_term(Message, Body),
		request_headers(Message, Headers),
		with_prepared(Application, [],
			mcp_server_streamable_http_transport::handle_mcp_request(
				'POST', Headers, Body, HTTPResponse
			)
		).

	% 2026 Streamable HTTP requires MCP-Protocol-Version, Mcp-Method, and
	% Mcp-Name (for tools/call, prompts/get, resources/read).
	request_headers(Message, Headers) :-
		method(Message, Method),
		Base = [
			'MCP-Protocol-Version'-'2026-07-28',
			'Mcp-Method'-Method,
			'Accept'-'application/json, text/event-stream',
			'Content-Type'-'application/json'
		],
		(	params(Message, Params),
			method_mcp_name(Method, Params, Name) ->
			Headers = ['Mcp-Name'-Name| Base]
		;	Headers = Base
		).

	method_mcp_name('tools/call', Params, Name) :-
		has_pair(Params, name, Name).
	method_mcp_name('prompts/get', Params, Name) :-
		has_pair(Params, name, Name).
	method_mcp_name('resources/read', Params, Name) :-
		has_pair(Params, uri, Name).

	default_headers([
		'MCP-Protocol-Version'-'2026-07-28',
		'Mcp-Method'-'server/discover',
		'Accept'-'application/json, text/event-stream',
		'Content-Type'-'application/json'
	]).

	default_headers_rest([
		'Accept'-'application/json, text/event-stream',
		'Content-Type'-'application/json'
	]).

	% decode http_response/3 into a JSON-RPC term (json_body/1 or atom/SSE)
	http_json_response(http_response(200, _Headers, json_body(Message)), Message) :-
		!.
	http_json_response(http_response(200, Headers, text_body(Body)), Message) :-
		!,
		http_json_response(http_response(200, Headers, Body), Message).
	http_json_response(http_response(200, Headers, Body), Message) :-
		atom(Body),
		memberchk('Content-Type'-CT, Headers),
		sub_atom(CT, _, _, _, 'application/json'),
		!,
		json_parse_term(Body, Message).
	http_json_response(http_response(already_sent, _, _), _) :-
		!,
		fail.
	http_json_response(http_response(200, Headers, Body), Message) :-
		% SSE body: take the last data: line as the JSON-RPC message
		atom(Body),
		memberchk('Content-Type'-CT, Headers),
		sub_atom(CT, _, _, _, 'text/event-stream'),
		!,
		last_sse_data(Body, JSON),
		json_parse_term(JSON, Message).

	last_sse_data(Body, JSON) :-
		atom_codes(Body, Codes),
		phrase(sse_data_lines(Lines), Codes),
		Lines = [_| _],
		last(Lines, JSON).

	sse_data_lines([]) -->
		[].
	sse_data_lines(Lines) -->
		[0'd,0'a,0't,0'a,0':,32], sse_line(LineCodes), [0'\n],
		{atom_codes(Line, LineCodes)},
		(	[0'\n] -> {Lines = [Line| Rest]}, sse_data_lines(Rest)
		;	sse_data_lines_rest(Line, Lines)
		).
	sse_data_lines(Lines) -->
		sse_skip_line, sse_data_lines(Lines).

	sse_data_lines_rest(Line, [Line| Rest]) -->
		sse_data_lines(Rest).

	sse_line([]) -->
		[].
	sse_line([]) -->
		[0'\n], !.
	sse_line([C|Cs]) -->
		[C], {C =\= 0'\n}, sse_line(Cs).

	sse_skip_line -->
		[0'\n], !.
	sse_skip_line -->
		[_], sse_skip_line.
	sse_skip_line -->
		[].

	last([X], X) :-
		!.
	last([_|Xs], X) :-
		last(Xs, X).

	json_serialize_term(Term, Atom) :-
		(	current_object(json) ->
			json::generate(atom(Atom), Term)
		;	% minimal fallback for tests without json library loaded as object
			write_term_to_atom(Term, Atom, [quoted(true)])
		).

	json_parse_term(Atom, Term) :-
		(	current_object(json) ->
			json::parse(atom(Atom), Term)
		;	read_term_from_atom(Atom, Term, [])
		).

	write_term_to_atom(Term, Atom, Options) :-
		open(atom(Atom), write, Stream),
		write_term(Stream, Term, Options),
		close(Stream).

	read_term_from_atom(Atom, Term, Options) :-
		open(atom(Atom), read, Stream),
		read_term(Stream, Term, Options),
		close(Stream).

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

	meta_2026_progress(Token, Meta) :-
		Meta = {
			'io.modelcontextprotocol/protocolVersion'-'2026-07-28',
			'io.modelcontextprotocol/clientCapabilities'-{
				tools-{},
				prompts-{},
				resources-{},
				subscriptions-{},
				elicitation-{}
			},
			progressToken-Token
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
	spec_to_message(ping_request(Id), Message) :-
		meta_2026(Meta),
		request(ping, {'_meta'-Meta}, Id, Message).
	spec_to_message(tools_list_request(Id), Message) :-
		meta_2026(Meta),
		request('tools/list', {'_meta'-Meta}, Id, Message).
	spec_to_message(tools_call_request(Name, Args, Id), Message) :-
		meta_2026(Meta),
		request('tools/call', {name-Name, arguments-Args, '_meta'-Meta}, Id, Message).
	spec_to_message(tools_call_with_progress(Name, Args, Token, Id), Message) :-
		meta_2026_progress(Token, Meta),
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

	sse_payload_atom(text_body(Body), Body) :-
		!,
		atom(Body).
	sse_payload_atom(Body, Body) :-
		atom(Body).

	has_pair({Pairs}, Key, Value) :-
		curly_member(Key-Value, Pairs).

	curly_member(Pair, (Pair, _)) :-
		!.
	curly_member(Pair, (_, Rest)) :-
		!,
		curly_member(Pair, Rest).
	curly_member(Pair, Pair).

:- end_object.
