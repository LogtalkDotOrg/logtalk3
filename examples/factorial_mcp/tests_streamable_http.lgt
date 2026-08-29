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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-28,
		comment is 'Unit tests for the "factorial_mcp" example over the Streamable HTTP adapter.'
	]).

	:- uses(json_rpc, [
		request/4, is_response/1, is_error_response/1, result/2,
		error_code/2, method/2, params/2
	]).

	:- uses(list, [
		last/2, member/2, memberchk/2
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	cover(factorial_mcp).
	cover(mcp_server_streamable_http_transport).

	setup :-
		mcp_server_streamable_http_transport::cleanup.

	cleanup :-
		mcp_server_streamable_http_transport::cleanup.

	% discover / tools list

	test(factorial_http_discover_01, true) :-
		call_json_rpc(discover_request(1), HTTPResponse),
		http_json_response(HTTPResponse, Response),
		is_response(Response),
		\+ is_error_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, complete),
		has_pair(Result, capabilities, Caps),
		has_pair(Caps, tools, _),
		has_pair(Result, supportedVersions, Versions),
		memberchk('2026-07-28', Versions).

	test(factorial_http_tools_list_01, true) :-
		call_json_rpc(tools_list_request(1), HTTPResponse),
		http_json_response(HTTPResponse, Response),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, complete),
		has_pair(Result, tools, Tools),
		member(Tool, Tools),
		has_pair(Tool, name, factorial).

	% HTTP transport basics

	test(factorial_http_method_not_allowed_01, deterministic(Status == 405)) :-
		with_prepared(
			mcp_server_streamable_http_transport::handle_mcp_request(
				'GET', [], '', http_response(Status, _, _)
			)
		).

	% ping was removed in MCP 2026-07-28; expect method not found
	test(factorial_http_ping_01, true) :-
		call_json_rpc(ping_request(1), HTTPResponse),
		http_json_response(HTTPResponse, Response),
		is_error_response(Response),
		error_code(Response, -32601).

	% auxiliary predicates

	with_prepared(Goal) :-
		catch(
			(	mcp_server_streamable_http_transport::prepare(factorial_mcp, [
					server_name('Factorial'),
					server_title('Factorial (Streamable HTTP)')
				]),
				call(Goal)
			),
			Error,
			(	mcp_server_streamable_http_transport::cleanup,
				throw(Error)
			)
		),
		mcp_server_streamable_http_transport::cleanup.

	:- meta_predicate(with_prepared(0)).

	call_json_rpc(Spec, HTTPResponse) :-
		spec_to_message(Spec, Message),
		json_serialize_term(Message, Body),
		request_headers(Message, Headers),
		with_prepared(
			mcp_server_streamable_http_transport::handle_mcp_request(
				'POST', Headers, Body, HTTPResponse
			)
		).

	% 2026 Streamable HTTP requires MCP-Protocol-Version, Mcp-Method, and
	% Mcp-Name for tools/call, prompts/get, and resources/read.
	request_headers(Message, Headers) :-
		method(Message, Method),
		Base = [
			mcp_protocol_version-'2026-07-28',
			mcp_method-Method,
			accept-'application/json, text/event-stream',
			content_type-'application/json'
		],
		(	params(Message, Params),
			method_mcp_name(Method, Params, Name) ->
			Headers = [mcp_name-Name| Base]
		;	Headers = Base
		).

	method_mcp_name('tools/call', Params, Name) :-
		has_pair(Params, name, Name).
	method_mcp_name('prompts/get', Params, Name) :-
		has_pair(Params, name, Name).
	method_mcp_name('resources/read', Params, Name) :-
		has_pair(Params, uri, Name).

	http_json_response(http_response(200, Headers, Body), Message) :-
		memberchk('Content-Type'-CT, Headers),
		sub_atom(CT, _, _, _, 'application/json'),
		!,
		Body = json_body(Message).
	http_json_response(http_response(200, Headers, Body), Message) :-
		memberchk('Content-Type'-CT, Headers),
		sub_atom(CT, _, _, _, 'text/event-stream'),
		!,
		last_sse_data(Body, JSON),
		json_parse_term(JSON, Message).

	last_sse_data(Body, JSON) :-
		Body = json_body(JSON),
		!.
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

	json_serialize_term(Term, Atom) :-
		(	current_object(json) ->
			json::generate(atom(Atom), Term)
		;	open(atom(Atom), write, Stream),
			write_term(Stream, Term, [quoted(true)]),
			close(Stream)
		).

	json_parse_term(Atom, Term) :-
		(	current_object(json) ->
			json::parse(atom(Atom), Term)
		;	open(atom(Atom), read, Stream),
			read_term(Stream, Term, []),
			close(Stream)
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
	spec_to_message(ping_request(Id), Message) :-
		meta_2026(Meta),
		request(ping, {'_meta'-Meta}, Id, Message).
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
