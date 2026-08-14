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


% Fixtures for MCP 2026-07-28 tests: MRTR, cache, progress, and multi-capability apps.


% Simple tools with optional multi-round echo

:- object(test_tools_2026,
	implements([mcp_tool_protocol, mcp_multiround_protocol, mcp_cache_protocol])).

	:- uses(list, [member/2, memberchk/2]).

	:- public(echo/2).
	:- mode(echo(+atom, -atom), one).
	:- info(echo/2, [
		comment is 'Echoes the input atom.',
		argnames is ['Input', 'Output']
	]).

	:- public(add/3).
	:- mode(add(+number, +number, -number), one).
	:- info(add/3, [
		comment is 'Adds two numbers.',
		argnames is ['X', 'Y', 'Sum']
	]).

	capabilities([]).

	tools([
		tool(echo, echo, 2),
		tool(add, add, 3),
		tool(ask_once, ask_once, 0)
	]).

	echo(Input, Input).
	add(X, Y, Sum) :-
		Sum is X + Y.

	% Multi-round tool: first call requests a name, second completes
	:- public(ask_once/0).
	:- mode(ask_once, one).
	:- info(ask_once/0, [
		comment is 'Asks for a name via MRTR then greets.'
	]).

	tool_call_round(ask_once, _Arguments, Context, RoundResult) :-
		Context = request_context(_ClientCaps, InputResponses, RequestState, _Progress),
		(	RequestState == none ->
			RoundResult = input_required(
				[input_request(name_key, form_elicitation('What is your name?', {type-object, properties-{name-{type-string}}, required-[name]}))],
				waiting_for_name
			)
		;	RequestState == waiting_for_name,
			member(input_response(name_key, accept(Content)), InputResponses),
			extract_name(Content, Name) ->
			atom_concat('Hello, ', Name, Temp),
			atom_concat(Temp, '!', Text),
			RoundResult = complete(text(Text))
		;	member(input_response(name_key, decline), InputResponses) ->
			RoundResult = complete(text('Declined.'))
		;	member(input_response(name_key, cancel), InputResponses) ->
			RoundResult = complete(text('Cancelled.'))
		;	% Missing required response — re-request
			RoundResult = input_required(
				[input_request(name_key, form_elicitation('What is your name?', {type-object, properties-{name-{type-string}}, required-[name]}))],
				waiting_for_name
			)
		).

	extract_name({Pairs}, Name) :-
		!,
		curly_member(name-Name, Pairs).
	extract_name(_, unknown).

	curly_member(Pair, (Pair, _)) :- !.
	curly_member(Pair, (_, Rest)) :- !, curly_member(Pair, Rest).
	curly_member(Pair, Pair).

	% Cache policy: tools_list cacheable for 1000ms private
	cache_policy(tools_list, _, 1000, private).
	cache_policy(discover, _, 5000, public).

:- end_object.


% Prompts with multi-round

:- object(test_prompts_2026,
	implements([mcp_tool_protocol, mcp_prompt_protocol, mcp_multiround_protocol])).

	:- uses(list, [member/2]).

	capabilities([prompts]).

	tools([]).

	prompts([
		prompt(greet_prompt, 'Greeting prompt', [
			argument(name, 'Name to greet', true)
		]),
		prompt(confirm_prompt, 'Needs confirmation', [])
	]).

	prompt_get(greet_prompt, Arguments, Result) :-
		(	member(name-Name, Arguments) ->
			atom_concat('Please greet ', Name, Text)
		;	Text = 'Please greet someone.'
		),
		Result = messages([message(user, text(Text))]).

	prompt_get_round(confirm_prompt, _Arguments, Context, RoundResult) :-
		Context = request_context(_Caps, InputResponses, RequestState, _Progress),
		(	RequestState == none ->
			RoundResult = input_required(
				[input_request(ok_key, form_elicitation('Confirm?', {type-object, properties-{ok-{type-boolean}}, required-[ok]}))],
				awaiting_confirm
			)
		;	member(input_response(ok_key, accept(_)), InputResponses) ->
			RoundResult = complete(messages([message(user, text('Confirmed.'))]))
		;	RoundResult = complete(messages([message(user, text('Not confirmed.'))]))
		).

:- end_object.


% Resources with cache and multi-round

:- object(test_resources_2026,
	implements([mcp_tool_protocol, mcp_resource_protocol, mcp_multiround_protocol, mcp_cache_protocol])).

	:- uses(list, [
		member/2
	]).

	capabilities([resources]).

	tools([]).

	resources([
		resource('logtalk://test/data', data, 'Test data', 'text/plain'),
		resource('logtalk://test/gated', gated, 'Gated resource', 'text/plain')
	]).

	resource_read('logtalk://test/data', _Arguments, Result) :-
		Result = contents([
			text_content('logtalk://test/data', 'text/plain', 'Hello 2026')
		]).

	resource_read_round('logtalk://test/gated', _Arguments, Context, RoundResult) :-
		Context = request_context(_Caps, InputResponses, RequestState, _Progress),
		(	RequestState == none ->
			RoundResult = input_required(
				[input_request(token_key, form_elicitation('Provide access token', {type-object, properties-{token-{type-string}}, required-[token]}))],
				need_token
			)
		;	member(input_response(token_key, accept(_)), InputResponses) ->
			RoundResult = complete(contents([
				text_content('logtalk://test/gated', 'text/plain', 'Secret data')
			]))
		;	RoundResult = complete(error('Access denied'))
		).

	cache_policy(resources_read, 'logtalk://test/data', 2000, private).
	cache_policy(resources_list, _, 1000, private).

:- end_object.


% All capabilities for 2026

:- object(test_all_2026,
	implements([
		mcp_tool_protocol,
		mcp_prompt_protocol,
		mcp_resource_protocol,
		mcp_multiround_protocol,
		mcp_cache_protocol
	])).

	:- uses(list, [member/2]).

	capabilities([prompts, resources]).

	:- public(ping_tool/1).
	:- mode(ping_tool(-atom), one).
	:- info(ping_tool/1, [
		comment is 'Returns pong.',
		argnames is ['Reply']
	]).

	tools([
		tool(ping_tool, ping_tool, 1)
	]).

	ping_tool(pong).

	prompts([
		prompt(help, 'Help prompt', [])
	]).

	prompt_get(help, _, Result) :-
		Result = messages([message(user, text('Help me.'))]).

	resources([
		resource('logtalk://all/info', info, 'Info', 'text/plain')
	]).

	resource_read('logtalk://all/info', _, Result) :-
		Result = contents([
			text_content('logtalk://all/info', 'text/plain', 'info')
		]).

	cache_policy(discover, _, 0, private).

:- end_object.
