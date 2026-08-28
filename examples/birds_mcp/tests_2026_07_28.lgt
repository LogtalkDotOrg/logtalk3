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
		date is 2026-08-28,
		comment is 'Unit tests for the "birds_mcp" example under the MCP 2026-07-28 adapter (MRTR).'
	]).

	:- uses(json_rpc, [
		request/4, response/3, is_response/1, is_error_response/1,
		result/2, id/2, write_message/2, read_message/2
	]).

	cover(birds_mcp).

	cleanup :-
		^^clean_file('mcp26_birds_in.tmp'),
		^^clean_file('mcp26_birds_out.tmp').

	% Discover advertises tools
	test(birds_mcp26_discover_01, true) :-
		run_2026([discover_request(1)], [Response]),
		is_response(Response),
		\+ is_error_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, complete),
		has_pair(Result, capabilities, Caps),
		has_pair(Caps, tools, _).

	% tools/list includes identify_bird
	test(birds_mcp26_tools_list_01, true) :-
		run_2026([tools_list_request(1)], [Response]),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, complete),
		has_pair(Result, tools, Tools),
		member(Tool, Tools),
		has_pair(Tool, name, identify_bird).

	% First tools/call round returns input_required with a question
	test(birds_mcp26_mrtr_round1_01, true) :-
		run_2026([tools_call_request(identify_bird, {}, 1)], [Response]),
		is_response(Response),
		result(Response, Result),
		has_pair(Result, resultType, input_required),
		has_pair(Result, inputRequests, Requests),
		Requests = {_},
		has_pair(Result, requestState, State),
		State \== none.

	% Second round: answer the first question, get another input_required or complete
	test(birds_mcp26_mrtr_round2_01, true) :-
		% Round 1
		run_2026([tools_call_request(identify_bird, {}, 1)], [R1]),
		result(R1, Res1),
		has_pair(Res1, resultType, input_required),
		has_pair(Res1, requestState, State1),
		% Round 2 with a yes answer echoed state
		run_2026([
			tools_call_with_state(
				identify_bird, {},
				[{key-q, action-accept, content-{answer-yes}}],
				State1,
				2
			)
		], [R2]),
		is_response(R2),
		result(R2, Res2),
		has_pair(Res2, resultType, Type2),
		(	Type2 == input_required
		;	Type2 == complete
		).

	% Cancel path yields complete with no-identification message
	test(birds_mcp26_mrtr_cancel_01, true) :-
		run_2026([tools_call_request(identify_bird, {}, 1)], [R1]),
		result(R1, Res1),
		has_pair(Res1, requestState, State1),
		run_2026([
			tools_call_with_state(
				identify_bird, {},
				[{key-q, action-cancel}],
				State1,
				2
			)
		], [R2]),
		result(R2, Res2),
		has_pair(Res2, resultType, complete),
		has_pair(Res2, content, [Item| _]),
		has_pair(Item, text, Text),
		sub_atom(Text, _, _, _, 'No bird could be identified').

	% -----------------------------------------------------------------
	% Helpers
	% -----------------------------------------------------------------

	run_2026(Specs, Responses) :-
		^^file_path('mcp26_birds_in.tmp', InFile),
		^^file_path('mcp26_birds_out.tmp', OutFile),
		open(InFile, write, Out),
		write_specs(Out, Specs),
		close(Out),
		open(InFile, read, In),
		open(OutFile, write, OutStream),
		mcp_server::start(
			'birds-expert',
			birds_mcp,
			In,
			OutStream,
			[protocol_adapter(mcp_server_2026_07_28_adapter)]
		),
		close(In),
		close(OutStream),
		open(OutFile, read, InStream),
		read_all(InStream, Responses),
		close(InStream).

	write_specs(_, []).
	write_specs(Stream, [Spec| Specs]) :-
		spec_to_message(Spec, Message),
		write_message(Stream, Message),
		write_specs(Stream, Specs).

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
				elicitation-{}
			}
		}.

	spec_to_message(discover_request(Id), Message) :-
		meta_2026(Meta),
		request('server/discover', {'_meta'-Meta}, Id, Message).
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

	:- uses(list, [member/2]).

	has_pair({Pairs}, Key, Value) :-
		curly_member(Key-Value, Pairs).

	curly_member(Pair, (Pair, _)) :- !.
	curly_member(Pair, (_, Rest)) :- !, curly_member(Pair, Rest).
	curly_member(Pair, Pair).

:- end_object.
