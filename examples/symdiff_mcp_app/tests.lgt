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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-01,
		comment is 'Unit tests for the symbolic differentiation MCP Apps example.'
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(term_io, [
		read_term_from_atom/3
	]).

	test(symdiff_mcp_app_capabilities, deterministic) :-
		symdiff_mcp_app::capabilities(Capabilities),
		assertion(memberchk(resources, Capabilities)),
		assertion(memberchk(ui, Capabilities)).

	test(symdiff_mcp_app_tools, deterministic) :-
		symdiff_mcp_app::tools(Tools),
		assertion(memberchk(tool(open_symdiff, open_symdiff, 0), Tools)),
		assertion(memberchk(tool(differentiate, differentiate, 3), Tools)).

	test(symdiff_mcp_app_input_schema, deterministic) :-
		symdiff_mcp_app::input_schema(differentiate, Schema),
		has_pair(Schema, type, object),
		has_pair(Schema, required, Required),
		assertion(memberchk(expression, Required)).

	test(symdiff_mcp_app_output_schema, deterministic) :-
		symdiff_mcp_app::output_schema(differentiate, Schema),
		has_pair(Schema, type, object),
		has_pair(Schema, required, Required),
		assertion(memberchk(status, Required)),
		assertion(memberchk(derivative, Required)),
		assertion(memberchk(simplified, Required)),
		assertion(memberchk(error, Required)).

	test(symdiff_mcp_app_tool_ui, deterministic) :-
		symdiff_mcp_app::tool_ui(open_symdiff, Options),
		assertion(memberchk(resource_uri('ui://symdiff/mcp-app.html'), Options)),
		memberchk(visibility(Visibility), Options),
		assertion(memberchk(model, Visibility)),
		assertion(memberchk(app, Visibility)).

	test(symdiff_mcp_app_differentiate_app_only, deterministic) :-
		symdiff_mcp_app::tool_ui(differentiate, Options),
		assertion(memberchk(visibility([app]), Options)),
		assertion(\+ memberchk(resource_uri(_), Options)).

	test(symdiff_mcp_app_open_tool_call, deterministic) :-
		symdiff_mcp_app::tool_call(open_symdiff, [], Result),
		Result = structured([text(Message)], Structured),
		assertion(atom(Message)),
		has_pair(Structured, status, idle).

	test(symdiff_mcp_app_resources, deterministic) :-
		symdiff_mcp_app::resources(Resources),
		assertion(memberchk(
			resource(
				'ui://symdiff/mcp-app.html',
				symdiff_ui,
				'Symbolic Differentiation App',
				'Interactive UI for symbolic differentiation and simplification',
				'text/html;profile=mcp-app'
			),
			Resources
		)).

	test(symdiff_mcp_app_resource_read, deterministic) :-
		symdiff_mcp_app::resource_read('ui://symdiff/mcp-app.html', {}, Result),
		Result = contents([text_content(URI, MimeType, HTML)]),
		assertion(URI == 'ui://symdiff/mcp-app.html'),
		assertion(MimeType == 'text/html;profile=mcp-app'),
		assertion(atom(HTML)),
		assertion(sub_atom(HTML, _, _, _, 'differentiate')),
		assertion(sub_atom(HTML, _, _, _, 'ui/initialize')),
		assertion(\+ sub_atom(HTML, _, _, _, '<script src=')),
		assertion(\+ sub_atom(HTML, _, _, _, '<link rel="stylesheet"')).

	test(symdiff_mcp_app_tool_call_success, deterministic) :-
		symdiff_mcp_app::tool_call(differentiate, [expression-'2*x**3 + x**2 - 4*x'], Result),
		Result = structured([text(Text)], Structured),
		assertion(atom(Text)),
		has_pair(Structured, status, success),
		has_pair(Structured, derivative, Derivative),
		has_pair(Structured, simplified, Simplified),
		read_term_from_atom(Derivative, DerivativeTerm, []),
		read_term_from_atom(Simplified, SimplifiedTerm, []),
		assertion(DerivativeTerm == 2 * (3*x**2*1) + 2*x**1*1-4*1),
		assertion(SimplifiedTerm == 2 * (3*x**2) + 2*x-4),
		has_pair(Structured, error, '').

	test(symdiff_mcp_app_tool_call_logarithm, deterministic) :-
		symdiff_mcp_app::tool_call(differentiate, [expression-'log(x**2)'], Result),
		Result = structured([text(_)], Structured),
		has_pair(Structured, status, success),
		has_pair(Structured, simplified, Simplified),
		assertion(Simplified \== '').

	test(symdiff_mcp_app_tool_call_parse_error, deterministic) :-
		symdiff_mcp_app::tool_call(differentiate, [expression-'x +'], Result),
		assert_error_result(Result).

	test(symdiff_mcp_app_tool_call_unsupported_expression, deterministic) :-
		symdiff_mcp_app::tool_call(differentiate, [expression-'sin(x)'], Result),
		assert_error_result(Result).

	test(symdiff_mcp_app_tool_call_variable, deterministic) :-
		symdiff_mcp_app::tool_call(differentiate, [expression-'X + 1'], Result),
		assert_error_result(Result).

	test(symdiff_mcp_app_tool_call_empty_expression, deterministic) :-
		symdiff_mcp_app::tool_call(differentiate, [expression-''], Result),
		assert_idle_result(Result).

	test(symdiff_mcp_app_tool_call_whitespace_expression, deterministic) :-
		symdiff_mcp_app::tool_call(differentiate, [expression-'  \t\n'], Result),
		assert_idle_result(Result).

	test(symdiff_mcp_app_tool_call_non_atom_expression, deterministic) :-
		symdiff_mcp_app::tool_call(differentiate, [expression-42], Result),
		assert_error_result(Result).

	test(symdiff_mcp_app_tool_call_missing_expression, deterministic) :-
		symdiff_mcp_app::tool_call(differentiate, [], Result),
		assert_idle_result(Result).

	assert_idle_result(structured([text(Message)], Structured)) :-
		assertion(atom(Message)),
		has_pair(Structured, status, idle),
		has_pair(Structured, derivative, ''),
		has_pair(Structured, simplified, ''),
		has_pair(Structured, error, '').

	assert_error_result(structured([text(Error)], Structured)) :-
		assertion(atom(Error)),
		has_pair(Structured, status, error),
		has_pair(Structured, derivative, ''),
		has_pair(Structured, simplified, ''),
		has_pair(Structured, error, Error).

	has_pair({Pairs}, Key, Value) :-
		curly_member(Key-Value, Pairs).

	curly_member(Pair, (Pair, _)) :-
		!.
	curly_member(Pair, (_, Rest)) :-
		!,
		curly_member(Pair, Rest).
	curly_member(Pair, Pair).

:- end_object.
