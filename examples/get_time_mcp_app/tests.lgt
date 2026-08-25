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
		date is 2026-08-25,
		comment is 'Unit tests for the get_time MCP Apps example (server-side Apps contract).'
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		memberchk/2
	]).

	% capabilities (MCP Apps extension)

	test(get_time_capabilities_include_resources_and_ui, deterministic) :-
		get_time::capabilities(Caps),
		assertion(memberchk(resources, Caps)),
		assertion(memberchk(ui, Caps)).

	% tools / tool_ui metadata

	test(get_time_tools_lists_get_time, deterministic) :-
		get_time::tools(Tools),
		assertion(memberchk(tool(get_time, get_time, 0), Tools)).

	test(get_time_tool_ui_resource_uri, deterministic) :-
		get_time::tool_ui(get_time, Options),
		assertion(memberchk(resource_uri('ui://get-time/mcp-app.html'), Options)).

	test(get_time_tool_ui_visibility_includes_model_and_app, deterministic) :-
		get_time::tool_ui(get_time, Options),
		memberchk(visibility(Visibility), Options),
		assertion(memberchk(model, Visibility)),
		assertion(memberchk(app, Visibility)).

	% resources / MIME type

	test(get_time_resources_lists_ui_resource, deterministic) :-
		get_time::resources(Resources),
		assertion(memberchk(
			resource(
				'ui://get-time/mcp-app.html',
				get_time_ui,
				'Get Time App',
				'Interactive UI for the get-time tool',
				'text/html;profile=mcp-app'
			),
			Resources
		)).

	test(get_time_resource_read_mime_and_html, deterministic) :-
		get_time::resource_read('ui://get-time/mcp-app.html', {}, Result),
		Result = contents(Contents),
		Contents = [text_content(URI, Mime, HTML)| _],
		assertion(URI == 'ui://get-time/mcp-app.html'),
		assertion(Mime == 'text/html;profile=mcp-app'),
		assertion(atom(HTML)),
		assertion(sub_atom(HTML, _, _, _, 'get_time')),
		assertion(sub_atom(HTML, _, _, _, 'ui/initialize')).

	% tools/call text result

	test(get_time_tool_call_returns_text, deterministic((atom(Text), Text \== ''))) :-
		get_time::tool_call(get_time, [], text(Text)).

	test(get_time_tool_call_text_looks_like_date_time, deterministic(Length > 0)) :-
		get_time::tool_call(get_time, [], Result),
		Result = text(Text),
		% format_date_time(..., date_time_medium, ...) is time-dependent but non-empty
		atom_length(Text, Length).

:- end_object.
