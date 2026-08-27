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


:- object(get_time,
	implements((mcp_tool_protocol, mcp_resource_protocol, mcp_ui_protocol))).

	:- info([
		version is 1:0:0,
		author is 'Example',
		date is 2026-08-27,
		comment is 'MCP Apps demo: get-time tool linked to a ``ui://`` HTML view. UI inspired by ``modelcontextprotocol/ext-apps``.'
	]).

	:- public(get_time/0).
	:- mode(get_time, one).
	:- info(get_time/0, [
		comment is 'Returns the current server time as ISO-8601 text.'
	]).

	:- uses(os, [
		date_time/7, path_concat/3
	]).

	:- uses(date, [
		format_date_time/4
	]).

	:- uses(reader, [
		file_to_codes/2
	]).

	capabilities([resources, ui]).

	tools([
		tool(get_time, get_time, 0)
	]).

	% prefer explicit tool_call/3 so the result is a clean text content item
	tool_call(get_time, _ArgPairs, structured([text(Text)], {})) :-
		date_time(Year, Month, Day, Hours, Minutes, Seconds, _),
		format_date_time(date_time(Year, Month, Day, Hours, Minutes, Seconds), 0, date_time_medium, Text).

	% link tool to UI resource (MCP Apps)
	tool_ui(get_time, [
		resource_uri('ui://get-time/mcp-app.html'),
		visibility([model, app])
	]).

	resources([
		resource(
			'ui://get-time/mcp-app.html',
			get_time_ui,
			'Get Time App',
			'Interactive UI for the get-time tool',
			'text/html;profile=mcp-app'
		)
	]).

	resource_read('ui://get-time/mcp-app.html', _Params, Result) :-
		mcp_app_html(HTML),
		Result = contents([
			text_content(
				'ui://get-time/mcp-app.html',
				'text/html;profile=mcp-app',
				HTML
			)
		]).

	mcp_app_html(HTML) :-
		this(This),
		object_property(This, file(_, Directory)),
		path_concat(Directory, 'mcp-app.html', File),
		file_to_codes(File, Codes),
		atom_codes(HTML, Codes).

:- end_object.
