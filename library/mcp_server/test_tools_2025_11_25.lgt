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


:- object(test_tools_2025_11_25,
	implements([mcp_tool_protocol, mcp_prompt_protocol, mcp_resource_protocol])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-29,
		comment is 'Test application for MCP 2025-11-25 features: icons, URL elicitation, enum schemas.'
	]).

	:- public(echo/2).
	:- mode(echo(+atom, -atom), one).
	:- info(echo/2, [
		comment is 'Echoes the input.',
		argnames is ['Input', 'Output']
	]).

	:- public(ask_color/0).
	:- mode(ask_color, one).
	:- info(ask_color/0, [
		comment is 'Asks for a color via form elicitation with a titled enum schema (SEP-1330).'
	]).

	:- public(open_docs/0).
	:- mode(open_docs, one).
	:- info(open_docs/0, [
		comment is 'URL-mode elicitation pointing the user at documentation (SEP-1036).'
	]).

	:- uses(list, [
		member/2
	]).

	capabilities([prompts, resources, elicitation]).

	tools([
		tool(echo, echo, 2),
		tool(ask_color, ask_color, 0),
		tool(open_docs, open_docs, 0)
	]).

	% SEP-973 icons (public so the adapter can call them)
	:- public(tool_icons/2).
	:- public(prompt_icons/2).
	:- public(resource_icons/2).

	tool_icons(echo, [
		{src-'https://example.com/icons/echo.png', mimeType-'image/png'}
	]).
	tool_icons(ask_color, [
		{src-'https://example.com/icons/palette.svg', mimeType-'image/svg+xml', sizes-['48x48']}
	]).

	prompt_icons(color_prompt, [
		{src-'https://example.com/icons/prompt.png', mimeType-'image/png'}
	]).

	resource_icons('logtalk://test/palette', [
		{src-'https://example.com/icons/resource.png', mimeType-'image/png'}
	]).
	resource_icons('logtalk://test/palettes/{name}', [
		{src-'https://example.com/icons/resource-template.png', mimeType-'image/png'}
	]).

	echo(Input, Input).

	% form elicitation with enum (titled options via enumNames — SEP-1330 pass-through)
	:- meta_predicate(tool_call(*, *, 3, *)).

	tool_call(ask_color, _Arguments, Elicit, Result) :-
		Schema = {
			type-object,
			properties-{
				color-{
					type-string,
					enum-[red, green, blue],
					enumNames-['Red', 'Green', 'Blue']
				}
			},
			required-[color]
		},
		call(Elicit, 'Pick a color', Schema, Answer),
		(	Answer = accept(Content),
			has_pair(Content, color, Color) ->
			atom_concat('You chose ', Color, Text),
			Result = text(Text)
		;	Answer == decline ->
			Result = text('Declined.')
		;	Result = text('Cancelled.')
		).

	% URL-mode elicitation uses a dedicated 4-argument tool_call that receives
	% Input/Output streams via the 2025 elicitation closure when available; for
	% unit tests we expose open_docs through a custom tool_call/3 that the
	% adapter cannot URL-elicit without streams — tests call elicit_url_request/5
	% directly. Here tool_call/3 documents the intended URL for integration.
	tool_call(open_docs, _Arguments, Result) :-
		Result = text('Open https://example.com/docs in a browser (URL elicitation requires stdio streams).').

	prompts([
		prompt(color_prompt, 'Color Prompt', 'Asks about a favorite color', [
			argument(color, 'Favorite color', true)
		])
	]).

	prompt_get(color_prompt, Arguments, Result) :-
		(	member(color-Color, Arguments) ->
			atom_concat('Talk about the color ', Color, Text)
		;	Text = 'Talk about a color.'
		),
		Result = messages([message(user, text(Text))]).

	resources([
		resource(
			'logtalk://test/palette',
			palette,
			'Color Palette',
			'A sample palette resource',
			'application/json'
		)
	]).

	resource_templates([
		resource_template(
			'logtalk://test/palettes/{name}',
			palette,
			'Named Palette',
			'A named palette resource',
			'application/json'
		)
	]).

	resource_read('logtalk://test/palette', _, Result) :-
		Result = contents([
			text_content(
				'logtalk://test/palette',
				'application/json',
				'{"colors":["red","green","blue"]}'
			)
		]).

	has_pair({Pairs}, Key, Value) :-
		curly_member(Key-Value, Pairs).

	curly_member(Pair, (Pair, _)) :-
		!.
	curly_member(Pair, (_, Rest)) :-
		!,
		curly_member(Pair, Rest).
	curly_member(Pair, Pair).

:- end_object.
