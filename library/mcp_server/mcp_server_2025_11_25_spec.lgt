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


:- object(mcp_server_2025_11_25_spec,
	extends(mcp_server_2025_06_18_spec)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-31,
		comment is 'MCP 2025-11-25 protocol handler. Extends the 2025-06-18 handler with version ``2025-11-25`` negotiation (still accepting ``2025-06-18``), optional ``serverInfo.description``, icons metadata on tools/prompts/resources (SEP-973), and URL-mode elicitation (SEP-1036). EnumSchema / ElicitResult enrichments (SEP-1330) are pass-through via application-supplied schemas. Does **not** implement sampling tool calling, experimental Tasks, or OAuth/OIDC.'
	]).

	:- public(elicit_url_request/5).
	:- mode(elicit_url_request(+stream, +stream, +atom, +atom, --compound), one).
	:- info(elicit_url_request/5, [
		comment is 'Sends a URL-mode ``elicitation/create`` request (SEP-1036) and reads the client response. ``Answer`` is ``accept(Content)``, ``decline``, or ``cancel``.',
		argnames is ['Input', 'Output', 'Message', 'URL', 'Answer']
	]).

	:- uses(json_rpc, [
		request/4, response/3, is_response/1, result/2, write_message/2, read_message/2
	]).

	% version (parent handle_initialize uses ::supported_specs/1)

	spec('2025-11-25').

	supported_specs(['2025-11-25', '2025-06-18']).

	% tools/list — optional icons (SEP-973)

	handle_tools_list(Id, Options, reply(Response)) :-
		^^option(application(Application), Options),
		Application::tools(ToolDescriptors),
		^^tool_descriptors_to_json(ToolDescriptors, Application, JsonTools0),
		enrich_with_icons(Application, tool_icons, name, JsonTools0, JsonTools),
		Result = {tools-JsonTools},
		response(Result, Id, Response).

	% prompts/list — optional icons

	handle_prompts_list(Id, Options, reply(Response)) :-
		^^option(application(Application), Options),
		(	conforms_to_protocol(Application, mcp_prompt_protocol),
			Application::prompts(PromptDescriptors) ->
			^^prompt_descriptors_to_json(PromptDescriptors, JsonPrompts0),
			enrich_with_icons(Application, prompt_icons, name, JsonPrompts0, JsonPrompts)
		;	JsonPrompts = []
		),
		Result = {prompts-JsonPrompts},
		response(Result, Id, Response).

	% resources/list — optional icons

	handle_resources_list(Id, Options, reply(Response)) :-
		^^option(application(Application), Options),
		(	conforms_to_protocol(Application, mcp_resource_protocol),
			Application::resources(ResourceDescriptors) ->
			^^resource_descriptors_to_json(ResourceDescriptors, Application, JsonResources0),
			enrich_with_icons(Application, resource_icons, uri, JsonResources0, JsonResources)
		;	JsonResources = []
		),
		Result = {resources-JsonResources},
		response(Result, Id, Response).

	% Icons: application may define tool_icons/2, prompt_icons/2, resource_icons/2
	% returning a list of icon objects (e.g. {src-'https://.../icon.png', mimeType-'image/png'}).

	enrich_with_icons(_, _, _, [], []).
	enrich_with_icons(Application, Pred, Key, [Item0| Items0], [Item| Items]) :-
		(	^^has_pair(Item0, Key, Id),
			Goal =.. [Pred, Id, Icons],
			catch(Application::Goal, _, fail),
			Icons = [_| _] ->
			add_icons_field(Item0, Icons, Item)
		;	Item = Item0
		),
		enrich_with_icons(Application, Pred, Key, Items0, Items).

	add_icons_field({Pairs0}, Icons, {(icons-Icons, Pairs0)}) :-
		nonvar(Pairs0),
		Pairs0 \== {},
		!.
	add_icons_field(_, Icons, {icons-Icons}).

	% URL-mode elicitation (SEP-1036)

	elicit_url_request(Input, Output, Message, URL, Answer) :-
		^^generate_elicit_id(ElicitId),
		Params = {
			message-Message,
			(mode)-url,
			url-URL
		},
		request('elicitation/create', Params, ElicitId, Request),
		write_message(Output, Request),
		read_message(Input, Response),
		(	is_response(Response) ->
			result(Response, ResultObj),
			(	^^has_pair(ResultObj, action, Action) ->
				(	Action == accept ->
					(	^^has_pair(ResultObj, content, Content) ->
						Answer = accept(Content)
					;	Answer = accept({})
					)
				;	Action == decline ->
					Answer = decline
				;	Answer = cancel
				)
			;	Answer = cancel
			)
		;	Answer = cancel
		).

:- end_object.
