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


:- protocol(mcp_multiround_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-14,
		comment is 'Protocol for multi-round tool/prompt/resource operations (MRTR) used by the MCP 2026-07-28 specification. Implementing objects may define the round hooks to request additional input from the client (elicitation, sampling, roots) and to resume with input responses and opaque request state. Existing applications that do not implement these hooks continue to use ``tool_call/3``, ``prompt_get/3``, ``resource_read/3``, or auto-dispatch; the 2026 adapter wraps those results as ``complete(Result)``. The 2026 adapter never invokes the 2025-06-18 ``tool_call/4`` elicitation API.',
		remarks is [
			'Request context' - 'Each round hook receives a ``request_context(ClientCapabilities, InputResponses, RequestState, Progress)`` term. ``ClientCapabilities`` is the curly-term advertised by the client. ``InputResponses`` is a list of ``input_response(Key, Response)`` terms (or empty on the first round). ``RequestState`` is application-owned opaque data (or ``none``). ``Progress`` is a callable closure for emitting progress notifications when the client supplied a progress token; it is only meaningful under the 2026 adapter.',
			'Complete results' - 'A hook returns ``complete(Result)`` where ``Result`` uses the existing canonical vocabulary: ``text(Atom)``, ``error(Atom)``, ``results(List)``, ``structured(StructuredContent)``, ``structured(Items, StructuredContent)`` for tools; ``messages(MessageList)`` or ``messages(Description, MessageList)`` for prompts; ``contents(ContentList)`` for resources.',
			'Input-required results' - 'A hook returns ``input_required(InputRequests, RequestState)``. ``InputRequests`` is a nonempty list of uniquely keyed ``input_request(Key, Request)`` terms, and/or ``RequestState`` is non-``none``. An input-required result must contain a nonempty request list, non-``none`` state, or both.',
			'Allowed request forms' - '``Request`` may be ``form_elicitation(Message, Schema)``, ``url_elicitation(Message, URL)``, ``sampling(Messages, ModelPreferences, SystemPrompt, IncludeContext)``, or ``roots``.',
			'Input responses' - 'On a subsequent round the adapter supplies ``InputResponses`` with the same keys. Response values are ``accept(Content)``, ``decline``, ``cancel`` (elicitation), sampling results, or roots results. Unrecognized keys are ignored. Malformed known values yield a ``-32602`` error. Missing required responses cause the adapter to re-issue the corresponding input requests.',
			'State integrity' - '``RequestState`` is opaque to the adapter. Applications that use it for authorization or business decisions must integrity-protect, bind, and validate it themselves.'
		]
	]).

	% round hooks

	:- public(tool_call_round/4).
	:- mode(tool_call_round(+atom, +list(pair), +compound, --compound), zero_or_one).
	:- info(tool_call_round/4, [
		comment is 'Handles one round of a multi-round tool call. ``Name`` is the MCP tool name, ``Arguments`` is a list of ``ArgumentName-Value`` pairs, ``Context`` is a ``request_context(ClientCapabilities, InputResponses, RequestState, Progress)`` term, and ``RoundResult`` is unified with either ``complete(Result)`` or ``input_required(InputRequests, RequestState)``. If not defined, the 2026 adapter falls back to ``tool_call/3`` (or auto-dispatch) and wraps the outcome as ``complete``.',
		argnames is ['Name', 'Arguments', 'Context', 'RoundResult']
	]).

	:- public(prompt_get_round/4).
	:- mode(prompt_get_round(+atom, +list(pair), +compound, --compound), zero_or_one).
	:- info(prompt_get_round/4, [
		comment is 'Handles one round of a multi-round prompt get. ``Name`` is the MCP prompt name, ``Arguments`` is a list of ``ArgumentName-Value`` pairs, ``Context`` is a ``request_context/4`` term, and ``RoundResult`` is unified with either ``complete(Result)`` or ``input_required(InputRequests, RequestState)``. If not defined, the 2026 adapter falls back to ``prompt_get/3`` and wraps the outcome as ``complete``.',
		argnames is ['Name', 'Arguments', 'Context', 'RoundResult']
	]).

	:- public(resource_read_round/4).
	:- mode(resource_read_round(+atom, +list(pair), +compound, --compound), zero_or_one).
	:- info(resource_read_round/4, [
		comment is 'Handles one round of a multi-round resource read. ``URI`` is the resource identifier, ``Arguments`` is a list of ``ArgumentName-Value`` pairs (currently unused by most applications), ``Context`` is a ``request_context/4`` term, and ``RoundResult`` is unified with either ``complete(Result)`` or ``input_required(InputRequests, RequestState)``. If not defined, the 2026 adapter falls back to ``resource_read/3`` and wraps the outcome as ``complete``.',
		argnames is ['URI', 'Arguments', 'Context', 'RoundResult']
	]).

:- end_protocol.
