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


:- protocol(mcp_tool_protocol).

	:- info([
		version is 0:3:1,
		author is 'Paulo Moura',
		date is 2026-08-11,
		comment is 'Protocol for Logtalk objects that provide tools to be exposed via an MCP (Model Context Protocol) server. Implements the MCP 2025-06-18 specification. Implementing objects must define the set of tools available and handle tool calls. Tool metadata (names, titles, descriptions, parameter schemas) can be derived automatically from ``info/2`` and ``mode/2`` directives on the tool predicates, or can be specified explicitly via the ``tool/1`` predicate.',
		remarks is [
			'Capabilities' - 'Objects can optionally define ``capabilities/1`` to declare additional application features. Currently supported: ``prompts`` and ``resources`` (server capabilities advertised during initialization) and ``elicitation`` (a required client capability that allows the server to ask the user questions during tool execution). If ``capabilities/1`` is not defined, only the ``tools`` server capability is advertised.',
			'Tool title' - 'The tool title (human-friendly display name) is derived from a ``title`` key in the predicate ``info/2`` directive. If not specified, the predicate name (functor) is used as the title.',
			'Structured output' - 'Tools can declare an output schema via ``output_schema/2`` and return ``structured(StructuredContent)`` or ``structured(ContentItems, StructuredContent)`` results. The structured content is included in the ``structuredContent`` field of the tool call response alongside the ``content`` array.',
			'Resource links' - 'Tool results can include ``resource_link(URI, Name)`` or ``resource_link(URI, Name, Description, MimeType)`` content items in ``results/1`` lists.',
			'Elicitation' - 'Tools that need user interaction during execution should define ``tool_call/4`` instead of ``tool_call/3``. The extra argument is an elicitation closure that can be called as ``call(Elicit, Message, Schema, Answer)`` where ``Message`` is the prompt text (an atom), ``Schema`` is a JSON Schema curly-term for the requested input, and ``Answer`` is unified with ``accept(Content)``, ``decline``, or ``cancel``.'
		]
	]).

	:- public(capabilities/1).
	:- mode(capabilities(-list(atom)), one).
	:- info(capabilities/1, [
		comment is 'Returns a list of additional MCP features used by this provider. ``prompts`` and ``resources`` are advertised as server capabilities. ``elicitation`` declares that the provider may ask the user questions and is enabled only when the client advertises the corresponding client capability. If not defined, only the ``tools`` server capability is advertised.',
		argnames is ['Capabilities']
	]).

	:- public(tools/1).
	:- mode(tools(-list(compound)), one).
	:- info(tools/1, [
		comment is 'Returns a list of tool descriptors available from this object. Each descriptor is a compound term ``tool(Name, Functor, Arity)`` where ``Name`` is the MCP tool name (an atom), ``Functor`` is the predicate functor, and ``Arity`` is the predicate arity. By default, the tool name is the predicate functor. Tool titles are derived from ``info/2`` ``title`` keys (falling back to the predicate name). Tool descriptions and parameter schemas are derived from the ``info/2`` and ``mode/2`` directives of the corresponding predicates.',
		argnames is ['Tools']
	]).

	:- public(tool_call/3).
	:- mode(tool_call(+atom, +list(pair), --compound), one).
	:- info(tool_call/3, [
		comment is 'Handles a tool call. ``Name`` is the MCP tool name (as declared in ``tools/1``), ``Arguments`` is a list of ``ArgumentName-Value`` pairs, and ``Result`` is unified with the tool result. The result must be one of: ``text(Atom)`` for a text result, ``error(Atom)`` for an error result, ``results(List)`` for a list of content items where each item is ``text(Atom)``, ``error(Atom)``, ``resource_link(URI, Name)``, or ``resource_link(URI, Name, Description, MimeType)``, ``structured(StructuredContent)`` for structured output with auto-generated text, or ``structured(ContentItems, StructuredContent)`` for structured output with explicit content items. If this predicate is not defined for a tool, the MCP server will use auto-dispatch: it calls the tool predicate as a message to the implementing object, collects output arguments, and returns them as a text result.',
		argnames is ['Name', 'Arguments', 'Result']
	]).

	:- public(tool_call/4).
	:- mode(tool_call(+atom, +list(pair), +callable, --compound), one).
	:- info(tool_call/4, [
		comment is 'Handles a tool call with elicitation support. Same as ``tool_call/3`` but receives an ``Elicit`` closure as the third argument. The closure can be called as ``call(Elicit, Message, Schema, Answer)`` where ``Message`` is a prompt text (an atom), ``Schema`` is a curly-term JSON Schema describing the requested input (e.g., ``{type-object, properties-{answer-{type-string, enum-[yes, no]}}, required-[answer]}``), and ``Answer`` is unified with ``accept(Content)`` (where ``Content`` is the user response as a curly-term), ``decline``, or ``cancel``. Only available when the application declares ``elicitation`` in its ``capabilities/1`` and the client advertises elicitation support during initialization. Falls back to ``tool_call/3`` and then auto-dispatch if not defined.',
		argnames is ['Name', 'Arguments', 'Elicit', 'Result']
	]).

	:- public(output_schema/2).
	:- mode(output_schema(+atom, -compound), zero_or_one).
	:- info(output_schema/2, [
		comment is 'Returns the JSON output schema for the given tool name. Optional; when defined, the schema is included in the tool descriptor as ``outputSchema`` and the tool can return ``structured(StructuredContent)`` or ``structured(ContentItems, StructuredContent)`` results. The schema must be a curly-term following JSON Schema format (e.g. ``{type-object, properties-{temperature-{type-number}}, required-[temperature]}``).',
		argnames is ['Name', 'Schema']
	]).

:- end_protocol.
