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


:- protocol(mcp_prompt_protocol).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-02-24,
		comment is 'Protocol for Logtalk objects that provide prompts to be exposed via an MCP (Model Context Protocol) server. Implementing objects must define the set of prompts available and handle prompt get requests. Prompts are templates for structured LLM interactions that can accept arguments to customize their behavior.',
		remarks is [
			'Capabilities' - 'Objects providing prompts must declare ``prompts`` in their ``capabilities/1`` predicate (from the ``mcp_tool_protocol`` protocol). The server will then advertise the ``prompts`` capability and handle ``prompts/list`` and ``prompts/get`` requests.',
			'Prompt descriptors' - 'Each prompt is described by a ``prompt(Name, Description, Arguments)`` term where ``Name`` is an atom, ``Description`` is a human-readable atom, and ``Arguments`` is a list of ``argument(ArgName, ArgDescription, Required)`` terms.',
			'Prompt messages' - 'The ``prompt_get/3`` predicate must return a result term containing a list of messages. Each message is a ``message(Role, Content)`` term where ``Role`` is ``user`` or ``assistant`` and ``Content`` is ``text(Text)`` where ``Text`` is an atom.'
		]
	]).

	:- public(prompts/1).
	:- mode(prompts(-list(compound)), one).
	:- info(prompts/1, [
		comment is 'Returns a list of prompt descriptors available from this object. Each descriptor is a compound term ``prompt(Name, Description, Arguments)`` where ``Name`` is the MCP prompt name (an atom), ``Description`` is a human-readable description (an atom), and ``Arguments`` is a list of ``argument(ArgName, ArgDescription, Required)`` terms describing the prompt arguments. ``ArgName`` and ``ArgDescription`` are atoms, and ``Required`` is the boolean ``true`` or ``false``.',
		argnames is ['Prompts']
	]).

	:- public(prompt_get/3).
	:- mode(prompt_get(+atom, +list(pair), --compound), one).
	:- info(prompt_get/3, [
		comment is 'Handles a prompt get request. ``Name`` is the MCP prompt name (as declared in ``prompts/1``), ``Arguments`` is a list of ``ArgumentName-Value`` pairs provided by the client, and ``Result`` is unified with the prompt result. The result must be one of: ``messages(MessageList)`` for a list of prompt messages, or ``messages(Description, MessageList)`` to also include a description. Each message in the list must be a ``message(Role, Content)`` term where ``Role`` is ``user`` or ``assistant`` and ``Content`` is ``text(Text)`` where ``Text`` is an atom.',
		argnames is ['Name', 'Arguments', 'Result']
	]).

:- end_protocol.
