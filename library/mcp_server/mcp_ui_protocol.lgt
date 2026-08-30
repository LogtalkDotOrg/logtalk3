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


:- protocol(mcp_ui_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-30,
		comment is 'Optional MCP Apps (``io.modelcontextprotocol/ui``) metadata for tools and resources. Applications that provide interactive UI resources declare ``ui`` in ``capabilities/1`` (and typically ``resources``), implement ``mcp_resource_protocol`` for ``ui://`` HTML resources, and may implement these predicates to attach tool and resource UI metadata. See the https://modelcontextprotocol.io/extensions/apps/overview spec for details.',
		see_also is [mcp_tool_protocol, mcp_resource_protocol, mcp_server]
	]).

	:- public(tool_ui/2).
	:- mode(tool_ui(?atom, -list), zero_or_more).
	:- info(tool_ui/2, [
		comment is 'Optional. UI metadata for a tool. ``Options`` is a list of ``resource_uri(URI)`` and/or ``visibility(List)`` terms. ``URI`` must be a ``ui://`` resource served by the application. ``List`` is a list of ``model`` and/or ``app`` (default when omitted on the wire is both). App-only tools use ``visibility([app])`` so hosts hide them from the model while still allowing iframe-proxied ``tools/call``.',
		argnames is ['ToolName', 'Options']
	]).

	:- public(resource_ui_meta/2).
	:- mode(resource_ui_meta(?atom, -compound), zero_or_more).
	:- info(resource_ui_meta/2, [
		comment is 'Optional. UI metadata for a ``ui://`` resource as a curly-term matching the Apps ``_meta.ui`` object (``csp``, ``permissions``, ``domain``, ``prefersBorder``). Attached when serializing resource descriptors and may be used for ``resources/read`` content.',
		argnames is ['URI', 'Meta']
	]).

:- end_protocol.
