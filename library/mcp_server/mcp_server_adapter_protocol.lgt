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


:- protocol(mcp_server_adapter_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-14,
		comment is 'Protocol for MCP specification adapters. Each adapter implements one MCP protocol version (e.g. 2025-06-18 or 2026-07-28). The ``mcp_server`` facade selects an adapter via the ``protocol_adapter/1`` option and delegates the server lifecycle to it.'
	]).

	:- public(spec/1).
	:- mode(spec(-atom), one).
	:- info(spec/1, [
		comment is 'Returns the MCP protocol version string implemented by this adapter (e.g. ``''2025-06-18''`` or ``''2026-07-28''``).',
		argnames is ['Version']
	]).

	:- public(start/4).
	:- mode(start(+object_identifier, +stream, +stream, +list), one).
	:- info(start/4, [
		comment is 'Starts the adapter against the given application object, input/output streams, and normalized server configuration options. Blocks until the client disconnects (EOF) or an exit condition is reached. The adapter is responsible for all request handling, capability negotiation, and cleanup of adapter-owned state.',
		argnames is ['Application', 'Input', 'Output', 'Options']
	]).

	:- public(notify/1).
	:- mode(notify(+compound), zero_or_one).
	:- info(notify/1, [
		comment is 'Publishes an application event to interested clients. Canonical event terms are ``tools_list_changed``, ``prompts_list_changed``, ``resources_list_changed``, and ``resource_updated(URI)``. The 2025-06-18 adapter may ignore or reject events not supported by that specification; the 2026-07-28 adapter filters events through active subscriptions.',
		argnames is ['Event']
	]).

	:- public(cleanup/0).
	:- mode(cleanup, one).
	:- info(cleanup/0, [
		comment is 'Releases any adapter-owned dynamic state. Called by the facade after the server loop terminates (success, failure, exception, or EOF).'
	]).

:- end_protocol.
