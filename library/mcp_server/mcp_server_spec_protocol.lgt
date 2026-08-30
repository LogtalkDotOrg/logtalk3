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


:- protocol(mcp_server_spec_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-30,
		comment is 'Protocol for MCP spec (version) handlers independent of transport. A handler validates and dispatches JSON-RPC messages and returns abstract outcomes that a transport (stdio or Streamable HTTP) renders to the wire.'
	]).

	:- public(spec/1).
	:- mode(spec(-atom), one).
	:- info(spec/1, [
		comment is 'MCP protocol version string implemented by this handler (e.g. ``''2025-06-18''`` or ``''2026-07-28''``).',
		argnames is ['Version']
	]).

	:- public(prepare/2).
	:- mode(prepare(+object_identifier, +list), one_or_error).
	:- info(prepare/2, [
		comment is 'Initializes handler state for ``Application`` with merged ``Options``. Does not open any transport.',
		argnames is ['Application', 'Options'],
		exceptions is [
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option')
		]
	]).

	:- public(handle_message/3).
	:- mode(handle_message(+nonvar, +list, -nonvar), one).
	:- info(handle_message/3, [
		comment is 'Handles one JSON-RPC Message under Options and returns an Outcome for the transport to render. Outcomes include ``reply(JsonRpcTerm)``, ``accepted``, ``no_reply``, and transport-agnostic error replies already encoded as JSON-RPC error objects inside ``reply/1``.',
		argnames is ['Message', 'Options', 'Outcome']
	]).

	:- public(notify/1).
	:- mode(notify(+compound), zero_or_one).
	:- info(notify/1, [
		comment is 'Publishes an application event. Transports that support server-initiated notifications deliver it; others may ignore it.',
		argnames is ['Event']
	]).

	:- public(cleanup/0).
	:- mode(cleanup, one).
	:- info(cleanup/0, [
		comment is 'Releases handler-owned dynamic state.'
	]).

:- end_protocol.
