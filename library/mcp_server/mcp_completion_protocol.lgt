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


:- protocol(mcp_completion_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-02,
		comment is 'Protocol for Logtalk objects that provide MCP prompt argument and resource URI completions. Used by the 2025-06-18, 2025-11-25, and 2026-07-28 adapters.',
		remarks is [
			'Capabilities' - 'Objects providing completions must declare ``completions`` in their ``capabilities/1`` predicate (from the ``mcp_tool_protocol`` protocol).',
			'References' - 'The reference is either ``prompt(Name)`` for a declared prompt or ``resource(URI)`` for a declared resource URI or URI template.',
			'Results' - 'The result is ``completion(Values)`` or ``completion(Values, Total, HasMore)``. ``Values`` is an application-ranked list of atoms. The server preserves the first 100 values and sets ``hasMore`` to true when truncation is required. ``Total`` is a non-negative integer and ``HasMore`` is ``true`` or ``false``.'
		]
	]).

	:- public(completion/4).
	:- mode(completion(+compound, +pair, +list(pair), --compound), one).
	:- info(completion/4, [
		comment is 'Returns completion suggestions for a prompt argument or resource URI. ``Reference`` is ``prompt(Name)`` or ``resource(URI)``, ``Argument`` is an ``ArgumentName-PartialValue`` pair, ``Context`` is a list of previously resolved ``Name-Value`` argument pairs, and ``Result`` is ``completion(Values)`` or ``completion(Values, Total, HasMore)``.',
		argnames is ['Reference', 'Argument', 'Context', 'Result']
	]).

:- end_protocol.
