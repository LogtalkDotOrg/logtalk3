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


:- protocol(mcp_cache_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-14,
		comment is 'Optional protocol for per-operation cache policy used by the MCP 2026-07-28 specification. An application may implement ``cache_policy/4`` to choose TTL (milliseconds) and scope (``public`` or ``private``) for complete results of ``server/discover``, list operations (``tools/list``, ``prompts/list``, ``resources/list``, ``resources/templates/list``), and ``resources/read``. Cache fields are never attached to ``input_required`` results or MRTR retries. When the predicate is not defined, or for an operation it does not cover, the server falls back to the options ``cache_ttl(0)`` and ``cache_scope(private)``.',
		remarks is [
			'TTL' - 'A non-negative integer number of milliseconds. ``0`` means the result must not be cached.',
			'Scope' - 'Either ``public`` (may be shared across clients) or ``private`` (client-specific).',
			'Applicability' - 'Only complete final results of discover, list, and resource-read operations receive cache fields. Tool calls, prompt gets that return ``input_required``, and any MRTR continuation never include ``ttlMs`` / ``cacheScope``.'
		]
	]).

	:- public(cache_policy/4).
	:- mode(cache_policy(+atom, +term, -integer, -atom), zero_or_one).
	:- info(cache_policy/4, [
		comment is 'Returns the cache policy for a given operation and request. ``Operation`` is one of ``discover``, ``tools_list``, ``prompts_list``, ``resources_list``, ``resources_templates_list``, or ``resources_read``. ``Request`` is the request-specific identifier or arguments (e.g. a resource URI for ``resources_read``, or a free term for list/discover). ``TTL`` is a non-negative integer (milliseconds). ``Scope`` is ``public`` or ``private``. Failure means the server should use its configured defaults.',
		argnames is ['Operation', 'Request', 'TTL', 'Scope']
	]).

:- end_protocol.
