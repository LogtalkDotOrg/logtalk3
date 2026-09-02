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


:- protocol(mcp_resource_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-02,
		comment is 'Protocol for Logtalk objects that provide resources to be exposed via an MCP (Model Context Protocol) server. Implementing objects must define the set of resources available and handle resource read requests. Applications may also expose parameterized resources using URI templates. Resources expose data and content from the application that MCP clients can access. Used by both the 2025-06-18 and 2026-07-28 adapters.',
		remarks is [
			'Capabilities' - 'Objects providing resources must declare ``resources`` in their ``capabilities/1`` predicate (from the ``mcp_tool_protocol`` protocol). The server will then advertise the ``resources`` capability and handle ``resources/list``, ``resources/templates/list``, and ``resources/read`` requests.',
			'Resource descriptors' - 'Each resource is described by a ``resource(URI, Name, Description, MimeType)`` or ``resource(URI, Name, Title, Description, MimeType)`` term where ``URI`` is the resource identifier (an atom), ``Name`` is a human-readable name (an atom), ``Title`` is an optional human-friendly display name (an atom), ``Description`` is a human-readable description (an atom), and ``MimeType`` is the MIME type of the resource content (an atom, e.g. ``''text/plain''``).',
			'Resource template descriptors' - 'Parameterized resources are optionally described by ``resource_template(URITemplate, Name, Description, MimeType)`` or ``resource_template(URITemplate, Name, Title, Description, MimeType)`` terms. ``URITemplate`` is an RFC 6570 URI template atom. Template listing is validated as a whole: an invalid template raises a ``domain_error(uri_template, URITemplate)`` error and no partial list is advertised. The server advertises valid templates without expanding them; applications validate, authorize, and read concrete expanded URIs using ``resource_read/3``.',
			'Resource contents' - 'The ``resource_read/3`` predicate must return a result term. The result must be ``contents(ContentList)`` where each item is ``text_content(URI, MimeType, Text)`` for text resources or ``blob_content(URI, MimeType, Base64Data)`` for binary resources encoded as base64.',
			'Multi-round (2026-07-28)' - 'For the 2026-07-28 adapter, applications that need additional input during a resource read implement ``resource_read_round/4`` from the ``mcp_multiround_protocol``. Existing ``resource_read/3`` remains valid and is wrapped as a ``complete`` result when the round hook is absent.',
			'Caching (2026-07-28)' - 'Complete ``resources/read`` results may include cache fields (``ttlMs``, ``cacheScope``) derived from ``mcp_cache_protocol`` or server options. Cache fields are never attached to ``input_required`` results.'
		]
	]).

	:- public(resources/1).
	:- mode(resources(-list(compound)), one).
	:- info(resources/1, [
		comment is 'Returns a list of resource descriptors available from this object. Each descriptor is a compound term ``resource(URI, Name, Description, MimeType)`` or ``resource(URI, Name, Title, Description, MimeType)`` where ``URI`` is the resource identifier (an atom, typically a URI like ``logtalk://my-app/data``), ``Name`` is a human-readable name (an atom), ``Title`` is an optional human-friendly display name (an atom), ``Description`` is a human-readable description (an atom), and ``MimeType`` is the MIME type (an atom, e.g. ``''text/plain''``, ``''application/json''``).',
		argnames is ['Resources']
	]).

	:- public(resource_templates/1).
	:- mode(resource_templates(-list(compound)), zero_or_one).
	:- info(resource_templates/1, [
		comment is 'Optional. Returns a list of parameterized resource descriptors. Each descriptor is a ``resource_template(URITemplate, Name, Description, MimeType)`` or ``resource_template(URITemplate, Name, Title, Description, MimeType)`` term. ``URITemplate`` is an RFC 6570 URI template atom. Concrete expanded URIs are handled by ``resource_read/3``.',
		argnames is ['ResourceTemplates']
	]).

	:- public(resource_read/3).
	:- mode(resource_read(+atom, +list(pair), --compound), one).
	:- info(resource_read/3, [
		comment is 'Handles a resource read request. ``URI`` is the resource identifier (as declared in ``resources/1``), ``Arguments`` is a list of ``ArgumentName-Value`` pairs (currently unused but reserved for future use), and ``Result`` is unified with the resource result. The result must be ``contents(ContentList)`` where each content item is either ``text_content(URI, MimeType, Text)`` for text resources or ``blob_content(URI, MimeType, Base64Data)`` for binary resources encoded as base64. ``Text`` and ``Base64Data`` must be atoms.',
		argnames is ['URI', 'Arguments', 'Result']
	]).

:- end_protocol.
