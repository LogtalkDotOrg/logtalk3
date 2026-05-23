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


:- protocol(open_api_provider_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Protocol for Logtalk objects that provide metadata used to derive OpenAPI 3.1.0 documents.',
		remarks is [
			'API metadata' - 'The ``api_info/1`` predicate returns an ``info(Title, Version, Summary, Properties)`` descriptor for the top-level OpenAPI ``info`` object. Currently recognized metadata properties: ``description(Description)``.',
			'Server descriptors' - 'The ``servers/1`` predicate returns a list of ``server(URL, Description)`` descriptors used to build the top-level OpenAPI ``servers`` array. ``URL`` must be a valid OpenAPI Server Object URL string, allowing absolute URLs, relative references, and templated variables such as ``https://{username}.example.com:{port}/{basePath}``.',
			'Top-level security defaults' - 'The ``security/1`` predicate returns a list of top-level OpenAPI security requirement alternatives. The returned list uses the same ``Requirements`` representation accepted by operation ``security(Requirements)`` properties: each requirement alternative is a list of ``Scheme-Scopes`` pairs and ``[]`` denotes an empty security requirement object. Scheme names must match declarations exposed by ``security_scheme/2``.',
			'Operation descriptors' - 'The ``operations/1`` predicate returns a list of ``operation(Id, Method, Path, Summary, Parameters, RequestBody, Responses, Properties)`` descriptors. The ``Id`` value is used as the OpenAPI ``operationId`` and must be unique across the returned descriptor list. Nested descriptors are ``parameter(Name, In, Description, Required, Schema)``, ``request_body(Description, Required, MediaTypes)``, ``response(Status, Description, MediaTypes)``, and ``media(MediaType, Schema)``. ``Status`` may be an integer HTTP status code, ``default``, or a wildcard range atom such as ``''2XX''``. Path parameters must match a template expression in ``Path`` and must use ``Required = true``. Currently recognized operation properties are ``description(Description)``, ``tags(Tags)``, ``deprecated(Boolean)``, and ``security(Requirements)`` where ``Requirements`` is a list of requirement alternatives represented as lists of ``Scheme-Scopes`` pairs and ``[]`` denotes an empty security requirement object. Scheme names referenced from ``security(Requirements)`` must match names exposed by ``security_scheme/2`` and operation-level security overrides any top-level ``security/1`` declaration.',
			'Reusable schemas' - 'The ``schema/2`` predicate maps reusable schema names to JSON Schema curly terms compatible with the ``json_schema`` library. References to reusable schemas are expressed using ``schema_ref(Name)`` terms.',
			'Reusable security schemes' - 'The ``security_scheme/2`` predicate maps reusable security scheme names to portable descriptor terms. Supported first-pass descriptors are ``api_key(In, Name)`` or ``api_key(In, Name, Options)``, ``http(Scheme)`` or ``http(Scheme, Options)``, ``mutual_tls`` or ``mutual_tls(Options)``, ``oauth2(Flows)`` or ``oauth2(Flows, Options)``, and ``openid_connect(URL)`` or ``openid_connect(URL, Options)``. ``Options`` currently support ``description(Description)`` for all scheme kinds, ``bearer_format(Format)`` for ``http`` schemes, and ``flows(Flows)`` for ``openid_connect/2`` when local scope declarations are needed. OAuth flow descriptors use ``implicit/2-3``, ``password/2-3``, ``client_credentials/2-3``, and ``authorization_code/3-4`` with ``scope(Name, Description)`` terms and optional ``refresh_url(URL)`` properties. Legacy OpenAPI Security Scheme Object curly terms are still accepted during transition. Provider-side validation checks fixed fields, enumerated ``in`` values, and URL-valued fields such as ``authorizationUrl``, ``tokenUrl``, ``refreshUrl``, and ``openIdConnectUrl``.'
		]
	]).

	:- public(api_info/1).
	:- mode(api_info(-compound), one).
	:- info(api_info/1, [
		comment is 'Returns the API metadata descriptor. The descriptor must be the compound term ``info(Title, Version, Summary, Properties)`` where ``Title``, ``Version``, and ``Summary`` are atoms and ``Properties`` is a list of optional metadata terms. Currently recognized properties: ``description(Description)`` where ``Description`` is an atom.',
		argnames is ['Info']
	]).

	:- public(servers/1).
	:- mode(servers(-list(compound)), one).
	:- info(servers/1, [
		comment is 'Returns a list of server descriptors available for this API. Each descriptor must be a compound term ``server(URL, Description)`` where ``URL`` is a valid OpenAPI Server Object URL atom, allowing absolute URLs, relative references, and templated variables, and ``Description`` is a human-readable atom.',
		argnames is ['Servers']
	]).

	:- public(security/1).
	:- mode(security(-list(compound)), zero_or_one).
	:- info(security/1, [
		comment is 'Returns the top-level OpenAPI security requirement alternatives for this API. The returned list uses the same representation as operation ``security(Requirements)`` properties: each element is a requirement alternative represented as a list of ``Scheme-Scopes`` pairs and ``[]`` denotes an empty security requirement object. Scheme names must be declared by ``security_scheme/2``.',
		argnames is ['Security']
	]).

	:- public(operations/1).
	:- mode(operations(-list(compound)), one).
	:- info(operations/1, [
		comment is 'Returns a list of operation descriptors exposed by this API. Each descriptor must be a compound term ``operation(Id, Method, Path, Summary, Parameters, RequestBody, Responses, Properties)`` where ``Id`` is an atom and unique across the returned descriptors, ``Method`` is a lowercase HTTP method atom, ``Path`` is a path-template atom, ``Summary`` is an atom, ``Parameters`` is a list of ``parameter(Name, In, Description, Required, Schema)`` terms, ``RequestBody`` is either ``none`` or ``request_body(Description, Required, MediaTypes)``, ``Responses`` is a list of ``response(Status, Description, MediaTypes)`` terms, and ``Properties`` is a list of optional operation property terms. ``Status`` may be an integer HTTP status code, ``default``, or a wildcard range atom such as ``''2XX''``. Path parameters must use ``In = path``, match a template expression in ``Path``, and use ``Required = true``. Currently recognized property terms are ``description(Description)``, ``tags(Tags)``, ``deprecated(Boolean)``, and ``security(Requirements)`` where ``Requirements`` is a list whose elements are requirement alternatives represented as lists of ``Scheme-Scopes`` pairs; the empty list element ``[]`` denotes an empty security requirement object, scheme names must be declared by ``security_scheme/2``, and referenced OAuth flow scopes must be declared by the corresponding security scheme when locally available.',
		argnames is ['Operations']
	]).

	:- public(schema/2).
	:- mode(schema(?atom, ?compound), zero_or_more).
	:- info(schema/2, [
		comment is 'Maps reusable schema names to JSON Schema terms. ``Name`` must be an atom and ``Schema`` must be a JSON Schema curly term compatible with the ``json_schema`` library. This predicate is intended both for lookup by name and for enumeration of all reusable schemas referenced from ``schema_ref(Name)`` terms.',
		argnames is ['Name', 'Schema']
	]).

	:- public(security_scheme/2).
	:- mode(security_scheme(?atom, ?compound), zero_or_more).
	:- info(security_scheme/2, [
		comment is 'Maps reusable security scheme names to security descriptor terms. ``Name`` must be an atom matching scheme names referenced from operation ``security(Requirements)`` properties and ``Scheme`` must use one of the supported descriptor shapes: ``api_key/2-3``, ``http/1-2``, ``mutual_tls/0-1``, ``oauth2/1-2``, or ``openid_connect/1-2``. OAuth flow descriptors are expressed using ``implicit/2-3``, ``password/2-3``, ``client_credentials/2-3``, and ``authorization_code/3-4`` with ``scope/2`` terms. During transition, legacy OpenAPI Security Scheme Object curly terms are still accepted. Provider-side validation checks required fields, valid ``apiKey`` locations, and URL-valued fields in OAuth or OpenID Connect descriptors.',
		argnames is ['Name', 'Scheme']
	]).

:- end_protocol.
