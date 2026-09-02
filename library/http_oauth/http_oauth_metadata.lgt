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


:- object(http_oauth_metadata,
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-01,
		comment is 'OAuth protected-resource metadata document and HTTP response builder following RFC 9728.'
	]).

	:- public(well_known_url/2).
	:- mode(well_known_url(+atom, -atom), one_or_error).
	:- info(well_known_url/2, [
		comment is 'Returns the RFC 9728 well-known metadata URL for a canonical protected-resource identifier.',
		argnames is ['ProtectedResource', 'URL'],
		exceptions is [
			'``ProtectedResource`` is not a canonical HTTPS protected-resource identifier' - domain_error(http_oauth_protected_resource, 'ProtectedResource')
		]
	]).

	:- public(document/3).
	:- mode(document(+atom, +list(compound), -term), one_or_error).
	:- info(document/3, [
		comment is 'Builds a protected-resource metadata JSON object from descriptor terms.',
		argnames is ['ProtectedResource', 'Descriptors', 'Document'],
		exceptions is [
			'``ProtectedResource`` is not a canonical HTTPS protected-resource identifier' - domain_error(http_oauth_protected_resource, 'ProtectedResource'),
			'``Descriptors`` is not a proper list' - type_error(list, 'Descriptors'),
			'An element ``Descriptor`` of the list ``Descriptors`` is not a valid metadata descriptor' - domain_error(http_oauth_metadata_descriptor, 'Descriptor'),
			'``Descriptors`` contains duplicate metadata members' - domain_error(http_oauth_metadata_descriptors, 'Descriptors')
		]
	]).

	:- public(document/4).
	:- mode(document(+atom, +list(compound), -term, +list(compound)), one_or_error).
	:- info(document/4, [
		comment is 'Builds a protected-resource metadata JSON object subject to profile options.',
		argnames is ['ProtectedResource', 'Descriptors', 'Document', 'Options'],
		exceptions is [
			'``ProtectedResource`` is not a canonical HTTPS protected-resource identifier' - domain_error(http_oauth_protected_resource, 'ProtectedResource'),
			'``Descriptors`` is not a proper list' - type_error(list, 'Descriptors'),
			'An element ``Descriptor`` of the list ``Descriptors`` is not a valid metadata descriptor' - domain_error(http_oauth_metadata_descriptor, 'Descriptor'),
			'``Descriptors`` contains duplicate metadata members' - domain_error(http_oauth_metadata_descriptors, 'Descriptors'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Descriptors`` omits a required metadata member or gives it an empty value' - domain_error(http_oauth_metadata_descriptors, 'Descriptors')
		]
	]).

	:- public(response/3).
	:- mode(response(+atom, +list(compound), -compound), one_or_error).
	:- info(response/3, [
		comment is 'Builds a normalized HTTP 200 response containing protected-resource metadata.',
		argnames is ['ProtectedResource', 'Descriptors', 'Response'],
		exceptions is [
			'``ProtectedResource`` is not a canonical HTTPS protected-resource identifier' - domain_error(http_oauth_protected_resource, 'ProtectedResource'),
			'``Descriptors`` is not a proper list' - type_error(list, 'Descriptors'),
			'An element ``Descriptor`` of the list ``Descriptors`` is not a valid metadata descriptor' - domain_error(http_oauth_metadata_descriptor, 'Descriptor'),
			'``Descriptors`` contains duplicate metadata members' - domain_error(http_oauth_metadata_descriptors, 'Descriptors')
		]
	]).

	:- public(response/4).
	:- mode(response(+atom, +list(compound), -compound, +list(compound)), one_or_error).
	:- info(response/4, [
		comment is 'Builds a normalized HTTP 200 metadata response subject to profile options.',
		argnames is ['ProtectedResource', 'Descriptors', 'Response', 'Options'],
		exceptions is [
			'``ProtectedResource`` is not a canonical HTTPS protected-resource identifier' - domain_error(http_oauth_protected_resource, 'ProtectedResource'),
			'``Descriptors`` is not a proper list' - type_error(list, 'Descriptors'),
			'An element ``Descriptor`` of the list ``Descriptors`` is not a valid metadata descriptor' - domain_error(http_oauth_metadata_descriptor, 'Descriptor'),
			'``Descriptors`` contains duplicate metadata members' - domain_error(http_oauth_metadata_descriptors, 'Descriptors'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Descriptors`` omits a required metadata member or gives it an empty value' - domain_error(http_oauth_metadata_descriptors, 'Descriptors')
		]
	]).

	:- uses(list, [
		member/2, memberchk/2, valid/1 as proper_list/1
	]).

	:- uses(type, [
		valid/2
	]).

	well_known_url(ProtectedResource, URL) :-
		protected_resource_components(ProtectedResource, _Normalized, Scheme, Authority, Path),
		metadata_path(Path, MetadataPath),
		url(atom)::generate([scheme(Scheme), authority(Authority), path(MetadataPath)], URL).

	document(ProtectedResource, Descriptors, Document) :-
		document(ProtectedResource, Descriptors, Document, []).

	document(ProtectedResource, Descriptors, Document, Options) :-
		protected_resource_components(ProtectedResource, Normalized, _Scheme, _Authority, _Path),
		validate_descriptors(Descriptors, Pairs),
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		^^option(required_members(RequiredMembers), MergedOptions),
		ensure_required_members(RequiredMembers, Pairs, Descriptors),
		pairs_object([resource-Normalized| Pairs], Document).

	response(ProtectedResource, Descriptors, Response) :-
		response(ProtectedResource, Descriptors, Response, []).

	response(ProtectedResource, Descriptors, Response, Options) :-
		document(ProtectedResource, Descriptors, Document, Options),
		http_core::response(
			http(1, 1), status(200, 'OK'), [content_type-'application/json'],
			content('application/json', json(Document)), [], Response
		).

	valid_option(required_members(Members)) :-
		proper_list(Members),
		valid_required_members(Members).

	default_option(required_members([])).

	valid_required_members([]).
	valid_required_members([authorization_servers| Members]) :-
		valid_required_members(Members).

	protected_resource_components(ProtectedResource, Normalized, https, Authority, Path) :-
		atom(ProtectedResource),
		url(atom)::normalize(ProtectedResource, Normalized),
		url(atom)::parse(Normalized, Components),
		memberchk(scheme(https), Components),
		memberchk(authority(Authority), Components),
		memberchk(fragment(''), Components),
		(	member(path(Path), Components) ->
			true
		;	Path = ''
		),
		!.
	protected_resource_components(ProtectedResource, _Normalized, _Scheme, _Authority, _Path) :-
		domain_error(http_oauth_protected_resource, ProtectedResource).

	metadata_path('', '/.well-known/oauth-protected-resource') :-
		!.
	metadata_path('/', '/.well-known/oauth-protected-resource') :-
		!.
	metadata_path(Path, MetadataPath) :-
		atom_concat('/.well-known/oauth-protected-resource', Path, MetadataPath).

	validate_descriptors(Descriptors, Pairs) :-
		(	proper_list(Descriptors) ->
			validate_descriptor_list(Descriptors, [], Pairs, Descriptors)
		;	type_error(list, Descriptors)
		).

	validate_descriptor_list([], _Seen, [], _AllDescriptors).
	validate_descriptor_list([Descriptor| Descriptors], Seen, [Name-JSONValue| Pairs], AllDescriptors) :-
		descriptor_pair(Descriptor, Name, JSONValue),
		(	member(Name, Seen) ->
			domain_error(http_oauth_metadata_descriptors, AllDescriptors)
		;	validate_descriptor_list(Descriptors, [Name| Seen], Pairs, AllDescriptors)
		).

	descriptor_pair(Descriptor, Name, JSONValue) :-
		valid_descriptor_pair(Descriptor, Name, JSONValue),
		!.
	descriptor_pair(Descriptor, _Name, _Value) :-
		domain_error(http_oauth_metadata_descriptor, Descriptor).

	valid_descriptor_pair(authorization_servers(Servers), authorization_servers, Servers) :-
		valid_https_urls(Servers).
	valid_descriptor_pair(jwks_uri(URL), jwks_uri, URL) :-
		valid_https_url(URL).
	valid_descriptor_pair(scopes_supported(Scopes), scopes_supported, Scopes) :-
		valid(list(atom), Scopes).
	valid_descriptor_pair(bearer_methods_supported(Methods), bearer_methods_supported, Methods) :-
		valid(list(atom), Methods).
	valid_descriptor_pair(resource_signing_alg_values_supported(Algorithms), resource_signing_alg_values_supported, Algorithms) :-
		valid(list(atom), Algorithms).
	valid_descriptor_pair(resource_name(Name), resource_name, Name) :-
		atom(Name).
	valid_descriptor_pair(resource_documentation(URL), resource_documentation, URL) :-
		valid_https_url(URL).
	valid_descriptor_pair(resource_policy_uri(URL), resource_policy_uri, URL) :-
		valid_https_url(URL).
	valid_descriptor_pair(resource_tos_uri(URL), resource_tos_uri, URL) :-
		valid_https_url(URL).
	valid_descriptor_pair(tls_client_certificate_bound_access_tokens(Boolean), tls_client_certificate_bound_access_tokens, JSONBoolean) :-
		json_boolean(Boolean, JSONBoolean).
	valid_descriptor_pair(authorization_details_types_supported(Types), authorization_details_types_supported, Types) :-
		valid(list(atom), Types).
	valid_descriptor_pair(dpop_signing_alg_values_supported(Algorithms), dpop_signing_alg_values_supported, Algorithms) :-
		valid(list(atom), Algorithms).
	valid_descriptor_pair(dpop_bound_access_tokens_required(Boolean), dpop_bound_access_tokens_required, JSONBoolean) :-
		json_boolean(Boolean, JSONBoolean).
	valid_descriptor_pair(signed_metadata(SignedMetadata), signed_metadata, SignedMetadata) :-
		atom(SignedMetadata).
	valid_descriptor_pair(extension(Name, Value), Name, Value) :-
		atom(Name),
		\+ registered_name(Name),
		nonvar(Value).

	valid_https_urls(Servers) :-
		proper_list(Servers),
		valid_https_urls_elements(Servers).

	valid_https_urls_elements([]).
	valid_https_urls_elements([Server| Servers]) :-
		valid_https_url(Server),
		valid_https_urls_elements(Servers).

	valid_https_url(URL) :-
		atom(URL),
		url(atom)::parse(URL, Components),
		memberchk(scheme(https), Components),
		memberchk(authority(_), Components),
		memberchk(fragment(''), Components).

	json_boolean(true, @true).
	json_boolean(false, @false).

	ensure_required_members([], _Pairs, _Descriptors).
	ensure_required_members([Name| Names], Pairs, Descriptors) :-
		(	memberchk(Name-Value, Pairs),
			required_member_value(Name, Value) ->
			ensure_required_members(Names, Pairs, Descriptors)
		;	domain_error(http_oauth_metadata_descriptors, Descriptors)
		).

	required_member_value(authorization_servers, [_| _]).

	pairs_object(Pairs, {Conjunction}) :-
		pairs_conjunction(Pairs, Conjunction).

	pairs_conjunction([Pair], Pair) :-
		!.
	pairs_conjunction([Pair| Pairs], (Pair, Conjunction)) :-
		pairs_conjunction(Pairs, Conjunction).

	registered_name(authorization_servers).
	registered_name(jwks_uri).
	registered_name(scopes_supported).
	registered_name(bearer_methods_supported).
	registered_name(resource_signing_alg_values_supported).
	registered_name(resource_name).
	registered_name(resource_documentation).
	registered_name(resource_policy_uri).
	registered_name(resource_tos_uri).
	registered_name(tls_client_certificate_bound_access_tokens).
	registered_name(authorization_details_types_supported).
	registered_name(dpop_signing_alg_values_supported).
	registered_name(dpop_bound_access_tokens_required).
	registered_name(signed_metadata).
	registered_name(resource).

:- end_object.
