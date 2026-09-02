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


:- object(http_oauth_introspection_client,
	imports([options, http_oauth_helpers])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-01,
		comment is 'OAuth token introspection HTTP client following RFC 7662.'
	]).

	:- public(introspect/4).
	:- mode(introspect(+atom, +atom, -compound, +list(compound)), one_or_error).
	:- info(introspect/4, [
		comment is 'Introspects an access token using an authenticated form POST and returns normalized introspection data.',
		argnames is ['Endpoint', 'Token', 'Introspection', 'Options'],
		exceptions is [
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` does not include a client authentication option' - domain_error(http_oauth_introspection_options, 'Options'),
			'``Endpoint`` is not an HTTPS URL or an explicitly allowed loopback HTTP URL' - domain_error(http_oauth_introspection_endpoint, 'Endpoint'),
			'``Token`` is not a valid RFC 6750 Bearer token' - domain_error(http_oauth_introspection_token, 'Token'),
			'The Basic client authentication username is invalid' - domain_error(http_authenticate_value(username), 'ClientId'),
			'The introspection endpoint returned a non-success status code' - domain_error(http_oauth_introspection_status, 'Code'),
			'The introspection response body is not JSON content' - domain_error(http_oauth_introspection_response, 'Body'),
			'The introspection response JSON value is not an object' - domain_error(http_oauth_introspection_response, 'JSON'),
			'The introspection response has invalid ``active`` member values ``Values``' - domain_error(http_oauth_introspection_active, 'Values'),
			'The introspection response has duplicate values ``Values`` for optional member ``Name``' - domain_error(http_oauth_introspection_member('Name'), 'Values'),
			'The introspection response has invalid ``scope`` member value ``Value``' - domain_error(http_oauth_scope, 'Value'),
			'An option passed using ``http_options/1`` is not a valid HTTP client request option' - domain_error(http_client_request_option, 'Option'),
			'The delegated HTTP client rejects the response stream' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- uses(http_core, [
		body/2, generate_headers/2, status/2
	]).

	:- uses(list, [
		append/3, member/2, memberchk/2, valid/1 as proper_list/1
	]).

	introspect(Endpoint, Token, Introspection, Options) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		(	^^option(authentication(Authentication), MergedOptions) ->
			true
		;	domain_error(http_oauth_introspection_options, Options)
		),
		^^option(token_type_hint(TokenTypeHint), MergedOptions),
		^^option(headers(CustomHeaders), MergedOptions),
		^^option(http_options(HTTPOptions0), MergedOptions),
		^^option(http_client(Client), MergedOptions),
		^^option(allow_insecure_localhost(AllowLocalhost), MergedOptions),
		ensure_endpoint(Endpoint, AllowLocalhost),
		(	^^valid_bearer_token(Token) ->
			true
		;	domain_error(http_oauth_introspection_token, Token)
		),
		authentication_headers(Authentication, AuthenticationHeaders),
		merge_headers(AuthenticationHeaders, CustomHeaders, Headers),
		parameters(Token, TokenTypeHint, Parameters),
		HTTPOptions = [headers([accept-'application/json'| Headers])| HTTPOptions0],
		Body = content('application/x-www-form-urlencoded', form(Parameters)),
		Client::post(Endpoint, Body, Response, HTTPOptions),
		response_introspection(Response, Introspection).

	default_option(token_type_hint(none)).
	default_option(headers([])).
	default_option(http_options([])).
	default_option(http_client(http_client)).
	default_option(allow_insecure_localhost(false)).

	valid_option(authentication(basic(ClientId, Secret))) :-
		atom(ClientId),
		atom(Secret).
	valid_option(authentication(bearer(Token))) :-
		^^valid_bearer_token(Token).
	valid_option(authentication(headers(Headers))) :-
		valid_headers(Headers),
		\+ member(authorization-_, Headers).
	valid_option(token_type_hint(TokenTypeHint)) :-
		% help document the 'none' special value
		(	TokenTypeHint == none ->
			true
		;	atom(TokenTypeHint)
		).
	valid_option(headers(Headers)) :-
		valid_headers(Headers),
		\+ member(authorization-_, Headers),
		\+ member(accept-_, Headers),
		\+ member(content_type-_, Headers).
	valid_option(http_options(Options)) :-
		proper_list(Options),
		\+ member(headers(_), Options),
		\+ member(body(_), Options).
	valid_option(http_client(Client)) :-
		nonvar(Client).
	valid_option(allow_insecure_localhost(Boolean)) :-
		once((Boolean == true; Boolean == false)).

	authentication_headers(basic(ClientId, Secret), [authorization-HeaderValue]) :-
		http_authenticate::generate_authorization(basic_authorization([username(ClientId), password(Secret)]), HeaderValue).
	authentication_headers(bearer(Token), [authorization-HeaderValue]) :-
		http_oauth::generate_authorization(bearer_authorization(Token), HeaderValue).
	authentication_headers(headers(Headers), Headers).

	parameters(Token, none, [token-Token]) :-
		!.
	parameters(Token, TokenTypeHint, [token-Token, token_type_hint-TokenTypeHint]).

	merge_headers(AuthenticationHeaders, CustomHeaders, Headers) :-
		append(AuthenticationHeaders, CustomHeaders, Headers).

	response_introspection(Response, oauth_introspection(Properties)) :-
		status(Response, status(Code, _)),
		(	Code >= 200, Code =< 299 ->
			true
		;	domain_error(http_oauth_introspection_status, Code)
		),
		body(Response, Body),
		(	Body = content(_, json(JSON)) ->
			true
		;	domain_error(http_oauth_introspection_response, Body)
		),
		json_pairs(JSON, Pairs),
		required_active(Pairs, Active),
		optional_properties([scope, client_id, username, token_type, exp, iat, nbf, sub, aud, iss, jti], Pairs, OptionalProperties),
		Properties = [active(Active)| OptionalPropertiesWithRaw],
		append(OptionalProperties, [raw(JSON)], OptionalPropertiesWithRaw).

	required_active(Pairs, Active) :-
		findall(Value, member(active-Value, Pairs), Values),
		(	Values == [@true] ->
			Active = true
		;	Values == [@false] ->
			Active = false
		;	domain_error(http_oauth_introspection_active, Values)
		).

	optional_properties([], _Pairs, []).
	optional_properties([Name| Names], Pairs, Properties) :-
		findall(Value, member(Name-Value, Pairs), Values),
		(	Values == [] ->
			Properties = Tail
		;	Values = [Value] ->
			property_value(Name, Value, Property),
			Properties = [Property| Tail]
		;	domain_error(http_oauth_introspection_member(Name), Values)
		),
		optional_properties(Names, Pairs, Tail).

	property_value(scope, Value, scopes(Scopes)) :-
		!,
		(	atom(Value),
			catch(^^parse_scope_value(Value, Scopes), _, fail) ->
			true
		;	domain_error(http_oauth_scope, Value)
		).
	property_value(Name, Value, Property) :-
		Property =.. [Name, Value].

	json_pairs({}, []) :-
		!.
	json_pairs({Pairs}, List) :-
		!,
		curly_pairs(Pairs, List).
	json_pairs(JSON, _Pairs) :-
		domain_error(http_oauth_introspection_response, JSON).

	curly_pairs((Pair, Pairs), [Pair| List]) :-
		!,
		curly_pairs(Pairs, List).
	curly_pairs(Pair, [Pair]).

	ensure_endpoint(Endpoint, AllowLocalhost) :-
		(	secure_endpoint(Endpoint) ->
			true
		;	AllowLocalhost == true,
			localhost_endpoint(Endpoint) ->
			true
		;	domain_error(http_oauth_introspection_endpoint, Endpoint)
		).

	secure_endpoint(Endpoint) :-
		url(atom)::parse(Endpoint, Components),
		memberchk(scheme(https), Components),
		memberchk(authority(_), Components),
		memberchk(fragment(''), Components).

	localhost_endpoint(Endpoint) :-
		url(atom)::parse(Endpoint, Components),
		memberchk(scheme(http), Components),
		memberchk(authority(Authority), Components),
		memberchk(fragment(''), Components),
		(	Authority == localhost
		;	sub_atom(Authority, 0, _, _, 'localhost:')
		;	Authority == '127.0.0.1'
		;	sub_atom(Authority, 0, _, _, '127.0.0.1:')
		;	Authority == '[::1]'
		;	sub_atom(Authority, 0, _, _, '[::1]:')
		).

	valid_headers(Headers) :-
		catch(generate_headers(codes(_), Headers), _, fail).

:- end_object.
