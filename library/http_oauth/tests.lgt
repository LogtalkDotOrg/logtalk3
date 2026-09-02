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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-01,
		comment is 'Unit tests for the "http_oauth" library.'
	]).

	cover(http_oauth_exact_scope_checker).
	cover(http_oauth).
	cover(http_oauth_metadata).
	cover(http_server_core_oauth_handler(_, _, _)).
	cover(http_server_core_oauth_endpoint_handler(_, _, _, _, _, _)).
	cover(http_oauth_introspection_client).
	cover(http_oauth_introspection_verifier(_, _)).
	cover(http_oauth_test_introspection_client).
	:- if(current_prolog_flag(bounded, false)).
		cover(http_oauth_jwt_verifier(_, _)).
	:- endif.
	cover(http_oauth_test_router).
	cover(http_oauth_test_verifier).

	:- uses(http_core, [
		body/2, header/3, property/2, status/2
	]).

	:- uses(list, [
		memberchk/2
	]).

	test(http_oauth_exact_scope_checker_2_01, deterministic) :-
		http_oauth_exact_scope_checker::sufficient([], []).

	test(http_oauth_exact_scope_checker_2_02, deterministic) :-
		http_oauth_exact_scope_checker::sufficient([read, write, admin], [read, write]).

	test(http_oauth_exact_scope_checker_2_03, deterministic) :-
		\+ http_oauth_exact_scope_checker::sufficient([read], [read, write]).

	test(http_oauth_exact_scope_checker_2_04, deterministic) :-
		\+ http_oauth_exact_scope_checker::sufficient(['Read'], [read]).

	test(http_oauth_authorization_round_trip_01, deterministic(Parsed == Authorization)) :-
		Authorization = bearer_authorization('abc-._~+/=='),
		http_oauth::generate_authorization(Authorization, HeaderValue),
		http_oauth::parse_authorization(HeaderValue, Parsed).

	test(http_oauth_challenge_round_trip_01, deterministic(Parsed == Challenge)) :-
		Challenge = bearer_challenge([
			realm('example'),
			scope([read, write]),
			error(invalid_token),
			error_description('quoted "detail"'),
			error_uri('https://example.com/errors/invalid_token'),
			resource_metadata('https://api.example.com/.well-known/oauth-protected-resource/mcp'),
			extension(example, 'one,two')
		]),
		http_oauth::generate_challenge(Challenge, HeaderValue),
		http_oauth::parse_challenge(HeaderValue, Parsed).

	test(http_oauth_authenticate_request_4_01, deterministic) :-
		request_with_token('valid-token', [original_property(kept)], Request),
		http_oauth::authenticate_request(Request, http_oauth_test_verifier, continue(Authenticated), [
			protected_resource('https://api.example.com/mcp')
		]),
		http_oauth::token_info(Authenticated, oauth_token_info(TokenProperties)),
		memberchk(scopes([read, write]), TokenProperties),
		http_oauth::scopes(Authenticated, [read, write]),
		property(Authenticated, original_property(kept)),
		\+ property(Authenticated, bearer_authorization(_)).

	test(http_oauth_authenticate_request_4_02, deterministic) :-
		Request = request(get, origin('/mcp'), http(1, 1), [], empty, []),
		http_oauth::authenticate_request(Request, http_oauth_test_verifier, respond(Response), [
			protected_resource('https://api.example.com/mcp'),
			resource_metadata('https://api.example.com/.well-known/oauth-protected-resource/mcp')
		]),
		status(Response, status(401, 'Unauthorized')),
		http_oauth::challenge(Response, bearer_challenge([
			resource_metadata('https://api.example.com/.well-known/oauth-protected-resource/mcp')
		])).

	test(http_oauth_authenticate_request_4_03, deterministic) :-
		request_with_token('other-token', [], Request),
		http_oauth::authenticate_request(Request, http_oauth_test_verifier, respond(Response), [
			protected_resource('https://api.example.com/mcp')
		]),
		status(Response, status(401, 'Unauthorized')),
		http_oauth::challenge(Response, bearer_challenge([error(invalid_token)])).

	test(http_oauth_authenticate_request_4_04, deterministic) :-
		Request = request(get, origin('/mcp'), http(1, 1), [authorization-'Bearer invalid$token'], empty, []),
		http_oauth::authenticate_request(Request, http_oauth_test_verifier, respond(Response), [
			protected_resource('https://api.example.com/mcp')
		]),
		status(Response, status(400, 'Bad Request')),
		http_oauth::challenge(Response, bearer_challenge([error(invalid_request)])).

	test(http_oauth_authenticate_request_4_05, deterministic) :-
		request_with_token('valid-token', [], Request),
		http_oauth::authenticate_request(Request, http_oauth_test_verifier, respond(Response), [
			protected_resource('https://api.example.com/other')
		]),
		status(Response, status(401, 'Unauthorized')).

	test(http_oauth_authenticate_request_4_06, deterministic) :-
		Request = request(get, origin('/mcp'), http(1, 1), [authorization-'Basic YWJjOmRlZg=='], empty, []),
		http_oauth::authenticate_request(Request, http_oauth_test_verifier, respond(Response), [
			protected_resource('https://api.example.com/mcp')
		]),
		status(Response, status(400, 'Bad Request')).

	test(http_oauth_authenticate_request_4_07, deterministic) :-
		Request = request(get, origin('/mcp'), http(1, 1), [authorization-'Bearer valid-token', authorization-'Bearer other-token'], empty, []),
		http_oauth::authenticate_request(Request, http_oauth_test_verifier, respond(Response), [
			protected_resource('https://api.example.com/mcp')
		]),
		status(Response, status(400, 'Bad Request')).

	test(http_oauth_authorize_request_3_01, deterministic) :-
		authenticated_request(Authenticated),
		http_oauth::authorize_request(Authenticated, continue(Authenticated), [required_scopes([read])]).

	test(http_oauth_authorize_request_3_02, deterministic) :-
		authenticated_request(Authenticated),
		http_oauth::authorize_request(Authenticated, respond(Response), [required_scopes([admin])]),
		status(Response, status(403, 'Forbidden')),
		http_oauth::challenge(Response, bearer_challenge([scope([admin]), error(insufficient_scope)])).

	test(http_oauth_protect_request_4_01, deterministic) :-
		request_with_token('valid-token', [], Request),
		http_oauth::protect_request(Request, http_oauth_test_verifier, continue(Protected), [
			protected_resource('https://api.example.com/mcp'),
			required_scopes([write])
		]),
		http_oauth::scopes(Protected, [read, write]).

	test(http_oauth_unauthorized_response_3_01, deterministic) :-
		http_oauth::unauthorized_response(Challenge, Response, [realm('example')]),
		Challenge == bearer_challenge([realm('example')]),
		status(Response, status(401, 'Unauthorized')),
		header(Response, www_authenticate, 'Bearer realm="example"').

	test(http_oauth_forbidden_response_3_01, deterministic) :-
		http_oauth::forbidden_response(Challenge, Response, [required_scopes([admin])]),
		Challenge == bearer_challenge([scope([admin]), error(insufficient_scope)]),
		status(Response, status(403, 'Forbidden')).

	test(http_oauth_unauthorized_response_4_01, deterministic) :-
		http_core::response(
			http(1, 0), status(200, 'OK'), [content_type-'text/plain', x_test-base],
			content('text/plain', text(base)), [etag(base), cache_control([private])], Response0
		),
		http_oauth::unauthorized_response(bearer_challenge([error(invalid_token)]), Response0, Response, [
			headers([content_type-'application/json', x_test-overlay]),
			body(content('application/json', json({error-invalid_token}))),
			properties([etag(overlay), connection([close])])
		]),
		status(Response, status(401, 'Unauthorized')),
		body(Response, content('application/json', json({error-invalid_token}))),
		header(Response, content_type, 'application/json'),
		header(Response, x_test, overlay),
		\+ header(Response, x_test, base),
		property(Response, etag(overlay)),
		property(Response, connection([close])),
		property(Response, cache_control([private])),
		\+ property(Response, etag(base)),
		http_oauth::challenge(Response, bearer_challenge([error(invalid_token)])).

	test(http_oauth_forbidden_response_4_01, deterministic) :-
		http_core::response(
			http(2, 0), status(200, 'OK'), [], content('text/plain', text(kept)), [], Response0
		),
		Challenge = bearer_challenge([scope([admin]), error(insufficient_scope)]),
		http_oauth::forbidden_response(Challenge, Response0, Response, []),
		status(Response, status(403, 'Forbidden')),
		body(Response, content('text/plain', text(kept))),
		http_oauth::challenge(Response, Challenge).

	test(http_oauth_parse_authorization_2_01, error(domain_error(http_oauth_bearer_token, 'invalid$token'))) :-
		http_oauth::parse_authorization('Bearer invalid$token', _).

	test(http_oauth_generate_challenge_2_01, error(domain_error(http_oauth_term(challenge), bearer_challenge([realm(one), realm(two)])))) :-
		http_oauth::generate_challenge(bearer_challenge([realm(one), realm(two)]), _).

	test(http_oauth_parse_challenge_2_01, error(domain_error(http_oauth_error, unknown_error))) :-
		http_oauth::parse_challenge('Bearer error="unknown_error"', _).

	test(http_oauth_parse_challenge_2_02, error(domain_error(http_oauth_challenge_parameters, 'Bearer realm="one", realm="two"'))) :-
		http_oauth::parse_challenge('Bearer realm="one", realm="two"', _).

	test(http_oauth_metadata_well_known_url_2_01, deterministic(URL == 'https://api.example.com/.well-known/oauth-protected-resource')) :-
		http_oauth_metadata::well_known_url('https://api.example.com', URL).

	test(http_oauth_metadata_well_known_url_2_02, deterministic(URL == 'https://api.example.com/.well-known/oauth-protected-resource/mcp')) :-
		http_oauth_metadata::well_known_url('https://api.example.com/mcp', URL).

	test(http_oauth_metadata_well_known_url_2_03, error(domain_error(http_oauth_protected_resource, 'http://api.example.com/mcp'))) :-
		http_oauth_metadata::well_known_url('http://api.example.com/mcp', _).

	test(http_oauth_metadata_document_4_01, deterministic(Document == {
		resource-'https://api.example.com/mcp',
		authorization_servers-['https://issuer.example.com'],
		scopes_supported-[read, write],
		tls_client_certificate_bound_access_tokens- @false,
		example_extension-'value'
	})) :-
		http_oauth_metadata::document('https://api.example.com/mcp', [
			authorization_servers(['https://issuer.example.com']),
			scopes_supported([read, write]),
			tls_client_certificate_bound_access_tokens(false),
			extension(example_extension, 'value')
		], Document, [required_members([authorization_servers])]).

	test(http_oauth_metadata_document_4_02, error(domain_error(http_oauth_metadata_descriptors, []))) :-
		http_oauth_metadata::document('https://api.example.com/mcp', [], _Document, [required_members([authorization_servers])]).

	test(http_oauth_metadata_document_4_03, error(domain_error(http_oauth_metadata_descriptor, extension(resource, spoofed)))) :-
		http_oauth_metadata::document('https://api.example.com/mcp', [extension(resource, spoofed)], _Document, []).

	test(http_oauth_metadata_document_4_04, error(domain_error(http_oauth_metadata_descriptor, jwks_uri('http://api.example.com/jwks.json')))) :-
		http_oauth_metadata::document('https://api.example.com/mcp', [jwks_uri('http://api.example.com/jwks.json')], _Document, []).

	test(http_oauth_metadata_document_3_01, deterministic(Document == {
		resource-'https://api.example.com/mcp',
		authorization_servers-['https://issuer.example.com'],
		jwks_uri-'https://api.example.com/jwks.json',
		bearer_methods_supported-[header],
		resource_signing_alg_values_supported-['RS256'],
		resource_name-'Example API',
		resource_documentation-'https://api.example.com/docs',
		resource_policy_uri-'https://api.example.com/policy',
		resource_tos_uri-'https://api.example.com/terms',
		authorization_details_types_supported-[payment],
		dpop_signing_alg_values_supported-['ES256'],
		dpop_bound_access_tokens_required- @true,
		signed_metadata-'signed-metadata'
	})) :-
		http_oauth_metadata::document('https://api.example.com/mcp', [
			authorization_servers(['https://issuer.example.com']),
			jwks_uri('https://api.example.com/jwks.json'),
			bearer_methods_supported([header]),
			resource_signing_alg_values_supported(['RS256']),
			resource_name('Example API'),
			resource_documentation('https://api.example.com/docs'),
			resource_policy_uri('https://api.example.com/policy'),
			resource_tos_uri('https://api.example.com/terms'),
			authorization_details_types_supported([payment]),
			dpop_signing_alg_values_supported(['ES256']),
			dpop_bound_access_tokens_required(true),
			signed_metadata('signed-metadata')
		], Document).

	test(http_oauth_metadata_response_3_01, deterministic) :-
		http_oauth_metadata::response('https://api.example.com/mcp', [authorization_servers(['https://issuer.example.com'])], Response),
		status(Response, status(200, 'OK')),
		header(Response, content_type, 'application/json').

	test(http_oauth_endpoint_handler_2_01, deterministic) :-
		Request = request(get, origin('/.well-known/oauth-protected-resource/mcp'), http(1, 1), [], empty, []),
		http_server_core_oauth_endpoint_handler(
			'https://api.example.com/mcp',
			[authorization_servers(['https://issuer.example.com'])],
			[required_members([authorization_servers])],
			http_oauth_test_verifier,
			http_oauth_test_handler,
			[protected_resource('https://api.example.com/mcp'), required_scopes([read])]
		)::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({resource-'https://api.example.com/mcp', authorization_servers-['https://issuer.example.com']}))).

	test(http_oauth_endpoint_handler_2_02, deterministic) :-
		Request = request(post, origin('/.well-known/oauth-protected-resource/mcp'), http(1, 1), [], empty, []),
		http_server_core_oauth_endpoint_handler(
			'https://api.example.com/mcp',
			[authorization_servers(['https://issuer.example.com'])],
			[required_members([authorization_servers])],
			http_oauth_test_verifier,
			http_oauth_test_handler,
			[protected_resource('https://api.example.com/mcp'), required_scopes([read])]
		)::handle(Request, Response),
		status(Response, status(405, 'Method Not Allowed')),
		header(Response, allow, 'GET').

	test(http_oauth_endpoint_handler_2_03, deterministic) :-
		Request = request(get, origin('/.well-known/oauth-protected-resource/mcp/near'), http(1, 1), [], empty, []),
		http_server_core_oauth_endpoint_handler(
			'https://api.example.com/mcp',
			[authorization_servers(['https://issuer.example.com'])],
			[required_members([authorization_servers])],
			http_oauth_test_verifier,
			http_oauth_test_handler,
			[protected_resource('https://api.example.com/mcp'), required_scopes([read])]
		)::handle(Request, Response),
		status(Response, status(401, 'Unauthorized')).

	test(http_oauth_endpoint_handler_2_04, deterministic) :-
		request_with_token('valid-token', [original_property(kept)], Request),
		http_server_core_oauth_endpoint_handler(
			'https://api.example.com/mcp',
			[authorization_servers(['https://issuer.example.com'])],
			[required_members([authorization_servers])],
			http_oauth_test_verifier,
			http_oauth_test_handler,
			[protected_resource('https://api.example.com/mcp'), required_scopes([read])]
		)::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({scopes-[read, write]}))).

	test(http_oauth_introspection_client_4_01, deterministic) :-
		http_oauth_introspection_client::introspect(
			'https://issuer.example.com/basic', 'active-token', oauth_introspection(Properties),
			[authentication(basic(client, secret)), http_client(http_oauth_test_introspection_client)]
		),
		memberchk(active(true), Properties),
		memberchk(scopes([read, write]), Properties),
		memberchk(aud(['https://api.example.com/mcp']), Properties).

	test(http_oauth_introspection_client_4_02, deterministic) :-
		http_oauth_introspection_client::introspect(
			'https://issuer.example.com/bearer', 'inactive-token', oauth_introspection(Properties),
			[authentication(bearer('client-token')), http_client(http_oauth_test_introspection_client)]
		),
		memberchk(active(false), Properties).

	test(http_oauth_introspection_client_4_03, deterministic) :-
		http_oauth_introspection_client::introspect(
			'https://issuer.example.com/headers', 'active-token', oauth_introspection(Properties),
			[authentication(headers([x_client_secret-secret])), http_client(http_oauth_test_introspection_client)]
		),
		memberchk(active(true), Properties).

	test(http_oauth_introspection_client_4_04, error(domain_error(http_oauth_introspection_options, [http_client(http_oauth_test_introspection_client)]))) :-
		http_oauth_introspection_client::introspect(
			'https://issuer.example.com/basic', 'active-token', _,
			[http_client(http_oauth_test_introspection_client)]
		).

	test(http_oauth_introspection_client_4_05, deterministic) :-
		http_oauth_introspection_client::introspect(
			'https://issuer.example.com/hint', 'active-token', oauth_introspection(Properties),
			[
				authentication(basic(client, secret)),
				token_type_hint(access_token),
				http_client(http_oauth_test_introspection_client)
			]
		),
		memberchk(active(true), Properties).

	test(http_oauth_introspection_client_4_06, deterministic) :-
		http_oauth_introspection_client::introspect(
			'http://localhost:8080/basic', 'active-token', oauth_introspection(Properties),
			[
				authentication(basic(client, secret)),
				http_client(http_oauth_test_introspection_client),
				allow_insecure_localhost(true)
			]
		),
		memberchk(active(true), Properties).

	test(http_oauth_introspection_verifier_3_01, deterministic) :-
		http_oauth_introspection_verifier('https://issuer.example.com/basic', [
			authentication(basic(client, secret)),
			http_client(http_oauth_test_introspection_client)
		])::verify('active-token', 'https://api.example.com/mcp', TokenInfo),
		TokenInfo = oauth_token_info(TokenProperties),
		memberchk(audience_validation(exact('https://api.example.com/mcp')), TokenProperties),
		memberchk(scopes([read, write]), TokenProperties).

	test(http_oauth_introspection_verifier_3_02, fail) :-
		http_oauth_introspection_verifier('https://issuer.example.com/basic', [
			authentication(basic(client, secret)),
			http_client(http_oauth_test_introspection_client)
		])::verify('inactive-token', 'https://api.example.com/mcp', _).

	test(http_oauth_introspection_verifier_3_03, fail) :-
		http_oauth_introspection_verifier('https://issuer.example.com/basic', [
			authentication(basic(client, secret)),
			http_client(http_oauth_test_introspection_client)
		])::verify('active-token', 'https://api.example.com/other', _).

	test(http_oauth_introspection_verifier_3_04, deterministic) :-
		http_oauth_introspection_verifier('https://issuer.example.com/basic', [
			audience_validation(trust_active),
			authentication(basic(client, secret)),
			http_client(http_oauth_test_introspection_client)
		])::verify('active-token', 'https://api.example.com/other', oauth_token_info(TokenProperties)),
		memberchk(audience_validation(trust_active), TokenProperties).

	test(http_oauth_introspection_verifier_3_05, deterministic) :-
		http_oauth_introspection_verifier('https://issuer.example.com/basic', [
			authentication(basic(client, secret)),
			http_client(http_oauth_test_introspection_client),
			audience_validation(trust_active)
		])::verify('active-token', 'https://api.example.com/other', oauth_token_info(TokenProperties)),
		memberchk(audience_validation(trust_active), TokenProperties).

	% the http_oauth_jwt_verifier/2 parametric object is only loaded when using a backend
	% supporting unbound integer arithmetic
	%
	% use the {}/1 control construct to avoid a linter warning about unknown objects when
	% using a backend with bounded integers

	test(http_oauth_jwt_verifier_3_01, deterministic, [condition(current_prolog_flag(bounded, false))]) :-
		jwt_token('https://api.example.com/mcp', 'read write', Token, Key),
		{http_oauth_jwt_verifier(Key, [now(1700000001)])::verify(
			Token, 'https://api.example.com/mcp', oauth_token_info(Properties)
		)},
		memberchk(scopes([read, write]), Properties),
		memberchk(audience_validation(exact('https://api.example.com/mcp')), Properties).

	test(http_oauth_jwt_verifier_3_02, deterministic, [condition(current_prolog_flag(bounded, false))]) :-
		jwt_token(['https://api.example.com/other', 'https://api.example.com/mcp'], read, Token, Key),
		{http_oauth_jwt_verifier(Key, [now(1700000001)])::verify(Token, 'https://api.example.com/mcp', _)}.

	test(http_oauth_jwt_verifier_3_03, fail, [condition(current_prolog_flag(bounded, false))]) :-
		jwt_token('https://api.example.com/other', read, Token, Key),
		{http_oauth_jwt_verifier(Key, [now(1700000001)])::verify(Token, 'https://api.example.com/mcp', _)}.

	test(http_oauth_jwt_verifier_3_04, fail, [condition(current_prolog_flag(bounded, false))]) :-
		{http_oauth_jwt_verifier('0123456789abcdef0123456789abcdef', [allow_missing_exp(true)])::verify(
			'not-a-jwt', 'https://api.example.com/mcp', _
		)}.

	test(http_router_oauth_2_01, deterministic) :-
		Request = request(get, origin('/public'), http(1, 1), [], empty, []),
		http_oauth_test_router::handle(Request, Response),
		status(Response, status(200, 'OK')).

	test(http_router_oauth_2_02, deterministic) :-
		Request = request(get, origin('/secret'), http(1, 1), [], empty, []),
		http_oauth_test_router::handle(Request, Response),
		status(Response, status(401, 'Unauthorized')).

	test(http_router_oauth_2_03, deterministic) :-
		http_oauth::generate_authorization(bearer_authorization('valid-token'), HeaderValue),
		Request = request(get, origin('/secret'), http(1, 1), [authorization-HeaderValue], empty, []),
		http_oauth_test_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({scopes-[read, write]}))).

	% auxiliary predicates

	request_with_token(Token, Properties, Request) :-
		http_oauth::generate_authorization(bearer_authorization(Token), HeaderValue),
		Request = request(get, origin('/mcp'), http(1, 1), [authorization-HeaderValue], empty, Properties).

	authenticated_request(Authenticated) :-
		request_with_token('valid-token', [], Request),
		http_oauth::authenticate_request(Request, http_oauth_test_verifier, continue(Authenticated), [
			protected_resource('https://api.example.com/mcp')
		]).

	jwt_token('https://api.example.com/mcp', 'read write',
		'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJhdWQiOiJodHRwczovL2FwaS5leGFtcGxlLmNvbS9tY3AiLCJzY29wZSI6InJlYWQgd3JpdGUiLCJzdWIiOiJhbGljZSIsImV4cCI6NDEwMjQ0NDgwMH0.cOMvgQqLyozt2A1CwYnSC2R_QB17VxfojVO2LagMjb8',
		'0123456789abcdef0123456789abcdef').
	jwt_token(['https://api.example.com/other', 'https://api.example.com/mcp'], read,
		'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJhdWQiOlsiaHR0cHM6Ly9hcGkuZXhhbXBsZS5jb20vb3RoZXIiLCJodHRwczovL2FwaS5leGFtcGxlLmNvbS9tY3AiXSwic2NvcGUiOiJyZWFkIiwic3ViIjoiYWxpY2UiLCJleHAiOjQxMDI0NDQ4MDB9.34kv5PiovCNVOhQkjtmEOm_p0o7EX1oWFjyHWoHaZu4',
		'0123456789abcdef0123456789abcdef').
	jwt_token('https://api.example.com/other', read,
		'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJhdWQiOiJodHRwczovL2FwaS5leGFtcGxlLmNvbS9vdGhlciIsInNjb3BlIjoicmVhZCIsInN1YiI6ImFsaWNlIiwiZXhwIjo0MTAyNDQ0ODAwfQ.0z_IR-Mr1MeS-QvXB6KFjs-xsAcobtyPjHXX0FvN1Rg',
		'0123456789abcdef0123456789abcdef').

:- end_object.
