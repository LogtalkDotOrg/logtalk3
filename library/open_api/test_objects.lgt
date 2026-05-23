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


:- object(
	duplicate_operation_id_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Duplicate Operation Id API', '1.0.0', 'Fixture provider with duplicate operation identifiers.', [])).
	servers([]).
	security([]).
	operations([
		operation(shared_operation, get, '/users', 'List users', [], none, [response(200, 'Listed users', [])], []),
		operation(shared_operation, post, '/users', 'Create user', [], none, [response(201, 'Created user', [])], [])
	]).
	schema(_, _) :-
		fail.
	security_scheme(_, _) :-
		fail.

:- end_object.


:- object(
	missing_security_scheme_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Missing Security Scheme API', '1.0.0', 'Fixture provider with undeclared security scheme references.', [])).
	servers([]).
	security([]).
	operations([
		operation(
			list_secured_users,
			get,
			'/secured-users',
			'List secured users',
			[],
			none,
			[response(200, 'Listed secured users', [])],
			[security([[missing_bearer-[]]])]
		)
	]).
	schema(_, _) :-
		fail.
	security_scheme(_, _) :-
		fail.

:- end_object.


:- object(
	invalid_oauth_scope_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Invalid OAuth Scope API', '1.0.0', 'Fixture provider with undeclared OAuth scope references.', [])).
	servers([]).
	security([]).
	operations([
		operation(
			list_scoped_users,
			get,
			'/scoped-users',
			'List scoped users',
			[],
			none,
			[response(200, 'Listed scoped users', [])],
			[security([[user_oauth-[admin_users]]])]
		)
	]).
	schema(_, _) :-
		fail.
	security_scheme(user_oauth, oauth2([
		client_credentials(
			'https://auth.example.com/oauth/token',
			[scope(read_users, 'Read user data')]
		)
	])).

:- end_object.


:- object(
	invalid_api_key_security_scheme_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Invalid API Key Security Scheme API', '1.0.0', 'Fixture provider with a malformed apiKey security scheme.', [])).
	servers([]).
	security([]).
	operations([]).
	schema(_, _) :-
		fail.
	security_scheme(api_key, {
		type-apiKey
	}).

:- end_object.


:- object(
	invalid_http_security_scheme_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Invalid HTTP Security Scheme API', '1.0.0', 'Fixture provider with a malformed http security scheme.', [])).
	servers([]).
	security([]).
	operations([]).
	schema(_, _) :-
		fail.
	security_scheme(user_http, {
		type-http
	}).

:- end_object.


:- object(
	invalid_oauth_security_scheme_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Invalid OAuth Security Scheme API', '1.0.0', 'Fixture provider with a malformed oauth2 security scheme.', [])).
	servers([]).
	security([]).
	operations([]).
	schema(_, _) :-
		fail.
	security_scheme(user_oauth, {
		type-oauth2
	}).

:- end_object.


:- object(
	invalid_openid_security_scheme_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Invalid OpenID Connect Security Scheme API', '1.0.0', 'Fixture provider with a malformed openIdConnect security scheme.', [])).
	servers([]).
	security([]).
	operations([]).
	schema(_, _) :-
		fail.
	security_scheme(user_oidc, {
		type-openIdConnect
	}).

:- end_object.


:- object(
	invalid_openid_flow_security_scheme_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Invalid OpenID Connect Flows Security Scheme API', '1.0.0', 'Fixture provider using invalid flows options for an openIdConnect security scheme.', [])).
	servers([]).
	security([]).
	operations([]).
	schema(_, _) :-
		fail.
	security_scheme(user_oidc, openid_connect(
		'https://auth.example.com/oidc',
		[
			description('OIDC scheme'),
			flows([
				implicit(
					'https://auth.example.com/oauth/authorize',
					[scope(profile, 'Read profile data')]
				)
			])
		]
	)).

:- end_object.


:- object(
	invalid_api_key_location_security_scheme_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Invalid API Key Location Security Scheme API', '1.0.0', 'Fixture provider with an invalid apiKey in value.', [])).
	servers([]).
	security([]).
	operations([]).
	schema(_, _) :-
		fail.
	security_scheme(api_key, api_key(body, 'X-API-Key')).

:- end_object.


:- object(
	invalid_oauth_url_security_scheme_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Invalid OAuth URL Security Scheme API', '1.0.0', 'Fixture provider with an invalid oauth2 URL field.', [])).
	servers([]).
	security([]).
	operations([]).
	schema(_, _) :-
		fail.
	security_scheme(user_oauth, oauth2([
		client_credentials(
			'not a url',
			[scope(read_users, 'Read user data')]
		)
	])).

:- end_object.


:- object(
	invalid_openid_url_security_scheme_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Invalid OpenID Connect URL Security Scheme API', '1.0.0', 'Fixture provider with an invalid openIdConnect URL field.', [])).
	servers([]).
	security([]).
	operations([]).
	schema(_, _) :-
		fail.
	security_scheme(user_oidc, openid_connect('not a url')).

:- end_object.


:- object(
	invalid_oauth_scope_descriptor_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Invalid OAuth Scope Descriptor API', '1.0.0', 'Fixture provider with a malformed oauth2 DSL scope descriptor.', [])).
	servers([]).
	security([]).
	operations([]).
	schema(_, _) :-
		fail.
	security_scheme(user_oauth, oauth2([
		client_credentials(
			'https://auth.example.com/oauth/token',
			[read_users]
		)
	])).

:- end_object.


:- object(
	invalid_server_url_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Invalid Server URL API', '1.0.0', 'Fixture provider with an invalid server URL.', [])).
	servers([
		server('not a url', 'Broken server URL')
	]).
	security([]).
	operations([]).
	schema(_, _) :-
		fail.
	security_scheme(_, _) :-
		fail.

:- end_object.


:- object(
	relative_external_docs_provider,
	implements((open_api_provider_protocol, application_protocol))).

	api_info(info('Relative External Docs API', '1.0.0', 'Fixture provider using a relative externalDocs URL reference.', [])).
	servers([]).
	security([]).
	operations([]).
	homepage('/docs').
	description('Relative docs fixture').
	license('Apache-2.0').
	schema(_, _) :-
		fail.
	security_scheme(_, _) :-
		fail.

:- end_object.


:- object(
	security_matrix_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Security Matrix API', '1.0.0', 'Fixture provider covering additional security scheme variants.', [])).
	servers([]).
	security([]).
	operations([]).
	schema(_, _) :-
		fail.
	security_scheme(api_key_query, api_key(query, api_key)).
	security_scheme(api_key_header, api_key(header, 'X-API-Key', [description('Header based API key')])).
	security_scheme(api_key_cookie, api_key(cookie, session_id)).
	security_scheme(mtls_plain, mutual_tls).
	security_scheme(mtls_described, mutual_tls([description('Mutual TLS') ])).
	security_scheme(password_oauth, oauth2([
		password(
			'https://auth.example.com/oauth/token',
			[scope(read_users, 'Read user data')],
			[refresh_url('https://auth.example.com/oauth/refresh')]
		),
		authorization_code(
			'https://auth.example.com/oauth/authorize',
			'https://auth.example.com/oauth/token',
			[scope(write_users, 'Write user data')]
		)
	], [description('Extended OAuth scheme')])).
	security_scheme(user_oidc, openid_connect(
		'https://auth.example.com/oidc',
		[
			description('OIDC scheme')
		]
	)).

:- end_object.


:- object(
	parameter_matrix_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Parameter Matrix API', '1.0.0', 'Fixture provider covering path, query, header, and cookie request parameters.', [])).
	servers([]).
	security([]).
	operations([
		operation(
			get_session,
			get,
			'/sessions/{id}',
			'Get session',
			[
				parameter(id, path, 'Session identifier', true, {type-string}),
				parameter(etag, header, 'Entity tag header', true, {type-string}),
				parameter(session_id, cookie, 'Session cookie', true, {type-string}),
				parameter(verbose, query, 'Verbose output', false, {type-boolean})
			],
			none,
			[response(200, 'OK', [])],
			[]
		)
	]).
	schema(_, _) :-
		fail.
	security_scheme(_, _) :-
		fail.

:- end_object.


:- object(
	payload_matrix_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Payload Matrix API', '1.0.0', 'Fixture provider covering optional, text, and form payload validation.', [])).
	servers([]).
	security([]).
	operations([
		operation(
			patch_profile_note,
			patch,
			'/profiles/{id}/note',
			'Patch profile note',
			[
				parameter(id, path, 'Profile identifier', true, {type-string})
			],
			request_body('Optional note', false, [media('text/plain', {type-string})]),
			[response(204, 'Updated', [])],
			[]
		),
		operation(
			submit_login,
			post,
			'/login',
			'Submit login',
			[],
			request_body('Login form', true, [media('application/x-www-form-urlencoded', {
				type-object,
				properties-{
					username-{type-string},
					password-{type-string}
				},
				required-[username, password],
				additionalProperties- @false
			})]),
			[response(200, 'OK', [media('text/plain', {type-string})])],
			[]
		)
	]).
	schema(_, _) :-
		fail.
	security_scheme(_, _) :-
		fail.

:- end_object.


:- object(
	invalid_relative_server_url_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Invalid Relative Server URL API', '1.0.0', 'Fixture provider with a malformed relative server URL.', [])).
	servers([
		server('users[1]', 'Broken relative server URL')
	]).
	security([]).
	operations([]).
	schema(_, _) :-
		fail.
	security_scheme(_, _) :-
		fail.

:- end_object.


:- object(
	valid_server_url_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Valid Server URL API', '1.0.0', 'Fixture provider with relative and templated server URLs.', [])).
	servers([
		server('/v1', 'Relative server URL'),
		server('https://{username}.gigantic-server.com:{port}/{basePath}', 'Templated server URL')
	]).
	security([]).
	operations([]).
	schema(_, _) :-
		fail.
	security_scheme(_, _) :-
		fail.

:- end_object.


:- object(
	invalid_path_parameter_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Invalid Path Parameter API', '1.0.0', 'Fixture provider with a mismatched path parameter name.', [])).
	servers([]).
	security([]).
	operations([
		operation(
			get_user,
			get,
			'/users/{id}',
			'Get user',
			[
				parameter(user_id, path, 'Broken path parameter', true, {type-string})
			],
			none,
			[response(200, 'OK', [])],
			[]
		)
	]).
	schema(_, _) :-
		fail.
	security_scheme(_, _) :-
		fail.

:- end_object.


:- object(
	wildcard_response_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Wildcard Response API', '1.0.0', 'Fixture provider with a wildcard response status.', [])).
	servers([]).
	security([]).
	operations([
		operation(
			list_items,
			get,
			'/items',
			'List items',
			[],
			none,
			[response('2XX', 'Successful response', [])],
			[]
		)
	]).
	schema(_, _) :-
		fail.
	security_scheme(_, _) :-
		fail.

:- end_object.


:- object(
	duplicate_query_parameter_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Duplicate Query Parameter API', '1.0.0', 'Fixture provider with duplicated query parameters.', [])).
	servers([]).
	security([]).
	operations([
		operation(
			list_users,
			get,
			'/users',
			'List users',
			[
				parameter(id, query, 'First id filter', false, {type-string}),
				parameter(id, query, 'Second id filter', false, {type-string})
			],
			none,
			[response(200, 'OK', [])],
			[]
		)
	]).
	schema(_, _) :-
		fail.
	security_scheme(_, _) :-
		fail.

:- end_object.


:- object(
	ignored_header_parameter_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Ignored Header Parameter API', '1.0.0', 'Fixture provider using a reserved header parameter name.', [])).
	servers([]).
	security([]).
	operations([
		operation(
			get_profiles,
			get,
			'/profiles',
			'Get profiles',
			[
				parameter('Accept', header, 'Ignored Accept header parameter', true, {type-string})
			],
			none,
			[response(200, 'OK', [])],
			[]
		)
	]).
	schema(_, _) :-
		fail.
	security_scheme(_, _) :-
		fail.

:- end_object.


:- object(
	media_range_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Media Range API', '1.0.0', 'Fixture provider using wildcard media type declarations.', [])).
	servers([]).
	security([]).
	operations([
		operation(
			accept_any_payload,
			post,
			'/upload',
			'Accept any payload',
			[],
			request_body('Any payload', true, [media('*/*', {type-string})]),
			[response(204, 'Uploaded', [])],
			[]
		),
		operation(
			download_text,
			get,
			'/download',
			'Download text',
			[],
			none,
			[response(200, 'OK', [media('text/*', {type-string})])],
			[]
		)
	]).
	schema(_, _) :-
		fail.
	security_scheme(_, _) :-
		fail.

:- end_object.


:- object(
	equivalent_path_template_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Equivalent Path Template API', '1.0.0', 'Fixture provider with equivalent templated paths.', [])).
	servers([]).
	security([]).
	operations([
		operation(
			get_pet_by_id,
			get,
			'/pets/{petId}',
			'Get pet by identifier',
			[
				parameter(petId, path, 'Pet identifier', true, {type-string})
			],
			none,
			[response(200, 'OK', [])],
			[]
		),
		operation(
			get_pet_by_name,
			post,
			'/pets/{name}',
			'Get pet by name',
			[
				parameter(name, path, 'Pet name', true, {type-string})
			],
			none,
			[response(200, 'OK', [])],
			[]
		)
	]).
	schema(_, _) :-
		fail.
	security_scheme(_, _) :-
		fail.

:- end_object.


:- object(
	invalid_component_name_provider,
	implements(open_api_provider_protocol)).

	api_info(info('Invalid Component Name API', '1.0.0', 'Fixture provider using an invalid component schema name.', [])).
	servers([]).
	security([]).
	operations([]).
	schema('bad/key', {type-string}).
	security_scheme(_, _) :-
		fail.

:- end_object.


:- object(
	http_request_protocol_fixture,
	implements(http_request_protocol)).

	method(put).
	target(origin('/users/11111111-1111-1111-1111-111111111111')).
	version(http(1, 1)).
	headers([]).
	header(_, _) :-
		fail.
	body(content('application/json', json({name-'Alice Example', active-true}))).
	property(query_pairs([verbose-true])).

:- end_object.


:- object(
	http_mixed_case_header_request_protocol_fixture,
	implements(http_request_protocol)).

	method(get).
	target(absolute([scheme(http), authority('api.example.com'), path('/sessions/session-1'), query('verbose=true'), fragment('')])).
	version(http(1, 1)).
	headers(['ETag'-'session-tag']).
	header(_, _) :-
		fail.
	body(empty).
	property(query_pairs([verbose-true])).
	property(cookies([session_id-'cookie-1'])).
	property(path_params([id-'session-1'])).

:- end_object.


:- object(
	http_invalid_request_body_protocol_fixture,
	implements(http_request_protocol)).

	method(patch).
	target(origin('/profiles/profile-1/note')).
	version(http(1, 1)).
	headers([]).
	header(_, _) :-
		fail.
	body(bogus).
	property(_) :-
		fail.

:- end_object.


:- object(
	http_unsupported_request_payload_protocol_fixture,
	implements(http_request_protocol)).

	method(post).
	target(origin('/login')).
	version(http(1, 1)).
	headers([]).
	header(_, _) :-
		fail.
	body(content('application/x-www-form-urlencoded', xml(invalid))).
	property(_) :-
		fail.

:- end_object.


:- object(
	http_response_protocol_fixture,
	implements(http_response_protocol)).

	version(http(1, 1)).
	status(status(404, 'Not Found')).
	headers([]).
	header(_, _) :-
		fail.
	body(content('application/json', json({code-not_found, message-'User not found'}))).
	property(_) :-
		fail.

:- end_object.


:- object(
	http_unsupported_response_payload_protocol_fixture,
	implements(http_response_protocol)).

	version(http(1, 1)).
	status(status(200, 'OK')).
	headers([]).
	header(_, _) :-
		fail.
	body(content('text/plain', raw)).
	property(_) :-
		fail.

:- end_object.


:- object(
	http_invalid_response_body_protocol_fixture,
	implements(http_response_protocol)).

	version(http(1, 1)).
	status(status(200, 'OK')).
	headers([]).
	header(_, _) :-
		fail.
	body(invalid_body).
	property(_) :-
		fail.

:- end_object.


:- object(
	http_integer_status_response_protocol_fixture,
	implements(http_response_protocol)).

	version(http(1, 1)).
	status(200).
	headers([]).
	header(_, _) :-
		fail.
	body(content('text/plain', text(ok))).
	property(_) :-
		fail.

:- end_object.
