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


:- object(http_oauth_test_verifier,
	implements(http_oauth_verifier_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-01,
		comment is 'Resource-sensitive OAuth verifier used by the http_oauth tests.'
	]).

	verify('valid-token', 'https://api.example.com/mcp', oauth_token_info([
		source(test),
		scopes([read, write]),
		audience_validation(exact('https://api.example.com/mcp')),
		claims([subject(alice)])
	])).

:- end_object.


:- object(http_oauth_test_router,
	implements(http_handler_protocol),
	imports([http_router,http_router_oauth(http_oauth_test_verifier, [protected_resource('https://api.example.com/mcp')])])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-01,
		comment is 'Router object used by the http_oauth route integration tests.'
	]).

	:- protected(show_public/2).
	:- info(show_public/2, [
		comment is 'Public route test handler.',
		argnames is ['Request', 'Response']
	]).

	:- protected(show_secret/2).
	:- info(show_secret/2, [
		comment is 'OAuth-protected route test handler.',
		argnames is ['Request', 'Response']
	]).

	:- protected(authorize_routed_request/2).
	:- info(authorize_routed_request/2, [
		comment is 'Delegates OAuth route authorization.',
		argnames is ['Request', 'Action']
	]).

	route(show_public, get, '/public', show_public).
	route(show_secret, get, '/secret', show_secret).

	route_metadata(show_secret, [oauth([required_scopes([write])])]).

	authorize_routed_request(Request, Action) :-
		^^authorize_oauth_request(Request, Action).

	show_public(Request, Response) :-
		http_core::version(Request, Version),
		http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(public)), [], Response).

	show_secret(Request, Response) :-
		http_core::version(Request, Version),
		http_core::property(Request, oauth_scopes(Scopes)),
		http_core::response(Version, status(200, 'OK'), [], content('application/json', json({scopes-Scopes})), [], Response).

:- end_object.


:- object(http_oauth_test_introspection_client).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-01,
		comment is 'Injectable HTTP client used by the OAuth introspection tests.'
	]).

	:- public(post/4).
	:- mode(post(+atom, +compound, -compound, +list(compound)), one).

	:- uses(list, [
		memberchk/2
	]).

	post(Endpoint, content('application/x-www-form-urlencoded', form(Parameters)), Response, Options) :-
		memberchk(token-Token, Parameters),
		valid_test_parameters(Endpoint, Parameters),
		memberchk(headers(Headers), Options),
		valid_test_authentication(Endpoint, Headers),
		response_json(Token, JSON),
		Response = response(http(1, 1), status(200, 'OK'), [content_type-'application/json'], content('application/json', json(JSON)), []).

	valid_test_parameters('https://issuer.example.com/hint', Parameters) :-
		!,
		memberchk(token_type_hint-access_token, Parameters).
	valid_test_parameters(_Endpoint, _Parameters).

	valid_test_authentication('https://issuer.example.com/basic', Headers) :-
		http_authenticate::generate_authorization(basic_authorization([username(client), password(secret)]), HeaderValue),
		memberchk(authorization-HeaderValue, Headers).
	valid_test_authentication('https://issuer.example.com/hint', Headers) :-
		http_authenticate::generate_authorization(basic_authorization([username(client), password(secret)]), HeaderValue),
		memberchk(authorization-HeaderValue, Headers).
	valid_test_authentication('http://localhost:8080/basic', Headers) :-
		http_authenticate::generate_authorization(basic_authorization([username(client), password(secret)]), HeaderValue),
		memberchk(authorization-HeaderValue, Headers).
	valid_test_authentication('https://issuer.example.com/bearer', Headers) :-
		memberchk(authorization-'Bearer client-token', Headers).
	valid_test_authentication('https://issuer.example.com/headers', Headers) :-
		memberchk(x_client_secret-secret, Headers).

	response_json('active-token', {
		active- @true,
		scope-'read write',
		sub-alice,
		aud-['https://api.example.com/mcp']
	}).
	response_json('inactive-token', {active- @false}).

:- end_object.


:- object(http_oauth_test_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-01,
		comment is 'Protected handler used by the http_oauth endpoint tests.'
	]).

	handle(Request, Response) :-
		http_core::version(Request, Version),
		http_core::property(Request, oauth_scopes(Scopes)),
		http_core::property(Request, original_property(kept)),
		http_core::response(Version, status(200, 'OK'), [], content('application/json', json({scopes-Scopes})), [], Response).

:- end_object.
