%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(open_id_test_handler,
	implements(http_handler_protocol)).

	:- public(handle/2).
	:- mode(handle(+compound, -compound), one).
	:- info(handle/2, [
		comment is 'Handles HTTP test requests for the local OpenID provider fixture.',
		argnames is ['Request', 'Response']
	]).

	:- uses(http_core, [
		header/3, response/6, target/2, version/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	handle(Request, Response) :-
		version(Request, Version),
		target(Request, Target),
		body_response(Target, Request, Body),
		response(Version, status(200, 'OK'), [], Body, [], Response).

	body_response(origin('/.well-known/openid-configuration'), Request, content('application/json', json({
		issuer-Issuer,
		authorization_endpoint-AuthorizationEndpoint,
		token_endpoint-TokenEndpoint,
		jwks_uri-JWKsURI,
		id_token_signing_alg_values_supported-['RS256', 'ES256']
	}))) :-
		issuer(Request, Issuer),
		atomic_list_concat([Issuer, '/authorize'], AuthorizationEndpoint),
		atomic_list_concat([Issuer, '/token'], TokenEndpoint),
		atomic_list_concat([Issuer, '/jwks'], JWKsURI).
	body_response(origin('/jwks'), _Request, content('application/json', json({keys-[]}))).
	body_response(origin('/token'), _Request, content('application/json', json({
		access_token-'access',
		id_token-'id',
		token_type-'Bearer',
		expires_in-3600,
		scope-'openid'
	}))).
	body_response(_, _Request, content('application/json', json({ok- @true}))).

	issuer(Request, Issuer) :-
		(	header(Request, host, host(Host, Port)) ->
			atomic_list_concat([Host, ':', Port], Authority)
		;	header(Request, host, host(Authority))
		),
		atomic_list_concat(['http://', Authority], Issuer).

:- end_object.
