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

	:- public(reset_jwks_sequence/0).
	:- mode(reset_jwks_sequence, one).
	:- info(reset_jwks_sequence/0, [
		comment is 'Resets the scripted JWKS sequence used by the local provider fixture.'
	]).

	:- public(set_jwks_sequence/1).
	:- mode(set_jwks_sequence(+list), one).
	:- info(set_jwks_sequence/1, [
		comment is 'Sets the scripted JWKS sequence used by the local provider fixture.',
		argnames is ['Sequence']
	]).

	:- private(jwks_sequence_/1).
	:- dynamic(jwks_sequence_/1).
	:- mode(jwks_sequence_(?list), zero_or_more).
	:- info(jwks_sequence_/1, [
		comment is 'Queued JWKS responses for the local provider fixture.',
		argnames is ['Sequence']
	]).

	:- uses(http_core, [
		body/2, header/3, response/6, target/2, version/2
	]).

	:- uses(list, [
		member/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	handle(Request, Response) :-
		version(Request, Version),
		target(Request, Target),
		body_response(Target, Request, Body),
		response(Version, status(200, 'OK'), [], Body, [], Response).

	reset_jwks_sequence :-
		retractall(jwks_sequence_(_)).

	set_jwks_sequence(Sequence) :-
		retractall(jwks_sequence_(_)),
		assertz(jwks_sequence_(Sequence)).

	body_response(origin('/.well-known/openid-configuration'), Request, content('application/json', json({
		issuer-Issuer,
		authorization_endpoint-AuthorizationEndpoint,
		token_endpoint-TokenEndpoint,
		jwks_uri-JWKsURI,
		userinfo_endpoint-UserInfoEndpoint,
		end_session_endpoint-EndSessionEndpoint,
		id_token_signing_alg_values_supported-['RS256', 'ES256']
	}))) :-
		issuer(Request, Issuer),
		atomic_list_concat([Issuer, '/authorize'], AuthorizationEndpoint),
		atomic_list_concat([Issuer, '/token'], TokenEndpoint),
		atomic_list_concat([Issuer, '/jwks'], JWKsURI),
		atomic_list_concat([Issuer, '/userinfo'], UserInfoEndpoint),
		atomic_list_concat([Issuer, '/logout'], EndSessionEndpoint).
	body_response(origin('/jwks'), _Request, content('application/json', json(JWKSet))) :-
		next_jwks_set(JWKSet),
		!.
	body_response(origin('/token'), Request, content('application/json', json({
		access_token-'access',
		id_token-'id',
		token_type-'Bearer',
		expires_in-3600,
		scope-'openid'
	}))) :-
		body(Request, content('application/x-www-form-urlencoded', form(Pairs))),
		member(grant_type-'authorization_code', Pairs),
		\+ member(client_secret-_, Pairs),
		\+ header(Request, authorization, _),
		!.
	body_response(origin('/token'), Request, content('application/json', json({
		access_token-'post-access',
		id_token-'id',
		token_type-'Bearer',
		expires_in-3600,
		scope-'openid'
	}))) :-
		body(Request, content('application/x-www-form-urlencoded', form(Pairs))),
		member(grant_type-'authorization_code', Pairs),
		member(client_secret-'secret', Pairs),
		!.
	body_response(origin('/token'), Request, content('application/json', json({
		access_token-'basic-access',
		id_token-'id',
		token_type-'Bearer',
		expires_in-3600,
		scope-'openid'
	}))) :-
		body(Request, content('application/x-www-form-urlencoded', form(Pairs))),
		member(grant_type-'authorization_code', Pairs),
		\+ member(client_secret-_, Pairs),
		header(Request, authorization, 'Basic Y2xpZW50OnNlY3JldA=='),
		!.
	body_response(origin('/token'), Request, content('application/json', json({
		access_token-'refreshed-access',
		token_type-'Bearer',
		expires_in-3600,
		refresh_token-'rotated-refresh',
		scope-'openid profile'
	}))) :-
		body(Request, content('application/x-www-form-urlencoded', form(Pairs))),
		member(grant_type-'refresh_token', Pairs),
		member(refresh_token-'refresh', Pairs),
		member(scope-'openid profile', Pairs),
		header(Request, authorization, 'Basic Y2xpZW50OnNlY3JldA=='),
		!.
	body_response(origin('/userinfo'), Request, content('application/json', json({
		sub-'123',
		name-'Alice'
	}))) :-
		header(Request, authorization, 'Bearer access'),
		!.
	body_response(_, _Request, content('application/json', json({ok- @true}))).

	next_jwks_set(JWKSet) :-
		retract(jwks_sequence_([JWKSet| RemainingJWKSets])),
		!,
		assert_remaining_jwks_sets(RemainingJWKSets).
	next_jwks_set({keys-[]}).

	assert_remaining_jwks_sets([]).
	assert_remaining_jwks_sets([JWKSet]) :-
		assertz(jwks_sequence_([JWKSet])).
	assert_remaining_jwks_sets([JWKSet1, JWKSet2| RemainingJWKSets]) :-
		assertz(jwks_sequence_([JWKSet1, JWKSet2| RemainingJWKSets])).

	issuer(Request, Issuer) :-
		(	header(Request, host, host(Host, Port)) ->
			atomic_list_concat([Host, ':', Port], Authority)
		;	header(Request, host, host(Authority))
		),
		atomic_list_concat(['http://', Authority], Issuer).

:- end_object.
