%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests_live,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-27,
		comment is 'Optional live tests for the "open_id" library using public OpenID Connect endpoints.'
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	:- uses(os, [
		environment_variable/2
	]).

	:- uses(http_core, [
		body/2, header/3, status/2
	]).

	cover(open_id).
	cover(open_id_client).
	cover(open_id_discovery).
	cover(open_id_jwks).
	cover(open_id_jwt).

	test(open_id_discover_3_live_google_01, deterministic(memberchk('RS256', Algorithms))) :-
		google_issuer(Issuer),
		open_id::discover(Issuer, Provider, []),
		open_id_discovery::property(Provider, issuer, Issuer),
		open_id_discovery::property(Provider, authorization_endpoint, 'https://accounts.google.com/o/oauth2/v2/auth'),
		open_id_discovery::property(Provider, token_endpoint, 'https://oauth2.googleapis.com/token'),
		open_id_discovery::property(Provider, jwks_uri, 'https://www.googleapis.com/oauth2/v3/certs'),
		open_id_discovery::property(Provider, id_token_signing_alg_values_supported, Algorithms).

	test(open_id_jwks_3_live_google_01, deterministic) :-
		google_provider(Provider),
		open_id::jwks(Provider, JWKSet, []),
		json_member(keys, JWKSet, Keys),
		list::valid(Keys),
		Keys = [_| _],
		open_id_jwks::select_key(JWKSet, {alg-'RS256'}, Key),
		json_member(kty, Key, 'RSA'),
		json_member(use, Key, sig).

	test(open_id_jwks_select_key_3_live_google_01, deterministic(Key == KeyById)) :-
		google_provider(Provider),
		open_id::jwks(Provider, JWKSet, []),
		open_id_jwks::select_key(JWKSet, {alg-'RS256'}, Key),
		json_member(kid, Key, KeyId),
		open_id_jwks::select_key(JWKSet, {alg-'RS256', kid-KeyId}, KeyById).

	test(open_id_verify_id_token_5_live_google_01, deterministic, [condition(google_id_token_configuration)]) :-
		google_issuer(Issuer),
		google_id_token(Token),
		google_id_token_options(Options),
		open_id::discover(Issuer, Provider, []),
		open_id::jwks(Provider, JWKSet, []),
		open_id::verify_id_token(Token, Provider, JWKSet, Claims, Options),
		json_member(iss, Claims, Issuer),
		json_member(sub, Claims, Subject),
		atom(Subject).

	test(open_id_discover_3_live_oidctest_01, deterministic(memberchk('RS256', Algorithms))) :-
		oidctest_issuer(Issuer),
		open_id::discover(Issuer, Provider, []),
		open_id_discovery::property(Provider, issuer, Issuer),
		open_id_discovery::property(Provider, authorization_endpoint, 'https://oidctest.wsweet.org/oauth2/authorize'),
		open_id_discovery::property(Provider, token_endpoint, 'https://oidctest.wsweet.org/oauth2/token'),
		open_id_discovery::property(Provider, jwks_uri, 'https://oidctest.wsweet.org/oauth2/jwks'),
		open_id_discovery::property(Provider, id_token_signing_alg_values_supported, Algorithms).

	test(open_id_jwks_3_live_oidctest_01, deterministic) :-
		oidctest_provider(Provider),
		open_id::jwks(Provider, JWKSet, []),
		open_id_jwks::select_key(JWKSet, {alg-'RS256', kid-'oidctest'}, Key),
		json_member(kty, Key, 'RSA'),
		json_member(use, Key, sig).

	test(open_id_authorization_code_flow_live_oidctest_01, deterministic) :-
		oidctest_provider(Provider),
		oidctest_session_id(SessionId),
		oidctest_authorization_url(Provider, AuthorizationURL, AuthorizationSession),
		oidctest_authorization_code(AuthorizationURL, SessionId, Code),
		open_id::exchange_code(
			Provider,
			Code,
			AuthorizationSession,
			Tokens,
			[client_secret_post('tardis')]
		),
		token_property(Tokens, access_token, AccessToken),
		token_property(Tokens, id_token, IdToken),
		atom(AccessToken),
		open_id::jwks(Provider, JWKSet, []),
		open_id::verify_id_token(
			IdToken,
			Provider,
			JWKSet,
			Claims,
			[expected_audience(private), expected_nonce('logtalk-live-nonce'), required_claims([sub, auth_time])]
		),
		json_member(sub, Claims, dwho).

	% auxiliary predicates

	google_issuer('https://accounts.google.com').

	google_provider(Provider) :-
		google_issuer(Issuer),
		open_id::discover(Issuer, Provider, []).

	google_id_token_configuration :-
		google_id_token(_),
		google_audience(_).

	google_id_token(Token) :-
		environment_variable('LOGTALK_OPEN_ID_GOOGLE_ID_TOKEN', Token),
		Token \== ''.

	google_audience(Audience) :-
		environment_variable('LOGTALK_OPEN_ID_GOOGLE_AUDIENCE', Audience),
		Audience \== ''.

	google_id_token_options([expected_audience(Audience), required_claims([sub])| NonceOptions]) :-
		google_audience(Audience),
		google_nonce_options(NonceOptions).

	google_nonce_options([expected_nonce(Nonce)]) :-
		environment_variable('LOGTALK_OPEN_ID_GOOGLE_NONCE', Nonce),
		Nonce \== '',
		!.
	google_nonce_options([]).

	oidctest_issuer('https://oidctest.wsweet.org/').

	oidctest_provider(Provider) :-
		oidctest_issuer(Issuer),
		open_id::discover(Issuer, Provider, []).

	oidctest_authorization_url(Provider, URL, Session) :-
		open_id::authorization_url(
			Provider,
			authorization_request([
				client_id(private),
				redirect_uri('http://localhost'),
				scope([openid, profile, email]),
				state('logtalk-live-state'),
				nonce('logtalk-live-nonce')
			]),
			URL,
			Session,
			[code_verifier('dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk')]
		).

	oidctest_session_id(SessionId) :-
		catch(
			oidctest_session_response(SessionId),
			Error,
			oidctest_session_id_error(Error, SessionId)
		).

	oidctest_session_id_error(Error, SessionId) :-
		Error = error(domain_error(http_header_value(set_cookie), SetCookie), _),
		!,
		(	oidctest_session_id_from_set_cookie(SetCookie, SessionId) ->
			true
		;	throw(Error)
		).
	oidctest_session_id_error(Error, _) :-
		throw(Error).

	oidctest_session_response(SessionId) :-
		http_client(http_socket_process)::post(
			'https://oidctest.wsweet.org/oauth2/',
			content('application/x-www-form-urlencoded', form([user-dwho, password-dwho])),
			Response,
			[headers([accept-'application/json'])]
		),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json(JSON))),
		json_member(result, JSON, 1),
		json_member(error, JSON, '0'),
		json_member(id, JSON, SessionId).

	oidctest_session_id_from_set_cookie(SetCookie, SessionId) :-
		header_cookie_name_value(SetCookie, Name, SessionId),
		Name == lemonldap,
		SessionId \== ''.

	oidctest_authorization_code(URL, SessionId, Code) :-
		http_client(http_socket_process)::get(
			URL,
			Response,
			[properties([cookies([lemonldap-SessionId])])]
		),
		status(Response, status(302, _)),
		header(Response, location, Location), !,
		url_query_value(Location, code, Code).

	url_query_value(URL, Name, Value) :-
		url(atom)::parse(URL, Components),
		memberchk(query(Query), Components),
		query_parameter(Query, Name, Value),
		!.

	query_parameter(Query, Name, Value) :-
		(	split_atom_at_first(Query, '&', Pair, Rest) ->
			(	query_parameter_pair(Pair, Name, Value) ->
				true
			;	query_parameter(Rest, Name, Value)
			)
		;	query_parameter_pair(Query, Name, Value)
		).

	query_parameter_pair(Pair, Name, Value) :-
		split_atom_at_first(Pair, '=', Name, Value).

	header_cookie_name_value(Header, Name, Value) :-
		(	split_atom_at_first(Header, ';', Pair, _Rest) ->
			true
		;	Pair = Header
		),
		split_atom_at_first(Pair, '=', Name, Value).

	split_atom_at_first(Atom, Separator, Left, Right) :-
		atom_length(Separator, SeparatorLength),
		sub_atom(Atom, Before, SeparatorLength, After, Separator),
		sub_atom(Atom, 0, Before, _, Left),
		Start is Before + SeparatorLength,
		sub_atom(Atom, Start, After, 0, Right).

	token_property(tokens(Properties), Name, Value) :-
		Term =.. [Name, Value],
		memberchk(Term, Properties).

	json_member(Key, Object, Value) :-
		json_object_pairs(Object, Pairs),
		json_pair_member(Pairs, Key, Value).

	json_pair_member([Pair| _], Key, Value) :-
		pair_key_value(Pair, Key, Value),
		!.
	json_pair_member([_| Pairs], Key, Value) :-
		json_pair_member(Pairs, Key, Value).

	json_object_pairs({}, []) :-
		!.
	json_object_pairs({Pairs}, PairsList) :-
		!,
		curly_pairs_to_list(Pairs, PairsList).
	json_object_pairs(json(Pairs), Pairs).

	curly_pairs_to_list((Pair, Rest), [Pair| Pairs]) :-
		!,
		curly_pairs_to_list(Rest, Pairs).
	curly_pairs_to_list(Pair, [Pair]).

	pair_key_value(Key-Value, Key, Value) :-
		!.
	pair_key_value(Key=Value, Key, Value) :-
		!.
	pair_key_value(':'(Key, Value), Key, Value).

:- end_object.
