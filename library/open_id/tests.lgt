%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-06,
		comment is 'Unit tests for the "open_id" library.'
	]).

	:- uses(list, [
		append/3
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	cover(open_id).
	cover(open_id_client).
	cover(open_id_discovery).
	cover(open_id_pkce).
	cover(open_id_logout).
	cover(open_id_response).
	cover(open_id_jwt).
	cover(open_id_jwks).
	cover(open_id_jwks_cache).
	cover(open_id_der).
	cover(open_id_openssl).
	cover(open_id_helpers).

	test(open_id_pkce_01, deterministic(Challenge == 'E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM')) :-
		open_id_pkce::code_challenge('dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk', Challenge).

	test(open_id_authorization_url_01, deterministic) :-
		Provider = provider([
			issuer('https://issuer.example'),
			authorization_endpoint('https://issuer.example/authorize')
		]),
		Request = authorization_request([
			client_id('client'),
			redirect_uri('https://client.example/cb'),
			scope([openid, profile]),
			state('state'),
			nonce('nonce')
		]),
		open_id::authorization_url(Provider, Request, URL, Session, [code_verifier('dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk')]),
		URL == 'https://issuer.example/authorize?response_type=code&client_id=client&redirect_uri=https%3A%2F%2Fclient.example%2Fcb&scope=openid+profile&state=state&nonce=nonce&code_challenge=E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM&code_challenge_method=S256',
		^^assertion(Session == session([
			state('state'),
			nonce('nonce'),
			code_verifier('dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk'),
			redirect_uri('https://client.example/cb'),
			client_id('client')
		])).

	test(open_id_authorization_response_01, deterministic(Response == authorization_response([
		code('code'),
		state('state'),
		iss('https://issuer.example'),
		session_state('session-state'),
		raw([code-'code', state-'state', iss-'https://issuer.example', session_state-'session-state'])
	]))) :-
		open_id::authorization_response(
			'https://client.example/cb?code=code&state=state&iss=https%3A%2F%2Fissuer.example&session_state=session-state',
			Response,
			[]
		).

	test(open_id_authorization_response_02, deterministic(Response == authorization_error([
		error(access_denied),
		error_description('Nope'),
		state('state'),
		raw([error-access_denied, error_description-'Nope', state-'state'])
	]))) :-
		open_id::authorization_response(
			'https://client.example/cb?error=access_denied&error_description=Nope&state=state',
			Response,
			[]
		).

	test(open_id_authorization_code_01, deterministic(Code == 'code')) :-
		Session = session([state('state')]),
		open_id::authorization_code('https://client.example/cb?code=code&state=state', Session, Code, []).

	test(open_id_authorization_code_02, error(domain_error(open_id_authorization_response, state_mismatch('expected', 'actual')))) :-
		Session = session([state('expected')]),
		open_id::authorization_code('https://client.example/cb?code=code&state=actual', Session, _Code, []).

	test(open_id_authorization_code_03, error(domain_error(open_id_authorization_response, authorization_error(authorization_error([
		error(access_denied),
		state('state'),
		raw([error-access_denied, state-'state'])
	]))))) :-
		Session = session([state('state')]),
		open_id::authorization_code('https://client.example/cb?error=access_denied&state=state', Session, _Code, []).

	test(open_id_logout_url_01, deterministic(URL == 'https://issuer.example/logout')) :-
		Provider = provider([
			issuer('https://issuer.example'),
			end_session_endpoint('https://issuer.example/logout')
		]),
		open_id::logout_url(Provider, logout_request([]), URL, []).

	test(open_id_logout_url_02, deterministic) :-
		Provider = provider([
			issuer('https://issuer.example'),
			end_session_endpoint('https://issuer.example/logout')
		]),
		Request = logout_request([
			id_token_hint('id-token'),
			post_logout_redirect_uri('https://client.example/logout/callback'),
			state('logout-state'),
			client_id('client'),
			logout_hint('user@example.com'),
			ui_locales([en, pt])
		]),
		open_id::logout_url(Provider, Request, URL, []),
		URL == 'https://issuer.example/logout?id_token_hint=id-token&post_logout_redirect_uri=https%3A%2F%2Fclient.example%2Flogout%2Fcallback&state=logout-state&client_id=client&logout_hint=user%40example.com&ui_locales=en+pt'.

	test(open_id_logout_url_03, error(domain_error(open_id_provider, missing(end_session_endpoint)))) :-
		Provider = provider([issuer('https://issuer.example')]),
		open_id::logout_url(Provider, logout_request([]), _URL, []).

	test(open_id_logout_url_04, error(domain_error(open_id_post_logout_redirect_uri, 'http://client.example/logout/callback'))) :-
		Provider = provider([
			issuer('https://issuer.example'),
			end_session_endpoint('https://issuer.example/logout')
		]),
		open_id::logout_url(
			Provider,
			logout_request([post_logout_redirect_uri('http://client.example/logout/callback')]),
			_URL,
			[]
		).

	test(open_id_authorization_response_03, error(domain_error(open_id_authorization_response, issuer_mismatch('https://issuer.example', 'https://other.example')))) :-
		open_id::authorization_response(
			'https://client.example/cb?code=code&state=state&iss=https%3A%2F%2Fother.example',
			_Response,
			[provider([issuer('https://issuer.example')])]
		).

	test(open_id_discovery_01, deterministic) :-
		JSON = {
			issuer-'https://issuer.example',
			authorization_endpoint-'https://issuer.example/authorize',
			token_endpoint-'https://issuer.example/token',
			jwks_uri-'https://issuer.example/jwks',
			userinfo_endpoint-'https://issuer.example/userinfo',
			end_session_endpoint-'https://issuer.example/logout'
		},
		open_id_discovery::provider('https://issuer.example', JSON, Provider),
		open_id_discovery::property(Provider, userinfo_endpoint, 'https://issuer.example/userinfo'),
		open_id_discovery::property(Provider, end_session_endpoint, 'https://issuer.example/logout'),
		open_id_discovery::property(Provider, jwks_uri, 'https://issuer.example/jwks').

	test(open_id_discovery_02, deterministic(URL == 'https://issuer.example/.well-known/openid-configuration')) :-
		open_id_discovery::discovery_url('https://issuer.example/', URL).

	test(open_id_jwks_01, deterministic(Key == {kty-'RSA', kid-'1', alg-'RS256', use-sig, n-'AQAB', e-'AQAB'})) :-
		JWKSet = {keys-[{kty-'RSA', kid-'1', alg-'RS256', use-sig, n-'AQAB', e-'AQAB'}]},
		Header = {alg-'RS256', kid-'1'},
		open_id_jwks::select_key(JWKSet, Header, Key).

	test(open_id_jwks_02, deterministic(Key == ECKey)) :-
		ECKey = {kty-'EC', crv-'P-256', key_ops-[verify], x-'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA', y-'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA'},
		JWKSet = {keys-[{kty-'RSA', kid-'unused', alg-'RS256', use-sig, n-'AQAB', e-'AQAB'}, ECKey]},
		Header = {alg-'ES256'},
		open_id_jwks::select_key(JWKSet, Header, Key).

	test(open_id_der_01, deterministic) :-
		rsa_public_jwk(Key),
		open_id_der::public_key_pem(Key, PEM),
		atom_concat('-----BEGIN PUBLIC KEY-----\n', BodyAndFooter, PEM),
		atom_concat(Body, '-----END PUBLIC KEY-----\n', BodyAndFooter),
		atom_concat('MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAzuxofQx+whGosAeo0SD9', _, Body).

	test(open_id_der_02, deterministic) :-
		ECKey = {kty-'EC', crv-'P-256', x-'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA', y-'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA'},
		open_id_der::public_key_pem(ECKey, PEM),
		sub_atom(PEM, 0, _, _, '-----BEGIN PUBLIC KEY-----'),
		sub_atom(PEM, _, _, 0, '-----END PUBLIC KEY-----\n').

	test(open_id_der_03, subsumes([0x30| _], DER)) :-
		es256_signature_fixture(Signature),
		open_id_der::es256_signature_der(Signature, DER).

	test(open_id_der_04, error(domain_error(open_id_jwk_public_key, {kty-'oct'}))) :-
		open_id_der::public_key_pem({kty-'oct'}, _).

	test(open_id_der_05, error(domain_error(open_id_es256_signature, [1, 2, 3]))) :-
		open_id_der::es256_signature_der([1, 2, 3], _).

	test(open_id_jwt_decode_01, deterministic) :-
		rs256_token(Token),
		open_id_jwt::decode(Token, Header, Claims),
		^^assertion(Header == {alg-'RS256', kid-'1'}),
		^^assertion(Claims == {
			iss-'https://issuer.example',
			aud-'client',
			azp-'client',
			nonce-'nonce',
			exp-4102444800,
			nbf-1700000000,
			iat-1700000000,
			custom-'ok',
			sub-subject
		}).

	test(open_id_jwt_claims_01, error(domain_error(open_id_jwt_algorithm, none))) :-
		Provider = provider([issuer('https://issuer.example')]),
		JWKSet = {keys-[{kty-'RSA', kid-'1', alg-'RS256', n-'AQAB', e-'AQAB'}]},
		Token = 'eyJhbGciOiJub25lIn0.eyJpc3MiOiJodHRwczovL2lzc3Vlci5leGFtcGxlIn0.',
		open_id::verify_id_token(Token, Provider, JWKSet, _Claims, [expected_audience('client')]).

	test(open_id_jwt_verify_01, deterministic) :-
		Provider = provider([issuer('https://issuer.example')]),
		rsa_jwk_set(JWKSet),
		rs256_token(Token),
		open_id::verify_id_token(Token, Provider, JWKSet, Claims, [expected_audience('client'), expected_nonce('nonce'), required_claims([custom]), now(1700000001)]),
		^^assertion(Claims == {
			iss-'https://issuer.example',
			aud-'client',
			azp-'client',
			nonce-'nonce',
			exp-4102444800,
			nbf-1700000000,
			iat-1700000000,
			custom-'ok',
			sub-subject
		}).

	test(open_id_jwt_verify_02, deterministic) :-
		Provider = provider([issuer('https://issuer.example')]),
		rsa_jwk_set(JWKSet),
		rs256_token_list_aud(Token),
		open_id::verify_id_token(Token, Provider, JWKSet, Claims, [expected_audience('client'), expected_nonce('nonce'), required_claims([custom]), now(1700000001)]),
		^^assertion(Claims == {
			iss-'https://issuer.example',
			aud-['client', 'other'],
			azp-'client',
			nonce-'nonce',
			exp-4102444800,
			nbf-1700000000,
			iat-1700000000,
			custom-'ok',
			sub-subject
		}).

	test(open_id_jwt_verify_03, error(domain_error(open_id_claims, missing(sub)))) :-
		Provider = provider([issuer('https://issuer.example')]),
		rsa_jwk_set(JWKSet),
		rs256_token_missing_sub(Token),
		open_id::verify_id_token(Token, Provider, JWKSet, _Claims, [expected_audience('client'), expected_nonce('nonce'), required_claims([custom]), now(1700000001)]).

	test(open_id_jwt_verify_04, error(domain_error(open_id_claims, missing(iat)))) :-
		Provider = provider([issuer('https://issuer.example')]),
		rsa_jwk_set(JWKSet),
		rs256_token_missing_iat(Token),
		open_id::verify_id_token(Token, Provider, JWKSet, _Claims, [expected_audience('client'), expected_nonce('nonce'), required_claims([custom]), now(1700000001)]).

	test(open_id_jwt_verify_05, error(domain_error(open_id_claim(iat), 1700000900))) :-
		Provider = provider([issuer('https://issuer.example')]),
		rsa_jwk_set(JWKSet),
		rs256_token_future_iat(Token),
		open_id::verify_id_token(Token, Provider, JWKSet, _Claims, [expected_audience('client'), expected_nonce('nonce'), required_claims([custom]), now(1700000001)]).

	test(open_id_openssl_01, error(existence_error(os_command, '__missing_open_id_test_command__'))) :-
		rsa_public_pem(PEM),
		open_id_openssl::verify('RS256', PEM, 'ignored', [0], [openssl_executable('__missing_open_id_test_command__')]).

	test(open_id_openssl_02, deterministic) :-
		rsa_public_pem(PEM),
		rs256_token(Token),
		jwt_signing_input_signature(Token, SigningInput, Signature),
		once(open_id_openssl::verify('RS256', PEM, SigningInput, Signature, [])).

	test(open_id_openssl_03, false) :-
		rsa_public_pem(PEM),
		rs256_token(Token),
		jwt_signing_input_signature(Token, SigningInput, Signature0),
		alter_signature(Signature0, Signature),
		open_id_openssl::verify('RS256', PEM, SigningInput, Signature, []).

	test(open_id_openssl_04, false) :-
		rsa_public_pem(PEM),
		es256_signature_fixture(Signature),
		open_id_openssl::verify('ES256', PEM, 'ignored', Signature, []).

	test(open_id_client_exchange_02, error(domain_error(open_id_session, missing(session([]), client_id)))) :-
		Provider = provider([token_endpoint('http://127.0.0.1:1/token')]),
		open_id::exchange_code(Provider, 'code', session([]), _Tokens, [allow_insecure_http(true)]).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		:- uses(list, [
			memberchk/2
		]).

		test(open_id_client_exchange_01, deterministic) :-
			with_test_server(exchange_code_assertions).

		test(open_id_discover_01, deterministic) :-
			with_test_server(discover_assertions).

		test(open_id_jwks_http_01, deterministic) :-
			with_test_server(jwks_assertions).

		test(open_id_client_exchange_03, deterministic) :-
			with_test_server(exchange_code_client_secret_post_assertions).

		test(open_id_client_exchange_04, deterministic) :-
			with_test_server(exchange_code_client_secret_basic_assertions).

		test(open_id_refresh_token_01, deterministic) :-
			with_test_server(refresh_token_assertions).

		test(open_id_userinfo_01, deterministic) :-
			with_test_server(userinfo_assertions).

		test(open_id_cached_jwks_01, deterministic) :-
			with_test_server(cached_jwks_assertions).

		test(open_id_verify_id_token_cached_01, deterministic) :-
			with_test_server(verify_id_token_cached_assertions, 2).

		exchange_code_assertions(Port) :-
			local_provider(Port, Provider),
			Session = session([client_id('client'), redirect_uri('http://127.0.0.1/cb'), code_verifier('dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk')]),
			open_id::exchange_code(Provider, 'code', Session, Tokens, [allow_insecure_http(true)]),
			Tokens = tokens(Properties),
			memberchk(access_token('access'), Properties),
			memberchk(token_type('Bearer'), Properties).

		exchange_code_client_secret_post_assertions(Port) :-
			local_provider(Port, Provider),
			Session = session([client_id('client'), redirect_uri('http://127.0.0.1/cb'), code_verifier('dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk')]),
			open_id::exchange_code(Provider, 'code', Session, Tokens, [allow_insecure_http(true), client_authentication(client_secret_post('secret'))]),
			Tokens = tokens(Properties),
			memberchk(access_token('post-access'), Properties).

		exchange_code_client_secret_basic_assertions(Port) :-
			local_provider(Port, Provider),
			Session = session([client_id('client'), redirect_uri('http://127.0.0.1/cb'), code_verifier('dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk')]),
			open_id::exchange_code(Provider, 'code', Session, Tokens, [allow_insecure_http(true), client_authentication(client_secret_basic('secret'))]),
			Tokens = tokens(Properties),
			memberchk(access_token('basic-access'), Properties).

		discover_assertions(Port) :-
			local_issuer(Port, Issuer),
			test_http_options(Options),
			open_id::discover(Issuer, Provider, Options),
			local_provider(Port, ExpectedProvider),
			ExpectedProvider = provider(ExpectedProperties),
			memberchk(issuer(Issuer), ExpectedProperties),
			open_id_discovery::property(Provider, issuer, Issuer),
			open_id_discovery::property(Provider, token_endpoint, TokenEndpoint),
			open_id_discovery::property(Provider, end_session_endpoint, EndSessionEndpoint),
			memberchk(token_endpoint(TokenEndpoint), ExpectedProperties),
			memberchk(end_session_endpoint(EndSessionEndpoint), ExpectedProperties).

		jwks_assertions(Port) :-
			local_provider(Port, Provider),
			test_http_options(Options),
			open_id::jwks(Provider, JWKSet, Options),
			JWKSet == {keys-[]}.

		refresh_token_assertions(Port) :-
			local_provider(Port, Provider),
			open_id::refresh_token(
				Provider,
				'refresh',
				Tokens,
				[
					allow_insecure_http(true),
					client_id('client'),
					client_authentication(client_secret_basic('secret')),
					scope([openid, profile])
				]
			),
			Tokens = tokens(Properties),
			memberchk(access_token('refreshed-access'), Properties),
			memberchk(refresh_token('rotated-refresh'), Properties),
			memberchk(scope('openid profile'), Properties).

		userinfo_assertions(Port) :-
			local_provider(Port, Provider),
			open_id::userinfo(Provider, 'access', Claims, [allow_insecure_http(true)]),
			Claims == {sub-'123', name-'Alice'}.

		cached_jwks_assertions(Port) :-
			local_jwks_uri(Port, JWKsURI),
			Provider = provider([issuer('https://issuer.example'), jwks_uri(JWKsURI)]),
			rsa_public_jwk_json('1', FirstKey),
			rsa_public_jwk_json('2', SecondKey),
			open_id_test_handler::reset_jwks_sequence,
			open_id_test_handler::set_jwks_sequence([{keys-[FirstKey]}, {keys-[SecondKey]}]),
			open_id_jwks_cache::clear,
			open_id::cached_jwks(Provider, FirstJWKSet, [allow_insecure_http(true), jwks_cache_ttl(3600)]),
			open_id::cached_jwks(Provider, SecondJWKSet, [allow_insecure_http(true), jwks_cache_ttl(3600)]),
			FirstJWKSet == {keys-[FirstKey]},
			SecondJWKSet == FirstJWKSet,
			open_id_test_handler::reset_jwks_sequence.

		verify_id_token_cached_assertions(Port) :-
			local_jwks_uri(Port, JWKsURI),
			Provider = provider([issuer('https://issuer.example'), jwks_uri(JWKsURI)]),
			rs256_token_rotated_kid(Token),
			rsa_public_jwk_json('1', FirstKey),
			rsa_public_jwk_json('2', SecondKey),
			open_id_test_handler::reset_jwks_sequence,
			open_id_test_handler::set_jwks_sequence([{keys-[FirstKey]}, {keys-[SecondKey]}]),
			open_id_jwks_cache::clear,
			open_id::verify_id_token(
				Token,
				Provider,
				Claims,
				[
					allow_insecure_http(true),
					expected_audience('client'),
					expected_nonce('nonce'),
					required_claims([custom]),
					now(1700000001)
				]
			),
			Claims == {
				iss-'https://issuer.example',
				aud-'client',
				azp-'client',
				nonce-'nonce',
				exp-4102444800,
				nbf-1700000000,
				iat-1700000000,
				custom-'ok',
				sub-subject
			},
			open_id_test_handler::reset_jwks_sequence.

		with_test_server(Goal) :-
			with_test_server(Goal, 1).

		with_test_server(Goal, Requests) :-
			http_socket_process::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(serve_requests(Requests, Listener), Tag),
			catch(
				call(Goal, Port),
				Error,
				(	threaded_exit(serve_requests(Requests, Listener), Tag),
					http_socket_process::close_listener(Listener),
					throw(Error)
				)
			),
			threaded_exit(serve_requests(Requests, Listener), Tag),
			http_socket_process::close_listener(Listener).

		serve_requests(0, _Listener) :-
			!.
		serve_requests(Requests, Listener) :-
			http_socket_process::serve_once(Listener, open_id_test_handler, _),
			NextRequests is Requests - 1,
			serve_requests(NextRequests, Listener).

		test_http_options([
			allow_insecure_http(true),
			headers([]),
			query([]),
			version(http(1, 1)),
			properties([]),
			client_authentication(none),
			clock_skew(0),
			now(1700000001),
			expected_audience('client'),
			expected_nonce('nonce'),
			required_claims([custom]),
			allow_algorithms(['RS256', 'ES256']),
			code_verifier('dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk'),
			openssl_executable(openssl),
			openssl_arguments([]),
			server_name('localhost'),
			connection_options([])
		]).

		local_issuer(Port, Issuer) :-
			atomic_list_concat(['http://127.0.0.1:', Port], Issuer).

		local_jwks_uri(Port, JWKsURI) :-
			local_issuer(Port, Issuer),
			atomic_list_concat([Issuer, '/jwks'], JWKsURI).

		local_provider(Port, provider([
			issuer(Issuer),
			authorization_endpoint(AuthorizationEndpoint),
			token_endpoint(TokenEndpoint),
			jwks_uri(JWKsURI),
			userinfo_endpoint(UserInfoEndpoint),
			end_session_endpoint(EndSessionEndpoint)
		])) :-
			atomic_list_concat(['http://127.0.0.1:', Port], Issuer),
			atomic_list_concat([Issuer, '/authorize'], AuthorizationEndpoint),
			atomic_list_concat([Issuer, '/token'], TokenEndpoint),
			atomic_list_concat([Issuer, '/jwks'], JWKsURI),
			atomic_list_concat([Issuer, '/userinfo'], UserInfoEndpoint),
			atomic_list_concat([Issuer, '/logout'], EndSessionEndpoint).

	:- endif.

	rsa_public_jwk({
		kty-'RSA',
		kid-'1',
		alg-'RS256',
		use-sig,
		n-'zuxofQx-whGosAeo0SD9BUVEKoglhV9gZGZwI7VlguUL-RPGgBWrsJchWjubbOlVvXmRc4kC02yOgff0i0nubqyOEnk0YIxL6mw-Egb4TkwowBatzdfE38BgPmc9GKpm4Gws_70LLcn79Bfyx509jU50AJ7UOEuSBSeeW-a5xX1DMZ46wXKa1jbNk3JvilRyIGMUPrFo-EEVDxJ_OoAEUY9bkKqhiBHjvzz-XMJbNttJ6K4vV6mf0a6kxeH9fqjBY1UnDzfYyYR-47rheDjv60VleiYzvkeQl04vDbfgHTwg6VAlG8RMeEkPnbiFqhNVAH2rtbuNnsJkd71HyAY8dw',
		e-'AQAB'
	}).

	rsa_public_jwk_json(KeyId, {
		kty-'RSA',
		kid-KeyId,
		alg-'RS256',
		use-sig,
		n-'zuxofQx-whGosAeo0SD9BUVEKoglhV9gZGZwI7VlguUL-RPGgBWrsJchWjubbOlVvXmRc4kC02yOgff0i0nubqyOEnk0YIxL6mw-Egb4TkwowBatzdfE38BgPmc9GKpm4Gws_70LLcn79Bfyx509jU50AJ7UOEuSBSeeW-a5xX1DMZ46wXKa1jbNk3JvilRyIGMUPrFo-EEVDxJ_OoAEUY9bkKqhiBHjvzz-XMJbNttJ6K4vV6mf0a6kxeH9fqjBY1UnDzfYyYR-47rheDjv60VleiYzvkeQl04vDbfgHTwg6VAlG8RMeEkPnbiFqhNVAH2rtbuNnsJkd71HyAY8dw',
		e-'AQAB'
	}).

	rsa_jwk_set({keys-[Key]}) :-
		rsa_public_jwk(Key).

	rsa_public_pem(PEM) :-
		atomic_list_concat([
			'-----BEGIN PUBLIC KEY-----\n',
			'MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAzuxofQx+whGosAeo0SD9\n',
			'BUVEKoglhV9gZGZwI7VlguUL+RPGgBWrsJchWjubbOlVvXmRc4kC02yOgff0i0nu\n',
			'bqyOEnk0YIxL6mw+Egb4TkwowBatzdfE38BgPmc9GKpm4Gws/70LLcn79Bfyx509\n',
			'jU50AJ7UOEuSBSeeW+a5xX1DMZ46wXKa1jbNk3JvilRyIGMUPrFo+EEVDxJ/OoAE\n',
			'UY9bkKqhiBHjvzz+XMJbNttJ6K4vV6mf0a6kxeH9fqjBY1UnDzfYyYR+47rheDjv\n',
			'60VleiYzvkeQl04vDbfgHTwg6VAlG8RMeEkPnbiFqhNVAH2rtbuNnsJkd71HyAY8\n',
			'dwIDAQAB\n',
			'-----END PUBLIC KEY-----\n'
		], PEM).

	rs256_token('eyJhbGciOiJSUzI1NiIsImtpZCI6IjEifQ.eyJpc3MiOiJodHRwczovL2lzc3Vlci5leGFtcGxlIiwiYXVkIjoiY2xpZW50IiwiYXpwIjoiY2xpZW50Iiwibm9uY2UiOiJub25jZSIsImV4cCI6NDEwMjQ0NDgwMCwibmJmIjoxNzAwMDAwMDAwLCJpYXQiOjE3MDAwMDAwMDAsImN1c3RvbSI6Im9rIiwic3ViIjoic3ViamVjdCJ9.pcCYUqt6wdrfGyAJqPOMK9ztPQHLeY8ibSzCAzBPUIR--83payXy0n6EWtvcdROf_Ajnp20hZ8o5K8DdJIrIqczJcB5gYEwsLviXXq1TwAlPQlHNloiJwjgJ-g3E7xSJr81W03FZUQkHdnzDmX6KQa7EoCUGM9rd335QC0uMmct73MNiVU0q6Mq976OaGZMmqt3KfKDkUecJQ5et8RAyJN3yr9FqmqM7cydEY8x67EMdQpq-VRMRbtS9fnr_JvoUfEG39qkCiZy-8TjGIuRFBueaBY6InfGD3cU-U7e9VB8-hVpDlZ5v2OCT5tTgkN0WwBqG7aFrPYf785fCKrY4wQ').

	rs256_token_list_aud('eyJhbGciOiJSUzI1NiIsImtpZCI6IjEifQ.eyJpc3MiOiJodHRwczovL2lzc3Vlci5leGFtcGxlIiwiYXVkIjpbImNsaWVudCIsIm90aGVyIl0sImF6cCI6ImNsaWVudCIsIm5vbmNlIjoibm9uY2UiLCJleHAiOjQxMDI0NDQ4MDAsIm5iZiI6MTcwMDAwMDAwMCwiaWF0IjoxNzAwMDAwMDAwLCJjdXN0b20iOiJvayIsInN1YiI6InN1YmplY3QifQ.EasklgA9a3YWYrLV88eC6VOrxqanWEDGWFQ0cYqXym3IK4bfE4ZB9-i-Qlw7KsqFZ3UG64ZLMJlLTn4XBrXnIgY1nQ1FsUyrwZky52A85JV1UHnQGLtciFAmKkJeZopcGHVB7he3rS4DHCpYjNi1GAKG9QARH4qjaJu50ekGKs0SZKhfOg7aq-tu7YZY0yvdjcU3zk62FALYIL_wd1-De_1QHw0YEo5omdGGa0ZlLs4L4sWcLvnfXnGXzNnsQgZkPZIjhfvZOJBG_s11H2qodt6XU5Xv3RIX3WfD0wCQScmOFkXcxyeMrkWwGpQt5W7oXyEc_9Ji_32fazI7nvtTyA').

	rs256_token_missing_sub('eyJhbGciOiJSUzI1NiIsImtpZCI6IjEifQ.eyJpc3MiOiJodHRwczovL2lzc3Vlci5leGFtcGxlIiwiYXVkIjoiY2xpZW50IiwiYXpwIjoiY2xpZW50Iiwibm9uY2UiOiJub25jZSIsImV4cCI6NDEwMjQ0NDgwMCwibmJmIjoxNzAwMDAwMDAwLCJpYXQiOjE3MDAwMDAwMDAsImN1c3RvbSI6Im9rIn0.OXskQlx3GGf9CKBJSCmsUR25VYYs8bj0v0UEVejkLdJ33QmnowrkF4CMNcqpehZ_x8G7Mdad974ko4y3WmTCAs8cLuNMhal59Gu4HSnkn8Qo0JSbU4Bh6BO2ZqSDILFDEMRtEQdmr4QtvAeRMhMou2aLSGepGAn-Kh_Zc30lLUqc1-Uz5rlGdsp0pvncublIp1bOFWF3_k2qDUZACcWZcD_4qurRLaJ2MtZdrvikPsWjZKpcigy7PAR5Eq_3AvKMf2D3YZQ6Mi8HKeO-igfeSgFqw3eSrKcc8tCiWYrJKfRHF9blW1Ho_Lsbc2WlSnDDgNvrcZWUppUCwvN7SaMuiA').

	rs256_token_missing_iat('eyJhbGciOiJSUzI1NiIsImtpZCI6IjEifQ.eyJpc3MiOiJodHRwczovL2lzc3Vlci5leGFtcGxlIiwiYXVkIjoiY2xpZW50IiwiYXpwIjoiY2xpZW50Iiwibm9uY2UiOiJub25jZSIsImV4cCI6NDEwMjQ0NDgwMCwibmJmIjoxNzAwMDAwMDAwLCJjdXN0b20iOiJvayIsInN1YiI6InN1YmplY3QifQ.dDQRQLb3ItYrn6T6e88f25p6JZz2ORzcTuH23d9YMdz63bYechIRmZEgAKCdwptrMaH0c1hCSMwxOqSrg-vRhm9lDXhjeReUcAJxHXerBsap9MkoBbVcMq8O9LuOSaM__xKRjoENQZA3wju0F3iINliK6zal05N1dZZlf2Z9asI_QFUCjiAaqz8-ijcPyzFes1EutTXoZL5sHpcnZfweuocj4n5iVQZJHo3-wAkfEQrr0iPx3o_TXo7yGBaxK8wGf-CaSMr8A_0t6_c_BxwsJe23X-DYJWMc3IRiMC9-OmeG6Jfpqw7G3Z5F8fezPQ1bH_3Dl7va3DWzVOYSIJti2w').

	rs256_token_future_iat('eyJhbGciOiJSUzI1NiIsImtpZCI6IjEifQ.eyJpc3MiOiJodHRwczovL2lzc3Vlci5leGFtcGxlIiwiYXVkIjoiY2xpZW50IiwiYXpwIjoiY2xpZW50Iiwibm9uY2UiOiJub25jZSIsImV4cCI6NDEwMjQ0NDgwMCwibmJmIjoxNzAwMDAwMDAwLCJpYXQiOjE3MDAwMDA5MDAsImN1c3RvbSI6Im9rIiwic3ViIjoic3ViamVjdCJ9.qauuW0gtBpM4xOH_0hZMHNMKUOtSm6RXMuSVFA8SjFOZd4sX16aNf_rVMmJVT7lZ3PjjHNTca0Fsz3RLE2f51jxRXWzzyN9uitvBCEdoMVRAfwz63K__zCTK9TRb6kYbXFCUMs-kQbUTcVBJ65zH3hakDOrwYpBg-KxtiGVW8RggxqTpnfM-4niPzpiGtJlZWnT7qJILNFTq45Rar7Iz1ejcoSlbn1X7y55h7UxfGIxsoXzX3_ybuB5cs7aMKO_sNc5tLOnbvko2qbdKB_wRUvOwLTW8NK-RWyIlirLFH-4IMO-deHBULeaAtPKZFDGP5vmb0cI-Vpc5xTFfkGdKLg').

	rs256_token_rotated_kid('eyJhbGciOiJSUzI1NiIsImtpZCI6IjIifQ.eyJpc3MiOiJodHRwczovL2lzc3Vlci5leGFtcGxlIiwiYXVkIjoiY2xpZW50IiwiYXpwIjoiY2xpZW50Iiwibm9uY2UiOiJub25jZSIsImV4cCI6NDEwMjQ0NDgwMCwibmJmIjoxNzAwMDAwMDAwLCJpYXQiOjE3MDAwMDAwMDAsImN1c3RvbSI6Im9rIiwic3ViIjoic3ViamVjdCJ9.FhvoWwbJTqnLzEil4h0sRu3ywbUcWyuGhqbQV4MKQSIrYA3akcCMFgGbTDFWkYXth7yRyfa56brjbM5sualRhbAs2HoeWGxzA5g3Jhx0S5Le1K-zWJMjeOaYf1jhFG0Y86wU8cy-dSBYUfswxEv_texXKdPUne07bEsPV0HRFbjFCPjFePcmqLXdjFpmgW1WlFInb7hizVbYZhQBnlb6uucLjECUFV-4D39PdtVrcTDm8rw_qNwEEVYBmYbY7vbHzApv-8LaMRhsu_YdS07ZRfdEorPlRWohpnltwXhPpA8gXYoPfoqHZ1yB0WgQrw6g0AgspgmX7_C3ZcpNJCm8iw').

	jwt_signing_input_signature(Token, SigningInput, Signature) :-
		atom::split(Token, '.', [Header, Payload, SignatureBase64URL]),
		atom_concat(Header, '.', HeaderDot),
		atom_concat(HeaderDot, Payload, SigningInput),
		atom_codes(SignatureBase64URL, SignatureCodes),
		base64url_no_padding::parse(codes(SignatureCodes), Signature).

	alter_signature([Byte| Bytes], [AlteredByte| Bytes]) :-
		AlteredByte is (Byte + 1) mod 256.

	es256_signature_fixture(Signature) :-
		RBytes = [0, 129, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
		SBytes = [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
		append(RBytes, SBytes, Signature).

:- end_object.
