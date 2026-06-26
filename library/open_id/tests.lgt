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
		date is 2026-06-25,
		comment is 'Unit tests for the "open_id" library.'
	]).

	:- uses(http_core, [
		body/2, status/2
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
	cover(open_id_jwt).
	cover(open_id_jwks).
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

	test(open_id_discovery_01, deterministic) :-
		JSON = {
			issuer-'https://issuer.example',
			authorization_endpoint-'https://issuer.example/authorize',
			token_endpoint-'https://issuer.example/token',
			jwks_uri-'https://issuer.example/jwks'
		},
		open_id_discovery::provider('https://issuer.example', JSON, Provider),
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
		atom_concat('MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDcpM/qWCUyI5TsO8n8LQkIXDLG', _, Body).

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
			custom-'ok'
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
			custom-'ok'
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
			custom-'ok'
		}).

	test(open_id_openssl_01, error(existence_error(os_command, '__missing_open_id_test_command__'))) :-
		rsa_public_pem(PEM),
		open_id_openssl::verify('RS256', PEM, 'ignored', [0], [openssl_executable('__missing_open_id_test_command__')]).

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

		exchange_code_assertions(Port) :-
			local_provider(Port, Provider),
			Session = session([client_id('client'), redirect_uri('http://127.0.0.1/cb'), code_verifier('dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk')]),
			open_id::exchange_code(Provider, 'code', Session, Tokens, [allow_insecure_http(true)]),
			Tokens = tokens(Properties),
			memberchk(access_token('access'), Properties),
			memberchk(token_type('Bearer'), Properties).

		discover_assertions(Port) :-
			local_issuer(Port, Issuer),
			test_http_options(Options),
			open_id::discover(Issuer, Provider, Options),
			local_provider(Port, ExpectedProvider),
			ExpectedProvider = provider(ExpectedProperties),
			memberchk(issuer(Issuer), ExpectedProperties),
			open_id_discovery::property(Provider, issuer, Issuer),
			open_id_discovery::property(Provider, token_endpoint, TokenEndpoint),
			memberchk(token_endpoint(TokenEndpoint), ExpectedProperties).

		jwks_assertions(Port) :-
			local_provider(Port, Provider),
			test_http_options(Options),
			open_id::jwks(Provider, JWKSet, Options),
			JWKSet == {keys-[]}.

		with_test_server(Goal) :-
			http_socket_process::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(http_socket_process::serve_once(Listener, open_id_test_handler, _), Tag),
			catch(
				call(Goal, Port),
				Error,
				(	threaded_exit(http_socket_process::serve_once(Listener, open_id_test_handler, _), Tag),
					http_socket_process::close_listener(Listener),
					throw(Error)
				)
			),
			threaded_exit(http_socket_process::serve_once(Listener, open_id_test_handler, _), Tag),
			http_socket_process::close_listener(Listener).

		test_http_options([
			allow_insecure_http(true),
			headers([]),
			query([]),
			version(http(1, 1)),
			properties([]),
			client_secret_post('secret'),
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

		local_provider(Port, provider([
			issuer(Issuer),
			authorization_endpoint(AuthorizationEndpoint),
			token_endpoint(TokenEndpoint),
			jwks_uri(JWKsURI)
		])) :-
			atomic_list_concat(['http://127.0.0.1:', Port], Issuer),
			atomic_list_concat([Issuer, '/authorize'], AuthorizationEndpoint),
			atomic_list_concat([Issuer, '/token'], TokenEndpoint),
			atomic_list_concat([Issuer, '/jwks'], JWKsURI).

	:- endif.

	rsa_public_jwk({
		kty-'RSA',
		kid-'1',
		alg-'RS256',
		use-sig,
		n-'3KTP6lglMiOU7DvJ_C0JCFwyxiIhzK5hJPIIdY78JkBKv9b6MuVH3cJH66aAvd-O_o0DiKX3WvNyYqisR12-kku4Fa_Z7aVKggm5Gn3m8M693raCIq_8IJ3pOYE8CE3scpX-X8AQV4y_AuMz3yZqGMv1zL1PHbYcqwNUPjhvifU',
		e-'AQAB'
	}).

	rsa_jwk_set({keys-[Key]}) :-
		rsa_public_jwk(Key).

	rsa_public_pem(PEM) :-
		atomic_list_concat([
			'-----BEGIN PUBLIC KEY-----\n',
			'MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDcpM/qWCUyI5TsO8n8LQkIXDLG\n',
			'IiHMrmEk8gh1jvwmQEq/1voy5UfdwkfrpoC9347+jQOIpfda83JiqKxHXb6SS7gV\n',
			'r9ntpUqCCbkafebwzr3etoIir/wgnek5gTwITexylf5fwBBXjL8C4zPfJmoYy/XM\n',
			'vU8dthyrA1Q+OG+J9QIDAQAB\n',
			'-----END PUBLIC KEY-----\n'
		], PEM).

	rs256_token('eyJhbGciOiJSUzI1NiIsImtpZCI6IjEifQ.eyJpc3MiOiJodHRwczovL2lzc3Vlci5leGFtcGxlIiwiYXVkIjoiY2xpZW50IiwiYXpwIjoiY2xpZW50Iiwibm9uY2UiOiJub25jZSIsImV4cCI6NDEwMjQ0NDgwMCwibmJmIjoxNzAwMDAwMDAwLCJpYXQiOjE3MDAwMDAwMDAsImN1c3RvbSI6Im9rIn0.jvn02BCxAcaTHQ2i-dofp8-EvgVaNi0FeRnqODN-8b0KVsIpLaqYrvJ2LBE5whwMea38fPPmU9cjDzbVgwGX5mo45_5J9QL7qgoENTWSjvX8BqVML_7YkQeHDWF-K5Zo8yGQpoT9c4bVhrnbww1_1A5KJKjEDZvLZdftsxY44l4').

	rs256_token_list_aud('eyJhbGciOiJSUzI1NiIsImtpZCI6IjEifQ.eyJpc3MiOiJodHRwczovL2lzc3Vlci5leGFtcGxlIiwiYXVkIjpbImNsaWVudCIsIm90aGVyIl0sImF6cCI6ImNsaWVudCIsIm5vbmNlIjoibm9uY2UiLCJleHAiOjQxMDI0NDQ4MDAsIm5iZiI6MTcwMDAwMDAwMCwiaWF0IjoxNzAwMDAwMDAwLCJjdXN0b20iOiJvayJ9.sp7ClbtG3Yd_3lKdaUYqbD3QRlxdBkJWQgcsgJLa6FT7wNLFqwD6mFVgrzIgZwtE624kDZw8u2z3jKxmC-PlcpUy8vp-eiOHTiKm879vlueOp9dKyA3YN1qmstpzEsC-3P_6637NjU7G8KJoQH7dYo-RLoCUpVCHNJiOjh03ScQ').

	es256_signature_fixture(Signature) :-
		RBytes = [0, 129, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
		SBytes = [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
		append(RBytes, SBytes, Signature).

:- end_object.
