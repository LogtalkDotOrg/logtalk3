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
		date is 2026-06-26,
		comment is 'Unit tests for the "jwt" library.'
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	cover(jwt).
	cover(jwt_claims).
	cover(jwt_compact).
	cover(jwt_der).
	cover(jwt_jwa).
	cover(jwt_jwk).
	cover(jwt_jwks).
	cover(jwt_jws).
	cover(jwt_openssl).

	test(jwt_decode_01, deterministic) :-
		hs256_token(Token),
		jwt::decode(Token, Header, Claims),
		^^assertion(Header == {alg-'HS256', typ-'JWT'}),
		^^assertion(Claims == {sub-'1234567890', name-'John Doe', iat-1516239022}).

	test(jwt_decode_02, error(domain_error(jwt_compact_serialization, 'abc.def'))) :-
		jwt::decode('abc.def', _Header, _Claims).

	test(jwt_peek_01, deterministic(Algorithm == 'HS256')) :-
		hs256_token(Token),
		jwt::peek_algorithm(Token, Algorithm).

	test(jwt_verify_hs256_01, deterministic) :-
		hs256_token(Token),
		jwt::verify(Token, 'your-256-bit-secret', Claims, [allow_missing_exp(true)]),
		^^assertion(Claims == {sub-'1234567890', name-'John Doe', iat-1516239022}).

	test(jwt_verify_hs256_02, fail) :-
		hs256_token(Token),
		jwt::verify(Token, 'wrong-secret', _Claims, [allow_missing_exp(true)]).

	test(jwt_sign_hs256_01, deterministic) :-
		jwt::sign({alg-'HS256', typ-'JWT'}, {sub-'123', exp-4102444800}, 'secret', Token, []),
		jwt::verify(Token, 'secret', Claims, [now(1700000001)]),
		^^assertion(Claims == {sub-'123', exp-4102444800}).

	test(jwt_claims_01, deterministic) :-
		Claims = {iss-'https://issuer.example', aud-['client', 'other'], exp-4102444800, iat-1700000000},
		Policy = [
			claim(iss, expected('https://issuer.example')),
			claim(aud, contains('client')),
			claim(iat, time(issued_at))
		],
		jwt::validate_claims(Claims, Policy, [now(1700000001)]).

	test(jwt_claims_02, error(domain_error(jwt_claim(exp), 10))) :-
		jwt::validate_claims({exp-10}, [], [now(1700000001), clock_skew(0)]).

	test(jwt_jwks_01, deterministic(Key == {kty-'oct', kid-'1', alg-'HS256', k-'c2VjcmV0'})) :-
		JWKSet = {keys-[{kty-'oct', kid-'1', alg-'HS256', k-'c2VjcmV0'}]},
		Header = {alg-'HS256', kid-'1'},
		jwt_jwks::select_key(JWKSet, Header, Key).

	test(jwt_der_01, deterministic) :-
		rsa_public_jwk(Key),
		jwt_jwk::public_key_pem(Key, PEM),
		atom_concat('-----BEGIN PUBLIC KEY-----\n', BodyAndFooter, PEM),
		atom_concat(Body, '-----END PUBLIC KEY-----\n', BodyAndFooter),
		atom_concat('MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDcpM/qWCUyI5TsO8n8LQkIXDLG', _, Body).

	test(jwt_verify_rs256_01, deterministic) :-
		rsa_jwk_set(JWKSet),
		rs256_token(Token),
		jwt::verify(Token, JWKSet, Claims, [now(1700000001), claim_policy([claim(iss, expected('https://issuer.example')), claim(aud, contains('client'))])]),
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

	test(jwt_openssl_01, error(existence_error(os_command, '__missing_jwt_test_command__'))) :-
		rsa_public_pem(PEM),
		jwt_openssl::verify('RS256', PEM, 'ignored', [0], [openssl_executable('__missing_jwt_test_command__')]).

	hs256_token('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c').

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

:- end_object.
