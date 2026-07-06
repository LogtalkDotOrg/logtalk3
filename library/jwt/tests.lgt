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
		comment is 'Unit tests for the "jwt" library.'
	]).

	:- uses(user, [
		atomic_list_concat/2, atomic_list_concat/3
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

	test(jwt_verify_hs256_01, deterministic(Claims == {sub-'1234567890', name-'John Doe', iat-1516239022})) :-
		hs256_rfc_key(Key),
		hs256_token_from_json('{"alg":"HS256","typ":"JWT"}', '{"sub":"1234567890","name":"John Doe","iat":1516239022}', Key, Token),
		jwt::verify(Token, Key, Claims, [allow_missing_exp(true)]).

	test(jwt_verify_hs256_02, fail) :-
		hs256_rfc_key(SigningKey),
		hs256_token_from_json('{"alg":"HS256","typ":"JWT"}', '{"sub":"1234567890","name":"John Doe","iat":1516239022}', SigningKey, Token),
		hs256_wrong_key(VerificationKey),
		jwt::verify(Token, VerificationKey, _Claims, [allow_missing_exp(true)]).

	test(jwt_verify_hs256_03, error(domain_error(jwt_claims, missing(exp)))) :-
		hs256_rfc_key(Key),
		hs256_token_from_json('{"alg":"HS256","typ":"JWT"}', '{"sub":"1234567890","name":"John Doe","iat":1516239022}', Key, Token),
		jwt::verify(Token, Key, _Claims, []).

	test(jwt_verify_algorithm_01, error(domain_error(jwt_algorithm, 'HS256'))) :-
		hs256_rfc_key(Key),
		hs256_token_from_json('{"alg":"HS256","typ":"JWT"}', '{"sub":"1234567890","name":"John Doe","iat":1516239022}', Key, Token),
		jwt::verify(Token, Key, _Claims, [allow_missing_exp(true), algorithm('RS256')]).

	test(jwt_verify_hs256_short_key_01, error(domain_error(jwt_symmetric_key, short))) :-
		hs256_token(Token),
		jwt::verify(Token, short, _Claims, [allow_missing_exp(true)]).

	test(jwt_verify_crit_01, error(domain_error(jwt_header, _))) :-
		hs256_key(Key),
		hs256_token_from_json('{"alg":"HS256","crit":["foo"],"foo":1}', '{"sub":"123","exp":4102444800}', Key, Token),
		jwt::verify(Token, Key, _Claims, [now(1700000001)]).

	test(jwt_verify_crit_02, error(domain_error(jwt_header, _))) :-
		hs256_key(Key),
		hs256_token_from_json('{"alg":"HS256","crit":[]}', '{"sub":"123","exp":4102444800}', Key, Token),
		jwt::verify(Token, Key, _Claims, [now(1700000001)]).

	test(jwt_verify_duplicate_header_01, error(domain_error(jwt_json_object, _))) :-
		hs256_key(Key),
		hs256_token_from_json('{"alg":"HS256","alg":"HS256"}', '{"sub":"123","exp":4102444800}', Key, Token),
		jwt::verify(Token, Key, _Claims, [now(1700000001)]).

	test(jwt_verify_duplicate_claims_01, error(domain_error(jwt_json_object, _))) :-
		hs256_key(Key),
		hs256_token_from_json('{"alg":"HS256"}', '{"sub":"123","sub":"456","exp":4102444800}', Key, Token),
		jwt::verify(Token, Key, _Claims, [now(1700000001)]).

	test(jwt_verify_claims_object_01, error(domain_error(jwt_json_object, _))) :-
		hs256_key(Key),
		hs256_token_from_json('{"alg":"HS256"}', '[1,2,3]', Key, Token),
		jwt::verify(Token, Key, _Claims, [now(1700000001), allow_missing_exp(true)]).

	test(jwt_sign_hs256_01, deterministic(Claims == {sub-'123', exp-4102444800})) :-
		hs256_key(Key),
		jwt::sign({alg-'HS256', typ-'JWT'}, {sub-'123', exp-4102444800}, Key, Token, []),
		jwt::verify(Token, Key, Claims, [now(1700000001)]).

	test(jwt_sign_hs256_short_key_01, error(domain_error(jwt_symmetric_key, short))) :-
		jwt::sign({alg-'HS256', typ-'JWT'}, {sub-'123', exp-4102444800}, short, _Token, []).

	test(jwt_sign_crit_01, error(domain_error(jwt_header, _))) :-
		hs256_key(Key),
		jwt::sign({alg-'HS256', crit-[foo], foo-1}, {sub-'123', exp-4102444800}, Key, _Token, []).

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

	test(jwt_jwks_01, deterministic(Key == {kty-'oct', kid-'1', alg-'HS256', k-'MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY'})) :-
		JWKSet = {keys-[{kty-'oct', kid-'1', alg-'HS256', k-'MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY'}]},
		Header = {alg-'HS256', kid-'1'},
		jwt_jwks::select_key(JWKSet, Header, Key).

	test(jwt_jwks_02, deterministic(Claims == {sub-'abc', exp-4102444800})) :-
		hs256_key(Key),
		hs256_oct_key(EncodedKey),
		jwt::sign({alg-'HS256', typ-'JWT'}, {sub-'abc', exp-4102444800}, Key, Token, []),
		JWKSet = {keys-[{kty-'oct', k-'MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZg'}, {kty-'oct', k-EncodedKey}]},
		jwt::verify(Token, JWKSet, Claims, [now(1700000001)]).

	test(jwt_jwk_oct_01, error(domain_error(jwt_symmetric_key, _))) :-
		jwt_jwk::symmetric_key_bytes({kty-'oct'}, _Bytes).

	test(jwt_jwk_oct_02, error(domain_error(jwt_symmetric_key, _))) :-
		jwt_jwk::symmetric_key_bytes({kty-'oct', k-'c2hvcnQ'}, _Bytes).

	test(jwt_jwk_bytes_01, error(domain_error(jwt_symmetric_key, bytes([1,2,3])))) :-
		jwt_jwk::symmetric_key_bytes(bytes([1,2,3]), _Bytes).

	test(jwt_jwk_rsa_01, error(domain_error(jwt_jwk_public_key, _))) :-
		jwt_jwk::public_key_pem({kty-'RSA', n-'AQAB'}, _PEM).

	test(jwt_jwk_rsa_02, error(domain_error(jwt_jwk_public_key, _))) :-
		valid_rsa_modulus_bytes(NBytes),
		NBytes = [First| _],
		rsa_jwk_from_bytes([0, First| NBytes], [1,0,1], Key),
		jwt_jwk::public_key_pem(Key, _PEM).

	test(jwt_jwk_rsa_03, error(domain_error(jwt_jwk_public_key, _))) :-
		undersized_rsa_modulus_bytes(NBytes),
		rsa_jwk_from_bytes(NBytes, [1,0,1], Key),
		jwt_jwk::public_key_pem(Key, _PEM).

	test(jwt_jwk_rsa_04, error(domain_error(jwt_jwk_public_key, _))) :-
		even_rsa_modulus_bytes(NBytes),
		rsa_jwk_from_bytes(NBytes, [1,0,1], Key),
		jwt_jwk::public_key_pem(Key, _PEM).

	test(jwt_jwk_rsa_05, error(domain_error(jwt_jwk_public_key, _))) :-
		valid_rsa_modulus_bytes(NBytes),
		rsa_jwk_from_bytes(NBytes, [2], Key),
		jwt_jwk::public_key_pem(Key, _PEM).

	test(jwt_jwk_rsa_06, error(domain_error(jwt_jwk_public_key, _))) :-
		valid_rsa_modulus_bytes(NBytes),
		rsa_jwk_from_bytes(NBytes, NBytes, Key),
		jwt_jwk::public_key_pem(Key, _PEM).

	test(jwt_jwk_ec_01, error(domain_error(jwt_jwk_public_key, _))) :-
		jwt_jwk::public_key_pem({kty-'EC', crv-'P-256', x-'AQAB', y-'AQAB'}, _PEM).

	test(jwt_jwk_ec_02, error(domain_error(jwt_jwk_public_key, _))) :-
		repeated_byte(32, 0, Zeros),
		ec_jwk_from_bytes(Zeros, Zeros, Key),
		jwt_jwk::public_key_pem(Key, _PEM).

	test(jwt_jwk_ec_03, error(domain_error(jwt_jwk_public_key, _))) :-
		repeated_byte(32, 255, Bytes),
		ec_jwk_from_bytes(Bytes, Bytes, Key),
		jwt_jwk::public_key_pem(Key, _PEM).

	test(jwt_jwk_ec_04, error(domain_error(jwt_jwk_public_key, _))) :-
		one_32_bytes(One),
		ec_jwk_from_bytes(One, One, Key),
		jwt_jwk::public_key_pem(Key, _PEM).

	test(jwt_jwk_ec_05, deterministic) :-
		p256_generator_jwk(Key),
		jwt_jwk::public_key_pem(Key, PEM),
		atom_concat('-----BEGIN PUBLIC KEY-----\n', _, PEM).

	test(jwt_der_01, deterministic) :-
		rsa_public_jwk(Key),
		jwt_jwk::public_key_pem(Key, PEM),
		atom_concat('-----BEGIN PUBLIC KEY-----\n', BodyAndFooter, PEM),
		atom_concat(Body, '-----END PUBLIC KEY-----\n', BodyAndFooter),
		atom_concat('MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAzuxofQx+whGosAeo0SD9', _, Body).

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

	hs256_rfc_key('your-256-bit-secretyour-256-bit-secret').

	hs256_key('0123456789abcdef0123456789abcdef').

	hs256_wrong_key('abcdef0123456789abcdef0123456789').

	hs256_oct_key('MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY').

	hs256_token_from_json(HeaderJSON, ClaimsJSON, Key, Token) :-
		atom_codes(HeaderJSON, HeaderCodes),
		base64url_no_padding::generate(codes(HeaderSegmentCodes), HeaderCodes),
		atom_codes(HeaderSegment, HeaderSegmentCodes),
		atom_codes(ClaimsJSON, ClaimsCodes),
		base64url_no_padding::generate(codes(ClaimsSegmentCodes), ClaimsCodes),
		atom_codes(ClaimsSegment, ClaimsSegmentCodes),
		atomic_list_concat([HeaderSegment, ClaimsSegment], '.', SigningInput),
		atom_codes(Key, KeyBytes),
		atom_codes(SigningInput, MessageBytes),
		hmac::digest(sha256, KeyBytes, MessageBytes, Signature),
		base64url_no_padding::generate(codes(SignatureSegmentCodes), Signature),
		atom_codes(SignatureSegment, SignatureSegmentCodes),
		atomic_list_concat([SigningInput, SignatureSegment], '.', Token).

	rsa_public_jwk({
		kty-'RSA',
		kid-'1',
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

	rs256_token('eyJhbGciOiJSUzI1NiIsImtpZCI6IjEifQ.eyJpc3MiOiJodHRwczovL2lzc3Vlci5leGFtcGxlIiwiYXVkIjoiY2xpZW50IiwiYXpwIjoiY2xpZW50Iiwibm9uY2UiOiJub25jZSIsImV4cCI6NDEwMjQ0NDgwMCwibmJmIjoxNzAwMDAwMDAwLCJpYXQiOjE3MDAwMDAwMDAsImN1c3RvbSI6Im9rIn0.OXskQlx3GGf9CKBJSCmsUR25VYYs8bj0v0UEVejkLdJ33QmnowrkF4CMNcqpehZ_x8G7Mdad974ko4y3WmTCAs8cLuNMhal59Gu4HSnkn8Qo0JSbU4Bh6BO2ZqSDILFDEMRtEQdmr4QtvAeRMhMou2aLSGepGAn-Kh_Zc30lLUqc1-Uz5rlGdsp0pvncublIp1bOFWF3_k2qDUZACcWZcD_4qurRLaJ2MtZdrvikPsWjZKpcigy7PAR5Eq_3AvKMf2D3YZQ6Mi8HKeO-igfeSgFqw3eSrKcc8tCiWYrJKfRHF9blW1Ho_Lsbc2WlSnDDgNvrcZWUppUCwvN7SaMuiA').

	rsa_jwk_from_bytes(NBytes, EBytes, {kty-'RSA', n-N, e-E}) :-
		bytes_base64url_atom(NBytes, N),
		bytes_base64url_atom(EBytes, E).

	ec_jwk_from_bytes(XBytes, YBytes, {kty-'EC', crv-'P-256', x-X, y-Y}) :-
		bytes_base64url_atom(XBytes, X),
		bytes_base64url_atom(YBytes, Y).

	bytes_base64url_atom(Bytes, Atom) :-
		base64url_no_padding::generate(codes(Codes), Bytes),
		atom_codes(Atom, Codes).

	valid_rsa_modulus_bytes([128| Bytes]) :-
		bytes_with_last(254, 0, 1, Bytes).

	even_rsa_modulus_bytes([128| Bytes]) :-
		bytes_with_last(254, 0, 2, Bytes).

	undersized_rsa_modulus_bytes([128| Bytes]) :-
		bytes_with_last(253, 0, 1, Bytes).

	one_32_bytes(Bytes) :-
		bytes_with_last(31, 0, 1, Bytes).

	bytes_with_last(0, _Byte, Last, [Last]) :-
		!.
	bytes_with_last(Count, Byte, Last, [Byte| Bytes]) :-
		Next is Count - 1,
		bytes_with_last(Next, Byte, Last, Bytes).

	repeated_byte(0, _Byte, []) :-
		!.
	repeated_byte(Count, Byte, [Byte| Bytes]) :-
		Next is Count - 1,
		repeated_byte(Next, Byte, Bytes).

	p256_generator_jwk({
		kty-'EC',
		crv-'P-256',
		x-'axfR8uEsQkf4vOblY6RA8ncDfYEt6zOg9KE5RdiYwpY',
		y-'T-NC4v4af5uO5-tKfA-eFivOM1drMV7Oy7ZAaDe_UfU'
	}).

:- end_object.
