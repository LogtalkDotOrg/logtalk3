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


:- object(paseto_v4_test_driver,
	extends(paseto_v4)).

	:- public(deterministic_local_encrypt/6).
	:- mode(deterministic_local_encrypt(+list(byte), +list(byte), +list(byte), +list(byte), +list(byte), -atom), one_or_error).
	:- info(deterministic_local_encrypt/6, [
		comment is 'Calls the protected explicit-nonce encryption predicate for conformance tests.',
		argnames is ['Key', 'Nonce', 'Payload', 'Footer', 'ImplicitAssertion', 'Token']
	]).

	deterministic_local_encrypt(Key, Nonce, Payload, Footer, ImplicitAssertion, Token) :-
		^^local_encrypt_with_nonce(Key, Nonce, Payload, Footer, ImplicitAssertion, Token).

	:- public(test_pae/2).
	:- mode(test_pae(+list(list(byte)), -list(byte)), one).
	:- info(test_pae/2, [
		comment is 'Exposes PAE for conformance tests.',
		argnames is ['Pieces', 'Encoding']
	]).

	test_pae(Pieces, Encoding) :-
		^^pae(Pieces, Encoding).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'Unit tests for the PASETO v4 library.'
	]).

	:- uses(crypto, [
		hex_bytes/2
	]).

	cover(paseto_v4).
	cover(paseto_helpers).
	cover(paseto_claims_helpers).
	cover(paseto_claims).
	cover(paseto_keys).
	cover(paseto).

	:- public(valid_custom_claim/4).
	:- mode(valid_custom_claim(+term, +atom, +term, +list(compound)), one).
	:- info(valid_custom_claim/4, [
		comment is 'Custom claim verifier used by policy tests.',
		argnames is ['Claims', 'Name', 'Value', 'Options']
	]).

	valid_custom_claim(_Claims, role, admin, _Options).

	test(paseto_v4_local_encrypt_vector_4_e_1, deterministic(Token == Expected)) :-
		hex_bytes('707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f', Key),
		hex_bytes('0000000000000000000000000000000000000000000000000000000000000000', Nonce),
		atom_codes('{"data":"this is a secret message","exp":"2022-01-01T00:00:00+00:00"}', Payload),
		Expected = 'v4.local.AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQAr68PS4AXe7If_ZgesdkUMvSwscFlAl1pk5HC0e8kApeaqMfGo_7OpBnwJOAbY9V7WU6abu74MmcUE8YWAiaArVI8XJ5hOb_4v9RmDkneN0S92dx0OW4pgy7omxgf3S8c3LlQg',
		paseto_v4_test_driver::deterministic_local_encrypt(Key, Nonce, Payload, [], [], Token).

	test(paseto_v4_local_decrypt_vector_4_e_1, deterministic(Payload == Expected)) :-
		hex_bytes('707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f', Key),
		Token = 'v4.local.AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQAr68PS4AXe7If_ZgesdkUMvSwscFlAl1pk5HC0e8kApeaqMfGo_7OpBnwJOAbY9V7WU6abu74MmcUE8YWAiaArVI8XJ5hOb_4v9RmDkneN0S92dx0OW4pgy7omxgf3S8c3LlQg',
		atom_codes('{"data":"this is a secret message","exp":"2022-01-01T00:00:00+00:00"}', Expected),
		paseto_v4::local_decrypt(Token, Key, Payload).

	test(paseto_v4_public_sign_vector_4_s_1, deterministic(Token == Expected)) :-
		hex_bytes('b4cbfb43df4ce210727d953e4a713307fa19bb7d9f85041438d9e11b942a3774', Seed),
		atom_codes('{"data":"this is a signed message","exp":"2022-01-01T00:00:00+00:00"}', Payload),
		Expected = 'v4.public.eyJkYXRhIjoidGhpcyBpcyBhIHNpZ25lZCBtZXNzYWdlIiwiZXhwIjoiMjAyMi0wMS0wMVQwMDowMDowMCswMDowMCJ9bg_XBBzds8lTZShVlwwKSgeKpLT3yukTw6JUz3W4h_ExsQV-P0V54zemZDcAxFaSeef1QlXEFtkqxT1ciiQEDA',
		paseto_v4::public_sign(Seed, Payload, Token).

	test(paseto_v4_public_verify_vector_4_s_1, deterministic(Payload == Expected)) :-
		hex_bytes('1eb9dbbbbc047c03fd70604e0071f0987e16b28b757225c11f00415d0e20b1a2', PublicKey),
		Token = 'v4.public.eyJkYXRhIjoidGhpcyBpcyBhIHNpZ25lZCBtZXNzYWdlIiwiZXhwIjoiMjAyMi0wMS0wMVQwMDowMDowMCswMDowMCJ9bg_XBBzds8lTZShVlwwKSgeKpLT3yukTw6JUz3W4h_ExsQV-P0V54zemZDcAxFaSeef1QlXEFtkqxT1ciiQEDA',
		atom_codes('{"data":"this is a signed message","exp":"2022-01-01T00:00:00+00:00"}', Expected),
		paseto_v4::public_verify(Token, PublicKey, Payload).

	test(paseto_v4_local_tampered, fail) :-
		hex_bytes('707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f', Key),
		Token = 'v4.local.BAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQAr68PS4AXe7If_ZgesdkUMvSwscFlAl1pk5HC0e8kApeaqMfGo_7OpBnwJOAbY9V7WU6abu74MmcUE8YWAiaArVI8XJ5hOb_4v9RmDkneN0S92dx0OW4pgy7omxgf3S8c3LlQg',
		paseto_v4::local_decrypt(Token, Key, _).

	test(paseto_v4_pae_test_vector, deterministic(Encoding == [1,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0't,0'e,0's,0't])) :-
		paseto_v4_test_driver::test_pae([[0't,0'e,0's,0't]], Encoding).

	test(paseto_v4_rejects_padded_base64url, error(representation_error(base64))) :-
		hex_bytes('707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f', Key),
		Token = 'v4.local.32VIErrEkmY4JVILovbmfPXKW9wT1OdQepjMTC_MOtjA4kiqw7_tcaOM5GNEcnTxl60WkwMsYXw6FSNb_UdJPXjpzm0W9ojM5f4O2mRvE2IcweP-PRdoHjd5-RHCiExR1IK6t4x-RMNXtQNbz7FvFZ_G-lFpk5RG3EOrwDL6CDqcerSQ==.eyJraWQiOiJ6VmhNaVBCUDlmUmYyc25FY1Q3Z0ZUaW9lQTlDT2NOeTlEZmdMMVc2MGhhTiJ9',
		paseto_v4::local_decrypt(Token, Key, _).

	test(paseto_claims_local_round_trip, deterministic((Decoded == Claims, Footer == {kid-'local-1'}))) :-
		hex_bytes('707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f', Key),
		Claims = {sub-'123', exp-4102444800},
		paseto::encrypt(Claims, Key, Token, [key_id('local-1')]),
		KeySet = key_set([local('local-1', [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]), local('local-1', Key)]),
		paseto::decrypt(Token, KeySet, Decoded, Footer, [now(1700000001)]).

	test(paseto_claims_public_round_trip, deterministic(Decoded == Claims)) :-
		hex_bytes('b4cbfb43df4ce210727d953e4a713307fa19bb7d9f85041438d9e11b942a3774', Seed),
		hex_bytes('1eb9dbbbbc047c03fd70604e0071f0987e16b28b757225c11f00415d0e20b1a2', PublicKey),
		Claims = {iss-'https://issuer.example', aud-['client'], exp-4102444800},
		paseto::sign(Claims, Seed, Token, [implicit_assertion([1,2,3])]),
		paseto::verify(Token, PublicKey, Decoded, [now(1700000001), implicit_assertion([1,2,3]), claim_policy([claim(iss, expected('https://issuer.example')), claim(aud, contains('client'))])]).

	test(paseto_claims_requires_exp, error(domain_error(paseto_claims, missing(exp)))) :-
		hex_bytes('b4cbfb43df4ce210727d953e4a713307fa19bb7d9f85041438d9e11b942a3774', Seed),
		hex_bytes('1eb9dbbbbc047c03fd70604e0071f0987e16b28b757225c11f00415d0e20b1a2', PublicKey),
		paseto::sign({sub-'123'}, Seed, Token, []),
		paseto::verify(Token, PublicKey, _, [now(1700000001)]).

	test(paseto_claims_unsafe_public_inspection, deterministic(Decoded == Claims)) :-
		hex_bytes('b4cbfb43df4ce210727d953e4a713307fa19bb7d9f85041438d9e11b942a3774', Seed),
		Claims = {sub-'123', exp-4102444800},
		paseto::sign(Claims, Seed, Token, []),
		paseto::claims(Token, Decoded).

	test(paseto_claims_wrong_implicit_assertion, fail) :-
		hex_bytes('b4cbfb43df4ce210727d953e4a713307fa19bb7d9f85041438d9e11b942a3774', Seed),
		hex_bytes('1eb9dbbbbc047c03fd70604e0071f0987e16b28b757225c11f00415d0e20b1a2', PublicKey),
		paseto::sign({sub-'123', exp-4102444800}, Seed, Token, [implicit_assertion([1,2,3])]),
		paseto::verify(Token, PublicKey, _, [now(1700000001), implicit_assertion([1,2,4])]).

	test(paseto_claims_duplicate_member, error(domain_error(paseto_json_object, _))) :-
		hex_bytes('b4cbfb43df4ce210727d953e4a713307fa19bb7d9f85041438d9e11b942a3774', Seed),
		hex_bytes('1eb9dbbbbc047c03fd70604e0071f0987e16b28b757225c11f00415d0e20b1a2', PublicKey),
		atom_codes('{"sub":"123","sub":"456","exp":4102444800}', Payload),
		paseto_v4::public_sign(Seed, Payload, Token),
		paseto::verify(Token, PublicKey, _, [now(1700000001)]).

	test(paseto_v4_key_generation, deterministic((LocalLength == 32, SeedLength == 32, PublicLength == 32))) :-
		paseto_v4::local_key(LocalKey),
		paseto_v4::public_keypair(Seed, PublicKey),
		list::length(LocalKey, LocalLength),
		list::length(Seed, SeedLength),
		list::length(PublicKey, PublicLength).

	test(paseto_v4_local_convenience_round_trip, deterministic(Payload == [1,2,3])) :-
		hex_bytes('707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f', Key),
		paseto_v4::local_encrypt(Key, [1,2,3], Token),
		paseto_v4::local_decrypt(Token, Key, Payload).

	test(paseto_facade_decrypt_4, deterministic(Decoded == Claims)) :-
		hex_bytes('707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f', Key),
		Claims = {sub-'123', exp-4102444800},
		paseto::encrypt(Claims, Key, Token, []),
		paseto::decrypt(Token, Key, Decoded, [now(1700000001)]).

	test(paseto_facade_claim, deterministic(Value == '123')) :-
		paseto::claim({sub-'123'}, sub, Value).

	test(paseto_facade_validate_claims, deterministic) :-
		paseto::validate_claims({sub-'123'}, [claim(sub, required)], [allow_missing_exp(true), required_claims([sub])]).

	test(paseto_facade_validate_claim, deterministic) :-
		paseto::validate_claim({role-admin}, claim(role, one_of([user, admin])), []).

	test(paseto_claims_custom_policy, deterministic) :-
		paseto_claims::validate_claim(
			{role-admin},
			claim(role, custom(tests::valid_custom_claim)),
			[]
		).

	test(paseto_claims_contains_invalid_type, error(domain_error(paseto_claim(aud), 42))) :-
		paseto_claims::validate_claim({aud-42}, claim(aud, contains(client)), []).

	test(paseto_claims_required_missing, error(domain_error(paseto_claims, missing(sub)))) :-
		paseto_claims::validate_claims({}, [], [allow_missing_exp(true), required_claims([sub])]).

	test(paseto_claims_not_before, deterministic) :-
		paseto_claims::validate_claims(
			{exp-200, nbf-100},
			[claim(nbf, time(not_before))],
			[now(100), clock_skew(0)]
		).

	test(paseto_claims_issued_at_with_max_age, deterministic) :-
		paseto_claims::validate_claims(
			{exp-200, iat-90},
			[claim(iat, time(issued_at))],
			[now(100), clock_skew(0), max_age(10)]
		).

	test(paseto_claims_issued_at_without_max_age, deterministic) :-
		paseto_claims::validate_claims(
			{exp-200, iat-90},
			[claim(iat, time(issued_at))],
			[now(100), clock_skew(0)]
		).

	test(paseto_claims_invalid_time_kind, error(domain_error(paseto_time_claim_kind, unknown))) :-
		paseto_claims::validate_claim({time-100}, claim(time, time(unknown)), [now(100), clock_skew(0)]).

	test(paseto_claims_invalid_policy, error(domain_error(paseto_claim_policy, invalid))) :-
		paseto_claims::validate_claim({}, invalid, []).

	test(paseto_claims_invalid_time_type, error(type_error(time_number, exp-later))) :-
		paseto_claims::validate_claims({exp-later}, [], [now(100)]).

	test(paseto_claims_json_pair_representations, deterministic((Value1 == one, Value2 == two))) :-
		paseto_claims::claim(json([first=one, ':'(second, two)]), first, Value1),
		paseto_claims::claim(json([first=one, ':'(second, two)]), second, Value2).

	test(paseto_footer_merge, deterministic((Decoded == Claims, Footer == {kid-'key-1', purpose-test}))) :-
		hex_bytes('707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f', Key),
		Claims = {exp-4102444800},
		paseto::encrypt(Claims, Key, Token, [footer({purpose-test}), key_id('key-1')]),
		paseto::decrypt(Token, Key, Decoded, Footer, [now(1700000001)]).

	test(paseto_public_key_set_without_key_id, deterministic(Decoded == Claims)) :-
		hex_bytes('b4cbfb43df4ce210727d953e4a713307fa19bb7d9f85041438d9e11b942a3774', Seed),
		hex_bytes('1eb9dbbbbc047c03fd70604e0071f0987e16b28b757225c11f00415d0e20b1a2', PublicKey),
		Claims = {exp-4102444800},
		paseto::sign(Claims, Seed, Token, []),
		paseto::verify(Token, key_set([public('public-1', PublicKey)]), Decoded, [now(1700000001)]).

	test(paseto_validate_key_set, deterministic) :-
		hex_bytes('707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f', Key),
		paseto::validate_key_set(key_set([local('local-1', Key)])).

	test(paseto_invalid_key_set, error(domain_error(paseto_key_set, invalid))) :-
		paseto::validate_key_set(invalid).

	test(paseto_invalid_key_record, error(domain_error(paseto_key_record, secret(key)))) :-
		paseto_keys::validate(key_set([secret(key)])).

	test(paseto_missing_selected_key, error(existence_error(paseto_key, _))) :-
		hex_bytes('707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f', Key),
		paseto_keys::select_keys(key_set([local('local-1', Key)]), public, missing, _).

	test(paseto_rejects_multiple_footer_dots, error(domain_error(paseto_compact_serialization, malformed))) :-
		hex_bytes('707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f', Key),
		paseto_v4::local_decrypt('v4.local.A.A.A', Key, _).

:- end_object.
