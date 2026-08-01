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


:- object(ed25519_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-01,
		comment is 'Unit tests for the "crypto" library implementation of Ed25519.'
	]).

	:- uses(crypto, [
		hex_bytes/2, ed25519_keypair/2, ed25519_public_key/2, ed25519_sign/3, ed25519_verify/3
	]).

	:- uses(list, [
		append/3, length/2
	]).

	cover(crypto).
	cover(ed25519).

	% ed25519_keypair/2, ed25519_public_key/2 tests

	test(crypto_ed25519_keypair_2_01, deterministic) :-
		ed25519_keypair(Seed, PublicKey),
		length(Seed, 32),
		length(PublicKey, 32),
		ed25519_public_key(Seed, PublicKey2),
		PublicKey == PublicKey2.

	test(crypto_ed25519_public_key_2_01, deterministic(Hex == 'd75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a')) :-
		hex_bytes('9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60', Seed),
		ed25519_public_key(Seed, PublicKeyBytes),
		hex_bytes(Hex, PublicKeyBytes).

	test(crypto_ed25519_public_key_2_02, error(instantiation_error)) :-
		ed25519_public_key(_, _).

	test(crypto_ed25519_public_key_2_03, error(type_error(list(byte,32), foo))) :-
		ed25519_public_key(foo, _).

	test(crypto_ed25519_public_key_2_04, error(type_error(list(byte,32), [1,2,3]))) :-
		ed25519_public_key([1,2,3], _).

	% ed25519_sign/3, ed25519_verify/3 tests
	%
	% _01/_02/_03 are TEST1/TEST2/TEST3 from RFC 8032 Section 7.1.

	test(crypto_ed25519_sign_3_01, deterministic) :-
		hex_bytes('9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60', Seed),
		ed25519_sign(Seed, [], Signature),
		hex_bytes(Hex, Signature),
		hex_bytes('e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b', Expected),
		hex_bytes(ExpectedHex, Expected),
		Hex == ExpectedHex.

	test(crypto_ed25519_sign_3_02, deterministic) :-
		hex_bytes('4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb', Seed),
		ed25519_sign(Seed, [0x72], Signature),
		hex_bytes(Hex, Signature),
		hex_bytes('92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00', Expected),
		hex_bytes(ExpectedHex, Expected),
		Hex == ExpectedHex.

	test(crypto_ed25519_sign_3_03, deterministic) :-
		hex_bytes('c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7', Seed),
		ed25519_sign(Seed, [0xaf, 0x82], Signature),
		hex_bytes(Hex, Signature),
		hex_bytes('6291d657deec24024827e69c3abe01a30ce548a284743a445e3680d7db5ac3ac18ff9b538d16f290ae67f760984dc6594a7c15e9716ed28dc027beceea1ec40a', Expected),
		hex_bytes(ExpectedHex, Expected),
		Hex == ExpectedHex.

	test(crypto_ed25519_sign_3_04, error(instantiation_error)) :-
		ed25519_sign(_, [], _).

	test(crypto_ed25519_sign_3_05, error(type_error(list(byte,32), [1,2,3]))) :-
		ed25519_sign([1,2,3], [], _).

	test(crypto_ed25519_verify_3_01, deterministic) :-
		hex_bytes('9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60', Seed),
		ed25519_public_key(Seed, PublicKey),
		ed25519_sign(Seed, [], Signature),
		ed25519_verify(PublicKey, [], Signature).

	test(crypto_ed25519_verify_3_02, fail) :-
		% signature over a different message must not verify
		hex_bytes('9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60', Seed),
		ed25519_public_key(Seed, PublicKey),
		ed25519_sign(Seed, [], Signature),
		ed25519_verify(PublicKey, [1], Signature).

	test(crypto_ed25519_verify_3_03, fail) :-
		% tampered signature (last byte flipped) must not verify
		hex_bytes('9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60', Seed),
		ed25519_public_key(Seed, PublicKey),
		ed25519_sign(Seed, [], Signature),
		append(Prefix, [Last], Signature),
		LastFlipped is xor(Last, 1),
		append(Prefix, [LastFlipped], Tampered),
		ed25519_verify(PublicKey, [], Tampered).

	test(crypto_ed25519_verify_3_04, fail) :-
		% non-canonical S (S = L, the group order itself; must satisfy 0 <= S < L)
		hex_bytes('9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60', Seed),
		ed25519_public_key(Seed, PublicKey),
		ed25519_sign(Seed, [], Signature),
		length(RBytes, 32),
		append(RBytes, _, Signature),
		GroupOrder is (1<<252) + 27742317777372353535851937790883648493,
		ed25519_le_bytes_32(GroupOrder, SBytes),
		append(RBytes, SBytes, BadSignature),
		ed25519_verify(PublicKey, [], BadSignature).

	% _05.._11: signature with a small-order or non-canonically-encoded point (one of the
	% 7 points of order dividing 8, per libsodium's ge25519_has_small_order blacklist,
	% "Taming the many EdDSAs") substituted as the R component of an otherwise-genuine
	% signature must not verify.

	test(crypto_ed25519_verify_3_05, deterministic) :-
		ed25519_check_bad_r('0000000000000000000000000000000000000000000000000000000000000000').
	test(crypto_ed25519_verify_3_06, deterministic) :-
		ed25519_check_bad_r('0100000000000000000000000000000000000000000000000000000000000000').
	test(crypto_ed25519_verify_3_07, deterministic) :-
		ed25519_check_bad_r('26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05').
	test(crypto_ed25519_verify_3_08, deterministic) :-
		ed25519_check_bad_r('c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a').
	test(crypto_ed25519_verify_3_09, deterministic) :-
		ed25519_check_bad_r('ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f').
	test(crypto_ed25519_verify_3_10, deterministic) :-
		ed25519_check_bad_r('edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f').
	test(crypto_ed25519_verify_3_11, deterministic) :-
		ed25519_check_bad_r('eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f').

	% _12.._18: same 7 points, this time substituted as the public key itself.

	test(crypto_ed25519_verify_3_12, deterministic) :-
		ed25519_check_bad_a('0000000000000000000000000000000000000000000000000000000000000000').
	test(crypto_ed25519_verify_3_13, deterministic) :-
		ed25519_check_bad_a('0100000000000000000000000000000000000000000000000000000000000000').
	test(crypto_ed25519_verify_3_14, deterministic) :-
		ed25519_check_bad_a('26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05').
	test(crypto_ed25519_verify_3_15, deterministic) :-
		ed25519_check_bad_a('c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a').
	test(crypto_ed25519_verify_3_16, deterministic) :-
		ed25519_check_bad_a('ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f').
	test(crypto_ed25519_verify_3_17, deterministic) :-
		ed25519_check_bad_a('edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f').
	test(crypto_ed25519_verify_3_18, deterministic) :-
		ed25519_check_bad_a('eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f').

	test(crypto_ed25519_verify_3_19, fail) :-
		% malformed length is simply not a valid signature
		hex_bytes('9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60', Seed),
		ed25519_public_key(Seed, PublicKey),
		ed25519_verify(PublicKey, [], [1,2,3]).

	% auxiliary predicates

	ed25519_check_bad_r(BadPointHex) :-
		hex_bytes('9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60', Seed),
		ed25519_public_key(Seed, PublicKey),
		ed25519_sign(Seed, [], Signature),
		length(_, 32),
		length(GoodR, 32),
		append(GoodR, SBytes, Signature),
		hex_bytes(BadPointHex, BadR),
		append(BadR, SBytes, BadSignature),
		\+ ed25519_verify(PublicKey, [], BadSignature).

	ed25519_check_bad_a(BadPointHex) :-
		hex_bytes('9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60', Seed),
		ed25519_sign(Seed, [], Signature),
		hex_bytes(BadPointHex, BadPublicKey),
		\+ ed25519_verify(BadPublicKey, [], Signature).

	ed25519_le_bytes_32(N, Bytes) :-
		length(Bytes, 32),
		ed25519_le_bytes_32_(N, Bytes).

	ed25519_le_bytes_32_(_, []) :-
		!.
	ed25519_le_bytes_32_(N, [B| Bs]) :-
		B is N /\ 0xff,
		N1 is N >> 8,
		ed25519_le_bytes_32_(N1, Bs).

:- end_object.
