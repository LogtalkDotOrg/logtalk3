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


:- object(x25519_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'Unit tests for the "crypto" library implementation of X25519.'
	]).

	:- uses(crypto, [
		hex_bytes/2, x25519_keypair/2, x25519_public_key/2, x25519_shared_secret/3
	]).

	:- uses(list, [
		length/2
	]).

	cover(crypto).
	cover(x25519).

	% RFC 7748 Section 5.2 test vectors

	test(crypto_x25519_shared_secret_3_01, deterministic(Hex == 'c3da55379de9c6908e94ea4df28d084f32eccf03491c71f754b4075577a28552')) :-
		hex_bytes('a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4', Scalar),
		hex_bytes('e6db6867583030db3594c1a424b15f7c726624ec26b3353b10a903a6d0ab1c4c', UCoordinate),
		x25519_shared_secret(Scalar, UCoordinate, Output),
		hex_bytes(Hex, Output).

	test(crypto_x25519_shared_secret_3_02, deterministic(Hex == '95cbde9476e8907d7aade45cb4b873f88b595a68799fa152e6f8f7647aac7957')) :-
		hex_bytes('4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d', Scalar),
		hex_bytes('e5210f12786811d3f4b7959d0538ae2c31dbe7106fc03c3efc4cd549c715a493', UCoordinate),
		x25519_shared_secret(Scalar, UCoordinate, Output),
		hex_bytes(Hex, Output).

	test(crypto_x25519_shared_secret_3_03, deterministic(Hex == '422c8e7a6227d7bca1350b3e2bb7279f7897b87bb6854b783c60e80311ae3079')) :-
		hex_bytes('0900000000000000000000000000000000000000000000000000000000000000', Input),
		x25519_shared_secret(Input, Input, Output),
		hex_bytes(Hex, Output).

	test(crypto_x25519_shared_secret_3_04, deterministic(Hex == '684cf59ba83309552800ef566f2f4d3c1c3887c49360e3875f2eb94d99532c51')) :-
		hex_bytes('0900000000000000000000000000000000000000000000000000000000000000', Input),
		x25519_iterate(1000, Input, Input, Output),
		hex_bytes(Hex, Output).

	% RFC 7748 Section 6.1 Diffie-Hellman example

	test(crypto_x25519_public_key_2_01, deterministic(Hex == '8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a')) :-
		hex_bytes('77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a', PrivateKey),
		x25519_public_key(PrivateKey, PublicKey),
		hex_bytes(Hex, PublicKey).

	test(crypto_x25519_public_key_2_02, deterministic(Hex == 'de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f')) :-
		hex_bytes('5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb', PrivateKey),
		x25519_public_key(PrivateKey, PublicKey),
		hex_bytes(Hex, PublicKey).

	test(crypto_x25519_shared_secret_3_05, deterministic(AliceSecret == BobSecret)) :-
		hex_bytes('77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a', AlicePrivateKey),
		hex_bytes('8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a', AlicePublicKey),
		hex_bytes('5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb', BobPrivateKey),
		hex_bytes('de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f', BobPublicKey),
		x25519_shared_secret(AlicePrivateKey, BobPublicKey, AliceSecret),
		x25519_shared_secret(BobPrivateKey, AlicePublicKey, BobSecret),
		hex_bytes('4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742', AliceSecret).

	test(crypto_x25519_keypair_2_01, deterministic) :-
		x25519_keypair(PrivateKey, PublicKey),
		length(PrivateKey, 32),
		length(PublicKey, 32),
		x25519_public_key(PrivateKey, DerivedPublicKey),
		PublicKey == DerivedPublicKey.

	% RFC 7748 requires masking the most significant input bit and accepting
	% non-canonical u-coordinates as if reduced modulo the field prime.

	test(crypto_x25519_shared_secret_3_06, deterministic(Secret1 == Secret2)) :-
		hex_bytes('77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a', PrivateKey),
		hex_bytes('0900000000000000000000000000000000000000000000000000000000000000', PublicKey1),
		hex_bytes('0900000000000000000000000000000000000000000000000000000000000080', PublicKey2),
		x25519_shared_secret(PrivateKey, PublicKey1, Secret1),
		x25519_shared_secret(PrivateKey, PublicKey2, Secret2).

	test(crypto_x25519_shared_secret_3_07, deterministic(Secret1 == Secret2)) :-
		hex_bytes('77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a', PrivateKey),
		hex_bytes('0900000000000000000000000000000000000000000000000000000000000000', PublicKey1),
		hex_bytes('f6ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f', PublicKey2),
		x25519_shared_secret(PrivateKey, PublicKey1, Secret1),
		x25519_shared_secret(PrivateKey, PublicKey2, Secret2).

	test(crypto_x25519_shared_secret_3_08, fail) :-
		hex_bytes('77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a', PrivateKey),
		hex_bytes('0000000000000000000000000000000000000000000000000000000000000000', LowOrderPublicKey),
		x25519_shared_secret(PrivateKey, LowOrderPublicKey, _).

	% Validation tests

	test(crypto_x25519_public_key_2_03, error(instantiation_error)) :-
		x25519_public_key(_, _).

	test(crypto_x25519_public_key_2_04, error(type_error(list(byte,32), foo))) :-
		x25519_public_key(foo, _).

	test(crypto_x25519_public_key_2_05, error(type_error(list(byte,32), [1,2,3]))) :-
		x25519_public_key([1,2,3], _).

	test(crypto_x25519_shared_secret_3_09, error(instantiation_error)) :-
		x25519_shared_secret(_, _, _).

	test(crypto_x25519_shared_secret_3_10, error(type_error(list(byte,32), foo))) :-
		hex_bytes('77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a', PrivateKey),
		x25519_shared_secret(PrivateKey, foo, _).

	test(crypto_x25519_shared_secret_3_11, error(type_error(list(byte,32), [1,2,3]))) :-
		hex_bytes('77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a', PrivateKey),
		x25519_shared_secret(PrivateKey, [1,2,3], _).

	% auxiliary predicates

	x25519_iterate(0, Scalar, _UCoordinate, Scalar) :-
		!.
	x25519_iterate(Iterations, Scalar, UCoordinate, Output) :-
		x25519_shared_secret(Scalar, UCoordinate, Next),
		NextIterations is Iterations - 1,
		x25519_iterate(NextIterations, Next, Scalar, Output).

:- end_object.
