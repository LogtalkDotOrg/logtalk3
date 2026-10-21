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


:- object(xchacha20_poly1305_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-21,
		comment is 'Unit tests for the "crypto" library implementation of XChaCha20-Poly1305.'
	]).

	:- uses(crypto, [
		chacha20_encrypt/5, hex_bytes/2, xchacha20_poly1305_encrypt/5, xchacha20_poly1305_decrypt/5
	]).

	:- uses(list, [
		append/3, length/2, member/2
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(integer, [
		sequence/3
	]).

	cover(crypto).
	cover(xchacha20_poly1305).

	% xchacha20_poly1305_encrypt/5 tests
	%
	% Vectors _01/_02/_03 exercise the public API end-to-end (values independently computed
	% and verified against a compiled reference implementation during development; see
	% NOTES.md for the verification methodology, since RFC 8439 itself only publishes a
	% vector for the inner 12-byte-nonce construction, not the 24-byte XChaCha nonce).

	test(crypto_xchacha20_poly1305_encrypt_5_01, deterministic(HexOut == '9985e48dd8c6d81b57fc883ecd307a3586')) :-
		sequence(0, 31, Key),
		sequence(0, 23, Nonce),
		sequence(0, 12, Aad),
		xchacha20_poly1305_encrypt(Key, Nonce, Aad, [7], CiphertextAndTag),
		hex_bytes(HexOut, CiphertextAndTag).

	test(crypto_xchacha20_poly1305_encrypt_5_02, deterministic) :-
		sequence(0, 31, Key),
		sequence(0, 23, Nonce),
		xchacha20_poly1305_encrypt(Key, Nonce, [], [], CiphertextAndTag),
		length(CiphertextAndTag, 16).

	test(crypto_xchacha20_poly1305_encrypt_5_03, deterministic) :-
		sequence(0, 31, Key),
		sequence(0, 23, Nonce),
		sequence(0, 199, Plaintext),
		xchacha20_poly1305_encrypt(Key, Nonce, [], Plaintext, CiphertextAndTag),
		length(CiphertextAndTag, 216),
		xchacha20_poly1305_decrypt(Key, Nonce, [], CiphertextAndTag, Decrypted),
		Decrypted == Plaintext.

	test(crypto_xchacha20_poly1305_encrypt_5_04, error(instantiation_error)) :-
		xchacha20_poly1305_encrypt(_, _, [], [], _).

	test(crypto_xchacha20_poly1305_encrypt_5_05, error(type_error(list(byte,32), foo))) :-
		sequence(0, 23, Nonce),
		xchacha20_poly1305_encrypt(foo, Nonce, [], [], _).

	test(crypto_xchacha20_poly1305_encrypt_5_06, error(type_error(list(byte,32), [1,2,3]))) :-
		sequence(0, 23, Nonce),
		xchacha20_poly1305_encrypt([1,2,3], Nonce, [], [], _).

	test(crypto_xchacha20_poly1305_encrypt_5_07, error(type_error(list(byte,24), [1,2,3]))) :-
		sequence(0, 31, Key),
		xchacha20_poly1305_encrypt(Key, [1,2,3], [], [], _).

	test(crypto_xchacha20_poly1305_encrypt_5_08, error(type_error(list(byte), foo))) :-
		sequence(0, 31, Key),
		sequence(0, 23, Nonce),
		xchacha20_poly1305_encrypt(Key, Nonce, foo, [], _).

	test(crypto_xchacha20_poly1305_encrypt_5_09, error(type_error(list(byte), foo))) :-
		sequence(0, 31, Key),
		sequence(0, 23, Nonce),
		xchacha20_poly1305_encrypt(Key, Nonce, [], foo, _).

	% xchacha20_poly1305_decrypt/5 tests

	test(crypto_xchacha20_poly1305_decrypt_5_01, deterministic(Plaintext == [7])) :-
		sequence(0, 31, Key),
		sequence(0, 23, Nonce),
		sequence(0, 12, Aad),
		hex_bytes('9985e48dd8c6d81b57fc883ecd307a3586', CiphertextAndTag),
		xchacha20_poly1305_decrypt(Key, Nonce, Aad, CiphertextAndTag, Plaintext).

	test(crypto_xchacha20_poly1305_decrypt_5_02, fail) :-
		% wrong AAD: same ciphertext/tag as _01 but AAD not matching what was authenticated
		sequence(0, 31, Key),
		sequence(0, 23, Nonce),
		hex_bytes('9985e48dd8c6d81b57fc883ecd307a3586', CiphertextAndTag),
		xchacha20_poly1305_decrypt(Key, Nonce, [], CiphertextAndTag, _).

	test(crypto_xchacha20_poly1305_decrypt_5_03, fail) :-
		% tampered tag (last byte flipped)
		sequence(0, 31, Key),
		sequence(0, 23, Nonce),
		sequence(0, 12, Aad),
		hex_bytes('9985e48dd8c6d81b57fc883ecd307a3587', CiphertextAndTag),
		xchacha20_poly1305_decrypt(Key, Nonce, Aad, CiphertextAndTag, _).

	test(crypto_xchacha20_poly1305_decrypt_5_04, fail) :-
		% tampered ciphertext (first byte flipped)
		sequence(0, 31, Key),
		sequence(0, 23, Nonce),
		sequence(0, 12, Aad),
		hex_bytes('9885e48dd8c6d81b57fc883ecd307a3586', CiphertextAndTag),
		xchacha20_poly1305_decrypt(Key, Nonce, Aad, CiphertextAndTag, _).

	test(crypto_xchacha20_poly1305_decrypt_5_05, fail) :-
		% wrong key
		sequence(1, 32, Key),
		sequence(0, 23, Nonce),
		sequence(0, 12, Aad),
		hex_bytes('9985e48dd8c6d81b57fc883ecd307a3586', CiphertextAndTag),
		xchacha20_poly1305_decrypt(Key, Nonce, Aad, CiphertextAndTag, _).

	test(crypto_xchacha20_poly1305_decrypt_5_06, error(instantiation_error)) :-
		xchacha20_poly1305_decrypt(_, _, [], [], _).

	test(crypto_xchacha20_poly1305_decrypt_5_07, error(type_error(list(byte,32), [1,2,3]))) :-
		sequence(0, 23, Nonce),
		xchacha20_poly1305_decrypt([1,2,3], Nonce, [], [], _).

	test(crypto_xchacha20_poly1305_decrypt_5_08, error(type_error(list(byte,24), [1,2,3]))) :-
		sequence(0, 31, Key),
		xchacha20_poly1305_decrypt(Key, [1,2,3], [], [], _).

	test(crypto_xchacha20_poly1305_decrypt_5_09, error(domain_error(minimum_byte_length(16), [1,2,3]))) :-
		sequence(0, 31, Key),
		sequence(0, 23, Nonce),
		xchacha20_poly1305_decrypt(Key, Nonce, [], [1,2,3], _).

	test(crypto_xchacha20_poly1305_block_boundaries, all(check_boundary(Key, Nonce, Length, AADLength))) :-
		sequence(0, 31, Key),
		sequence(0, 23, Nonce),
		member(Length, [0,1,15,16,17,63,64,65,127,128,129,200]),
		member(AADLength, [0,15,16,17]).

	test(crypto_xchacha20_poly1305_boundary_vectors, all(check_boundary_vector(Key, Nonce, AAD, Length, Hex))) :-
		sequence(0, 31, Key),
		sequence(0, 23, Nonce),
		sequence(0, 16, AAD),
		boundary_vector(Length, Hex).

	test(crypto_chacha20_encrypt_5_rfc8439, deterministic) :-
		KeyWords = [50462976,117835012,185207048,252579084,319951120,387323156,454695192,522067228],
		NonceWords = [0,1241513984,0],
		atom_codes('Ladies and Gentlemen of the class of \'99: If I could offer you only one tip for the future, sunscreen would be it.', Plaintext),
		hex_bytes('6e2e359a2568f98041ba0728dd0d6981e97e7aec1d4360c20a27afccfd9fae0bf91b65c5524733ab8f593dabcd62b3571639d624e65152ab8f530c359f0861d807ca0dbf500d6a6156a38e088a22b65e52bc514d16ccf806818ce91ab77937365af90bbf74a35be6b40b8eedf2785e42874d', Expected),
		chacha20_encrypt(KeyWords, 1, NonceWords, Plaintext, Ciphertext),
		assertion(Ciphertext == Expected),
		chacha20_encrypt(KeyWords, 1, NonceWords, Ciphertext, Decrypted),
		assertion(Decrypted == Plaintext),
		forall(
			member(Length, [0,1,63,64,65,114]),
			assertion(check_chacha_prefix(KeyWords, NonceWords, Length, Plaintext, Expected))
		).

	% auxiliary predicates

	check_boundary(Key, Nonce, Length, AADLength) :-
		input_bytes(Length, Plaintext),
		input_bytes(AADLength, AAD),
		xchacha20_poly1305_encrypt(Key, Nonce, AAD, Plaintext, Ciphertext),
		CiphertextLength is Length + 16,
		length(Ciphertext, CiphertextLength),
		xchacha20_poly1305_decrypt(Key, Nonce, AAD, Ciphertext, Decrypted),
		assertion(Decrypted == Plaintext),
		append(Prefix, [Last], Ciphertext),
		TamperedLast is xor(Last, 1),
		append(Prefix, [TamperedLast], Tampered),
		assertion(\+ crypto::xchacha20_poly1305_decrypt(Key, Nonce, AAD, Tampered, _)).

	check_boundary_vector(Key, Nonce, AAD, Length, Hex) :-
		input_bytes(Length, Plaintext),
		hex_bytes(Hex, Expected),
		xchacha20_poly1305_encrypt(Key, Nonce, AAD, Plaintext, Ciphertext),
		assertion(Ciphertext == Expected).

	check_chacha_prefix(KeyWords, NonceWords, Length, Plaintext, Expected) :-
		length(Input, Length),
		append(Input, _, Plaintext),
		length(Prefix, Length),
		append(Prefix, _, Expected),
		chacha20_encrypt(KeyWords, 1, NonceWords, Input, Output),
		assertion(Output == Prefix).

	input_bytes(0, []) :-
		!.
	input_bytes(Length, Bytes) :-
		Last is Length - 1,
		sequence(0, Last, Bytes).

	boundary_vector(16, '9ec30d7c94d78ba93b4d2cc5c75fa6e7d9c4d0cb6292473b8ca691bf18accf09').
	boundary_vector(17, '9ec30d7c94d78ba93b4d2cc5c75fa6e75b7a59e994cb5c4db5d2631137e13431df').
	boundary_vector(64, '9ec30d7c94d78ba93b4d2cc5c75fa6e75b563ab6e9c30bfc670325ad21b0eb467e2794c765422f43fdbc8472e30c7d4d418bd06a7341cd2e3fa2a906b3da7acb01ce3cc2b1929807ec2b168dbfbe0f81').
	boundary_vector(65, '9ec30d7c94d78ba93b4d2cc5c75fa6e75b563ab6e9c30bfc670325ad21b0eb467e2794c765422f43fdbc8472e30c7d4d418bd06a7341cd2e3fa2a906b3da7acbf2833d8c73f4bad93b6dddbcc3e49953bd').

:- end_object.
