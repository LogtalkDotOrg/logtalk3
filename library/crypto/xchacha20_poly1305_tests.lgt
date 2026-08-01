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
		date is 2026-08-01,
		comment is 'Unit tests for the "crypto" library implementation of XChaCha20-Poly1305.'
	]).

	:- uses(crypto, [
		hex_bytes/2, xchacha20_poly1305_encrypt/5, xchacha20_poly1305_decrypt/5
	]).

	:- uses(list, [
		length/2
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

:- end_object.
