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


:- category(xchacha20_poly1305,
	complements(crypto)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'XChaCha20 stream cipher and XChaCha20-Poly1305 authenticated encryption with associated data algorithm implementation. Requires exact, unbounded integer arithmetic for the Poly1305 130-bit accumulator.'
	]).

	:- public(xchacha20/4).
	:- mode(xchacha20(+list(byte), +list(byte), +list(byte), -list(byte)), one_or_error).
	:- info(xchacha20/4, [
		comment is 'Encrypts or decrypts Input using XChaCha20. Clients should only use this unauthenticated stream cipher as part of a construction that provides its own authentication.',
		argnames is ['Key', 'Nonce', 'Input', 'Output'],
		exceptions is [
			'``Key`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Key`` is neither a variable nor a list of 32 bytes' - type_error(list(byte, 32), 'Key'),
			'``Key`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``Key`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Nonce`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Nonce`` is neither a variable nor a list of 24 bytes' - type_error(list(byte, 24), 'Nonce'),
			'``Nonce`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``Nonce`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Input`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Input`` is neither a variable nor a list of bytes' - type_error(list(byte), 'Input'),
			'``Input`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``Input`` contains an integer outside the byte range' - domain_error(byte, 'Byte')
		]
	]).

	:- public(xchacha20_subkey_and_nonce/4).
	:- mode(xchacha20_subkey_and_nonce(+list(byte), +list(byte), -list(integer), -list(integer)), one).
	:- info(xchacha20_subkey_and_nonce/4, [
		comment is 'Derives the ChaCha20 subkey words and nonce words for an XChaCha20 key and nonce. The caller is responsible for validating the key and nonce and should only use the result in a construction that provides authentication.',
		argnames is ['Key', 'Nonce', 'SubkeyWords', 'NonceWords']
	]).

	:- public(chacha20_encrypt/5).
	:- mode(chacha20_encrypt(+list(integer), +non_negative_integer, +list(integer), +list(byte), -list(byte)), one).
	:- info(chacha20_encrypt/5, [
		comment is 'Encrypts or decrypts bytes using ChaCha20 words and the given initial block counter. The caller is responsible for validating all arguments and should only use this unauthenticated stream cipher as part of a construction that provides authentication.',
		argnames is ['KeyWords', 'Counter', 'NonceWords', 'Input', 'Output']
	]).

	:- public(chacha20_block/4).
	:- mode(chacha20_block(+list(integer), +non_negative_integer, +list(integer), -list(byte)), one).
	:- info(chacha20_block/4, [
		comment is 'Computes a 64-byte ChaCha20 block using key words and the given block counter and nonce words. The caller is responsible for validating all arguments and should only use it as part of a construction that provides authentication.',
		argnames is ['KeyWords', 'Counter', 'NonceWords', 'Block']
	]).

	:- public(xchacha20_poly1305_encrypt/5).
	:- mode(xchacha20_poly1305_encrypt(+list(byte), +list(byte), +list(byte), +list(byte), -list(byte)), one_or_error).
	:- info(xchacha20_poly1305_encrypt/5, [
		comment is 'Encrypts Plaintext with XChaCha20 and appends a 16-byte Poly1305 authentication tag covering AAD and the ciphertext, following the IETF ChaCha20-Poly1305 AEAD construction (RFC 8439) extended with the 24-byte XChaCha nonce. Available only on backends with unbounded integer arithmetic.',
		argnames is ['Key', 'Nonce', 'AAD', 'Plaintext', 'CiphertextAndTag'],
		exceptions is [
			'``Key`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Key`` is neither a variable nor a list of 32 bytes' - type_error(list(byte, 32), 'Key'),
			'``Key`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``Key`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Nonce`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Nonce`` is neither a variable nor a list of 24 bytes' - type_error(list(byte, 24), 'Nonce'),
			'``Nonce`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``Nonce`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``AAD`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``AAD`` is neither a variable nor a list of bytes' - type_error(list(byte), 'AAD'),
			'``AAD`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``AAD`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Plaintext`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Plaintext`` is neither a variable nor a list of bytes' - type_error(list(byte), 'Plaintext'),
			'``Plaintext`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``Plaintext`` contains an integer outside the byte range' - domain_error(byte, 'Byte')
		]
	]).

	:- public(xchacha20_poly1305_decrypt/5).
	:- mode(xchacha20_poly1305_decrypt(+list(byte), +list(byte), +list(byte), +list(byte), -list(byte)), zero_or_one_or_error).
	:- info(xchacha20_poly1305_decrypt/5, [
		comment is 'Verifies the trailing 16-byte Poly1305 tag of CiphertextAndTag against AAD using constant-time comparison and, only if it matches, decrypts the ciphertext with XChaCha20. Fails, without decrypting anything, if the tag does not match. Available only on backends with unbounded integer arithmetic.',
		argnames is ['Key', 'Nonce', 'AAD', 'CiphertextAndTag', 'Plaintext'],
		exceptions is [
			'``Key`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Key`` is neither a variable nor a list of 32 bytes' - type_error(list(byte, 32), 'Key'),
			'``Key`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``Key`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``Nonce`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``Nonce`` is neither a variable nor a list of 24 bytes' - type_error(list(byte, 24), 'Nonce'),
			'``Nonce`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``Nonce`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``AAD`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``AAD`` is neither a variable nor a list of bytes' - type_error(list(byte), 'AAD'),
			'``AAD`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``AAD`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``CiphertextAndTag`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``CiphertextAndTag`` is neither a variable nor a list of bytes' - type_error(list(byte), 'CiphertextAndTag'),
			'``CiphertextAndTag`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``CiphertextAndTag`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``CiphertextAndTag`` contains fewer than 16 bytes' - domain_error(minimum_byte_length(16), 'CiphertextAndTag')
		]
	]).

 	:- uses(list, [
		append/2, append/3, length/2
 	]).

	:- uses(type, [
		check/3
	]).

	xchacha20_poly1305_decrypt(Key, Nonce, AAD, CiphertextAndTag, Plaintext) :-
		context(Context),
		check(list(byte, 32), Key, Context),
		check(list(byte, 24), Nonce, Context),
		check(list(byte), AAD, Context),
		check(list(byte), CiphertextAndTag, Context),
		length(CiphertextAndTag, TotalLength),
		(	TotalLength >= 16 ->
			true
		;	domain_error(minimum_byte_length(16), CiphertextAndTag)
		),
		CiphertextLength is TotalLength - 16,
		length(Ciphertext, CiphertextLength),
		append(Ciphertext, Tag, CiphertextAndTag),
		xchacha20_subkey_and_nonce(Key, Nonce, SubkeyWords, ChachaNonceWords),
		aead_chacha20poly1305_compute_tag(SubkeyWords, ChachaNonceWords, AAD, Ciphertext, ComputedTag),
		@constant_time_equal(ComputedTag, Tag),
		chacha20_encrypt(SubkeyWords, 1, ChachaNonceWords, Ciphertext, Plaintext).

	xchacha20_poly1305_encrypt(Key, Nonce, AAD, Plaintext, CiphertextAndTag) :-
		context(Context),
		check(list(byte, 32), Key, Context),
		check(list(byte, 24), Nonce, Context),
		check(list(byte), AAD, Context),
		check(list(byte), Plaintext, Context),
		xchacha20_subkey_and_nonce(Key, Nonce, SubkeyWords, ChachaNonceWords),
		aead_chacha20poly1305_encrypt(SubkeyWords, ChachaNonceWords, AAD, Plaintext, Ciphertext, Tag),
		append(Ciphertext, Tag, CiphertextAndTag).

	% -- XChaCha20-Poly1305 AEAD construction (RFC 8439 Section 2.8) --

	aead_chacha20poly1305_encrypt(KeyWords, NonceWords, AAD, Plaintext, Ciphertext, Tag) :-
		poly1305_key_gen(KeyWords, NonceWords, Otk),
		chacha20_encrypt(KeyWords, 1, NonceWords, Plaintext, Ciphertext),
		poly1305_aead_mac(Otk, AAD, Ciphertext, Tag).

	aead_chacha20poly1305_compute_tag(KeyWords, NonceWords, AAD, Ciphertext, Tag) :-
		poly1305_key_gen(KeyWords, NonceWords, Otk),
		poly1305_aead_mac(Otk, AAD, Ciphertext, Tag).

	poly1305_aead_mac(Otk, AAD, Ciphertext, Tag) :-
		pad16(AAD, PadAAD),
		pad16(Ciphertext, PadCiphertext),
		length(AAD, AADLength),
		length(Ciphertext, CiphertextLength),
		int_to_le_bytes_fixed(AADLength, 8, AADLengthBytes),
		int_to_le_bytes_fixed(CiphertextLength, 8, CiphertextLengthBytes),
		append([AAD, PadAAD, Ciphertext, PadCiphertext, AADLengthBytes, CiphertextLengthBytes], MacData),
		poly1305_mac(MacData, Otk, Tag).

	poly1305_key_gen(KeyWords, NonceWords, Otk) :-
		chacha20_block(KeyWords, 0, NonceWords, Block64),
		length(Otk, 32),
		append(Otk, _, Block64).

	pad16(Bytes, Padded) :-
		length(Bytes, Length),
		Remainder is Length mod 16,
		(	Remainder =:= 0 ->
			Padded = []
		;	PadLength is 16 - Remainder,
			length(Padded, PadLength),
			zero_list(Padded)
		).

	zero_list([]).
	zero_list([0| Zeros]) :-
		zero_list(Zeros).

	% -- Poly1305 (RFC 8439 Section 2.5) --

	poly1305_mac(Message, Key, Tag) :-
		length(RBytes, 16),
		append(RBytes, SBytes, Key),
		le_bytes_to_int(RBytes, R0),
		poly1305_clamp(R0, R),
		le_bytes_to_int(SBytes, S),
		PolyPrime is (1 << 130) - 5,
		poly1305_blocks(Message, R, PolyPrime, 0, Accumulator),
		TagInt is (Accumulator + S) /\ ((1 << 128) - 1),
		int_to_le_bytes_fixed(TagInt, 16, Tag).

	poly1305_clamp(R0, R) :-
		R is R0 /\ 0x0ffffffc0ffffffc0ffffffc0fffffff.

	poly1305_blocks([], _, _, Accumulator, Accumulator) :-
		!.
	poly1305_blocks(Message, R, PolyPrime, Accumulator0, Accumulator) :-
		length(Message, Length),
		(	Length >= 16 ->
			length(Block, 16),
			append(Block, Rest, Message)
		;	Block = Message, Rest = []
		),
		append(Block, [0x01], BlockPadded),
		le_bytes_to_int(BlockPadded, N),
		Accumulator1 is ((Accumulator0 + N) * R) mod PolyPrime,
		poly1305_blocks(Rest, R, PolyPrime, Accumulator1, Accumulator).

	% Little-endian byte list <-> non-negative integer, arbitrary length.

	le_bytes_to_int(Bytes, Int) :-
		le_bytes_to_int(Bytes, 0, 0, Int).
	le_bytes_to_int([], _, Accumulator, Accumulator).
	le_bytes_to_int([Byte| Bytes], Shift, Accumulator0, Int) :-
		Accumulator1 is Accumulator0 \/ (Byte << Shift),
		Shift1 is Shift + 8,
		le_bytes_to_int(Bytes, Shift1, Accumulator1, Int).

	int_to_le_bytes_fixed(Int, Count, Bytes) :-
		length(Bytes, Count),
		int_to_le_bytes_fixed_loop(Int, Bytes).
	int_to_le_bytes_fixed_loop(_, []) :-
		!.
	int_to_le_bytes_fixed_loop(Int, [Byte| Bytes]) :-
		Byte is Int /\ 0xff,
		Int1 is Int >> 8,
		int_to_le_bytes_fixed_loop(Int1, Bytes).

	% -- XChaCha20 stream cipher --

	xchacha20(Key, Nonce, Input, Output) :-
		context(Context),
		check(list(byte, 32), Key, Context),
		check(list(byte, 24), Nonce, Context),
		check(list(byte), Input, Context),
		xchacha20_subkey_and_nonce(Key, Nonce, SubkeyWords, ChachaNonceWords),
		chacha20_encrypt(SubkeyWords, 0, ChachaNonceWords, Input, Output).

	xchacha20_subkey_and_nonce(Key, Nonce, SubkeyWords, ChachaNonceWords) :-
		length(Nonce16, 16),
		append(Nonce16, Nonce8, Nonce),
		bytes_to_words32(Key, KeyWords),
		bytes_to_words32(Nonce16, Nonce16Words),
		hchacha20(KeyWords, Nonce16Words, SubkeyBytes),
		bytes_to_words32(SubkeyBytes, SubkeyWords),
		ChachaNonceBytes = [0, 0, 0, 0| Nonce8],
		bytes_to_words32(ChachaNonceBytes, ChachaNonceWords).

	chacha20_encrypt(_, _, _, [], []) :-
		!.
	chacha20_encrypt(KeyWords, Counter, NonceWords, Input, Output) :-
		chacha20_block(KeyWords, Counter, NonceWords, Keystream),
		length(Input, Length),
		( Length >= 64 ->
			length(Block, 64),
			append(Block, Rest, Input),
			KeystreamBlock = Keystream
		;	Block = Input,
			Rest = [],
			length(KeystreamBlock, Length),
			append(KeystreamBlock, _, Keystream)
		),
		xor_bytes(Block, KeystreamBlock, OutputBlock),
		Counter1 is Counter + 1,
		chacha20_encrypt(KeyWords, Counter1, NonceWords, Rest, OutputRest),
		append(OutputBlock, OutputRest, Output).

	chacha20_block(KeyWords, Counter, NonceWords, OutBytes) :-
		chacha20_constants(C0, C1, C2, C3),
		KeyWords = [K0,K1,K2,K3,K4,K5,K6,K7],
		NonceWords = [N0,N1,N2],
		VInit = v(C0,C1,C2,C3, K0,K1,K2,K3, K4,K5,K6,K7, Counter,N0,N1,N2),
		chacha20_rounds(10, VInit, VFinal),
		VInit = v(I0,I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I13,I14,I15),
		VFinal = v(F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15),
		Mask is (1 << 32) - 1,
		O0 is (I0+F0)/\Mask, O1 is (I1+F1)/\Mask, O2 is (I2+F2)/\Mask, O3 is (I3+F3)/\Mask,
		O4 is (I4+F4)/\Mask, O5 is (I5+F5)/\Mask, O6 is (I6+F6)/\Mask, O7 is (I7+F7)/\Mask,
		O8 is (I8+F8)/\Mask, O9 is (I9+F9)/\Mask, O10 is (I10+F10)/\Mask, O11 is (I11+F11)/\Mask,
		O12 is (I12+F12)/\Mask, O13 is (I13+F13)/\Mask, O14 is (I14+F14)/\Mask, O15 is (I15+F15)/\Mask,
		words32_to_bytes([O0,O1,O2,O3,O4,O5,O6,O7,O8,O9,O10,O11,O12,O13,O14,O15], OutBytes).

	hchacha20(KeyWords, Nonce16Words, OutBytes) :-
		chacha20_constants(C0, C1, C2, C3),
		KeyWords = [K0,K1,K2,K3,K4,K5,K6,K7],
		Nonce16Words = [N0,N1,N2,N3],
		VInit = v(C0,C1,C2,C3, K0,K1,K2,K3, K4,K5,K6,K7, N0,N1,N2,N3),
		chacha20_rounds(10, VInit, VFinal),
		VFinal = v(F0,F1,F2,F3,_,_,_,_,_,_,_,_,F12,F13,F14,F15),
		words32_to_bytes([F0,F1,F2,F3,F12,F13,F14,F15], OutBytes).

	chacha20_constants(0x61707865, 0x3320646e, 0x79622d32, 0x6b206574).

	chacha20_rounds(0, V, V) :-
		!.
	chacha20_rounds(N, V0, V) :-
		N > 0,
		chacha20_double_round(V0, V1),
		N1 is N - 1,
		chacha20_rounds(N1, V1, V).

	chacha20_double_round(
		v(V0,V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15),
		v(Vo0,Vo1,Vo2,Vo3,Vo4,Vo5,Vo6,Vo7,Vo8,Vo9,Vo10,Vo11,Vo12,Vo13,Vo14,Vo15)
	) :-
		chacha20_qr(V0,V4,V8,V12, A0,A4,A8,A12),
		chacha20_qr(V1,V5,V9,V13, A1,A5,A9,A13),
		chacha20_qr(V2,V6,V10,V14, A2,A6,A10,A14),
		chacha20_qr(V3,V7,V11,V15, A3,A7,A11,A15),
		chacha20_qr(A0,A5,A10,A15, Vo0,B5,B10,B15),
		chacha20_qr(A1,A6,A11,A12, Vo1,B6,B11,Vo12),
		chacha20_qr(A2,A7,A8,A13,  Vo2,B7,Vo8,B13),
		chacha20_qr(A3,A4,A9,A14,  Vo3,Vo4,Vo9,B14),
		Vo5=B5, Vo6=B6, Vo7=B7, Vo10=B10, Vo11=B11, Vo13=B13, Vo14=B14, Vo15=B15.

	chacha20_qr(A0, B0, C0, D0, A, B, C, D) :-
		Mask is (1 << 32) - 1,
		A1 is (A0 + B0) /\ Mask, DA1 is xor(D0, A1), chacha20_rotl32(DA1, 16, D1),
		C1 is (C0 + D1) /\ Mask, BC1 is xor(B0, C1), chacha20_rotl32(BC1, 12, B1),
		A2 is (A1 + B1) /\ Mask, DA2 is xor(D1, A2), chacha20_rotl32(DA2, 8, D2),
		C2 is (C1 + D2) /\ Mask, BC2 is xor(B1, C2), chacha20_rotl32(BC2, 7, B2),
		A = A2, B = B2, C = C2, D = D2.

	chacha20_rotl32(X, N, R) :-
		Mask is (1 << 32) - 1,
		R is ((X << N) \/ (X >> (32 - N))) /\ Mask.

	bytes_to_words32([], []).
	bytes_to_words32([Byte| Bytes], [Word| Words]) :-
		length(Chunk, 4),
		append(Chunk, Rest, [Byte| Bytes]),
		le_bytes_to_int(Chunk, Word),
		bytes_to_words32(Rest, Words).

	words32_to_bytes([], []).
	words32_to_bytes([Word| Words], Bytes) :-
		int_to_le_bytes_fixed(Word, 4, WordBytes),
		words32_to_bytes(Words, Rest),
		append(WordBytes, Rest, Bytes).

	xor_bytes([], [], []).
	xor_bytes([Byte1| Bytes1], [Byte2| Bytes2], [XorByte| XorBytes]) :-
		XorByte is xor(Byte1, Byte2),
		xor_bytes(Bytes1, Bytes2, XorBytes).

:- end_category.
