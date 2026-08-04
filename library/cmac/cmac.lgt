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


:- object(cmac,
	implements(cmac_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-04,
		comment is 'CMAC (Cipher-based Message Authentication Code) implementation as specified in NIST SP 800-38B.',
		see_also is [aes128, aes192, aes256]
	]).

	:- uses(list, [
		append/3, length/2, take/3
	]).

	:- uses(type, [
		check/3
	]).

	digest(Cipher, KeyBytes, MessageBytes, DigestBytes) :-
		prepare_cipher(Cipher, KeyBytes, BlockSize, ReductionConstant, PreparedKey),
		digest_checked(Cipher, PreparedKey, BlockSize, ReductionConstant, MessageBytes, DigestBytes).

	hex_digest(Cipher, KeyBytes, MessageBytes, HexDigest) :-
		digest(Cipher, KeyBytes, MessageBytes, DigestBytes),
		bytes_hex(DigestBytes, HexDigest).

	digest(Cipher, KeyBytes, MessageBytes, Length, DigestBytes) :-
		prepare_cipher(Cipher, KeyBytes, BlockSize, ReductionConstant, PreparedKey),
		check_length(Length, BlockSize),
		digest_checked(Cipher, PreparedKey, BlockSize, ReductionConstant, MessageBytes, FullDigestBytes),
		take(Length, FullDigestBytes, DigestBytes).

	hex_digest(Cipher, KeyBytes, MessageBytes, Length, HexDigest) :-
		digest(Cipher, KeyBytes, MessageBytes, Length, DigestBytes),
		bytes_hex(DigestBytes, HexDigest).

	prepare_cipher(Cipher, KeyBytes, BlockSize, ReductionConstant, PreparedKey) :-
		(	var(Cipher) ->
			instantiation_error
		;	conforms_to_protocol(Cipher, block_cipher_prepared_key_protocol) ->
			Cipher::block_size(BlockSize),
			block_size_reduction_constant(BlockSize, ReductionConstant),
			Cipher::prepare_key(KeyBytes, PreparedKey)
		;	domain_error(cmac_cipher, Cipher)
		).

	block_size_reduction_constant(8, 0x1B) :-
		!.
	block_size_reduction_constant(16, 0x87) :-
		!.
	block_size_reduction_constant(BlockSize, _) :-
		domain_error(cmac_block_size, BlockSize).

	check_length(Length, BlockSize) :-
		(	var(Length) ->
			instantiation_error
		;	integer(Length) ->
			(	Length >= 1, Length =< BlockSize ->
				true
			;	domain_error(cmac_output_length(1, BlockSize), Length)
			)
		;	type_error(integer, Length)
		).

	digest_checked(Cipher, PreparedKey, BlockSize, ReductionConstant, MessageBytes, DigestBytes) :-
		context(Context),
		check(list(byte), MessageBytes, Context),
		zero_bytes(BlockSize, ZeroBlock),
		Cipher::encrypt_prepared_block(PreparedKey, ZeroBlock, L),
		derive_subkey(L, ReductionConstant, K1),
		derive_subkey(K1, ReductionConstant, K2),
		cmac_blocks(Cipher, PreparedKey, BlockSize, MessageBytes, ZeroBlock, K1, K2, DigestBytes).

	cmac_blocks(Cipher, PreparedKey, BlockSize, MessageBytes, Chain, K1, K2, DigestBytes) :-
		length(MessageBytes, Length),
		(	Length > BlockSize ->
			take_block(BlockSize, MessageBytes, Block, Rest),
			xor_bytes(Chain, Block, InputBlock),
			Cipher::encrypt_prepared_block(PreparedKey, InputBlock, NextChain),
			cmac_blocks(Cipher, PreparedKey, BlockSize, Rest, NextChain, K1, K2, DigestBytes)
		;	Length =:= BlockSize ->
			xor_bytes(MessageBytes, K1, LastBlock),
			xor_bytes(Chain, LastBlock, InputBlock),
			Cipher::encrypt_prepared_block(PreparedKey, InputBlock, DigestBytes)
		;	pad_final_block(BlockSize, MessageBytes, PaddedBlock),
			xor_bytes(PaddedBlock, K2, LastBlock),
			xor_bytes(Chain, LastBlock, InputBlock),
			Cipher::encrypt_prepared_block(PreparedKey, InputBlock, DigestBytes)
		).

	derive_subkey([FirstByte| Bytes], ReductionConstant, Subkey) :-
		MostSignificantBit is FirstByte >> 7,
		shift_left([FirstByte| Bytes], Shifted),
		(	MostSignificantBit =:= 0 ->
			Subkey = Shifted
		;	xor_last_byte(Shifted, ReductionConstant, Subkey)
		).

	shift_left(Bytes, Shifted) :-
		shift_left(Bytes, 0, Shifted, _).

	shift_left([], Carry, [], Carry).
	shift_left([Byte| Bytes], CarryIn, [ShiftedByte| ShiftedBytes], CarryOut) :-
		shift_left(Bytes, CarryIn, ShiftedBytes, NextCarry),
		ShiftedByte is ((Byte << 1) /\ 0xFF) \/ NextCarry,
		CarryOut is Byte >> 7.

	xor_last_byte([Byte], Constant, [XorByte]) :-
		!,
		XorByte is xor(Byte, Constant).
	xor_last_byte([Byte| Bytes], Constant, [Byte| XorBytes]) :-
		xor_last_byte(Bytes, Constant, XorBytes).

	take_block(0, Bytes, [], Bytes) :-
		!.
	take_block(Count, [Byte| Bytes], [Byte| Block], Rest) :-
		NextCount is Count - 1,
		take_block(NextCount, Bytes, Block, Rest).

	pad_final_block(BlockSize, Bytes, PaddedBlock) :-
		length(Bytes, Length),
		ZeroCount is BlockSize - Length - 1,
		zero_bytes(ZeroCount, Zeros),
		append(Bytes, [0x80| Zeros], PaddedBlock).

	zero_bytes(0, []) :-
		!.
	zero_bytes(Count, [0| Bytes]) :-
		NextCount is Count - 1,
		zero_bytes(NextCount, Bytes).

	xor_bytes([], [], []).
	xor_bytes([Byte1| Bytes1], [Byte2| Bytes2], [Byte| Bytes]) :-
		Byte is xor(Byte1, Byte2),
		xor_bytes(Bytes1, Bytes2, Bytes).

	bytes_hex(Bytes, Hex) :-
		bytes_hex_codes(Bytes, Codes),
		atom_codes(Hex, Codes).

	bytes_hex_codes([], []).
	bytes_hex_codes([Byte| Bytes], [HighCode, LowCode| Codes]) :-
		High is (Byte >> 4) /\ 0x0F,
		Low is Byte /\ 0x0F,
		hex_digit_code(High, HighCode),
		hex_digit_code(Low, LowCode),
		bytes_hex_codes(Bytes, Codes).

	hex_digit_code(Digit, Code) :-
		(	Digit < 10 ->
			Code is 0'0 + Digit
		;	Code is 0'a + Digit - 10
		).

:- end_object.
