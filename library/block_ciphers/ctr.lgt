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

:- object(ctr,
	imports(block_cipher_modes_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'Generic Counter mode encryption and decryption using a whole-block big-endian counter.'
	]).

	:- public(crypt/5).
	:- mode(crypt(+object_identifier, +list(byte), +list(byte), +list(byte), --list(byte)), one_or_error).
	:- info(crypt/5, [
		comment is 'Encrypts or decrypts bytes using an explicit initial whole-block big-endian counter.',
		argnames is ['Cipher', 'Key', 'InitialCounter', 'Input', 'Output']
	]).

	crypt(Cipher, Key, InitialCounter, Input, Output) :-
		^^prepare_cipher(Cipher, Key, BlockSize, PreparedKey),
		^^check_block(BlockSize, InitialCounter),
		^^check_bytes(Input),
		crypt_bytes(Input, BlockSize, Cipher, PreparedKey, InitialCounter, Output).

	crypt_bytes([], _, _, _, _, []) :-
		!.
	crypt_bytes(Input, BlockSize, Cipher, PreparedKey, Counter, Output) :-
		Input = [_| _],
		take_up_to(BlockSize, Input, Chunk, RestInput),
		Cipher::encrypt_prepared_block(PreparedKey, Counter, Keystream),
		xor_prefix(Chunk, Keystream, Output, RestOutput),
		increment_counter(Counter, NextCounter),
		crypt_bytes(RestInput, BlockSize, Cipher, PreparedKey, NextCounter, RestOutput).

	take_up_to(0, Bytes, [], Bytes) :-
		!.
	take_up_to(_, [], [], []) :-
		!.
	take_up_to(Count, [Byte| Bytes], [Byte| Prefix], Rest) :-
		NextCount is Count - 1,
		take_up_to(NextCount, Bytes, Prefix, Rest).

	xor_prefix([], _, Bytes, Bytes).
	xor_prefix([Byte| Bytes], [KeyByte| KeyBytes], [OutputByte| OutputBytes], Rest) :-
		OutputByte is xor(Byte, KeyByte),
		xor_prefix(Bytes, KeyBytes, OutputBytes, Rest).

	increment_counter(Counter, NextCounter) :-
		increment_counter(Counter, NextCounter, _).

	increment_counter([], [], 1).
	increment_counter([Byte| Bytes], [NextByte| NextBytes], Carry) :-
		increment_counter(Bytes, NextBytes, Carry0),
		Value is Byte + Carry0,
		NextByte is Value mod 256,
		Carry is Value div 256.

:- end_object.
