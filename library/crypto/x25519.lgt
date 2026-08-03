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


:- category(x25519,
	complements(crypto)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'X25519 Diffie-Hellman key agreement implementation (RFC 7748). Requires exact, unbounded integer arithmetic for arithmetic modulo the 255-bit field prime.'
	]).

	:- public(x25519_keypair/2).
	:- mode(x25519_keypair(-list(byte), -list(byte)), one).
	:- info(x25519_keypair/2, [
		comment is 'Generates a fresh random 32-byte PrivateKey and derives the corresponding 32-byte PublicKey. Available only on backends with unbounded integer arithmetic.',
		argnames is ['PrivateKey', 'PublicKey']
	]).

	:- public(x25519_public_key/2).
	:- mode(x25519_public_key(+list(byte), -list(byte)), one_or_error).
	:- info(x25519_public_key/2, [
		comment is 'Derives the 32-byte X25519 PublicKey corresponding to a 32-byte PrivateKey. Available only on backends with unbounded integer arithmetic.',
		argnames is ['PrivateKey', 'PublicKey'],
		exceptions is [
			'``PrivateKey`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``PrivateKey`` is neither a variable nor a list of 32 bytes' - type_error(list(byte, 32), 'PrivateKey'),
			'``PrivateKey`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``PrivateKey`` contains an integer outside the byte range' - domain_error(byte, 'Byte')
		]
	]).

	:- public(x25519_shared_secret/3).
	:- mode(x25519_shared_secret(+list(byte), +list(byte), -list(byte)), zero_or_one_or_error).
	:- info(x25519_shared_secret/3, [
		comment is 'Computes the 32-byte X25519 SharedSecret using PrivateKey and PeerPublicKey. Fails when the result is all zeroes. Available only on backends with unbounded integer arithmetic.',
		argnames is ['PrivateKey', 'PeerPublicKey', 'SharedSecret'],
		exceptions is [
			'``PrivateKey`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``PrivateKey`` is neither a variable nor a list of 32 bytes' - type_error(list(byte, 32), 'PrivateKey'),
			'``PrivateKey`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``PrivateKey`` contains an integer outside the byte range' - domain_error(byte, 'Byte'),
			'``PeerPublicKey`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'``PeerPublicKey`` is neither a variable nor a list of 32 bytes' - type_error(list(byte, 32), 'PeerPublicKey'),
			'``PeerPublicKey`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``PeerPublicKey`` contains an integer outside the byte range' - domain_error(byte, 'Byte')
		]
	]).

	:- uses(list, [
		length/2
	]).

	:- uses(type, [
		check/3
	]).

	x25519_keypair(PrivateKey, PublicKey) :-
		@random_bytes(32, PrivateKey),
		x25519_public_key(PrivateKey, PublicKey).

	x25519_public_key(PrivateKey, PublicKey) :-
		context(Context),
		check(list(byte, 32), PrivateKey, Context),
		x25519_scalar_mult(PrivateKey, [9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], PublicKey).

	x25519_shared_secret(PrivateKey, PeerPublicKey, SharedSecret) :-
		context(Context),
		check(list(byte, 32), PrivateKey, Context),
		check(list(byte, 32), PeerPublicKey, Context),
		x25519_scalar_mult(PrivateKey, PeerPublicKey, SharedSecret),
		\+ all_zero_bytes(SharedSecret).

	x25519_scalar_mult(ScalarBytes, UBytes, ResultBytes) :-
		le_bytes_to_int(ScalarBytes, Scalar0),
		Scalar is (Scalar0 /\ ((1 << 255) - 8)) \/ (1 << 254),
		le_bytes_to_int(UBytes, U0),
		U is U0 /\ ((1 << 255) - 1),
		x25519_ladder(254, Scalar, U, 1, 0, U, 1, 0, X2, Z2),
		x25519_prime(Prime),
		x25519_modexp(Z2, Prime - 2, Prime, Z2Inverse),
		Result is (X2 * Z2Inverse) mod Prime,
		int_to_le_bytes_fixed(Result, 32, ResultBytes).

	x25519_ladder(-1, _Scalar, _U, X2, Z2, X3, Z3, Swap, X, Z) :-
		!,
		conditional_swap(Swap, X2, X3, X, _),
		conditional_swap(Swap, Z2, Z3, Z, _).
	x25519_ladder(BitIndex, Scalar, U, X2, Z2, X3, Z3, Swap0, X, Z) :-
		Bit is (Scalar >> BitIndex) /\ 1,
		Swap1 is xor(Swap0, Bit),
		conditional_swap(Swap1, X2, X3, X2a, X3a),
		conditional_swap(Swap1, Z2, Z3, Z2a, Z3a),
		x25519_prime(Prime),
		A is (X2a + Z2a) mod Prime,
		AA is (A * A) mod Prime,
		B is (X2a - Z2a) mod Prime,
		BB is (B * B) mod Prime,
		E is (AA - BB) mod Prime,
		C is (X3a + Z3a) mod Prime,
		D is (X3a - Z3a) mod Prime,
		DA is (D * A) mod Prime,
		CB is (C * B) mod Prime,
		X3b is ((DA + CB) * (DA + CB)) mod Prime,
		Z3b is (U * (DA - CB) * (DA - CB)) mod Prime,
		X2b is (AA * BB) mod Prime,
		Z2b is (E * (AA + 121665 * E)) mod Prime,
		NextBitIndex is BitIndex - 1,
		x25519_ladder(NextBitIndex, Scalar, U, X2b, Z2b, X3b, Z3b, Bit, X, Z).

	conditional_swap(Swap, A, B, A1, B1) :-
		A1 is A + Swap * (B - A),
		B1 is B + Swap * (A - B).

	x25519_prime(Prime) :-
		Prime is (1 << 255) - 19.

	x25519_modexp(_Base, 0, Modulus, Result) :-
		!,
		Result is 1 mod Modulus.
	x25519_modexp(Base, Exponent, Modulus, Result) :-
		HalfExponent is Exponent // 2,
		x25519_modexp(Base, HalfExponent, Modulus, Half),
		Square is (Half * Half) mod Modulus,
		(	Exponent mod 2 =:= 1 ->
			Result is (Square * (Base mod Modulus)) mod Modulus
		;	Result = Square
		).

	le_bytes_to_int(Bytes, Integer) :-
		le_bytes_to_int(Bytes, 0, 0, Integer).
	le_bytes_to_int([], _Shift, Integer, Integer).
	le_bytes_to_int([Byte| Bytes], Shift, Integer0, Integer) :-
		Integer1 is Integer0 \/ (Byte << Shift),
		NextShift is Shift + 8,
		le_bytes_to_int(Bytes, NextShift, Integer1, Integer).

	int_to_le_bytes_fixed(Integer, Count, Bytes) :-
		length(Bytes, Count),
		int_to_le_bytes_fixed(Integer, Bytes).
	int_to_le_bytes_fixed(_Integer, []) :-
		!.
	int_to_le_bytes_fixed(Integer, [Byte| Bytes]) :-
		Byte is Integer /\ 0xff,
		NextInteger is Integer >> 8,
		int_to_le_bytes_fixed(NextInteger, Bytes).

	all_zero_bytes([]).
	all_zero_bytes([0| Bytes]) :-
		all_zero_bytes(Bytes).

:- end_category.
