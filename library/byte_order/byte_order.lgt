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


:- object(byte_order).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-04,
		comment is 'Predicates for converting between integers and fixed-size byte lists in big-endian and little-endian order.'
	]).

	:- public(integer_to_bytes/4).
	:- mode(integer_to_bytes(+atom, +integer, +integer, -list(integer)), one).
	:- info(integer_to_bytes/4, [
		comment is 'Converts the low-order bytes of an integer into a byte list with the requested byte order and size.',
		argnames is ['Order', 'ByteCount', 'Integer', 'Bytes']
	]).

	:- public(integer_to_bytes/5).
	:- mode(integer_to_bytes(+atom, +integer, +integer, -list(integer), --term), one).
	:- info(integer_to_bytes/5, [
		comment is 'Converts the low-order bytes of an integer into a byte list with the requested byte order and size, represented as a difference list.',
		argnames is ['Order', 'ByteCount', 'Integer', 'Bytes', 'Tail']
	]).

	:- public(bytes_to_integer/3).
	:- mode(bytes_to_integer(+atom, +list(integer), -integer), one).
	:- info(bytes_to_integer/3, [
		comment is 'Converts a byte list with the requested byte order into an unsigned integer.',
		argnames is ['Order', 'Bytes', 'Integer']
	]).

	:- public(bytes_to_integer/5).
	:- mode(bytes_to_integer(+atom, +integer, +list(integer), -integer, -list(integer)), one).
	:- info(bytes_to_integer/5, [
		comment is 'Consumes the requested number of bytes from a list and converts them with the requested byte order into an unsigned integer.',
		argnames is ['Order', 'ByteCount', 'Bytes', 'Integer', 'Rest']
	]).

	:- public(signed_integer_to_bytes/4).
	:- mode(signed_integer_to_bytes(+atom, +integer, +integer, -list(integer)), one).
	:- info(signed_integer_to_bytes/4, [
		comment is 'Converts the low-order bytes of a signed integer two\'s-complement representation into a byte list with the requested byte order and size.',
		argnames is ['Order', 'ByteCount', 'Integer', 'Bytes']
	]).

	:- public(signed_integer_to_bytes/5).
	:- mode(signed_integer_to_bytes(+atom, +integer, +integer, -list(integer), --term), one).
	:- info(signed_integer_to_bytes/5, [
		comment is 'Converts the low-order bytes of a signed integer two\'s-complement representation into a byte list with the requested byte order and size, represented as a difference list.',
		argnames is ['Order', 'ByteCount', 'Integer', 'Bytes', 'Tail']
	]).

	:- public(bytes_to_signed_integer/3).
	:- mode(bytes_to_signed_integer(+atom, +list(integer), -integer), one).
	:- info(bytes_to_signed_integer/3, [
		comment is 'Converts a byte list with the requested byte order from a two\'s-complement representation into a signed integer.',
		argnames is ['Order', 'Bytes', 'Integer']
	]).

	:- public(bytes_to_signed_integer/5).
	:- mode(bytes_to_signed_integer(+atom, +integer, +list(integer), -integer, -list(integer)), one).
	:- info(bytes_to_signed_integer/5, [
		comment is 'Consumes the requested number of bytes from a list and converts them with the requested byte order from a two\'s-complement representation into a signed integer.',
		argnames is ['Order', 'ByteCount', 'Bytes', 'Integer', 'Rest']
	]).

	integer_to_bytes(Order, ByteCount, Integer, Bytes) :-
		integer_to_bytes(Order, ByteCount, Integer, Bytes, []).

	integer_to_bytes(little, ByteCount, Integer, Bytes, Tail) :-
		integer_to_little_endian_bytes(ByteCount, Integer, Bytes, Tail).
	integer_to_bytes(big, ByteCount, Integer, Bytes, Tail) :-
		integer_to_big_endian_bytes(ByteCount, Integer, Bytes, Tail).

	bytes_to_integer(little, Bytes, Integer) :-
		little_endian_bytes_to_integer(Bytes, Integer).
	bytes_to_integer(big, Bytes, Integer) :-
		big_endian_bytes_to_integer(Bytes, 0, Integer).

	bytes_to_integer(little, ByteCount, Bytes, Integer, Rest) :-
		little_endian_bytes_to_integer(ByteCount, Bytes, Integer, Rest).
	bytes_to_integer(big, ByteCount, Bytes, Integer, Rest) :-
		big_endian_bytes_to_integer(ByteCount, Bytes, 0, Integer, Rest).

	signed_integer_to_bytes(Order, ByteCount, Integer, Bytes) :-
		signed_integer_to_bytes(Order, ByteCount, Integer, Bytes, []).

	signed_integer_to_bytes(Order, ByteCount, Integer, Bytes, Tail) :-
		integer_to_bytes(Order, ByteCount, Integer, Bytes, Tail).

	bytes_to_signed_integer(little, Bytes, Integer) :-
		little_endian_bytes_to_signed_integer(Bytes, Integer).
	bytes_to_signed_integer(big, [Byte| Bytes], Integer) :-
		sign_accumulator(Byte, Integer0),
		big_endian_bytes_to_integer([Byte| Bytes], Integer0, Integer).

	bytes_to_signed_integer(little, ByteCount, Bytes, Integer, Rest) :-
		little_endian_bytes_to_signed_integer(ByteCount, Bytes, Integer, Rest).
	bytes_to_signed_integer(big, ByteCount, [Byte| Bytes], Integer, Rest) :-
		sign_accumulator(Byte, Integer0),
		big_endian_bytes_to_integer(ByteCount, [Byte| Bytes], Integer0, Integer, Rest).

	integer_to_little_endian_bytes(0, _Integer, Tail, Tail) :-
		!.
	integer_to_little_endian_bytes(ByteCount, Integer, [Byte| Bytes], Tail) :-
		Byte is Integer /\ 255,
		NextInteger is Integer >> 8,
		NextByteCount is ByteCount - 1,
		integer_to_little_endian_bytes(NextByteCount, NextInteger, Bytes, Tail).

	integer_to_big_endian_bytes(0, _Integer, Tail, Tail) :-
		!.
	integer_to_big_endian_bytes(ByteCount, Integer, [Byte| Bytes], Tail) :-
		Shift is (ByteCount - 1) * 8,
		Byte is (Integer >> Shift) /\ 255,
		NextByteCount is ByteCount - 1,
		integer_to_big_endian_bytes(NextByteCount, Integer, Bytes, Tail).

	little_endian_bytes_to_integer([], 0).
	little_endian_bytes_to_integer([Byte| Bytes], Integer) :-
		little_endian_bytes_to_integer(Bytes, Integer0),
		Integer is Integer0 * 256 + Byte.

	little_endian_bytes_to_integer(0, Rest, 0, Rest) :-
		!.
	little_endian_bytes_to_integer(ByteCount, [Byte| Bytes], Integer, Rest) :-
		NextByteCount is ByteCount - 1,
		little_endian_bytes_to_integer(NextByteCount, Bytes, Integer0, Rest),
		Integer is Integer0 * 256 + Byte.

	big_endian_bytes_to_integer([], Integer, Integer).
	big_endian_bytes_to_integer([Byte| Bytes], Integer0, Integer) :-
		Integer1 is Integer0 * 256 + Byte,
		big_endian_bytes_to_integer(Bytes, Integer1, Integer).

	big_endian_bytes_to_integer(0, Rest, Integer, Integer, Rest) :-
		!.
	big_endian_bytes_to_integer(ByteCount, [Byte| Bytes], Integer0, Integer, Rest) :-
		Integer1 is Integer0 * 256 + Byte,
		NextByteCount is ByteCount - 1,
		big_endian_bytes_to_integer(NextByteCount, Bytes, Integer1, Integer, Rest).

	little_endian_bytes_to_signed_integer([Byte], Integer) :-
		!,
		sign_accumulator(Byte, Integer0),
		Integer is Integer0 * 256 + Byte.
	little_endian_bytes_to_signed_integer([Byte, NextByte| Bytes], Integer) :-
		little_endian_bytes_to_signed_integer([NextByte| Bytes], Integer0),
		Integer is Integer0 * 256 + Byte.

	little_endian_bytes_to_signed_integer(1, [Byte| Rest], Integer, Rest) :-
		!,
		sign_accumulator(Byte, Integer0),
		Integer is Integer0 * 256 + Byte.
	little_endian_bytes_to_signed_integer(ByteCount, [Byte| Bytes], Integer, Rest) :-
		NextByteCount is ByteCount - 1,
		little_endian_bytes_to_signed_integer(NextByteCount, Bytes, Integer0, Rest),
		Integer is Integer0 * 256 + Byte.

	sign_accumulator(Byte, Integer) :-
		(	Byte /\ 128 =:= 0 ->
			Integer = 0
		;	Integer = -1
		).

:- end_object.
