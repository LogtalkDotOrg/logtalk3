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


:- protocol(ieee_754_protocol).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-05-09,
		comment is 'IEEE 754 floating-point support library protocol. Intended to be implemented by parameterized objects such as ``ieee_754(Precision, ByteOrder, NaNRepresentation)``.'
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --term), one_or_error).
	:- info(parse/2, [
		comment is 'Parses an IEEE 754 value from a source term for the selected precision, byte order, and NaN representation policy. Supported source terms are ``bytes(Bytes)`` and ``bits(Bits)``.',
		argnames is ['Source', 'Value'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid source term' - domain_error(ieee_754_source, 'Source'),
			'``Source`` does not contain a valid encoding for the selected object' - domain_error(ieee_754_encoding, 'Source')
		]
	]).

	:- public(generate/2).
	:- mode(generate(+compound, ++term), one_or_error).
	:- info(generate/2, [
		comment is 'Generates an IEEE 754 encoding for a value term using the selected precision and byte order. Supported sink terms are ``bytes(Bytes)`` and ``bits(Bits)``. Finite values are encoded using the selected precision rounding semantics.',
		argnames is ['Sink', 'Value'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``Sink`` is neither a variable nor a valid sink term' - domain_error(ieee_754_sink, 'Sink'),
			'``Value`` is neither a valid finite float nor a supported IEEE 754 special value term' - domain_error(ieee_754_value, 'Value'),
			'``Value`` cannot be represented in the selected precision' - domain_error(ieee_754_representation, 'Value')
		]
	]).

	:- public(generate/3).
	:- mode(generate(+term, -list(byte), --variable), one_or_error).
	:- info(generate/3, [
		comment is 'Generates an IEEE 754 encoding as a list of bytes with an open tail. Intended for use by libraries that build larger binary payloads using difference lists.',
		argnames is ['Value', 'Bytes', 'Tail'],
		exceptions is [
			'``Value`` is neither a valid finite float nor a supported IEEE 754 special value term' - domain_error(ieee_754_value, 'Value'),
			'``Value`` cannot be represented in the selected precision' - domain_error(ieee_754_representation, 'Value')
		]
	]).

	:- public(valid/1).
	:- mode(valid(@term), zero_or_one).
	:- info(valid/1, [
		comment is 'True iff the argument is a valid value term for the selected object. Valid terms are finite floats plus the special values supported by the selected NaN representation policy.',
		argnames is ['Value']
	]).

	:- public(exactly_representable/1).
	:- mode(exactly_representable(@term), zero_or_one).
	:- info(exactly_representable/1, [
		comment is 'True iff the argument can be encoded in the selected precision and decoded again without loss of value information under the selected NaN representation policy. Intended for callers such as MessagePack that choose the smallest exact binary format.',
		argnames is ['Value']
	]).

	:- public(precision/1).
	:- mode(precision(-atom), one).
	:- info(precision/1, [
		comment is 'Returns the selected IEEE 754 precision. Expected values are ``half``, ``single``, or ``double``.',
		argnames is ['Precision']
	]).

	:- public(order/1).
	:- mode(order(-atom), one).
	:- info(order/1, [
		comment is 'Returns the selected byte order. Expected values are ``big`` or ``little``.',
		argnames is ['ByteOrder']
	]).

	:- public(nan_representation/1).
	:- mode(nan_representation(-atom), one).
	:- info(nan_representation/1, [
		comment is 'Returns the selected NaN representation policy. Expected values are ``canonical`` and ``payloads``.',
		argnames is ['NaNRepresentation']
	]).

	:- public(byte_count/1).
	:- mode(byte_count(-integer), one).
	:- info(byte_count/1, [
		comment is 'Returns the number of bytes used by the selected precision.',
		argnames is ['ByteCount']
	]).

:- end_protocol.
