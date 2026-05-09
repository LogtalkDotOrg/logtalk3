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


:- protocol(ieee_754_fields_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-09,
		comment is 'IEEE 754 exact bit-field support library protocol. Intended to be implemented by parameterized objects such as ``ieee_754_fields(Precision, ByteOrder)``.'
	]).

	:- public(classify/2).
	:- mode(classify(++compound, --atom), one_or_error).
	:- info(classify/2, [
		comment is 'Classifies an IEEE 754 encoding source term as ``zero``, ``subnormal``, ``normal``, ``infinity``, or ``not_a_number`` for the selected precision and byte order. Supported source terms are ``bytes(Bytes)`` and ``bits(Bits)``.',
		argnames is ['Source', 'Class'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid source term' - domain_error(ieee_754_source, 'Source'),
			'``Source`` does not contain a valid encoding for the selected object' - domain_error(ieee_754_encoding, 'Source')
		]
	]).

	:- public(fields/5).
	:- mode(fields(++compound, --integer, --integer, --integer, --atom), one_or_error).
	:- info(fields/5, [
		comment is 'Extracts the exact IEEE 754 sign bit, exponent bits, mantissa bits, and classification from a source term.',
		argnames is ['Source', 'Sign', 'ExponentBits', 'MantissaBits', 'Class'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid source term' - domain_error(ieee_754_source, 'Source'),
			'``Source`` does not contain a valid encoding for the selected object' - domain_error(ieee_754_encoding, 'Source')
		]
	]).

	:- public(finite_binary_rational/4).
	:- mode(finite_binary_rational(++compound, --integer, --integer, --integer), one_or_error).
	:- info(finite_binary_rational/4, [
		comment is 'Extracts the exact finite IEEE 754 value as ``(-1)^Sign * Significand * 2^Exponent`` from a source term. Zero encodings return a zero significand and exponent ``0``.',
		argnames is ['Source', 'Sign', 'Significand', 'Exponent'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid source term' - domain_error(ieee_754_source, 'Source'),
			'``Source`` does not contain a valid encoding for the selected object' - domain_error(ieee_754_encoding, 'Source'),
			'``Source`` does not encode a finite IEEE 754 value' - domain_error(ieee_754_finite_encoding, 'Source')
		]
	]).

	:- public(nan_payload/2).
	:- mode(nan_payload(++compound, --integer), one_or_error).
	:- info(nan_payload/2, [
		comment is 'Extracts the raw NaN mantissa payload bits from a source term.',
		argnames is ['Source', 'PayloadBits'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid source term' - domain_error(ieee_754_source, 'Source'),
			'``Source`` does not contain a valid encoding for the selected object' - domain_error(ieee_754_encoding, 'Source'),
			'``Source`` does not encode a NaN value' - domain_error(ieee_754_nan_encoding, 'Source')
		]
	]).

	:- public(nan_kind/2).
	:- mode(nan_kind(++compound, --atom), one_or_error).
	:- info(nan_kind/2, [
		comment is 'Classifies a NaN source term as ``quiet`` or ``signaling`` using the exact IEEE 754 quiet/signaling discriminator bit for the selected precision.',
		argnames is ['Source', 'Kind'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid source term' - domain_error(ieee_754_source, 'Source'),
			'``Source`` does not contain a valid encoding for the selected object' - domain_error(ieee_754_encoding, 'Source'),
			'``Source`` does not encode a NaN value' - domain_error(ieee_754_nan_encoding, 'Source')
		]
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

	:- public(byte_count/1).
	:- mode(byte_count(-integer), one).
	:- info(byte_count/1, [
		comment is 'Returns the number of bytes used by the selected precision.',
		argnames is ['ByteCount']
	]).

:- end_protocol.
