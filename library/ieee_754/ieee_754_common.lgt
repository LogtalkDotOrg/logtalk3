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


:- category(ieee_754_common(_Precision_, _ByteOrder_)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-08-04,
		comment is 'Shared IEEE 754 exact bit and byte handling predicates for the high-level codec and low-level field inspection objects.',
		parnames is ['Precision', 'ByteOrder']
	]).

	:- protected(source_bits/2).
	:- mode(source_bits(++compound, --integer), one_or_error).
	:- info(source_bits/2, [
		comment is 'Validates and normalizes a source term into canonical-order unsigned integer bits for the selected precision and byte order.',
		argnames is ['Source', 'Bits'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is not a variable but uses ``bytes(Bytes)`` or ``bits(Bits)`` with an encoding incompatible with the selected precision' - domain_error(ieee_754_encoding, 'Source'),
			'``Source`` is not a valid IEEE 754 source term' - domain_error(ieee_754_source, 'Source')
		]
	]).

	:- protected(bits_fields/5).
	:- mode(bits_fields(+integer, -integer, -integer, -integer, -atom), one).
	:- info(bits_fields/5, [
		comment is 'Extracts exact IEEE 754 sign, exponent bits, mantissa bits, and a class atom from a validated bit pattern.',
		argnames is ['Bits', 'Sign', 'ExponentBits', 'MantissaBits', 'Class']
	]).

	:- protected(bits_finite_binary_rational/4).
	:- mode(bits_finite_binary_rational(+integer, -integer, -integer, -integer), zero_or_one).
	:- info(bits_finite_binary_rational/4, [
		comment is 'Extracts an exact finite IEEE 754 value from a validated bit pattern as ``(-1)^Sign * Significand * 2^Exponent``.',
		argnames is ['Bits', 'Sign', 'Significand', 'Exponent']
	]).

	:- protected(bits_nan_payload/2).
	:- mode(bits_nan_payload(+integer, -integer), zero_or_one).
	:- info(bits_nan_payload/2, [
		comment is 'Extracts the raw NaN mantissa payload bits from a validated bit pattern.',
		argnames is ['Bits', 'PayloadBits']
	]).

	:- protected(bits_nan_kind/2).
	:- mode(bits_nan_kind(+integer, -atom), zero_or_one).
	:- info(bits_nan_kind/2, [
		comment is 'Classifies a validated NaN bit pattern as ``quiet`` or ``signaling`` using the quiet/signaling discriminator bit for the selected precision.',
		argnames is ['Bits', 'Kind']
	]).

	:- protected(canonical_nan_bits/1).
	:- mode(canonical_nan_bits(-integer), one).
	:- info(canonical_nan_bits/1, [
		comment is 'Returns the canonical quiet NaN bit pattern for the selected precision.',
		argnames is ['Bits']
	]).

	:- protected(nan_bits/1).
	:- mode(nan_bits(+integer), zero_or_one).
	:- info(nan_bits/1, [
		comment is 'True when the validated bit pattern encodes a NaN value for the selected precision.',
		argnames is ['Bits']
	]).

	:- protected(valid_bits/1).
	:- mode(valid_bits(+integer), zero_or_one).
	:- info(valid_bits/1, [
		comment is 'True when the integer fits in the selected precision bit width.',
		argnames is ['Bits']
	]).

	:- protected(zero_bits/2).
	:- mode(zero_bits(+integer, -integer), one).
	:- info(zero_bits/2, [
		comment is 'Returns the zero encoding bits with the requested sign bit.',
		argnames is ['Sign', 'Bits']
	]).

	:- protected(zero_from_sign/2).
	:- mode(zero_from_sign(+integer, -float), one).
	:- info(zero_from_sign/2, [
		comment is 'Constructs a positive or negative floating-point zero from a sign bit.',
		argnames is ['Sign', 'Zero']
	]).

	:- protected(apply_float_sign/3).
	:- mode(apply_float_sign(+integer, +float, -float), one).
	:- info(apply_float_sign/3, [
		comment is 'Applies a sign bit to a positive floating-point magnitude.',
		argnames is ['Sign', 'Magnitude', 'Value']
	]).

	:- protected(precision_spec/5).
	:- mode(precision_spec(+atom, -integer, -integer, -integer, -integer), one).
	:- info(precision_spec/5, [
		comment is 'Returns the exponent width, mantissa width, exponent bias, and byte count for a supported IEEE 754 precision.',
		argnames is ['Precision', 'ExponentWidth', 'MantissaWidth', 'Bias', 'ByteCount']
	]).

	:- uses(byte_order, [
		bytes_to_integer/3
	]).

	:- uses(type, [
		valid/2
	]).

	source_bits(Source, _) :-
		var(Source),
		instantiation_error.
	source_bits(bytes(Bytes), Bits) :-
		!,
		precision_spec(_Precision_, _, _, _, Count),
		(	valid(list(byte, Count), Bytes) ->
			bytes_to_integer(_ByteOrder_, Bytes, Bits)
		;	domain_error(ieee_754_encoding, bytes(Bytes))
		).
	source_bits(bits(Bits), Bits) :-
		!,
		(	valid_bits(Bits) ->
			true
		;	domain_error(ieee_754_encoding, bits(Bits))
		).
	source_bits(Source, _) :-
		domain_error(ieee_754_source, Source).

	bits_fields(Bits, Sign, ExponentBits, MantissaBits, Class) :-
		precision_spec(_Precision_, ExponentWidth, MantissaWidth, _, _),
		Sign is (Bits >> (ExponentWidth + MantissaWidth)) /\ 0x01,
		ExponentBits is (Bits >> MantissaWidth) /\ ((1 << ExponentWidth) - 1),
		MantissaBits is Bits /\ ((1 << MantissaWidth) - 1),
		AllOnesExponent is (1 << ExponentWidth) - 1,
		(	ExponentBits =:= 0 ->
			(	MantissaBits =:= 0 ->
				Class = zero
			;	Class = subnormal
			)
		;	ExponentBits =:= AllOnesExponent ->
			(	MantissaBits =:= 0 ->
				Class = infinity
			;	Class = not_a_number
			)
		;	Class = normal
		).

	bits_finite_binary_rational(Bits, Sign, Significand, Exponent) :-
		bits_fields(Bits, Sign, ExponentBits, MantissaBits, Class),
		(	Class == zero ->
			Significand = 0,
			Exponent = 0
		;	Class == subnormal ->
			precision_spec(_Precision_, _, MantissaWidth, Bias, _),
			Significand = MantissaBits,
			Exponent is 1 - Bias - MantissaWidth
		;	Class == normal ->
			precision_spec(_Precision_, _, MantissaWidth, Bias, _),
			Significand is (1 << MantissaWidth) + MantissaBits,
			Exponent is ExponentBits - Bias - MantissaWidth
		;	fail
		).

	bits_nan_payload(Bits, PayloadBits) :-
		bits_fields(Bits, _Sign, _ExponentBits, PayloadBits, not_a_number).

	bits_nan_kind(Bits, Kind) :-
		bits_fields(Bits, _Sign, _ExponentBits, PayloadBits, not_a_number),
		precision_spec(_Precision_, _, MantissaWidth, _, _),
		QuietBit is 1 << (MantissaWidth - 1),
		(	PayloadBits /\ QuietBit =:= 0 ->
			Kind = signaling
		;	Kind = quiet
		).

	canonical_nan_bits(Bits) :-
		precision_spec(_Precision_, ExponentWidth, MantissaWidth, _, _),
		ExponentBits is (1 << ExponentWidth) - 1,
		MantissaBits is 1 << (MantissaWidth - 1),
		Bits is (ExponentBits << MantissaWidth) \/ MantissaBits.

	nan_bits(Bits) :-
		bits_fields(Bits, _Sign, _ExponentBits, _MantissaBits, not_a_number).

	valid_bits(Bits) :-
		integer(Bits),
		Bits >= 0,
		precision_spec(_Precision_, _, _, _, Count),
		BitCount is Count * 8,
		Bits < (1 << BitCount).

	zero_bits(Sign, Bits) :-
		precision_spec(_Precision_, ExponentWidth, MantissaWidth, _, _),
		Bits is Sign << (ExponentWidth + MantissaWidth).

	zero_from_sign(0, 0.0).
	zero_from_sign(1, NegativeZero) :-
		NegativeZero is -0.0.

	apply_float_sign(0, Magnitude, Magnitude).
	apply_float_sign(1, Magnitude, Value) :-
		Value is -Magnitude.

	precision_spec(half, 5, 10, 15, 2).
	precision_spec(single, 8, 23, 127, 4).
	precision_spec(double, 11, 52, 1023, 8).

:- end_category.
