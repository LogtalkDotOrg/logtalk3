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


:- object(ieee_754(_Precision_, _ByteOrder_, _NaNRepresentation_),
	implements(ieee_754_protocol),
	imports(ieee_754_common(_Precision_, _ByteOrder_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-09,
		comment is 'IEEE 754 floating-point encoder and decoder.',
		parameters is [
			'Precision' - 'Floating-point format precision. Supported values are ``half``, ``single``, and ``double``.',
			'ByteOrder' - 'Byte order used when parsing and generating byte sequences. Supported values are ``big`` and ``little``.',
			'NaNRepresentation' - 'NaN decoding and generation policy. Supported values are ``canonical`` and ``payloads``.'
		]
	]).

	parse(Source, Value) :-
		^^source_bits(Source, Bits),
		bits_to_value(Bits, Value).

	generate(Sink, Value) :-
		value_bits(Value, Bits),
		sink_bits(Sink, Bits).

	generate(Value, Bytes, Tail) :-
		value_bits(Value, Bits),
		byte_count(Count),
		^^integer_to_bytes(Count, Bits, CanonicalBytes),
		^^order_bytes(_ByteOrder_, CanonicalBytes, OrderedBytes),
		^^bytes_tail(OrderedBytes, Bytes, Tail).

	valid(Value) :-
		valid_value(Value).

	exactly_representable(Value) :-
		valid_value(Value),
		value_bits(Value, Bits),
		bits_to_value(Bits, RoundtripValue),
		same_value(Value, RoundtripValue, Bits),
		value_bits(RoundtripValue, RoundtripBits),
		Bits == RoundtripBits.

	precision(_Precision_).

	order(_ByteOrder_).

	nan_representation(_NaNRepresentation_).

	byte_count(Count) :-
		^^precision_spec(_Precision_, _, _, _, Count).

	sink_bits(Sink, _) :-
		var(Sink),
		!,
		instantiation_error.
	sink_bits(bytes(Bytes), Bits) :-
		!,
		byte_count(Count),
		^^integer_to_bytes(Count, Bits, CanonicalBytes),
		^^order_bytes(_ByteOrder_, CanonicalBytes, Bytes).
	sink_bits(bits(Bits), Bits) :-
		!.
	sink_bits(Sink, _) :-
		domain_error(ieee_754_sink, Sink).

	valid_value(Value) :-
		(   float(Value) ->
			true
		;   Value == @infinity ->
			true
		;   Value == @negative_infinity ->
			true
		;   Value == @not_a_number ->
			true
		;   _NaNRepresentation_ == payloads,
			Value = not_a_number(Bytes),
			valid_nan_bytes(Bytes)
		).

	same_value(@infinity, @infinity, _).
	same_value(@negative_infinity, @negative_infinity, _).
	same_value(@not_a_number, @not_a_number, _).
	same_value(not_a_number(Bytes), not_a_number(Bytes), _) :-
		!.
	same_value(Value, RoundtripValue, Bits) :-
		float(Value),
		float(RoundtripValue),
		(   Value =:= 0 ->
			value_bits(RoundtripValue, RoundtripBits),
			RoundtripBits == Bits
		;   RoundtripValue =:= Value
		).

	value_bits(Value, _) :-
		var(Value),
		!,
		domain_error(ieee_754_value, Value).
	value_bits(@infinity, Bits) :-
		!,
		special_bits(infinity, Bits).
	value_bits(@negative_infinity, Bits) :-
		!,
		special_bits(negative_infinity, Bits).
	value_bits(@not_a_number, Bits) :-
		!,
		special_bits(not_a_number, Bits).
	value_bits(not_a_number(Bytes), Bits) :-
		!,
		(   _NaNRepresentation_ == payloads,
			valid_nan_bytes(Bytes) ->
			^^bytes_to_unsigned_integer(Bytes, Bits)
		;   domain_error(ieee_754_value, not_a_number(Bytes))
		).
	value_bits(Value, Bits) :-
		float(Value),
		!,
		(   finite_value_bits(Value, Bits) ->
			true
		;   domain_error(ieee_754_representation, Value)
		).
	value_bits(Value, _) :-
		domain_error(ieee_754_value, Value).

	finite_value_bits(0.0, Bits) :-
		!,
		^^zero_bits(0, Bits).
	finite_value_bits(Value, Bits) :-
		Value = -0.0,
		!,
		^^zero_bits(1, Bits).
	finite_value_bits(Value, Bits) :-
		float_sign_and_abs(Value, Sign, AbsValue),
		(   rounds_to_zero(AbsValue) ->
			^^zero_bits(Sign, Bits)
		;   _Precision_ == double ->
			float_to_ieee754_double(Value, Bits)
		;   float_to_ieee754_double(Value, DoubleBits),
			round_double_bits(DoubleBits, Bits)
		).

	rounds_to_zero(AbsValue) :-
		^^precision_spec(_Precision_, _, MantissaWidth, Bias, _),
		ZeroThresholdExponent is -Bias - MantissaWidth,
		catch(
			power_of_two_float(ZeroThresholdExponent, Threshold),
			error(evaluation_error(underflow), _),
			Threshold = 0.0
		),
		AbsValue =< Threshold.

	round_double_bits(DoubleBits, Bits) :-
		double_bits_to_binary_rational(DoubleBits, Sign, Significand, Exponent),
		binary_rational_to_bits(Sign, Significand, Exponent, Bits).

	double_bits_to_binary_rational(DoubleBits, Sign, Significand, Exponent) :-
		Sign is (DoubleBits >> 63) /\ 0x01,
		ExponentBits is (DoubleBits >> 52) /\ 0x7ff,
		MantissaBits is DoubleBits /\ 0xfffffffffffff,
		(   ExponentBits =:= 0 ->
			Significand = MantissaBits,
			Exponent = -1074
		;   Significand is (1 << 52) + MantissaBits,
			Exponent is ExponentBits - 1023 - 52
		).

	binary_rational_to_bits(Sign, Significand, Exponent, Bits) :-
		^^precision_spec(_Precision_, ExponentWidth, MantissaWidth, Bias, _),
		Emax is Bias,
		Emin is 1 - Bias,
		integer_floor_log2(Significand, SignificandLog2),
		UnbiasedExponent is SignificandLog2 + Exponent,
		(   UnbiasedExponent > Emax ->
			fail
		;   UnbiasedExponent >= Emin ->
			K is Exponent + MantissaWidth - UnbiasedExponent,
			round_scaled_power_of_two(Significand, K, RoundedSignificand0),
			renormalize_rounded_significand(RoundedSignificand0, MantissaWidth, UnbiasedExponent, Emax, RoundedSignificand, RoundedExponent),
			ExponentBits is RoundedExponent + Bias,
			MantissaBits is RoundedSignificand - (1 << MantissaWidth),
			Bits is (Sign << (ExponentWidth + MantissaWidth)) \/ (ExponentBits << MantissaWidth) \/ MantissaBits
		;   K is Exponent + MantissaWidth - Emin,
			round_scaled_power_of_two(Significand, K, RoundedMantissa0),
			(   RoundedMantissa0 =:= 0 ->
				Bits is Sign << (ExponentWidth + MantissaWidth)
			;   RoundedMantissa0 >= (1 << MantissaWidth) ->
				ExponentBits = 1,
				Bits is (Sign << (ExponentWidth + MantissaWidth)) \/ (ExponentBits << MantissaWidth)
			;   Bits is (Sign << (ExponentWidth + MantissaWidth)) \/ RoundedMantissa0
			)
		).

	renormalize_rounded_significand(RoundedSignificand0, MantissaWidth, UnbiasedExponent0, Emax, RoundedSignificand, RoundedExponent) :-
		(   RoundedSignificand0 =:= (1 << (MantissaWidth + 1)) ->
			RoundedSignificand is 1 << MantissaWidth,
			RoundedExponent is UnbiasedExponent0 + 1,
			RoundedExponent =< Emax
		;   RoundedSignificand = RoundedSignificand0,
			RoundedExponent = UnbiasedExponent0
		).

	round_scaled_power_of_two(Significand, K, Rounded) :-
		(   K >= 0 ->
			Rounded is Significand << K
		;   Shift is -K,
			Quotient is Significand >> Shift,
			Mask is (1 << Shift) - 1,
			Remainder is Significand /\ Mask,
			Half is 1 << (Shift - 1),
			(   Remainder < Half ->
				Rounded = Quotient
			;   Remainder > Half ->
				Rounded is Quotient + 1
			;   Quotient /\ 1 =:= 0 ->
				Rounded = Quotient
			;   Rounded is Quotient + 1
			)
		).

	bits_to_value(Bits, Value) :-
		^^precision_spec(_Precision_, ExponentWidth, MantissaWidth, Bias, ByteCount),
		Sign is (Bits >> (ExponentWidth + MantissaWidth)) /\ 0x01,
		ExponentBits is (Bits >> MantissaWidth) /\ ((1 << ExponentWidth) - 1),
		MantissaBits is Bits /\ ((1 << MantissaWidth) - 1),
		AllOnesExponent is (1 << ExponentWidth) - 1,
		SubnormalExponent is 1 - Bias - MantissaWidth,
		decode_bits_term(Sign, ExponentBits, AllOnesExponent, MantissaBits, MantissaWidth, Bias, SubnormalExponent, ByteCount, Bits, Value).

	decode_bits_term(Sign, 0, _, 0, _, _, _, _, _, Value) :-
		!,
		^^zero_from_sign(Sign, Value).
	decode_bits_term(Sign, 0, _, MantissaBits, _, _, SubnormalExponent, _, _, Value) :-
		!,
		Magnitude is MantissaBits * (2.0 ** SubnormalExponent),
		^^apply_float_sign(Sign, Magnitude, Value).
	decode_bits_term(0, AllOnesExponent, AllOnesExponent, 0, _, _, _, _, _, @infinity) :-
		!.
	decode_bits_term(1, AllOnesExponent, AllOnesExponent, 0, _, _, _, _, _, @negative_infinity) :-
		!.
	decode_bits_term(_, AllOnesExponent, AllOnesExponent, MantissaBits, _, _, _, ByteCount, Bits, Value) :-
		MantissaBits =\= 0,
		!,
		parse_nan_bits(ByteCount, Bits, Value).
	decode_bits_term(Sign, ExponentBits, _, MantissaBits, MantissaWidth, Bias, _, _, _, Value) :-
		Scale is 2.0 ** MantissaWidth,
		UnbiasedExponent is ExponentBits - Bias,
		Magnitude is (1.0 + MantissaBits / Scale) * (2.0 ** UnbiasedExponent),
		^^apply_float_sign(Sign, Magnitude, Value).

	parse_nan_bits(ByteCount, Bits, Value) :-
		(   _NaNRepresentation_ == canonical ->
			Value = @not_a_number
		;   ^^canonical_nan_bits(CanonicalBits),
			canonical_nan_bytes(ByteCount, CanonicalBits, CanonicalBytes),
			^^integer_to_bytes(ByteCount, Bits, Bytes),
			(   Bytes == CanonicalBytes ->
				Value = @not_a_number
			;   Value = not_a_number(Bytes)
			)
		).

	special_bits(infinity, Bits) :-
		^^precision_spec(_Precision_, ExponentWidth, MantissaWidth, _, _),
		ExponentBits is (1 << ExponentWidth) - 1,
		Bits is ExponentBits << MantissaWidth.
	special_bits(negative_infinity, Bits) :-
		^^precision_spec(_Precision_, ExponentWidth, MantissaWidth, _, _),
		ExponentBits is (1 << ExponentWidth) - 1,
		Bits is (1 << (ExponentWidth + MantissaWidth)) \/ (ExponentBits << MantissaWidth).
	special_bits(not_a_number, Bits) :-
		^^canonical_nan_bits(Bits).

	canonical_nan_bytes(ByteCount, Bits, Bytes) :-
		^^integer_to_bytes(ByteCount, Bits, Bytes).

	valid_nan_bytes(Bytes) :-
		byte_count(Count),
		^^valid_bytes(Bytes, Count),
		^^bytes_to_unsigned_integer(Bytes, Bits),
		^^nan_bits(Bits).

	integer_floor_log2(Integer, Log2) :-
		integer_floor_log2(Integer, 0, Log2).

	integer_floor_log2(Integer, Log20, Log2) :-
		(   Integer < 2 ->
			Log2 = Log20
		;   NextInteger is Integer >> 1,
			NextLog2 is Log20 + 1,
			integer_floor_log2(NextInteger, NextLog2, Log2)
		).

	power_of_two_float(Exponent, Float) :-
		(   Exponent >= 0 ->
			power_of_two_float_up(Exponent, 1.0, Float)
		;   PositiveExponent is -Exponent,
			power_of_two_float_down(PositiveExponent, 1.0, Float)
		).

	power_of_two_float_up(0, Float, Float) :-
		!.
	power_of_two_float_up(Exponent, Float0, Float) :-
		Exponent > 0,
		Float1 is Float0 * 2.0,
		NextExponent is Exponent - 1,
		power_of_two_float_up(NextExponent, Float1, Float).

	power_of_two_float_down(0, Float, Float) :-
		!.
	power_of_two_float_down(Exponent, Float0, Float) :-
		Exponent > 0,
		Float1 is Float0 / 2.0,
		NextExponent is Exponent - 1,
		power_of_two_float_down(NextExponent, Float1, Float).

	float_to_ieee754_double(0.0, 0) :-
		!.
	float_to_ieee754_double(Value, Bits) :-
		Value = -0.0,
		!,
		Bits = 0x8000000000000000.
	float_to_ieee754_double(Value, Bits) :-
		float_sign_and_abs(Value, Sign, AbsValue),
		normalize_binary_float(AbsValue, Significand, Exponent),
		encode_ieee754_double_finite(Significand, Exponent, ExponentBits, MantissaBits),
		Bits is (Sign << 63) \/ (ExponentBits << 52) \/ MantissaBits.

	float_sign_and_abs(Value, 1, AbsValue) :-
		Value < 0.0,
		!,
		AbsValue is -Value.
	float_sign_and_abs(Value, 0, Value).

	normalize_binary_float(Value, Significand, Exponent) :-
		(   Value >= 1.0 ->
			normalize_binary_float_down(Value, 0, Significand, Exponent)
		;   normalize_binary_float_up(Value, 0, Significand, Exponent)
		).

	normalize_binary_float_down(Value, Exponent0, Significand, Exponent) :-
		(   Value < 2.0 ->
			Significand = Value,
			Exponent = Exponent0
		;   NextValue is Value / 2.0,
			NextExponent is Exponent0 + 1,
			normalize_binary_float_down(NextValue, NextExponent, Significand, Exponent)
		).

	normalize_binary_float_up(Value, Exponent0, Significand, Exponent) :-
		(   Value >= 1.0 ->
			Significand = Value,
			Exponent = Exponent0
		;   NextValue is Value * 2.0,
			NextExponent is Exponent0 - 1,
			normalize_binary_float_up(NextValue, NextExponent, Significand, Exponent)
		).

	encode_ieee754_double_finite(Significand, Exponent, ExponentBits, MantissaBits) :-
		(   Exponent > 1023 ->
			fail
		;   Exponent >= -1022 ->
			significand_fraction_bits(Significand, 52, MantissaBits, Remainder),
			Remainder = 0.0,
			ExponentBits is Exponent + 1023
		;   Exponent >= -1074 ->
			Shift is Exponent + 1074,
			scaled_significand_bits(Significand, Shift, MantissaBits, Remainder),
			Remainder = 0.0,
			ExponentBits = 0
		;   fail
		).

	significand_fraction_bits(Significand, Precision, Bits, Remainder) :-
		Fraction is Significand - 1.0,
		fraction_bits(Precision, Fraction, 0, Bits, Remainder).

	fraction_bits(0, Fraction, Bits, Bits, Fraction) :-
		!.
	fraction_bits(Precision, Fraction0, Bits0, Bits, Fraction) :-
		Precision > 0,
		Twice is Fraction0 * 2.0,
		(   Twice >= 1.0 ->
			Bit = 1,
			Fraction1 is Twice - 1.0
		;   Bit = 0,
			Fraction1 = Twice
		),
		Bits1 is (Bits0 << 1) \/ Bit,
		NextPrecision is Precision - 1,
		fraction_bits(NextPrecision, Fraction1, Bits1, Bits, Fraction).

	scaled_significand_bits(Significand, Shift, Bits, Remainder) :-
		Fraction is Significand - 1.0,
		scaled_bits(Shift, Fraction, 1, Bits, Remainder).

	scaled_bits(0, Fraction, Bits, Bits, Fraction) :-
		!.
	scaled_bits(Shift, Fraction0, Bits0, Bits, Fraction) :-
		Shift > 0,
		Twice is Fraction0 * 2.0,
		(   Twice >= 1.0 ->
			Bit = 1,
			Fraction1 is Twice - 1.0
		;   Bit = 0,
			Fraction1 = Twice
		),
		Bits1 is (Bits0 << 1) \/ Bit,
		NextShift is Shift - 1,
		scaled_bits(NextShift, Fraction1, Bits1, Bits, Fraction).

:- end_object.
