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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-09,
		comment is 'Unit tests for the "ieee_754" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(ieee_754(_, _, _)).
	cover(ieee_754_fields(_, _)).

	test(ieee_754_queries_01, deterministic([Precision, Order, NaNRepresentation, ByteCount] == [half, big, canonical, 2])) :-
		ieee_754(half, big, canonical)::precision(Precision),
		ieee_754(half, big, canonical)::order(Order),
		ieee_754(half, big, canonical)::nan_representation(NaNRepresentation),
		ieee_754(half, big, canonical)::byte_count(ByteCount).

	test(ieee_754_queries_02, deterministic(ByteCount == 4)) :-
		ieee_754(single, little, payloads)::byte_count(ByteCount).

	test(ieee_754_queries_03, deterministic(ByteCount == 8)) :-
		ieee_754(double, big, payloads)::byte_count(ByteCount).

	test(ieee_754_fields_queries_01, deterministic([Precision, Order, ByteCount] == [half, little, 2])) :-
		ieee_754_fields(half, little)::precision(Precision),
		ieee_754_fields(half, little)::order(Order),
		ieee_754_fields(half, little)::byte_count(ByteCount).

	test(ieee_754_valid_01, true) :-
		ieee_754(single, little, canonical)::valid(1.5).

	test(ieee_754_valid_02, true) :-
		ieee_754(single, little, canonical)::valid(@infinity).

	test(ieee_754_valid_03, true) :-
		ieee_754(single, little, payloads)::valid(not_a_number([0x7f, 0xa0, 0x00, 0x01])).

	test(ieee_754_valid_04, fail) :-
		ieee_754(single, little, canonical)::valid(not_a_number([0x7f, 0xa0, 0x00, 0x01])).

	test(ieee_754_valid_05, fail) :-
		ieee_754(single, little, payloads)::valid(foo).

	test(ieee_754_parse_error_01, error(instantiation_error)) :-
		ieee_754(half, big, canonical)::parse(_, _).

	test(ieee_754_parse_error_02, error(domain_error(ieee_754_source, foo))) :-
		ieee_754(half, big, canonical)::parse(foo, _).

	test(ieee_754_parse_error_03, error(domain_error(ieee_754_encoding, bytes([0x00])))) :-
		ieee_754(half, big, canonical)::parse(bytes([0x00]), _).

	test(ieee_754_parse_error_04, error(domain_error(ieee_754_encoding, bytes([0x00, 256])))) :-
		ieee_754(half, big, canonical)::parse(bytes([0x00, 256]), _).

	test(ieee_754_parse_error_05, error(domain_error(ieee_754_encoding, bits(65536)))) :-
		ieee_754(half, big, canonical)::parse(bits(65536), _).

	test(ieee_754_fields_error_01, error(instantiation_error)) :-
		ieee_754_fields(half, big)::classify(_, _).

	test(ieee_754_fields_error_02, error(domain_error(ieee_754_source, foo))) :-
		ieee_754_fields(half, big)::fields(foo, _, _, _, _).

	test(ieee_754_fields_error_03, error(domain_error(ieee_754_finite_encoding, bits(0x7c00)))) :-
		ieee_754_fields(half, big)::finite_binary_rational(bits(0x7c00), _, _, _).

	test(ieee_754_fields_error_04, error(domain_error(ieee_754_nan_encoding, bits(0x3e00)))) :-
		ieee_754_fields(half, big)::nan_payload(bits(0x3e00), _).

	test(ieee_754_fields_error_05, error(domain_error(ieee_754_nan_encoding, bits(0x3e00)))) :-
		ieee_754_fields(half, big)::nan_kind(bits(0x3e00), _).

	test(ieee_754_generate_error_01, error(instantiation_error)) :-
		ieee_754(half, big, canonical)::generate(_, 1.0).

	test(ieee_754_generate_error_02, error(domain_error(ieee_754_sink, foo))) :-
		ieee_754(half, big, canonical)::generate(foo, 1.0).

	test(ieee_754_generate_error_03, error(domain_error(ieee_754_value, foo))) :-
		ieee_754(half, big, canonical)::generate(bits(_), foo).

	test(ieee_754_generate_error_04, error(domain_error(ieee_754_value, not_a_number([0x7f, 0xa0, 0x00, 0x01])))) :-
		ieee_754(single, little, canonical)::generate(bits(_), not_a_number([0x7f, 0xa0, 0x00, 0x01])).

	test(ieee_754_generate_error_05, error(domain_error(ieee_754_value, not_a_number([0x00, 0x00, 0x00, 0x01])))) :-
		ieee_754(single, little, payloads)::generate(bits(_), not_a_number([0x00, 0x00, 0x00, 0x01])).

	test(ieee_754_generate_error_06, error(domain_error(ieee_754_representation, 70000.0))) :-
		ieee_754(half, big, canonical)::generate(bits(_), 70000.0).

	test(ieee_754_half_parse_01, true(Value =~= 1.5)) :-
		ieee_754(half, big, canonical)::parse(bytes([0x3e, 0x00]), Value).

	test(ieee_754_half_parse_02, true(Value =~= 5.960464477539063e-8)) :-
		ieee_754(half, big, canonical)::parse(bits(0x0001), Value).

	test(ieee_754_half_parse_03, true(Value =~= -1.5)) :-
		ieee_754(half, big, canonical)::parse(bits(0xbe00), Value).

	test(ieee_754_half_parse_04, deterministic(Value == @infinity)) :-
		ieee_754(half, big, canonical)::parse(bits(0x7c00), Value).

	test(ieee_754_half_parse_05, deterministic(Value == @negative_infinity)) :-
		ieee_754(half, big, canonical)::parse(bits(0xfc00), Value).

	test(ieee_754_half_parse_06, deterministic((ground(Bits), memberchk(Bits, [0x0000, 0x8000])))) :-
		ieee_754(half, big, canonical)::parse(bits(0x8000), Value),
		ieee_754(half, big, canonical)::generate(bits(Bits), Value).

	test(ieee_754_half_generate_01, deterministic(Bytes == [0x00, 0x3e])) :-
		ieee_754(half, little, canonical)::generate(bytes(Bytes), 1.5).

	test(ieee_754_half_generate_02, deterministic(Bits == 0x4000)) :-
		ieee_754(half, big, canonical)::generate(bits(Bits), 1.99951171875).

	test(ieee_754_half_generate_03, deterministic(Bits == 0x0000)) :-
		ieee_754(half, big, canonical)::generate(bits(Bits), 1.0e-20).

	test(ieee_754_half_generate_04, deterministic(Bits == 0x0400)) :-
		Value is 2047.0 * (2.0 ** -25),
		ieee_754(half, big, canonical)::generate(bits(Bits), Value).

	test(ieee_754_half_special_01, deterministic(Bits == 0x7c00)) :-
		ieee_754(half, big, canonical)::generate(bits(Bits), @infinity).

	test(ieee_754_half_special_02, deterministic(Bits == 0xfc00)) :-
		ieee_754(half, big, canonical)::generate(bits(Bits), @negative_infinity).

	test(ieee_754_half_special_03, deterministic(Bits == 0x7e00)) :-
		ieee_754(half, big, canonical)::generate(bits(Bits), @not_a_number).

	test(ieee_754_fields_classify_01, deterministic(Class == zero)) :-
		ieee_754_fields(half, big)::classify(bits(0x0000), Class).

	test(ieee_754_fields_classify_02, deterministic(Class == subnormal)) :-
		ieee_754_fields(half, big)::classify(bits(0x0001), Class).

	test(ieee_754_fields_classify_03, deterministic(Class == normal)) :-
		ieee_754_fields(single, little)::classify(bytes([0x00, 0x00, 0x60, 0x40]), Class).

	test(ieee_754_fields_classify_04, deterministic(Class == infinity)) :-
		ieee_754_fields(double, big)::classify(bits(0x7ff0000000000000), Class).

	test(ieee_754_fields_classify_05, deterministic(Class == not_a_number)) :-
		ieee_754_fields(double, big)::classify(bits(0x7ff8000000000001), Class).

	test(ieee_754_fields_fields_01, deterministic([Sign, ExponentBits, MantissaBits, Class] == [0, 15, 512, normal])) :-
		ieee_754_fields(half, big)::fields(bits(0x3e00), Sign, ExponentBits, MantissaBits, Class).

	test(ieee_754_fields_fields_02, deterministic([Sign, ExponentBits, MantissaBits, Class] == [1, 0, 0, zero])) :-
		ieee_754_fields(half, big)::fields(bits(0x8000), Sign, ExponentBits, MantissaBits, Class).

	test(ieee_754_fields_fields_03, deterministic([Sign, ExponentBits, MantissaBits, Class] == [0, 2047, 0x8000000000001, not_a_number])) :-
		ieee_754_fields(double, big)::fields(bits(0x7ff8000000000001), Sign, ExponentBits, MantissaBits, Class).

	test(ieee_754_fields_finite_binary_rational_01, deterministic([Sign, Significand, Exponent] == [0, 1536, -10])) :-
		ieee_754_fields(half, big)::finite_binary_rational(bits(0x3e00), Sign, Significand, Exponent).

	test(ieee_754_fields_finite_binary_rational_02, deterministic([Sign, Significand, Exponent] == [0, 1, -24])) :-
		ieee_754_fields(half, big)::finite_binary_rational(bits(0x0001), Sign, Significand, Exponent).

	test(ieee_754_fields_finite_binary_rational_03, deterministic([Sign, Significand, Exponent] == [1, 0, 0])) :-
		ieee_754_fields(double, big)::finite_binary_rational(bits(0x8000000000000000), Sign, Significand, Exponent).

	test(ieee_754_fields_nan_payload_01, deterministic(PayloadBits == 0x8000000000001)) :-
		ieee_754_fields(double, big)::nan_payload(bits(0x7ff8000000000001), PayloadBits).

	test(ieee_754_fields_nan_kind_01, deterministic(Kind == quiet)) :-
		ieee_754_fields(double, big)::nan_kind(bits(0x7ff8000000000001), Kind).

	test(ieee_754_fields_nan_kind_02, deterministic(Kind == signaling)) :-
		ieee_754_fields(double, big)::nan_kind(bits(0x7ff0000000000001), Kind).

	test(ieee_754_fields_nan_kind_03, deterministic(Kind == quiet)) :-
		ieee_754_fields(single, little)::nan_kind(bytes([0x01, 0x00, 0xc0, 0x7f]), Kind).

	test(ieee_754_single_parse_01, true(Value =~= 3.5)) :-
		ieee_754(single, little, canonical)::parse(bytes([0x00, 0x00, 0x60, 0x40]), Value).

	test(ieee_754_single_parse_02, deterministic(Value == @not_a_number)) :-
		ieee_754(single, little, payloads)::parse(bytes([0x00, 0x00, 0xc0, 0x7f]), Value).

	test(ieee_754_single_parse_03, deterministic(Value == not_a_number([0x7f, 0xa0, 0x00, 0x01]))) :-
		ieee_754(single, little, payloads)::parse(bytes([0x01, 0x00, 0xa0, 0x7f]), Value).

	test(ieee_754_single_generate_01, deterministic(Bytes == [0x00, 0x00, 0x60, 0x40])) :-
		ieee_754(single, little, canonical)::generate(bytes(Bytes), 3.5).

	test(ieee_754_single_generate_02, deterministic(Bytes == [0x01, 0x00, 0xa0, 0x7f])) :-
		ieee_754(single, little, payloads)::generate(bytes(Bytes), not_a_number([0x7f, 0xa0, 0x00, 0x01])).

	test(ieee_754_single_generate_03, deterministic(Bytes == [0x00, 0x00, 0x80, 0x3f, 0xff])) :-
		ieee_754(single, little, canonical)::generate(1.0, Bytes, [0xff]).

	test(ieee_754_single_exactly_representable_01, true) :-
		ieee_754(single, little, canonical)::exactly_representable(1.5).

	test(ieee_754_single_exactly_representable_02, fail) :-
		ieee_754(single, little, canonical)::exactly_representable(1.1).

	test(ieee_754_single_exactly_representable_03, true) :-
		ieee_754(single, little, payloads)::exactly_representable(not_a_number([0x7f, 0xa0, 0x00, 0x01])).

	test(ieee_754_double_parse_01, true(Value =~= 0.5)) :-
		ieee_754(double, big, payloads)::parse(bytes([0x3f, 0xe0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]), Value).

	test(ieee_754_double_parse_02, deterministic(Value == @not_a_number)) :-
		ieee_754(double, big, payloads)::parse(bits(0x7ff8000000000000), Value).

	test(ieee_754_double_parse_03, deterministic(Value == not_a_number([0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]))) :-
		ieee_754(double, big, payloads)::parse(bits(0x7ff8000000000001), Value).

	test(ieee_754_double_generate_01, deterministic(Bits == 0x3fe0000000000000)) :-
		ieee_754(double, big, canonical)::generate(bits(Bits), 0.5).

	test(ieee_754_double_generate_02, deterministic(Bits == 0)) :-
		ieee_754(double, big, canonical)::generate(bits(Bits), 0.0).

	test(ieee_754_double_generate_03, deterministic(Bits == 0xbff4000000000000)) :-
		ieee_754(double, big, canonical)::generate(bits(Bits), -1.25).

	test(ieee_754_double_generate_04, deterministic(Bytes == [0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01])) :-
		ieee_754(double, big, payloads)::generate(bytes(Bytes), not_a_number([0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01])).

	test(ieee_754_double_generate_05, deterministic((ground(Bits), memberchk(Bits, [0, 0x8000000000000000])))) :-
		ieee_754(double, big, canonical)::parse(bits(0x8000000000000000), Value),
		ieee_754(double, big, canonical)::generate(bits(Bits), Value).

	test(ieee_754_double_generate_06, deterministic(Bits == 0x0000000000000001)) :-
		Value is 2.0 ** -1074,
		ieee_754(double, big, canonical)::generate(bits(Bits), Value).

	test(ieee_754_double_exactly_representable_01, true) :-
		ieee_754(double, big, canonical)::exactly_representable(0.5).

	test(ieee_754_double_exactly_representable_02, true) :-
		ieee_754(double, big, canonical)::exactly_representable(@infinity).

	test(ieee_754_double_exactly_representable_03, true) :-
		ieee_754(double, big, canonical)::exactly_representable(@negative_infinity).

	test(ieee_754_double_exactly_representable_04, true) :-
		ieee_754(double, big, canonical)::exactly_representable(@not_a_number).

	test(ieee_754_double_negative_zero_01, deterministic(Bytes == [0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]), [condition(-0.0 \== 0.0)]) :-
		ieee_754(double, big, canonical)::parse(bits(0x8000000000000000), Value),
		ieee_754(double, big, canonical)::generate(bytes(Bytes), Value).

	test(ieee_754_double_negative_zero_02, deterministic(Bytes == [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]), [condition(-0.0 == 0.0)]) :-
		ieee_754(double, big, canonical)::parse(bits(0x8000000000000000), Value),
		ieee_754(double, big, canonical)::generate(bytes(Bytes), Value).

	test(ieee_754_double_negative_zero_03, deterministic(Bits == 0x8000000000000000), [condition(-0.0 \== 0.0)]) :-
		ieee_754(double, big, canonical)::generate(bits(Bits), -0.0).

	test(ieee_754_double_negative_zero_04, deterministic(Bits == 0), [condition(-0.0 == 0.0)]) :-
		ieee_754(double, big, canonical)::generate(bits(Bits), -0.0).

:- end_object.
