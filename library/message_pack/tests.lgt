%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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
		date is 2026-05-08,
		comment is 'Unit tests for the "message_pack" library.'
	]).

	:- uses(message_pack(atom), [
		parse/2, generate/2
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	cover(message_pack(_)).

	condition :-
		current_prolog_flag(bounded, false).

	test(message_pack_parse_2_01, true(Term == 0)) :-
		parse([0x00], Term).

	test(message_pack_parse_2_02, true(Term == 127)) :-
		parse([0x7f], Term).

	test(message_pack_parse_2_03, true(Term == 200)) :-
		parse([0xcc, 0xc8], Term).

	test(message_pack_parse_2_04, true(Term == -1)) :-
		parse([0xff], Term).

	test(message_pack_parse_2_05, true(Term == -200)) :-
		parse([0xd1, 0xff, 0x38], Term).

	test(message_pack_parse_2_06, true(Term == @null)) :-
		parse([0xc0], Term).

	test(message_pack_parse_2_07, true(Term == @false)) :-
		parse([0xc2], Term).

	test(message_pack_parse_2_08, true(Term == @true)) :-
		parse([0xc3], Term).

	test(message_pack_parse_2_09, true(Term == bytes([1,2,3]))) :-
		parse([0xc4, 0x03, 0x01, 0x02, 0x03], Term).

	test(message_pack_parse_2_10, true(Term == hello)) :-
		parse([0xa5, 0x68, 0x65, 0x6c, 0x6c, 0x6f], Term).

	test(message_pack_parse_2_11, true(Term == chars([h,i]))) :-
		message_pack(chars)::parse([0xa2, 0x68, 0x69], Term).

	test(message_pack_parse_2_12, true(Term == codes([104,105]))) :-
		message_pack(codes)::parse([0xa2, 0x68, 0x69], Term).

	test(message_pack_parse_2_12a, true(Codes == [0x00e9])) :-
		parse([0xa2, 0xc3, 0xa9], Term),
		atom_codes(Term, Codes).

	test(message_pack_parse_2_12b, true(Term == chars(Chars))) :-
		char_code(Char, 0x20ac),
		Chars = [Char],
		message_pack(chars)::parse([0xa3, 0xe2, 0x82, 0xac], Term).

	test(message_pack_parse_2_12c, true(Term == codes([0x1f600]))) :-
		message_pack(codes)::parse([0xa4, 0xf0, 0x9f, 0x98, 0x80], Term).

	test(message_pack_parse_2_12d, error(domain_error(message_pack_byte_sequence, [0xa2, 0xc0, 0x80]))) :-
		parse([0xa2, 0xc0, 0x80], _).

	test(message_pack_parse_2_12e, error(domain_error(message_pack_byte_sequence, [0xa2, 0xc2, 0x20]))) :-
		parse([0xa2, 0xc2, 0x20], _).

	test(message_pack_parse_2_13, true(Term == [1,@false,foo])) :-
		parse([0x93, 0x01, 0xc2, 0xa3, 0x66, 0x6f, 0x6f], Term).

	test(message_pack_parse_2_14, true(Term == {a-1, b - @true})) :-
		parse([0x82, 0xa1, 0x61, 0x01, 0xa1, 0x62, 0xc3], Term).

	test(message_pack_parse_2_14a, true(Term == {})) :-
		parse([0x80], Term).

	test(message_pack_parse_2_15, true(Term =~= 3.5)) :-
		parse([0xca, 0x40, 0x60, 0x00, 0x00], Term).

	test(message_pack_parse_2_15a, true(Bytes == [0xca, 0x00, 0x00, 0x00, 0x00])) :-
		parse([0xca, 0x00, 0x00, 0x00, 0x00], Term),
		generate(Term, Bytes).

	test(message_pack_parse_2_15b, true(Bytes == [0xca, 0x80, 0x00, 0x00, 0x00])) :-
		parse([0xca, 0x80, 0x00, 0x00, 0x00], Term),
		generate(Term, Bytes).

	test(message_pack_parse_2_15c, true(Term == @infinity)) :-
		parse([0xca, 0x7f, 0x80, 0x00, 0x00], Term).

	test(message_pack_parse_2_15d, true(Term == @negative_infinity)) :-
		parse([0xca, 0xff, 0x80, 0x00, 0x00], Term).

	test(message_pack_parse_2_15e, true(Term == @not_a_number)) :-
		parse([0xca, 0x7f, 0xc0, 0x00, 0x00], Term).

	test(message_pack_parse_2_15f, true(Term == @not_a_number)) :-
		parse([0xca, 0x7f, 0xa0, 0x00, 0x01], Term).

	test(message_pack_parse_2_16, true(Term == ext(-1, bytes([0,0,0,0])))) :-
		parse([0xd6, 0xff, 0x00, 0x00, 0x00, 0x00], Term).

	test(message_pack_parse_2_17, true(Term == 18446744073709551615)) :-
		parse([0xcf, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff], Term).

	test(message_pack_parse_2_18, true(Term == -9223372036854775808)) :-
		parse([0xd3, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], Term).

	test(message_pack_parse_2_19, true(Term =~= 1.1)) :-
		parse([0xcb, 0x3f, 0xf1, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9a], Term).

	test(message_pack_parse_2_20, true(Term == @infinity)) :-
		parse([0xcb, 0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], Term).

	test(message_pack_parse_2_21, true(Term == @negative_infinity)) :-
		parse([0xcb, 0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], Term).

	test(message_pack_parse_2_22, true(Term == @not_a_number)) :-
		parse([0xcb, 0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], Term).

	test(message_pack_parse_2_22a, true(Bytes == [0xca, 0x00, 0x00, 0x00, 0x00])) :-
		parse([0xcb, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], Term),
		generate(Term, Bytes).

	test(message_pack_parse_2_22b, true(Bytes == [0xca, 0x80, 0x00, 0x00, 0x00])) :-
		parse([0xcb, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], Term),
		generate(Term, Bytes).

	test(message_pack_parse_2_22c, true(Term == @not_a_number)) :-
		parse([0xcb, 0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01], Term).

	test(message_pack_parse_2_23, true(Term == ext(5, bytes([0x2a])))) :-
		parse([0xd4, 0x05, 0x2a], Term).

	test(message_pack_parse_2_24, true(Term == ext(-2, bytes([0x10, 0x20])))) :-
		parse([0xd5, 0xfe, 0x10, 0x20], Term).

	test(message_pack_parse_2_25, true(Term == ext(1, bytes([1,2,3,4,5,6,7,8])))) :-
		parse([0xd7, 0x01, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08], Term).

	test(message_pack_parse_2_26, true(Term == ext(127, bytes([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16])))) :-
		parse([0xd8, 0x7f, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10], Term).

	test(message_pack_parse_2_27, true(Term == ext(-1, bytes([0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02])))) :-
		parse([0xc7, 0x0c, 0xff, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02], Term).

	test(message_pack_parse_2_28, true(Term == ext(42, bytes(Payload)))) :-
		repeated_byte_list(256, 0xaa, Payload),
		parse([0xc8, 0x01, 0x00, 0x2a| Payload], Term).

	test(message_pack_parse_2_29, true(Term == bytes([1,2,3]))) :-
		parse([0xc6, 0x00, 0x00, 0x00, 0x03, 0x01, 0x02, 0x03], Term).

	test(message_pack_parse_2_30, true(Term == hi)) :-
		parse([0xdb, 0x00, 0x00, 0x00, 0x02, 0x68, 0x69], Term).

	test(message_pack_parse_2_31, true(Term == [1,2])) :-
		parse([0xdd, 0x00, 0x00, 0x00, 0x02, 0x01, 0x02], Term).

	test(message_pack_parse_2_32, true(Term == {a-1})) :-
		parse([0xdf, 0x00, 0x00, 0x00, 0x01, 0xa1, 0x61, 0x01], Term).

	test(message_pack_parse_2_33, true(Term == ext(127, bytes([0x2a])))) :-
		parse([0xc9, 0x00, 0x00, 0x00, 0x01, 0x7f, 0x2a], Term).

	test(message_pack_parse_2_34, error(domain_error(message_pack_byte_sequence, [0xc1]))) :-
		parse([0xc1], _).

	test(message_pack_parse_2_35, error(domain_error(message_pack_byte_sequence, [0xc4, 0x02, 0x01]))) :-
		parse([0xc4, 0x02, 0x01], _).

	test(message_pack_parse_2_36, error(domain_error(message_pack_byte_sequence, [0xc7, 0x02, 0x05, 0xaa]))) :-
		parse([0xc7, 0x02, 0x05, 0xaa], _).

	test(message_pack_parse_2_37, error(domain_error(message_pack_byte_sequence, [0xde, 0x00, 0x01, 0xa1, 0x61]))) :-
		parse([0xde, 0x00, 0x01, 0xa1, 0x61], _).

	test(message_pack_parse_2_38, error(domain_error(message_pack_byte_sequence, [0x00, 0x00]))) :-
		parse([0x00, 0x00], _).

	test(message_pack_parse_2_39, error(domain_error(message_pack_byte_sequence, [0x91, 0x92, 0x01]))) :-
		parse([0x91, 0x92, 0x01], _).

	test(message_pack_parse_2_40, error(domain_error(message_pack_byte_sequence, [0x91, 0x81, 0xa1, 0x61]))) :-
		parse([0x91, 0x81, 0xa1, 0x61], _).

	test(message_pack_parse_2_41, error(domain_error(message_pack_byte_sequence, [0x81, 0xa1, 0x61, 0x92, 0x01]))) :-
		parse([0x81, 0xa1, 0x61, 0x92, 0x01], _).

	test(message_pack_parse_2_42, error(domain_error(message_pack_byte_sequence, [0x81, 0xa1, 0x61, 0x81, 0xa1, 0x62]))) :-
		parse([0x81, 0xa1, 0x61, 0x81, 0xa1, 0x62], _).

	test(message_pack_generate_2_01, true(Bytes == [0x00])) :-
		generate(0, Bytes).

	test(message_pack_generate_2_01a, true(Bytes == [0x7f])) :-
		generate(127, Bytes).

	test(message_pack_generate_2_01b, true(Bytes == [0xcc, 0x80])) :-
		generate(128, Bytes).

	test(message_pack_generate_2_01c, true(Bytes == [0xcc, 0xff])) :-
		generate(255, Bytes).

	test(message_pack_generate_2_01d, true(Bytes == [0xcd, 0x01, 0x00])) :-
		generate(256, Bytes).

	test(message_pack_generate_2_02, true(Bytes == [0xcc, 0xc8])) :-
		generate(200, Bytes).

	test(message_pack_generate_2_03, true(Bytes == [0xff])) :-
		generate(-1, Bytes).

	test(message_pack_generate_2_03a, true(Bytes == [0xe0])) :-
		generate(-32, Bytes).

	test(message_pack_generate_2_03b, true(Bytes == [0xd0, 0xdf])) :-
		generate(-33, Bytes).

	test(message_pack_generate_2_03c, true(Bytes == [0xd0, 0x80])) :-
		generate(-128, Bytes).

	test(message_pack_generate_2_03d, true(Bytes == [0xd1, 0xff, 0x7f])) :-
		generate(-129, Bytes).

	test(message_pack_generate_2_03e, true(Bytes == [0xd2, 0xff, 0xff, 0x7f, 0xff])) :-
		generate(-32769, Bytes).

	test(message_pack_generate_2_03f, true(Bytes == [0xd3, 0xff, 0xff, 0xff, 0xff, 0x7f, 0xff, 0xff, 0xff])) :-
		generate(-2147483649, Bytes).

	test(message_pack_generate_2_03g, true(Bytes == [0xcb, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01])) :-
		parse([0xcb, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01], Term),
		generate(Term, Bytes).

	test(message_pack_generate_2_04, true(Bytes == [0xc0])) :-
		generate(@null, Bytes).

	test(message_pack_generate_2_05, true(Bytes == [0xc2])) :-
		generate(@false, Bytes).

	test(message_pack_generate_2_06, true(Bytes == [0xc3])) :-
		generate(@true, Bytes).

	test(message_pack_generate_2_07, true(Bytes == [0xc4, 0x03, 0x01, 0x02, 0x03])) :-
		generate(bytes([1,2,3]), Bytes).

	test(message_pack_generate_2_08, true(Bytes == [0xa5, 0x68, 0x65, 0x6c, 0x6c, 0x6f])) :-
		generate(hello, Bytes).

	test(message_pack_generate_2_08a, true(Bytes == [0xa2, 0xc3, 0xa9])) :-
		char_code(Char, 0x00e9),
		generate(chars([Char]), Bytes).

	test(message_pack_generate_2_08b, true(Bytes == [0xa3, 0xe2, 0x82, 0xac])) :-
		generate(codes([0x20ac]), Bytes).

	test(message_pack_generate_2_08c, true(Bytes == [0xa4, 0xf0, 0x9f, 0x98, 0x80])) :-
		generate(codes([0x1f600]), Bytes).

	test(message_pack_generate_2_08d, error(domain_error(message_pack_term, codes([-1])))) :-
		generate(codes([-1]), _).

	test(message_pack_generate_2_08e, error(domain_error(message_pack_term, codes([1114112])))) :-
		generate(codes([1114112]), _).

	test(message_pack_generate_2_08f, error(domain_error(message_pack_term, codes([55296])))) :-
		generate(codes([55296]), _).

	test(message_pack_generate_2_09, true(Bytes == [0x93, 0x01, 0xc2, 0xa3, 0x66, 0x6f, 0x6f])) :-
		generate([1,@false,foo], Bytes).

	test(message_pack_generate_2_10, true(Bytes == [0x82, 0xa1, 0x61, 0x01, 0xa1, 0x62, 0xc3])) :-
		generate({a-1, b - @true}, Bytes).

	test(message_pack_generate_2_10a, true(Bytes == [0x80])) :-
		generate({}, Bytes).

	test(message_pack_generate_2_11, true(Bytes == [0xd6, 0xff, 0x00, 0x00, 0x00, 0x00])) :-
		generate(ext(-1, bytes([0,0,0,0])), Bytes).

	test(message_pack_generate_2_12, true(Bytes == [0xca, 0x40, 0x60, 0x00, 0x00])) :-
		generate(3.5, Bytes).

	test(message_pack_generate_2_13, true(Bytes == [0xca, 0x7f, 0x80, 0x00, 0x00])) :-
		generate(@infinity, Bytes).

	test(message_pack_generate_2_14, true(Bytes == [0xca, 0xff, 0x80, 0x00, 0x00])) :-
		generate(@negative_infinity, Bytes).

	test(message_pack_generate_2_15, true(Bytes == [0xca, 0x7f, 0xc0, 0x00, 0x00])) :-
		generate(@not_a_number, Bytes).

	test(message_pack_generate_2_16, true(Bytes == [0xcb, 0x3f, 0xf1, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9a])) :-
		generate(1.1, Bytes).

	test(message_pack_generate_2_17, true(Bytes == [0xd4, 0x05, 0x2a])) :-
		generate(ext(5, bytes([0x2a])), Bytes).

	test(message_pack_generate_2_18, true(Bytes == [0xd5, 0xfe, 0x10, 0x20])) :-
		generate(ext(-2, bytes([0x10, 0x20])), Bytes).

	test(message_pack_generate_2_19, true(Bytes == [0xd7, 0x01, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08])) :-
		generate(ext(1, bytes([1,2,3,4,5,6,7,8])), Bytes).

	test(message_pack_generate_2_20, true(Bytes == [0xd8, 0x7f, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10])) :-
		generate(ext(127, bytes([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16])), Bytes).

	test(message_pack_generate_2_21, true(Bytes == [0xc7, 0x0c, 0xff, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02])) :-
		generate(ext(-1, bytes([0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02])), Bytes).

	test(message_pack_generate_2_22, true(Bytes == [0xc8, 0x01, 0x00, 0x2a| Payload])) :-
		repeated_byte_list(256, 0xaa, Payload),
		generate(ext(42, bytes(Payload)), Bytes).

	test(message_pack_generate_2_23, true(Bytes == [0xd9, 0x20| Codes])) :-
		repeated_code_atom(32, 0'a, Atom),
		atom_codes(Atom, Codes),
		generate(Atom, Bytes).

	test(message_pack_generate_2_24, true(Bytes == [0xda, 0x01, 0x00| Codes])) :-
		repeated_code_atom(256, 0'a, Atom),
		atom_codes(Atom, Codes),
		generate(Atom, Bytes).

	test(message_pack_generate_2_24a, true(Bytes == [0xdb, 0x00, 0x01, 0x00, 0x00| Codes])) :-
		repeated_code_atom(65536, 0'a, Atom),
		atom_codes(Atom, Codes),
		generate(Atom, Bytes).

	test(message_pack_generate_2_25, true(Bytes == [0xc5, 0x01, 0x00| Payload])) :-
		repeated_byte_list(256, 0xaa, Payload),
		generate(bytes(Payload), Bytes).

	test(message_pack_generate_2_25a, true(Bytes == [0xc6, 0x00, 0x01, 0x00, 0x00| Payload])) :-
		repeated_byte_list(65536, 0xaa, Payload),
		generate(bytes(Payload), Bytes).

	test(message_pack_generate_2_26, true(Bytes == [0xdc, 0x00, 0x10| Array])) :-
		ascending_integer_list(16, Array),
		generate(Array, Bytes).

	test(message_pack_generate_2_27, true(Bytes == [0xde, 0x00, 0x10| PairsBytes])) :-
		identity_map(16, Map),
		identity_pair_bytes(16, PairsBytes),
		generate(Map, Bytes).

	test(message_pack_generate_2_27a, true(Bytes == [0xcf, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00])) :-
		generate(4294967296, Bytes).

	test(message_pack_generate_2_27b, true(Bytes == [0xc9, 0x00, 0x01, 0x00, 0x00, 0x2a| Payload])) :-
		repeated_byte_list(65536, 0xaa, Payload),
		generate(ext(42, bytes(Payload)), Bytes).

	test(message_pack_generate_2_28, true(Bytes == [0xca, 0x80, 0x00, 0x00, 0x00])) :-
		generate(-0.0, Bytes).

	test(message_pack_generate_2_29, error(instantiation_error)) :-
		generate(_, _).

	test(message_pack_generate_2_30, error(domain_error(message_pack_term, foo(bar)))) :-
		generate(foo(bar), _).

	test(message_pack_generate_2_31, error(domain_error(message_pack_term, ext(128, bytes([0]))))) :-
		generate(ext(128, bytes([0])), _).

	test(message_pack_generate_2_32, error(domain_error(message_pack_term, ext(1, bytes([256]))))) :-
		generate(ext(1, bytes([256])), _).

	test(message_pack_generate_2_33, error(domain_error(message_pack_term, {a}))) :-
		generate({a}, _).

	test(message_pack_generate_2_34, error(representation_error(pair))) :-
		generate({a, b-2}, _).

	test(message_pack_generate_2_35, error(domain_error(message_pack_term, {a-1, b}))) :-
		generate({a-1, b}, _).

	test(message_pack_generate_2_36, error(instantiation_error)) :-
		generate({a-_}, _).

	test(message_pack_roundtrip_2_01, true(Decoded == {foo-[1,2,3], ok - @true})) :-
		generate({foo-[1,2,3], ok - @true}, Bytes),
		parse(Bytes, Decoded).

	test(message_pack_roundtrip_2_02, true(Decoded == ext(5, bytes([16,32,48])))) :-
		generate(ext(5, bytes([16,32,48])), Bytes),
		parse(Bytes, Decoded).

	test(message_pack_roundtrip_2_03, true(Decoded == ext(-1, bytes([0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02])))) :-
		generate(ext(-1, bytes([0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02])), Bytes),
		parse(Bytes, Decoded).

	test(message_pack_roundtrip_2_04, true(Decoded == Atom)) :-
		repeated_code_atom(256, 0'a, Atom),
		generate(Atom, Bytes),
		parse(Bytes, Decoded).

	test(message_pack_roundtrip_2_05, true(Decoded == Map)) :-
		identity_map(16, Map),
		generate(Map, Bytes),
		parse(Bytes, Decoded).

	test(message_pack_roundtrip_2_06, true(Bytes == [0xca, 0x80, 0x00, 0x00, 0x00])) :-
		parse([0xca, 0x80, 0x00, 0x00, 0x00], NegativeZero),
		generate(NegativeZero, Bytes).

	test(message_pack_roundtrip_2_07, true(Bytes == [0xca, 0x00, 0x00, 0x00, 0x01])) :-
		parse([0xca, 0x00, 0x00, 0x00, 0x01], Float),
		generate(Float, Bytes).

	repeated_byte_list(Length, Byte, Bytes) :-
		repeated_byte_list(Length, Byte, Bytes, []).

	repeated_byte_list(0, _, Bytes, Bytes) :-
		!.
	repeated_byte_list(Length, Byte, [Byte| Bytes0], Bytes) :-
		Length > 0,
		NextLength is Length - 1,
		repeated_byte_list(NextLength, Byte, Bytes0, Bytes).

	repeated_code_atom(Length, Code, Atom) :-
		repeated_byte_list(Length, Code, Codes),
		atom_codes(Atom, Codes).

	ascending_integer_list(Count, Integers) :-
		ascending_integer_list(0, Count, Integers).

	ascending_integer_list(Count, Count, []) :-
		!.
	ascending_integer_list(Integer, Count, [Integer| Integers]) :-
		Integer < Count,
		NextInteger is Integer + 1,
		ascending_integer_list(NextInteger, Count, Integers).

	identity_map(Count, Map) :-
		(   Count =:= 0 ->
			Map = {}
		;   identity_map_pairs(0, Count, Pairs),
			Map = {Pairs}
		).

	identity_map_pairs(Integer, Count, Integer-Integer) :-
		Integer =:= Count - 1,
		!.
	identity_map_pairs(Integer, Count, (Integer-Integer, Map)) :-
		Integer < Count,
		NextInteger is Integer + 1,
		identity_map_pairs(NextInteger, Count, Map).

	identity_pair_bytes(Count, Bytes) :-
		identity_pair_bytes(0, Count, Bytes).

	identity_pair_bytes(Count, Count, []) :-
		!.
	identity_pair_bytes(Integer, Count, [Integer, Integer| Bytes]) :-
		Integer < Count,
		NextInteger is Integer + 1,
		identity_pair_bytes(NextInteger, Count, Bytes).

:- end_object.
