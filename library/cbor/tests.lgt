:- encoding('UTF-8').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 0:2:0,
		author is 'Paulo Moura',
		date is 2021-03-02,
		comment is 'Unit tests for the "cbor" library.'
	]).

	:- uses(cbor, [
		parse/2, generate/2
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	cover(cbor).

	condition :-
		current_prolog_flag(bounded, false).

	% test cases from https://tools.ietf.org/html/rfc8949#appendix-A

	% parse/2 tests

	test(cbor_parse_2_01, true(Term == 0)) :-
		parse([0x00], Term).

	test(cbor_parse_2_02, true(Term == 1)) :-
		parse([0x01], Term).

	test(cbor_parse_2_03, true(Term == 10)) :-
		parse([0x0a], Term).

	test(cbor_parse_2_04, true(Term == 23)) :-
		parse([0x17], Term).

	test(cbor_parse_2_05, true(Term == 24)) :-
		parse([0x18, 0x18], Term).

	test(cbor_parse_2_06, true(Term == 25)) :-
		parse([0x18, 0x19], Term).

	test(cbor_parse_2_07, true(Term == 100)) :-
		parse([0x18, 0x64], Term).

	test(cbor_parse_2_08, true(Term == 1000)) :-
		parse([0x19, 0x03, 0xe8], Term).

	test(cbor_parse_2_09, true(Term == 1000000)) :-
		parse([0x1a, 0x00, 0x0f, 0x42, 0x40], Term).

	test(cbor_parse_2_10, true(Term == 1000000000000)) :-
		parse([0x1b, 0x00, 0x00, 0x00, 0xe8, 0xd4, 0xa5, 0x10, 0x00], Term).

	test(cbor_parse_2_11, true(Term == 18446744073709551615)) :-
		parse([0x1b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff], Term).

	test(cbor_parse_2_12, true(Term == 18446744073709551616)) :-
		parse([0xc2, 0x49, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], Term).

	test(cbor_parse_2_13, true(Term == -18446744073709551616)) :-
		parse([0x3b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff], Term).

	test(cbor_parse_2_14, true(Term == -18446744073709551617)) :-
		parse([0xc3, 0x49, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], Term).

	test(cbor_parse_2_15, true(Term == -1)) :-
		parse([0x20], Term).

	test(cbor_parse_2_16, true(Term == -10)) :-
		parse([0x29], Term).

	test(cbor_parse_2_17, true(Term == -100)) :-
		parse([0x38, 0x63], Term).

	test(cbor_parse_2_18, true(Term == -1000)) :-
		parse([0x39, 0x03, 0xe7], Term).

	test(cbor_parse_2_19, true(Term == zero)) :-
		parse([0xf9, 0x00, 0x00], Term).

	test(cbor_parse_2_20, true(Term == negzero)) :-
		parse([0xf9, 0x80, 0x00], Term).

	test(cbor_parse_2_21a, true(Term =~= 1.0)) :-
		parse([0xf9, 0x3c, 0x00], Term).

	test(cbor_parse_2_21b, true(Term =~= 1.0)) :-
		% decimal fraction encoding
		parse([196, 130, 32, 10], Term).

	test(cbor_parse_2_22a, true(Term =~= 1.1)) :-
		parse([0xfb, 0x3f, 0xf1, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9a], Term).

	test(cbor_parse_2_22b, true(Term =~= 1.1)) :-
		parse([196, 130, 32, 11], Term).

	test(cbor_parse_2_23a, true(Term =~= 1.5)) :-
		parse([0xf9, 0x3e, 0x00], Term).

	test(cbor_parse_2_23b, true(Term =~= 1.5)) :-
		% decimal fraction encoding
		parse([196, 130, 32, 15], Term).

	test(cbor_parse_2_24a, true(Term =~= 65504.0)) :-
		parse([0xf9, 0x7b, 0xff], Term).

	test(cbor_parse_2_24b, true(Term =~= 65504.0)) :-
		% decimal fraction encoding
		parse([196, 130, 32, 26, 0, 9, 254, 192], Term).

	test(cbor_parse_2_25a, true(Term =~= 100000.0)) :-
		parse([0xfa, 0x47, 0xc3, 0x50, 0x00], Term).

	test(cbor_parse_2_25b, true(Term =~= 100000.0)) :-
		% decimal fraction encoding
		parse([196, 130, 32, 26, 0, 15, 66, 64], Term).

	test(cbor_parse_2_26, true(Term =~= 3.4028234663852886e+38)) :-
		parse([0xfa, 0x7f, 0x7f, 0xff, 0xff], Term).

	test(cbor_parse_2_27, true(Term =~= 1.0e+300)) :-
		parse([0xfb, 0x7e, 0x37, 0xe4, 0x3c, 0x88, 0x00, 0x75, 0x9c], Term).

	test(cbor_parse_2_28, true(Term =~= 5.960464477539063e-8)) :-
		parse([0xf9, 0x00, 0x01], Term).

	test(cbor_parse_2_29, true(Term =~= 0.00006103515625)) :-
		parse([0xf9, 0x04, 0x00], Term).

	test(cbor_parse_2_30, true(Term =~= -4.0)) :-
		parse([0xf9, 0xc4, 0x00], Term).

	test(cbor_parse_2_31, true(Term =~= -4.1)) :-
		parse([0xfb,0xc0, 0x10, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66], Term).

	test(cbor_parse_2_32, true(Term == inf)) :-
		parse([0xf9, 0x7c, 0x00], Term).

	test(cbor_parse_2_33, true(Term == nan)) :-
		parse([0xf9, 0x7e, 0x00], Term).

	test(cbor_parse_2_34, true(Term == neginf)) :-
		parse([0xf9, 0xfc, 0x00], Term).

	test(cbor_parse_2_35, true(Term == inf)) :-
		parse([0xfa, 0x7f, 0x80, 0x00, 0x00], Term).

	test(cbor_parse_2_36, true(Term == nan)) :-
		parse([0xfa, 0x7f, 0xc0, 0x00, 0x00], Term).

	test(cbor_parse_2_37, true(Term == neginf)) :-
		parse([0xfa, 0xff, 0x80, 0x00, 0x00], Term).

	test(cbor_parse_2_38, true(Term == inf)) :-
		parse([0xfb, 0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], Term).

	test(cbor_parse_2_39, true(Term == nan)) :-
		parse([0xfb, 0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], Term).

	test(cbor_parse_2_40, true(Term == neginf)) :-
		parse([0xfb, 0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], Term).

	test(cbor_parse_2_41, true(Term == false)) :-
		parse([0xf4], Term).

	test(cbor_parse_2_42, true(Term == true)) :-
		parse([0xf5], Term).

	test(cbor_parse_2_43, true(Term == null)) :-
		parse([0xf6], Term).

	test(cbor_parse_2_44, true(Term == undefined)) :-
		parse([0xf7], Term).

	test(cbor_parse_2_45, true(Term == simple(16))) :-
		parse([0xf0], Term).

	test(cbor_parse_2_46, true(Term == simple(255))) :-
		parse([0xf8, 0xff], Term).

	test(cbor_parse_2_47a, true(Term == tag(0,'2013-03-21T20:04:00Z'))) :-
		parse([0xc0, 0x74, 0x32, 0x30, 0x31, 0x33, 0x2d, 0x30, 0x33, 0x2d, 0x32, 0x31, 0x54, 0x32, 0x30, 0x3a, 0x30, 0x34, 0x3a, 0x30, 0x30, 0x5a], Term).

	test(cbor_parse_2_47b, true(Term == tag(0,'2013-03-21T20:04:00Z'))) :-
		parse([192,95,50,48,49,51,45,48,51,45,50,49,84,50,48,58,48,52,58,48,48,90,255], Term).

	test(cbor_parse_2_48, true(Term == tag(1, 1363896240))) :-
		parse([0xc1, 0x1a, 0x51, 0x4b, 0x67, 0xb0], Term).

	test(cbor_parse_2_49, true(Term == tag(1, 1363896240.5))) :-
		parse([0xc1, 0xfb, 0x41, 0xd4, 0x52, 0xd9, 0xec, 0x20, 0x00, 0x00], Term).

%   |23(h'01020304')               | 0xd74401020304                     |
%   +------------------------------+------------------------------------+
%   |24(h'6449455446')             | 0xd818456449455446                 |
%   +------------------------------+------------------------------------+

	% 32("http://www.example.com")
	test(cbor_parse_2_52a, true(Term == tag(32, 'http://www.example.com'))) :-
		parse([0xd8, 0x20, 0x76, 0x68, 0x74, 0x74, 0x70, 0x3a, 0x2f, 0x2f, 0x77, 0x77, 0x77, 0x2e, 0x65, 0x78, 0x61, 0x6d, 0x70, 0x6c, 0x65, 0x2e, 0x63, 0x6f, 0x6d], Term).

	test(cbor_parse_2_52b, true(Term == tag(32, 'http://www.example.com'))) :-
		parse([216,32,95,104,116,116,112,58,47,47,119,119,119,46,101,120,97,109,112,108,101,46,99,111,109,255], Term).

%   +------------------------------+------------------------------------+
%   |h''                           | 0x40                               |
%   +------------------------------+------------------------------------+
%   |h'01020304'                   | 0x4401020304                       |
%   +------------------------------+------------------------------------+

	test(cbor_parse_2_62, true(Term == [])) :-
		parse([0x80], Term).

	test(cbor_parse_2_63, true(Term == [1, 2, 3])) :-
		parse([0x83, 0x01, 0x02, 0x03], Term).

	test(cbor_parse_2_64, true(Term == [1, [2, 3], [4, 5]])) :-
		parse([0x83, 0x01, 0x82, 0x02, 0x03, 0x82, 0x04, 0x05], Term).

	test(cbor_parse_2_65, true(Term == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25])) :-
		parse([0x98, 0x19, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x18, 0x18, 0x19], Term).

	test(cbor_parse_2_66, true(Term == {})) :-
		parse([0xa0], Term).

	test(cbor_parse_2_67, true(Term == {1-2, 3-4})) :-
		parse([0xa2, 0x01, 0x02, 0x03, 0x04], Term).

	test(cbor_parse_2_68, true(Term == {a-1, b-[2,3]})) :-
		parse([0xa2, 0x61, 0x61, 0x01, 0x61, 0x62, 0x82, 0x02, 0x03], Term).

	test(cbor_parse_2_69, true(Term == [a, {b-c}])) :-
		parse([0x9f, 0x5f, 0x61, 0xff, 0xbf, 0x5f, 0x62, 0xff, 0x5f, 0x63, 0xff, 0xff, 0xff], Term).

	test(cbor_parse_2_70a, true(Term == {a-'A', b-'B', c-'C', d-'D', e-'E'})) :-
		parse([0xa5, 0x61, 0x61, 0x61, 0x41, 0x61, 0x62, 0x61, 0x42, 0x61, 0x63, 0x61, 0x43, 0x61, 0x64, 0x61, 0x44, 0x61, 0x65, 0x61, 0x45], Term).

	test(cbor_parse_2_70b, true(Term == {a-'A', b-'B', c-'C', d-'D', e-'E'})) :-
		parse([191,95,97,255,95,65,255,95,98,255,95,66,255,95,99,255,95,67,255,95,100,255,95,68,255,95,101,255,95,69,255,255], Term).

%   |(_ h'0102', h'030405')        | 0x5f42010243030405ff               |
%   +------------------------------+------------------------------------+
%   |(_ "strea", "ming")           | 0x7f657374726561646d696e67ff       |
%   +------------------------------+------------------------------------+

	% [_ ]
	test(cbor_parse_2_73, true(Term == [])) :-
		parse([0x9f, 0xff], Term).

	% [_ 1, [2, 3], [_ 4, 5]]
	test(cbor_parse_2_74, true(Term == [1, [2,3], [4,5]])) :-
		parse([0x9f, 0x01, 0x82, 0x02, 0x03, 0x9f, 0x04, 0x05, 0xff, 0xff], Term).

	% [_ 1, [2, 3], [4, 5]]
	test(cbor_parse_2_75, true(Term == [1, [2,3], [4,5]])) :-
		parse([0x9f, 0x01, 0x82, 0x02, 0x03, 0x82, 0x04, 0x05, 0xff], Term).

	% [1, [2, 3], [_ 4, 5]]
	test(cbor_parse_2_76, true(Term == [1, [2,3], [4,5]])) :-
		parse([0x83, 0x01, 0x82, 0x02, 0x03, 0x9f, 0x04, 0x05, 0xff], Term).

	% [1, [_ 2, 3], [4, 5]]
	test(cbor_parse_2_77, true(Term == [1, [2,3], [4,5]])) :-
		parse([0x83, 0x01, 0x9f, 0x02, 0x03, 0xff, 0x82, 0x04, 0x05], Term).

	% [_ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25]
	test(cbor_parse_2_78, true(Term == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25])) :-
		parse([0x9f, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x18, 0x18, 0x19, 0xff], Term).

	% {_ "a": 1, "b": [_ 2, 3]}
	test(cbor_parse_2_79, true(Term == {a-1, b-[2,3]})) :-
		parse([0xbf, 0x61, 0x61, 0x01, 0x61, 0x62, 0x9f, 0x02, 0x03, 0xff, 0xff], Term).

	% ["a", {_ "b": "c"}]
	test(cbor_parse_2_80, true(Term == [a, {b-c}])) :-
		parse([0x82, 0x61, 0x61, 0xbf, 0x61, 0x62, 0x61, 0x63, 0xff], Term).

	% {_ "Fun": true, "Amt": -2}
	test(cbor_parse_2_81, true(Term == {'Fun'-true, 'Amt'-(-2)})) :-
		parse([0xbf, 0x63, 0x46, 0x75, 0x6e, 0xf5, 0x63, 0x41, 0x6d, 0x74, 0x21, 0xff], Term).

	% generate/2 tests

	test(cbor_generate_2_01, true(Encoding == [0x00])) :-
		generate(0, Encoding).

	test(cbor_generate_2_02, true(Encoding == [0x01])) :-
		generate(1, Encoding).

	test(cbor_generate_2_03, true(Encoding == [0x0a])) :-
		generate(10, Encoding).

	test(cbor_generate_2_04, true(Encoding == [0x17])) :-
		generate(23, Encoding).

	test(cbor_generate_2_05, true(Encoding == [0x18, 0x18])) :-
		generate(24, Encoding).

	test(cbor_generate_2_06, true(Encoding == [0x18, 0x19])) :-
		generate(25, Encoding).

	test(cbor_generate_2_07, true(Encoding == [0x18, 0x64])) :-
		generate(100, Encoding).

	test(cbor_generate_2_08, true(Encoding == [0x19, 0x03, 0xe8])) :-
		generate(1000, Encoding).

	test(cbor_generate_2_09, true(Encoding == [0x1a, 0x00, 0x0f, 0x42, 0x40])) :-
		generate(1000000, Encoding).

	test(cbor_generate_2_10, true(Encoding == [0x1b, 0x00, 0x00, 0x00, 0xe8, 0xd4, 0xa5, 0x10, 0x00])) :-
		generate(1000000000000, Encoding).

	test(cbor_generate_2_11, true(Encoding == [0x1b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff])) :-
		generate(18446744073709551615, Encoding).

	test(cbor_generate_2_12, true(Encoding == [0xc2, 0x49, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])) :-
		generate(18446744073709551616, Encoding).

	test(cbor_generate_2_13, true(Encoding == [0x3b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff])) :-
		generate(-18446744073709551616, Encoding).

	test(cbor_generate_2_14, true(Encoding == [0xc3, 0x49, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])) :-
		generate(-18446744073709551617, Encoding).

	test(cbor_generate_2_15, true(Encoding == [0x20])) :-
		generate(-1, Encoding).

	test(cbor_generate_2_16, true(Encoding == [0x29])) :-
		generate(-10, Encoding).

	test(cbor_generate_2_17, true(Encoding == [0x38, 0x63])) :-
		generate(-100, Encoding).

	test(cbor_generate_2_18, true(Encoding == [0x39, 0x03, 0xe7])) :-
		generate(-1000, Encoding).

	test(cbor_generate_2_19, true(Encoding == [0xf9, 0x00, 0x00])) :-
		generate(0.0, Encoding).

	test(cbor_generate_2_20, true(Encoding == [0xf9, 0x80, 0x00]), [condition(-0.0 \== 0.0)]) :-
		generate(-0.0, Encoding).

	- test(cbor_generate_2_21a, true(Encoding == [0xf9, 0x3c, 0x00])) :-
		generate(1.0, Encoding).

	test(cbor_generate_2_21b, true(Encoding == [196, 130, 32, 10])) :-
		% decimal fraction encoding
		generate(1.0, Encoding).

	- test(cbor_generate_2_22a, true(Encoding == [0xfb, 0x3f, 0xf1, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9a])) :-
		generate(1.1, Encoding).

	test(cbor_generate_2_22b, true(Encoding == [196, 130, 32, 11])) :-
		% decimal fraction encoding
		generate(1.1, Encoding).

	- test(cbor_generate_2_23a, true(Encoding == [0xf9, 0x3e, 0x00])) :-
		generate(1.5, Encoding).

	test(cbor_generate_2_23b, true(Encoding == [196, 130, 32, 15])) :-
		% decimal fraction encoding
		generate(1.5, Encoding).

	- test(cbor_generate_2_24a, true(Encoding == [0xf9, 0x7b, 0xff])) :-
		generate(65504.0, Encoding).

	test(cbor_generate_2_24b, true(Encoding == [196, 130, 32, 26, 0, 9, 254, 192])) :-
		% decimal fraction encoding
		generate(65504.0, Encoding).

	- test(cbor_generate_2_25a, true(Encoding == [0xfa, 0x47, 0xc3, 0x50, 0x00])) :-
		generate(100000.0, Encoding).

	test(cbor_generate_2_25b, true(Encoding == [196, 130, 32, 26, 0, 15, 66, 64])) :-
		% decimal fraction encoding
		generate(100000.0, Encoding).

	- test(cbor_generate_2_26a, true(Encoding == [0xfa, 0x7f, 0x7f, 0xff, 0xff])) :-
		generate(3.4028234663852886e+38, Encoding).

	test(cbor_generate_2_26b, true(Encoding == [196,130,22,27,0,120,228,127,199,120,251,86])) :-
		% decimal fraction encoding
		generate(3.4028234663852886e+38, Encoding).

	- test(cbor_generate_2_27a, true(Encoding == [0xfb, 0x7e, 0x37, 0xe4, 0x3c, 0x88, 0x00, 0x75, 0x9c])) :-
		generate(1.0e+300, Encoding).

	test(cbor_generate_2_27b, true(Encoding == [196, 130, 25, 1, 43, 10])) :-
		% decimal fraction encoding
		generate(1.0e+300, Encoding).

	- test(cbor_generate_2_28a, true(Encoding == [0xf9, 0x00, 0x01])) :-
		generate(5.960464477539063e-8, Encoding).

	test(cbor_generate_2_28b, true(Encoding == [196,130,54,27,0,21,45,2,199,225,74,247])) :-
		% decimal fraction encoding
		generate(5.960464477539063e-8, Encoding).

	- test(cbor_generate_2_29a, true(Encoding == [0xf9, 0x04, 0x00])) :-
		generate(0.00006103515625, Encoding).

	test(cbor_generate_2_29b, true(Encoding == [196,130,45,27,0,0,0,1,107,204,65,233])) :-
		% decimal fraction encoding
		generate(0.00006103515625, Encoding).

	- test(cbor_generate_2_30a, true(Encoding == [0xf9, 0xc4, 0x00])) :-
		generate(-4.0, Encoding).

	test(cbor_generate_2_30b, true(Encoding == [196,130,32,56,39])) :-
		% decimal fraction encoding
		generate(-4.0, Encoding).

	- test(cbor_generate_2_31a, true(Encoding == [0xfb,0xc0, 0x10, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66])) :-
		generate(-4.1, Encoding).

	test(cbor_generate_2_31b, true(Encoding == [196,130,32,56,40])) :-
		% decimal fraction encoding
		generate(-4.1, Encoding).

	test(cbor_generate_2_32, true(Encoding == [0xf9, 0x7c, 0x00])) :-
		generate(inf, Encoding).

	test(cbor_generate_2_33, true(Encoding == [0xf9, 0x7e, 0x00])) :-
		generate(nan, Encoding).

	test(cbor_generate_2_34, true(Encoding == [0xf9, 0xfc, 0x00])) :-
		generate(neginf, Encoding).

	- test(cbor_generate_2_35, true(Encoding == [0xfa, 0x7f, 0x80, 0x00, 0x00])) :-
		generate(inf, Encoding).

	- test(cbor_generate_2_36, true(Encoding == [0xfa, 0x7f, 0xc0, 0x00, 0x00])) :-
		generate(nan, Encoding).

	- test(cbor_generate_2_37, true(Encoding == [0xfa, 0xff, 0x80, 0x00, 0x00])) :-
		generate(neginf, Encoding).

	- test(cbor_generate_2_38, true(Encoding == [0xfb, 0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])) :-
		generate(inf, Encoding).

	- test(cbor_generate_2_39, true(Encoding == [0xfb, 0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])) :-
		generate(nan, Encoding).

	- test(cbor_generate_2_40, true(Encoding == [0xfb, 0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])) :-
		generate(neginf, Encoding).

	test(cbor_generate_2_41, true(Encoding == [0xf4])) :-
		generate(false, Encoding).

	test(cbor_generate_2_42, true(Encoding == [0xf5])) :-
		generate(true, Encoding).

	test(cbor_generate_2_43, true(Encoding == [0xf6])) :-
		generate(null, Encoding).

	test(cbor_generate_2_44, true(Encoding == [0xf7])) :-
		generate(undefined, Encoding).

	test(cbor_generate_2_45, true(Encoding == [0xf0])) :-
		generate(simple(16), Encoding).

	test(cbor_generate_2_46, true(Encoding == [0xf8, 0xff])) :-
		generate(simple(255), Encoding).

	- test(cbor_generate_2_47a, true(Encoding == [0xc0, 0x74, 0x32, 0x30, 0x31, 0x33, 0x2d, 0x30, 0x33, 0x2d, 0x32, 0x31, 0x54, 0x32, 0x30, 0x3a, 0x30, 0x34, 0x3a, 0x30, 0x30, 0x5a])) :-
		generate(tag(0, '2013-03-21T20:04:00Z'), Encoding).

	test(cbor_generate_2_47b, true(Encoding == [192,95,50,48,49,51,45,48,51,45,50,49,84,50,48,58,48,52,58,48,48,90,255])) :-
		generate(tag(0, '2013-03-21T20:04:00Z'), Encoding).

	test(cbor_generate_2_48, true(Encoding == [0xc1, 0x1a, 0x51, 0x4b, 0x67, 0xb0])) :-
		generate(tag(1, 1363896240), Encoding).

	- test(cbor_generate_2_49a, true(Encoding == [0xc1, 0xfb, 0x41, 0xd4, 0x52, 0xd9, 0xec, 0x20, 0x00, 0x00])) :-
		generate(tag(1, 1363896240.5), Encoding).

	test(cbor_generate_2_49b, true(Encoding == [193,196,130,32,27,0,0,0,3,44,242,12,229])) :-
		% decimal fraction encoding
		generate(tag(1, 1363896240.5), Encoding).

%   |23(h'01020304')               | 0xd74401020304                     |
%   +------------------------------+------------------------------------+
%   |24(h'6449455446')             | 0xd818456449455446                 |

	% 32("http://www.example.com")
	- test(cbor_generate_2_52a, true(Encoding == [0xd8, 0x20, 0x76, 0x68, 0x74, 0x74, 0x70, 0x3a, 0x2f, 0x2f, 0x77, 0x77, 0x77, 0x2e, 0x65, 0x78, 0x61, 0x6d, 0x70, 0x6c, 0x65, 0x2e, 0x63, 0x6f, 0x6d])) :-
		generate(tag(32, 'http://www.example.com'), Encoding).

	test(cbor_generate_2_52b, true(Encoding == [216,32,95,104,116,116,112,58,47,47,119,119,119,46,101,120,97,109,112,108,101,46,99,111,109,255])) :-
		% decimal fraction encoding
		generate(tag(32, 'http://www.example.com'), Encoding).

%   |h''                           | 0x40                               |
%   +------------------------------+------------------------------------+
%   |h'01020304'                   | 0x4401020304                       |
%   +------------------------------+------------------------------------+

	test(cbor_generate_2_55, true(Encoding == [0x60])) :-
		generate('', Encoding).

	test(cbor_generate_2_56, true(Encoding == [0x5f, 0x61, 0xff])) :-
		generate('a', Encoding).

	test(cbor_generate_2_57, true(Encoding == [0x5f, 0x49, 0x45, 0x54, 0x46, 0xff])) :-
		generate('IETF', Encoding).

	test(cbor_generate_2_58, true(Encoding == [0x5f, 0x27, 0x5c, 0xff])) :-
		generate('\'\\', Encoding).

%   |"\u00fc"                      | 0x62c3bc                           |
%   +------------------------------+------------------------------------+
%   |"\u6c34"                      | 0x63e6b0b4                         |
%   +------------------------------+------------------------------------+
%   |"\ud800\udd51"                | 0x64f0908591                       |
%   +------------------------------+------------------------------------+

	test(cbor_generate_2_62, true(Encoding == [0x80])) :-
		generate([], Encoding).

	test(cbor_generate_2_63, true(Encoding == [0x9f, 0x01, 0x02, 0x03, 0xff])) :-
		generate([1, 2, 3], Encoding).

	test(cbor_generate_2_64, true(Encoding == [0x9f, 0x01, 0x9f, 0x02, 0x03, 0xff, 0x9f, 0x04, 0x05, 0xff, 0xff])) :-
		generate([1, [2, 3], [4, 5]], Encoding).

	test(cbor_generate_2_65, true(Encoding == [0x9f, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x18, 0x18, 0x19, 0xff])) :-
		generate([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25], Encoding).

	test(cbor_generate_2_66, true(Encoding == [0xa0])) :-
		generate({}, Encoding).

	- test(cbor_generate_2_67a, true(Encoding == [0xa2, 0x01, 0x02, 0x03, 0x04])) :-
		generate({1-2, 3-4}, Encoding).

	test(cbor_generate_2_67b, true(Encoding == [0xbf, 0x01, 0x02, 0x03, 0x04, 0xff])) :-
		generate({1-2, 3-4}, Encoding).

	test(cbor_generate_2_68, true(Encoding == [0xbf, 0x5f, 0x61, 0xff, 0x01, 0x5f, 0x62, 0xff, 0x9f, 0x02, 0x03, 0xff, 0xff])) :-
		generate({a-1, b-[2, 3]}, Encoding).

	test(cbor_generate_2_69, true(Encoding == [0x9f, 0x5f, 0x61, 0xff, 0xbf, 0x5f, 0x62, 0xff, 0x5f, 0x63, 0xff, 0xff, 0xff])) :-
		generate([a, {b-c}], Encoding).

	- test(cbor_generate_2_70a, true(Encoding == [0xa5, 0x61, 0x61, 0x61, 0x41, 0x61, 0x62, 0x61, 0x42, 0x61, 0x63, 0x61, 0x43, 0x61, 0x64, 0x61, 0x44, 0x61, 0x65, 0x61, 0x45])) :-
		generate({a-'A', b-'B', c-'C', d-'D', e-'E'}, Encoding).

	test(cbor_generate_2_70b, true(Encoding == [191,95,97,255,95,65,255,95,98,255,95,66,255,95,99,255,95,67,255,95,100,255,95,68,255,95,101,255,95,69,255,255])) :-
		generate({a-'A', b-'B', c-'C', d-'D', e-'E'}, Encoding).


%   |(_ h'0102', h'030405')        | 0x5f42010243030405ff               |
%   +------------------------------+------------------------------------+
%   |(_ "strea", "ming")           | 0x7f657374726561646d696e67ff       |
%   +------------------------------+------------------------------------+
%   |[_ ]                          | 0x9fff                             |
%   +------------------------------+------------------------------------+
%   |[_ 1, [2, 3], [_ 4, 5]]       | 0x9f018202039f0405ffff             |
%   +------------------------------+------------------------------------+
%   |[_ 1, [2, 3], [4, 5]]         | 0x9f01820203820405ff               |
%   +------------------------------+------------------------------------+
%   |[1, [2, 3], [_ 4, 5]]         | 0x83018202039f0405ff               |
%   +------------------------------+------------------------------------+
%   |[1, [_ 2, 3], [4, 5]]         | 0x83019f0203ff820405               |
%   +------------------------------+------------------------------------+
%   |[_ 1, 2, 3, 4, 5, 6, 7, 8, 9, | 0x9f0102030405060708090a0b0c0d0e0f |
%   |10, 11, 12, 13, 14, 15, 16,   | 101112131415161718181819ff         |
%   |17, 18, 19, 20, 21, 22, 23,   |                                    |
%   |24, 25]                       |                                    |
%   +------------------------------+------------------------------------+
%   |{_ "a": 1, "b": [_ 2, 3]}     | 0xbf61610161629f0203ffff           |
%   +------------------------------+------------------------------------+
%   |["a", {_ "b": "c"}]           | 0x826161bf61626163ff               |
%   +------------------------------+------------------------------------+
%   |{_ "Fun": true, "Amt": -2}    | 0xbf6346756ef563416d7421ff


	% text string tests

	test(cbor_text_string_utf_8_01, true(Map == {el-'Γειά σου κόσμε!'})) :-
		generate({el-'Γειά σου κόσμε!'}, Encoding),
		parse(Encoding, Map).

	test(cbor_text_string_utf_8_02, true(Map == {en-'Hello world!'})) :-
		generate({en-'Hello world!'}, Encoding),
		parse(Encoding, Map).

	test(cbor_text_string_utf_8_03, true(Map == {es-'¡Hola mundo!'})) :-
		generate({es-'¡Hola mundo!'}, Encoding),
		parse(Encoding, Map).

	test(cbor_text_string_utf_8_04, true(Map == {ja-'こんにちは世界!'})) :-
		generate({ja-'こんにちは世界!'}, Encoding),
		parse(Encoding, Map).

	test(cbor_text_string_utf_8_05, true(Map == {ko-'여보세요 세계!'})) :-
		generate({ko-'여보세요 세계!'}, Encoding),
		parse(Encoding, Map).

	test(cbor_text_string_utf_8_06, true(Map == {nl-'Hallo wereld!'})) :-
		generate({nl-'Hallo wereld!'}, Encoding),
		parse(Encoding, Map).

	test(cbor_text_string_utf_8_07, true(Map == {pt-'Olá mundo!'})) :-
		generate({pt-'Olá mundo!'}, Encoding),
		parse(Encoding, Map).

	test(cbor_text_string_utf_8_08, true(Map == {ru-'Здравствулте! мир!'})) :-
		generate({ru-'Здравствулте! мир!'}, Encoding),
		parse(Encoding, Map).

	test(cbor_text_string_utf_8_09, true(Map == {zh-'你好世界!'})) :-
		generate({zh-'你好世界!'}, Encoding),
		parse(Encoding, Map).

:- end_object.
