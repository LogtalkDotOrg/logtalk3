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


	cover(string(_)).

	% atom_string/2 tests

	test(atom_string_2_01, true(String == "hello")) :-
		string::atom_string(hello, String).

	test(atom_string_2_02, true(Atom == hello)) :-
		string::atom_string(Atom, "hello").

	test(atom_string_2_03, true(String == "")) :-
		string::atom_string('', String).

	test(atom_string_2_04, true(Atom == '')) :-
		string::atom_string(Atom, "").

	% number_string/2 tests

	test(number_string_2_01, true(String == "42")) :-
		string::number_string(42, String).

	test(number_string_2_02, true(Number == 42)) :-
		string::number_string(Number, "42").

	test(number_string_2_03, true(String == "-17")) :-
		string::number_string(-17, String).

	test(number_string_2_04, true(Number == -17)) :-
		string::number_string(Number, "-17").

	- test(number_string_2_05, true(String == "3.14")) :-
		string::number_string(3.14, String).

	test(number_string_2_06, true(Number == 3.14)) :-
		string::number_string(Number, "3.14").

	% string_chars/2 tests

	test(string_chars_2_01, true(Chars == [h,e,l,l,o])) :-
		string::string_chars("hello", Chars).

	test(string_chars_2_02, true(String == "hello")) :-
		string::string_chars(String, [h,e,l,l,o]).

	test(string_chars_2_03, true(Chars == [])) :-
		string::string_chars("", Chars).

	test(string_chars_2_04, true(String == "")) :-
		string::string_chars(String, []).

	% string_codes/2 tests

	test(string_codes_2_01, true(Codes == [104,101,108,108,111])) :-
		string::string_codes("hello", Codes).

	test(string_codes_2_02, true(String == "hello")) :-
		string::string_codes(String, [104,101,108,108,111]).

	test(string_codes_2_03, true(Codes == [])) :-
		string::string_codes("", Codes).

	test(string_codes_2_04, true(String == "")) :-
		string::string_codes(String, []).

	% string_concat/3 tests

	test(string_concat_3_01, true(String == "helloworld")) :-
		string::string_concat("hello", "world", String).

	test(string_concat_3_02, true(String == "hello")) :-
		string::string_concat("hello", "", String).

	test(string_concat_3_03, true(String == "world")) :-
		string::string_concat("", "world", String).

	test(string_concat_3_04, true(String == "")) :-
		string::string_concat("", "", String).

	test(string_concat_3_05, true(S1 == "hello")) :-
		string::string_concat(S1, "world", "helloworld").

	test(string_concat_3_06, true(S2 == "world")) :-
		string::string_concat("hello", S2, "helloworld").

	% string_length/2 tests

	test(string_length_2_01, true(Length == 5)) :-
		string::string_length("hello", Length).

	test(string_length_2_02, true(Length == 0)) :-
		string::string_length("", Length).

	test(string_length_2_03, true(Length == 11)) :-
		string::string_length("hello world", Length).

	% sub_string/5 tests

	test(sub_string_5_01, true(Sub == "ell")) :-
		string::sub_string("hello", 1, 3, 1, Sub).

	test(sub_string_5_02, true(Sub == "hello")) :-
		string::sub_string("hello", 0, 5, 0, Sub).

	test(sub_string_5_03, true(Sub == "")) :-
		string::sub_string("hello", 0, 0, 5, Sub).

	test(sub_string_5_04, true(Before == 1)) :-
		string::sub_string("hello", Before, 3, 1, "ell").

	test(sub_string_5_05, true(Length == 3)) :-
		string::sub_string("hello", 1, Length, 1, "ell").

	test(sub_string_5_06, true(After == 1)) :-
		string::sub_string("hello", 1, 3, After, "ell").

	% string_upper/2 tests

	test(string_upper_2_01, true(Upper == "HELLO")) :-
		string::string_upper("hello", Upper).

	test(string_upper_2_02, true(Upper == "HELLO")) :-
		string::string_upper("Hello", Upper).

	test(string_upper_2_03, true(Upper == "HELLO")) :-
		string::string_upper("HELLO", Upper).

	test(string_upper_2_04, true(Upper == "")) :-
		string::string_upper("", Upper).

	test(string_upper_2_05, true(Upper == "HELLO123")) :-
		string::string_upper("hello123", Upper).

	% string_lower/2 tests

	test(string_lower_2_01, true(Lower == "hello")) :-
		string::string_lower("HELLO", Lower).

	test(string_lower_2_02, true(Lower == "hello")) :-
		string::string_lower("Hello", Lower).

	test(string_lower_2_03, true(Lower == "hello")) :-
		string::string_lower("hello", Lower).

	test(string_lower_2_04, true(Lower == "")) :-
		string::string_lower("", Lower).

	test(string_lower_2_05, true(Lower == "hello123")) :-
		string::string_lower("HELLO123", Lower).

	% split_string/4 tests (from ECLiPSe documentation examples)

	% split at every /
	test(split_string_4_01, true(L == ["", "usr", "local", "eclipse"])) :-
		string::split_string("/usr/local/eclipse", "/", "", L).

	% split at every sequence of /
	test(split_string_4_02, true(L == ["usr", "local", "eclipse"])) :-
		string::split_string("/usr/local//eclipse/", "/", "/", L).

	% split and strip padding
	test(split_string_4_03, true(L == ["comma", "separated", "data items"])) :-
		string::split_string(" comma, separated , data items ", ",", " ", L).

	% just strip padding
	test(split_string_4_04, true(L == ["Hello world"])) :-
		string::split_string("   Hello world...", "", " .", L).

	% empty string
	test(split_string_4_05, true(L == [])) :-
		string::split_string("", ",", " ", L).

	% no separators in string
	test(split_string_4_06, true(L == ["hello"])) :-
		string::split_string("hello", ",", "", L).

	% only separators
	test(split_string_4_07, true(L == ["", "", ""])) :-
		string::split_string(",,", ",", "", L).

	% atomics_to_string/2 tests (from ECLiPSe documentation examples)

	test(atomics_to_string_2_01, true(S == "abcdef")) :-
		string::atomics_to_string([abc, def], S).

	test(atomics_to_string_2_02, true(S == "theman is aged 20")) :-
		string::atomics_to_string([the, man, " is aged ", 20], S).

	test(atomics_to_string_2_03, true(S == "123")) :-
		string::atomics_to_string([1, 2, 3], S).

	test(atomics_to_string_2_04, true(S == "")) :-
		string::atomics_to_string([], S).

	test(atomics_to_string_2_05, true(S == "hello")) :-
		string::atomics_to_string([hello], S).

	% atomics_to_string/3 tests (from ECLiPSe documentation examples)

	test(atomics_to_string_3_01, true(S == "usr/local/bin")) :-
		string::atomics_to_string([usr, local, bin], "/", S).

	test(atomics_to_string_3_02, true(S == "1 -> 2 -> 3")) :-
		string::atomics_to_string([1, 2, 3], " -> ", S).

	test(atomics_to_string_3_03, true(S == "")) :-
		string::atomics_to_string([], ",", S).

	test(atomics_to_string_3_04, true(S == "hello")) :-
		string::atomics_to_string([hello], ",", S).

	test(atomics_to_string_3_05, true(S == "a,b,c")) :-
		string::atomics_to_string([a, b, c], ",", S).

