%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:12:0,
		author is 'Paulo Moura',
		date is 2024-09-24,
		comment is 'Unit tests for the ISO Prolog standard number_codes/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.8.4

	test(iso_number_codes_2_01, true(L == [0'3,0'3])) :-
		{number_codes(33, L)}.

	test(iso_number_codes_2_02, true) :-
		{number_codes(33, [0'3,0'3])}.

	test(iso_number_codes_2_03, true(N == 33.0)) :-
		{number_codes(33.0, L), number_codes(N, L)}.

	test(iso_number_codes_2_04, true) :-
		{number_codes(33.0, [0'3| _L])}.

	test(iso_number_codes_2_05, true(A == -25)) :-
		{number_codes(A, [0'-,0'2,0'5])}.

	test(iso_number_codes_2_06, true(A == 3)) :-
		{number_codes(A, [0' , 0'3])}.

	test(iso_number_codes_2_07, true(A == 15)) :-
		{number_codes(A, [0'0,0'x,0'f])}.

	test(iso_number_codes_2_08, true(A == 0'a)) :-
		{number_codes(A, [0'0,39,0'a])}.

	test(iso_number_codes_2_09, true(A == 4.2)) :-
		{number_codes(A, [0'4,0'.,0'2])}.

	test(iso_number_codes_2_10, true(A == 4.2)) :-
		{number_codes(A, [0'4,0'2,0'.,0'0,0'e,0'-,0'1])}.

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.8.1 NOTES

	test(iso_number_codes_2_11, true) :-
		C = [0'0, 0'., 0'1],
		{number_codes(X, C), number_codes(X, C)}.

	quick_check(iso_number_codes_2_12, round_trip(+integer)).

	quick_check(iso_number_codes_2_13, round_trip(+float)).

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(eddbali_number_codes_2_14, error(instantiation_error)) :-
		{number_codes(_A, _L)}.

	test(eddbali_number_codes_2_15, error(type_error(number,a))) :-
		{number_codes(a, _L)}.

	test(eddbali_number_codes_2_16, error(type_error(list,4))) :-
		{number_codes(_A, 4)}.

	test(eddbali_number_codes_2_17, error(representation_error(character_code))) :-
		{number_codes(_A, [0'4,-1])}.

	test(sics_number_codes_2_18, error(instantiation_error)) :-
		{number_codes(_A, [0'a|_L])}.

	test(sics_number_codes_2_19, error(instantiation_error)) :-
		{number_codes(_A, [0'a,_L])}.

	test(sics_number_codes_2_20, true(X-Y == 273-[50,55,51])) :-
		{number_chars(X, [' ','0','x','1','1','1']), number_codes(X, Y)}.

	test(sics_number_codes_2_21, true(X-Y == 73-[55,51])) :-
		{number_chars(X, [' ','0','o','1','1','1']), number_codes(X, Y)}.

	test(sics_number_codes_2_22, true(X-Y == 7-[55])) :-
		{number_chars(X, [' ','0','b','1','1','1']), number_codes(X, Y)}.

	test(sics_number_codes_2_23, true(N == 10)) :-
		{number_codes(N, [48,39,92,110])}. % 0'\n

	test(sics_number_codes_2_24, error(syntax_error(_))) :-
		% the original test used "ä" but that rises portability issues
		% due to the lack of a standard way to specify text encodings
		{number_codes(_N, [97])}.

	test(sics_number_codes_2_25, error(syntax_error(_))) :-
		{number_codes(_X, [0'0,0'x,0'0,0'.,0'0])}.

	% tests from the Logtalk portability work

	% the ISO standard specifies a representation_error(character_code)
	% but there seens to be some agreement between Prolog implementers
	% that the correct exception in this case is a type_error(integer,a)
	% until a consensus if found, we accept both exception terms
	test(lgt_number_codes_2_26, errors([representation_error(character_code), type_error(integer,a)])) :-
		{number_codes(_A, [0'4,a])}.

	test(lgt_number_codes_2_27, true(A-B == 52-50)) :-
		{number_codes(42, [A,B])}.

	test(lgt_number_codes_2_28, errors([representation_error(character_code), type_error(integer,a)])) :-
		{number_codes(1234, [a,b,c,d])}.

	test(lgt_number_codes_2_29, false) :-
		{number_codes(123, [0'2| _])}.

	test(lgt_number_codes_2_30, error(type_error(list,[0'2| 0'3]))) :-
		{number_codes(123, [0'2| 0'3])}.

	test(lgt_number_codes_2_31, error(syntax_error(_))) :-
		{number_codes(_, [0'3,0'%])}.

	test(lgt_number_codes_2_32, error(syntax_error(_))) :-
		{number_codes(_, [0'3,0'/,0'*,0'-,0'*,0'/])}.

	test(sics_number_codes_2_33, true(X == 273)) :-
		{number_codes(X, [0'0,0'x,0'1,0'1,0'1])}.

	test(sics_number_codes_2_34, true(X == 73)) :-
		{number_codes(X, [0'0,0'o,0'1,0'1,0'1])}.

	test(sics_number_codes_2_35, true(X == 7)) :-
		{number_codes(X, [0'0,0'b,0'1,0'1,0'1])}.

	% tests from (or derived from) the WG17 test suite

	test(wg17_number_codes_2_36, true) :-
		{number_codes(1.0e9, [0'1,0'.,0'0,0'E,0'9])}.

	test(wg17_number_codes_2_37, true) :-
		{number_codes(1.0e9, [0'1,0'.,0'0,0'E,0'+,0'9])}.

	test(wg17_number_codes_2_38, error(syntax_error(_))) :-
		{number_codes(1, [])}.

	test(wg17_number_codes_2_39, error(syntax_error(_))) :-
		{number_codes(_, [0'3,0'.])}.

	test(wg17_number_codes_2_40, error(syntax_error(_))) :-
		{number_codes(_, [0'0,0'B,0'1])}.

	test(wg17_number_codes_2_41, error(syntax_error(_))) :-
		{number_codes(_, [0'0,0'O,0'7])}.

	test(wg17_number_codes_2_42, error(syntax_error(_))) :-
		{number_codes(_, [0'0,0'X,0'f])}.

	test(wg17_number_codes_2_43, true(X == 32)) :-
		{number_codes(X, [48,39,32])}.

	test(lgt_number_codes_2_44, error(type_error(list, [0'1,0'2,0'3| 0'4]))) :-
		{number_codes(123, [0'1,0'2,0'3| 0'4])}.

	test(lgt_number_codes_2_45, error(type_error(integer, '4'))) :-
		{number_codes(123, [0'1,0'2,0'3,'4'])}.

	test(lgt_number_codes_2_46, true(Tail == [])) :-
		{number_codes(123, [0'1,0'2,0'3| Tail])}.

	% auxiliary predicates

	round_trip(X) :-
		{number_codes(X, L),
		 number_codes(Y, L),
		 X == Y}.

:- end_object.
