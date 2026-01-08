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
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2025-12-15,
		comment is 'Tests for the TOON library.'
	]).

	:- uses(toon, [
		parse/2, generate/2
	]).

	:- uses(lgtunit, [
		assertion/1,
		op(700, xfx, =~=), (=~=)/2
	]).

	cover(toon(_,_,_)).

	% Parsing tests

	test(toon_parse_empty_object, true(Term == {})) :-
		parse(atom(''), Term).

	test(toon_parse_object_one_element, true(Term == {name-'John'})) :-
		parse(atom('name: John'), Term).

	test(toon_parse_object_two_elements, true(Term == {name-'John', age-30})) :-
		parse(atom('name: John\nage: 30'), Term).

	test(toon_parse_object_three_elements, true(Term == {name-'John', age-30, active- @true})) :-
		parse(atom('name: John\nage: 30\nactive: true'), Term).

	test(toon_parse_primitives_true, true(Term == @true)) :-
		parse(atom('true'), Term).

	test(toon_parse_primitives_false, true(Term == @false)) :-
		parse(atom('false'), Term).

	test(toon_parse_primitives_null, true(Term == @null)) :-
		parse(atom('null'), Term).

	test(toon_parse_primitives_number, true(Term == 42)) :-
		parse(atom('42'), Term).

	test(toon_parse_primitives_float, true(Term =~= 3.14)) :-
		parse(atom('3.14'), Term).

	test(toon_parse_inline_array, true(Term == {numbers-[1,2,3]})) :-
		parse(atom('numbers[3]: 1,2,3'), Term).

	test(toon_parse_nested_object, true(Term == {person-{name-'John', age-30}})) :-
		parse(atom('person:\n  name: John\n  age: 30'), Term).

	test(toon_parse_quoted_string, true(Term == {message-'Hello, World!'})) :-
		parse(atom('message: "Hello, World!"'), Term).

	test(toon_parse_quoted_string_with_colon, true(Term == {special-'value with: colon'})) :-
		parse(atom('special: "value with: colon"'), Term).

	test(toon_parse_codes, true) :-
		atom_codes('name: John', Codes),
		parse(codes(Codes), _Term).

	test(toon_parse_chars, true(Term == {a-b})) :-
		atom_chars('a: b', Chars),
		parse(chars(Chars), Term).

	test(toon_parse_file_empty_object, true(Term == {})) :-
		^^file_path('test_files/simple/empty_object.toon', Path),
		parse(file(Path), Term).

	test(toon_parse_file_object_one_element, true(Term == {name-'John'})) :-
		^^file_path('test_files/simple/object_one_element.toon', Path),
		parse(file(Path), Term).

	test(toon_parse_file_object_two_elements, true(Term == {name-'John', age-30})) :-
		^^file_path('test_files/simple/object_two_elements.toon', Path),
		parse(file(Path), Term).

	test(toon_parse_file_primitives, true) :-
		^^file_path('test_files/simple/primitives.toon', Path),
		parse(file(Path), Term),
		Term = {string-String, number-Number, float-Float, bool_true-BoolTrue, bool_false-BoolFalse, null_value-Null},
		assertion(String == hello),
		assertion(Number == 42),
		assertion(Float =~= 3.14),
		assertion(BoolTrue == @true),
		assertion(BoolFalse == @false),
		assertion(Null == @null).

	test(toon_parse_file_inline_array, true(Term == {numbers-[1,2,3]})) :-
		^^file_path('test_files/simple/inline_array.toon', Path),
		parse(file(Path), Term).

	test(toon_parse_file_nested_object, true(Term == {person-{name-'John', age-30}})) :-
		^^file_path('test_files/simple/nested_object.toon', Path),
		parse(file(Path), Term).

	test(toon_parse_object_curly, true(Term == {a-1, b-2})) :-
		toon(curly,dash,atom)::parse(atom('a: 1\nb: 2'), Term).

	test(toon_parse_object_list, true(Term == toon([a-1, b-2]))) :-
		toon(list,dash,atom)::parse(atom('a: 1\nb: 2'), Term).

	test(toon_parse_pair_equal, true(Term == toon([a=1, b=2]))) :-
		toon(list,equal,atom)::parse(atom('a: 1\nb: 2'), Term).

	test(toon_parse_pair_colon, true(Term == toon([':'(a,1), ':'(b,2)]))) :-
		toon(list,colon,atom)::parse(atom('a: 1\nb: 2'), Term).

	% Generation tests

	test(toon_generate_empty_object, true(Atom == '')) :-
		generate(atom(Atom), {}).

	test(toon_generate_object_one_element, true(Atom == 'name: John')) :-
		generate(atom(Atom), {name-'John'}).

	test(toon_generate_primitives_true, true(Atom == 'true')) :-
		generate(atom(Atom), @true).

	test(toon_generate_primitives_false, true(Atom == 'false')) :-
		generate(atom(Atom), @false).

	test(toon_generate_primitives_null, true(Atom == 'null')) :-
		generate(atom(Atom), @null).

	test(toon_generate_primitives_number, true(Atom == '42')) :-
		generate(atom(Atom), 42).

	test(toon_generate_primitives_float, true(sub_atom(Atom, 0, _, _, '3.14'))) :-
		generate(atom(Atom), 3.14).

	test(toon_generate_inline_array, true(sub_atom(Atom, _, _, _, '[3]: 1,2,3'))) :-
		generate(atom(Atom), [1,2,3]).

	test(toon_generate_codes, true(Codes == [110,97,109,101,58,32,74,111,104,110])) :-
		generate(codes(Codes), {name-'John'}).

	test(toon_generate_chars, true(Chars == [n,a,m,e,':',' ','J',o,h,n])) :-
		generate(chars(Chars), {name-'John'}).

:- end_object.
