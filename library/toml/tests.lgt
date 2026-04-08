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
		date is 2026-04-09,
		comment is 'Tests for the "toml" library.'
	]).

	:- uses(toml, [
		parse/2, generate/2
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	cover(toml).
	cover(toml(_)).
	cover(toml(_, _, _)).

	setup :-
		^^clean_file('test_files/output.toml').

	test(parse_simple_file, deterministic) :-
		^^file_path('test_files/simple.toml', Path),
		parse(file(Path), TOML),
		assertion(
			TOML == toml([
				title-'TOML Example',
				owner-toml([
					name-'Tom Preston-Werner',
					organization-'GitHub',
					active-'@'(true)
				]),
				database-toml([
					server-'192.168.1.1',
					ports-[8001,8001,8002],
					enabled-'@'(true)
				])
			])
		).

	test(parse_dotted_keys, deterministic(TOML == toml([site-toml([owner-toml([name-'Ada'])])])) ) :-
		parse(atom('site.owner.name = "Ada"\n'), TOML).

	test(parse_inline_table, deterministic(TOML == toml([point-toml([x-1, y-2])])) ) :-
		parse(atom('point = {x = 1, y = 2}\n'), TOML).

	test(parse_strings_and_special_values, deterministic(TOML == toml([
		literal-'literal text',
		basic-'line\nfeed',
		pi-3.14,
		limit-'@'(inf),
		flag-'@'(false)
	]))) :-
		parse(atom('literal = ''literal text''\nbasic = "line\\nfeed"\npi = 3.14\nlimit = inf\nflag = false\n'), TOML).

	test(parse_multiline_basic_string, deterministic(TOML == toml([
		poem-'Roses are red\nViolets are blue',
		message-'The quick brown fox jumps over the lazy dog.'
	]))) :-
		parse(atom('poem = """\nRoses are red\nViolets are blue"""\nmessage = """\nThe quick brown \\\n\n  fox jumps over \\\n    the lazy dog."""\n'), TOML).

	test(parse_multiline_literal_string, deterministic(TOML == toml([
		regex-'I [dw]on''t need \\d{2} apples'
	]))) :-
		parse(atom('regex = ''''''\nI [dw]on''t need \\d{2} apples''''''\n'), TOML).

	test(parse_temporal_values, deterministic(TOML == toml([
		birthday-date(1979, 5, 27),
		alarm-time(7, 32, 0),
		meeting-date_time(1979, 5, 27, 7, 32, 0),
		stamp-date_time(1979, 5, 27, 0, 32, 0.999999, -25200)
	]))) :-
		parse(atom('birthday = 1979-05-27\nalarm = 07:32:00\nmeeting = 1979-05-27T07:32:00\nstamp = 1979-05-27 00:32:00.999999-07:00\n'), TOML).

	test(parse_array_of_tables, deterministic(TOML == toml([
		products-[
			toml([name-'Hammer', sku-738594937]),
			toml([name-'Nail', sku-284758393, color-'gray'])
		]
	]))) :-
		parse(atom('[[products]]\nname = "Hammer"\nsku = 738594937\n\n[[products]]\nname = "Nail"\nsku = 284758393\ncolor = "gray"\n'), TOML).

	test(parse_nested_array_of_tables, deterministic(TOML == toml([
		fruits-[
			toml([
				name-'apple',
				physical-toml([color-'red']),
				varieties-[toml([name-'red delicious'])]
			]),
			toml([
				name-'banana',
				varieties-[toml([name-'plantain'])]
			])
		]
	]))) :-
		parse(atom('[[fruits]]\nname = "apple"\n\n[fruits.physical]\ncolor = "red"\n\n[[fruits.varieties]]\nname = "red delicious"\n\n[[fruits]]\nname = "banana"\n\n[[fruits.varieties]]\nname = "plantain"\n'), TOML).

	test(parse_curly_representation, deterministic(TOML == {title-'Example', owner-{name-'Tom'}})) :-
		toml(curly, dash, atom)::parse(atom('title = "Example"\n[owner]\nname = "Tom"\n'), TOML).

	test(generate_atom, deterministic(Atom == 'title = "Example"\ncount = 2\n\n[owner]\nname = "Tom"\n')) :-
		generate(atom(Atom), toml([
			title-'Example',
			count-2,
			owner-toml([
				name-'Tom'
			])
		])).

	test(generate_chars_representation, deterministic(Atom == 'title = "Example"\n')) :-
		toml(chars)::generate(atom(Atom), toml([
			title-chars(['E',x,a,m,p,l,e])
		])).

	test(generate_temporal_values, deterministic(Atom == 'birthday = 1979-05-27\nalarm = 07:32:00\nmeeting = 1979-05-27T07:32:00\nstamp = 1979-05-27T00:32:00.999999-07:00\n')) :-
		generate(atom(Atom), toml([
			birthday-date(1979, 5, 27),
			alarm-time(7, 32, 0),
			meeting-date_time(1979, 5, 27, 7, 32, 0),
			stamp-date_time(1979, 5, 27, 0, 32, 0.999999, -25200)
		])).

	test(generate_array_of_tables, deterministic(Atom == '[[products]]\nname = "Hammer"\n\n[[products]]\nname = "Nail"\n')) :-
		generate(atom(Atom), toml([
			products-[
				toml([name-'Hammer']),
				toml([name-'Nail'])
			]
		])).

	test(roundtrip_file, deterministic) :-
		^^file_path('test_files/roundtrip_nested.toml', Path),
		parse(file(Path), TOML),
		generate(atom(Atom), TOML),
		parse(atom(Atom), ParsedAgain),
		assertion(TOML == ParsedAgain).

	test(roundtrip_multiline_string, deterministic) :-
		parse(atom('text = """\nalpha\nbeta"""\nraw = ''''''\nC:\\Temp\\logs\n''''''\n'), TOML),
		generate(atom(Atom), TOML),
		parse(atom(Atom), ParsedAgain),
		assertion(TOML == ParsedAgain).

	test(roundtrip_array_of_tables, deterministic) :-
		parse(atom('[[products]]\nname = "Hammer"\n\n[[products]]\nname = "Nail"\nsku = 2\n'), TOML),
		generate(atom(Atom), TOML),
		parse(atom(Atom), ParsedAgain),
		assertion(TOML == ParsedAgain).

	test(reject_duplicate_key, false) :-
		parse(atom('a = 1\na = 2\n'), _).

	test(reject_reopened_explicit_table, false) :-
		parse(atom('[a]\nvalue = 1\n[a]\nother = 2\n'), _).

	test(reject_array_of_tables_after_static_array, false) :-
		parse(atom('fruits = []\n[[fruits]]\nname = "apple"\n'), _).

	test(reject_generating_invalid_term, error(domain_error(toml_term, [a-1]))) :-
		generate(chars(_), [a-1]).

:- end_object.
