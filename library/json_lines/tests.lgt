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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2025-05-27,
		comment is 'Tests for the "json_lines" library.'
	]).

	:- uses(json_lines, [
		parse/2, generate/2
	]).

	:- uses(lgtunit, [
		assertion/1, assertion/2
	]).

	cover(json_lines).
	cover(json_lines(_)).
	cover(json_lines(_, _, _)).

	setup :-
		^^clean_file('test_files/output01.jsonl').

	cleanup :-
		setup,
		^^clean_text_input.

	test(parse_codes, true(Terms == [[1]])) :-
		parse(codes([0'[, 0'1, 0'], 0'\n]), Terms).

	test(parse_skip_ws, true(Terms == [{a-b}])) :-
		atom_codes('\r\t {"a":"b"}\n', Codes),
		parse(codes(Codes), Terms).

	test(parse_no_final_end_of_line, true(Terms == [{a-b}, {c-d}])) :-
		atom_codes('{"a":"b"}\n{"c":"d"}', Codes),
		parse(codes(Codes), Terms).

	test(parse_arrays, true) :-
		^^file_path('test_files/arrays.jsonl', Path),
		parse(file(Path), Terms),
		assertion(
			Terms == [
				['Name',    'Session', 'Score', 'Completed'],
				['Gilbert', '2013',    24,      '@'(true)],
				['Alexa',   '2013',    29,      '@'(true)],
				['May',     '2012B',   14,      '@'(false)],
				['Deloise', '2012A',   19,      '@'(true)]
			]
		).

	test(parse_objects, true) :-
		^^file_path('test_files/objects.jsonl', Path),
		parse(file(Path), Terms),
		assertion(
			Terms == [
				{name-'Gilbert', session-'2013',  score-24, completed-'@'(true)},
				{name-'Alexa',   session-'2013',  score-29, completed-'@'(true)},
				{name-'May',     session-'2012B', score-14, completed-'@'(false)},
				{name-'Deloise', session-'2012A', score-19, completed-'@'(true)}
			]
		).

	test(parse_no_final_newline, true(Terms == [1,2,3])) :-
		^^file_path('test_files/no_final_newline.jsonl', Path),
		parse(file(Path), Terms).

	test(parse_line, true(Term1-Term2 == {a-0}-{b-[1,2,3]})) :-
		^^set_text_input(in, '{"a":0}\n{"b":[1,2,3]}\n'),
		parse(line(in), [Term1]),
		parse(line(in), [Term2]),
		close(in).

	test(parse_chars, true(Terms == [{a-b}])) :-
		atom_chars('{"a":"b"}', Chars),
		parse(chars(Chars), Terms).

	test(parse_object_curly, true(Terms == [{a-1,b-2,c-3}])) :-
		json_lines(curly,dash,atom)::parse(atom('{"a":1, "b":2, "c":3}'), Terms).

	test(parse_object_list, true(Terms == [json([a-1,b-2,c-3])])) :-
		json_lines(list,dash,atom)::parse(atom('{"a":1, "b":2, "c":3}'), Terms).

	test(parse_pair_equal, true(Terms == [json([a=1,b=2,c=3])])) :-
		json_lines(list,equal,atom)::parse(atom('{"a":1, "b":2, "c":3}'), Terms).

	test(parse_pair_colon, true(Terms == [json([':'(a,1),':'(b,2),':'(c,3)])])) :-
		json_lines(list,colon,atom)::parse(atom('{"a":1, "b":2, "c":3}'), Terms).

	test(generate_chars, true(Terms == [{a-b}])) :-
		generate(chars(Chars), [{a-b}]),
		parse(chars(Chars), Terms).

	test(parse_generate_atom, true(Atom == '[1,2,{"a":"b"}]\n')) :-
		parse(atom('[1,2,{"a":"b"}]'), Terms),
		generate(atom(Atom), Terms).

	test(encode_pair_string_number, true(Atom == '{"a":1}\n')) :-
		generate(atom(Atom), [{a-1}]).

	test(encode_pair_dash_string_object, true(Atom == '{"a":{"b":"c"}}\n')) :-
		generate(atom(Atom), [{a-{b-c}}]).

	test(encode_pair_equal_string_object, true(Atom == '{"a":{"b":"c"}}\n')) :-
		generate(atom(Atom), [{a={b=c}}]).

	test(encode_pair_colon_string_object, true(Atom == '{"a":{"b":"c"}}\n')) :-
		generate(atom(Atom), [{':'(a,{':'(b,c)})}]).

	test(roundtrip_object_representation, true(Terms-Chars == [json([])]-['{','}','\n'])) :-
		json_lines(list,dash,atom)::(parse(chars(['{','}']), Terms), generate(chars(Chars), Terms)).

	test(roundtrip_string_representation, true(Terms-Chars == [chars(['{','}'])]-['"','{','}','"','\n'])) :-
		json_lines(curly,dash,chars)::(parse(chars(['"','{','}','"']), Terms), generate(chars(Chars), Terms)).

	test(atom_parse_codes, true(Terms == [hello])) :-
		json_lines(atom)::parse(codes([34,104,101,108,108,111,34]), Terms).

	test(chars_parse_codes, true(Terms == [chars([h,e,l,l,o])])) :-
		json_lines(chars)::parse(codes([34,104,101,108,108,111,34]), Terms).

	test(codes_parse_codes, true(Terms == [codes([104,101,108,108,111])])) :-
		json_lines(codes)::parse(codes([34,104,101,108,108,111,34]), Terms).

	test(json_generate_error_object_list_representation, error(domain_error(json_term, {a-b}))) :-
		json_lines(list,dash,atom)::generate(chars(_), [{a-b}]).

	test(json_generate_error_object_curly_representation, error(domain_error(json_term, json([a-b])))) :-
		json_lines(curly,dash,atom)::generate(chars(_), [json([a-b])]).

:- end_object.
