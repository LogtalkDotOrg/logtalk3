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

	test(parse_codes, deterministic(Terms == [[1]])) :-
		parse(codes([0'[, 0'1, 0'], 0'\n]), Terms).

	test(parse_error_invalid_source, error(domain_error(json_lines_source, 42))) :-
		parse(42, _Terms).

	test(parse_error_unescaped_newline, error(domain_error(json_lines, codes([34,108,105,110,101,10,98,114,101,97,107,34])))) :-
		parse(codes([34,108,105,110,101,10,98,114,101,97,107,34]), _Terms).

	test(parse_error_unescaped_control_code, error(domain_error(json_lines, codes([34,1,34])))) :-
		parse(codes([34,1,34]), _Terms).

	test(parse_error_negative_leading_zero, error(domain_error(json_lines, codes([45,48,49])))) :-
		parse(codes([45,48,49]), _Terms).

	test(parse_error_negative_missing_integer_digit, error(domain_error(json_lines, codes([45,46,49])))) :-
		parse(codes([45,46,49]), _Terms).

	test(parse_error_missing_fraction_digit, error(domain_error(json_lines, codes([49,46])))) :-
		parse(codes([49,46]), _Terms).

	test(parse_error_missing_exponent_digit, error(domain_error(json_lines, codes([49,101])))) :-
		parse(codes([49,101]), _Terms).

	test(parse_error_minus_only, error(domain_error(json_lines, codes([45])))) :-
		parse(codes([45]), _Terms).

	test(parse_error_blank_line_file, error(domain_error(json_lines, file(_)))) :-
		^^file_path('test_files/output01.jsonl', Path),
		open(Path, write, Stream),
		nl(Stream),
		close(Stream),
		parse(file(Path), _Terms).

	test(parse_skip_ws, deterministic(Terms == [{a-b}])) :-
		atom_codes('\r\t {"a":"b"}\n', Codes),
		parse(codes(Codes), Terms).

	test(parse_no_final_end_of_line, deterministic(Terms == [{a-b}, {c-d}])) :-
		atom_codes('{"a":"b"}\n{"c":"d"}', Codes),
		parse(codes(Codes), Terms).

	test(parse_arrays, deterministic) :-
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

	test(parse_objects, deterministic) :-
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

	test(parse_no_final_newline, deterministic(Terms == [1,2,3])) :-
		^^file_path('test_files/no_final_newline.jsonl', Path),
		parse(file(Path), Terms).

	test(parse_stream_does_not_close, deterministic(Terms == [1])) :-
		^^set_text_input(in, '1\n'),
		parse(stream(in), Terms),
		close(in).

	test(parse_line, deterministic(Term1-Term2 == {a-0}-{b-[1,2,3]})) :-
		^^set_text_input(in, '{"a":0}\n{"b":[1,2,3]}\n'),
		parse(line(in), [Term1]),
		parse(line(in), [Term2]),
		close(in).

	test(parse_chars, deterministic(Terms == [{a-b}])) :-
		atom_chars('{"a":"b"}', Chars),
		parse(chars(Chars), Terms).

	test(parse_valid_escape, deterministic) :-
		parse(codes([34,92,116,32,32,92,114,32,32,92,110,32,92,102,32,92,98,32,92,47,32,92,92,32,92,34,32,32,34]), _Terms).

	test(parse_unicode_surrogate_pair, deterministic(Terms == [codes([119070])])) :-
		json_lines(codes)::parse(codes([34,92,117,68,56,51,52,92,117,68,68,49,69,34]), Terms).

	test(parse_error_unicode_isolated_high_surrogate, error(domain_error(json_lines, codes([34,92,117,68,56,51,52,34])))) :-
		json_lines(codes)::parse(codes([34,92,117,68,56,51,52,34]), _Terms).

	test(parse_error_unicode_isolated_low_surrogate, error(domain_error(json_lines, codes([34,92,117,68,68,49,69,34])))) :-
		json_lines(codes)::parse(codes([34,92,117,68,68,49,69,34]), _Terms).

	test(parse_object_curly, deterministic(Terms == [{a-1,b-2,c-3}])) :-
		json_lines(curly,dash,atom)::parse(atom('{"a":1, "b":2, "c":3}'), Terms).

	test(parse_object_list, deterministic(Terms == [json([a-1,b-2,c-3])])) :-
		json_lines(list,dash,atom)::parse(atom('{"a":1, "b":2, "c":3}'), Terms).

	test(parse_pair_equal, deterministic(Terms == [json([a=1,b=2,c=3])])) :-
		json_lines(list,equal,atom)::parse(atom('{"a":1, "b":2, "c":3}'), Terms).

	test(parse_pair_colon, deterministic(Terms == [json([':'(a,1),':'(b,2),':'(c,3)])])) :-
		json_lines(list,colon,atom)::parse(atom('{"a":1, "b":2, "c":3}'), Terms).

	test(generate_chars, deterministic(Terms == [{a-b}])) :-
		generate(chars(Chars), [{a-b}]),
		parse(chars(Chars), Terms).

	test(generate_file, deterministic(Terms == [{a-b}])) :-
		^^file_path('test_files/output01.jsonl', Path),
		generate(file(Path), [{a-b}]),
		parse(file(Path), Terms).

	test(generate_error_invalid_sink, error(domain_error(json_lines_sink, line(_)))) :-
		generate(line(_), []).

	test(generate_error_non_list, error(type_error(list, foo))) :-
		generate(codes(_), foo).

	test(generate_error_non_ground_term, error(instantiation_error)) :-
		generate(codes(_), [foo(_)]).

	test(generate_control_character, deterministic(Codes == [34,92,117,48,48,48,49,34,10])) :-
		generate(codes(Codes), [codes([1])]).

	test(generate_backslash_u_literal_roundtrip, deterministic(Terms == [codes([92,117,48,48,52,49])])) :-
		generate(codes(Codes), [codes([92,117,48,48,52,49])]),
		json_lines(codes)::parse(codes(Codes), Terms).

	test(parse_generate_atom, deterministic(Atom == '[1,2,{"a":"b"}]\n')) :-
		parse(atom('[1,2,{"a":"b"}]'), Terms),
		generate(atom(Atom), Terms).

	test(encode_pair_string_number, deterministic(Atom == '{"a":1}\n')) :-
		generate(atom(Atom), [{a-1}]).

	test(encode_pair_dash_string_object, deterministic(Atom == '{"a":{"b":"c"}}\n')) :-
		generate(atom(Atom), [{a-{b-c}}]).

	test(encode_pair_equal_string_object, deterministic(Atom == '{"a":{"b":"c"}}\n')) :-
		generate(atom(Atom), [{a={b=c}}]).

	test(encode_pair_colon_string_object, deterministic(Atom == '{"a":{"b":"c"}}\n')) :-
		generate(atom(Atom), [{':'(a,{':'(b,c)})}]).

	test(roundtrip_object_representation, deterministic(Terms-Chars == [json([])]-['{','}','\n'])) :-
		json_lines(list,dash,atom)::(parse(chars(['{','}']), Terms), generate(chars(Chars), Terms)).

	test(roundtrip_string_representation, deterministic(Terms-Chars == [chars(['{','}'])]-['"','{','}','"','\n'])) :-
		json_lines(curly,dash,chars)::(parse(chars(['"','{','}','"']), Terms), generate(chars(Chars), Terms)).

	test(atom_parse_codes, deterministic(Terms == [hello])) :-
		json_lines(atom)::parse(codes([34,104,101,108,108,111,34]), Terms).

	test(chars_parse_codes, deterministic(Terms == [chars([h,e,l,l,o])])) :-
		json_lines(chars)::parse(codes([34,104,101,108,108,111,34]), Terms).

	test(codes_parse_codes, deterministic(Terms == [codes([104,101,108,108,111])])) :-
		json_lines(codes)::parse(codes([34,104,101,108,108,111,34]), Terms).

	test(json_generate_error_object_list_representation, error(domain_error(json_term, {a-b}))) :-
		json_lines(list,dash,atom)::generate(chars(_), [{a-b}]).

	test(json_generate_error_object_curly_representation, error(domain_error(json_term, json([a-b])))) :-
		json_lines(curly,dash,atom)::generate(chars(_), [json([a-b])]).

:- end_object.
