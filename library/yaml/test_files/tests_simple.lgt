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


:- object(tests_simple,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-01-25,
		comment is 'Tests for the "yaml" library.'
	]).

	:- uses(yaml, [
		parse/2, generate/2, parse_all/2, generate_all/2
	]).

	:- uses(list, [
		member/2
	]).

	:- uses(lgtunit, [
		assertion/1, assertion/2
	]).

	cover(yaml).

	setup :-
		^^clean_file('simple/output01.yaml').

	cleanup :-
		setup,
		^^clean_text_input.

	test(parse_simple_codes, true(YAML == yaml([name-john, age-30, city-'New York']))) :-
		atom_codes('name: john\nage: 30\ncity: New York', Codes),
		parse(codes(Codes), YAML).

	test(parse_simple_atom, true(YAML == yaml([key-value]))) :-
		parse(atom('{key: value}'), YAML).

	test(parse_list_values, true(YAML == [apple, banana, orange])) :-
		atom_codes('- apple\n- banana\n- orange', Codes),
		parse(codes(Codes), YAML).

	test(parse_null_value, true(YAML == '@'(null))) :-
		atom_codes(null, Codes),
		parse(codes(Codes), YAML).

	test(parse_boolean_true, true(YAML == '@'(true))) :-
		atom_codes(true, Codes),
		parse(codes(Codes), YAML).

	test(parse_boolean_false, true(YAML == '@'(false))) :-
		atom_codes(false, Codes),
		parse(codes(Codes), YAML).

	test(parse_number_integer, true(YAML == 42)) :-
		parse(codes([52, 50]), YAML).

	test(parse_number_float, true(YAML == 3.14)) :-
		parse(atom('3.14'), YAML).

	test(parse_single_quoted_string, true(YAML == 'hello world')) :-
		atom_codes('\'hello world\'', Codes),
		parse(codes(Codes), YAML).

	test(parse_double_quoted_string, true(YAML == 'hello world')) :-
		atom_codes('"hello world"', Codes),
		parse(codes(Codes), YAML).

	test(parse_flow_mapping, true(YAML == yaml([name-'Alice', age-28]))) :-
		atom_codes('{name: Alice, age: 28}', Codes),
		parse(codes(Codes), YAML).

	test(parse_flow_sequence, true(YAML == [1, 2, 3, 4])) :-
		atom_codes('[1, 2, 3, 4]', Codes),
		parse(codes(Codes), YAML).

	test(parse_nested_structure, true(YAML == yaml([
		person-yaml([
			name-'Alice',
			age-28,
			contact-yaml([
				email-'alice@example.com',
				phone-'555-1234'
			])
		])
	]))) :-
		^^file_path('simple/nested.yaml', Path),
		parse(file(Path), YAML).

	test(parse_complex_structure, true(YAML == yaml([
		students-[
			yaml([name-'Bob', grade-'A']),
			yaml([name-'Charlie', grade-'B']),
			yaml([name-'Diana', grade-'A'])
		]
	]))) :-
		^^file_path('simple/complex.yaml', Path),
		parse(file(Path), YAML).

	test(generate_simple_yaml, true(atom(Result))) :-
		YAML = yaml([name-'John', age-30]),
		generate(codes(Codes), YAML),
		atom_codes(Result, Codes).

	test(generate_list_yaml, true(atom(Result))) :-
		YAML = [apple, banana, orange],
		generate(codes(Codes), YAML),
		atom_codes(Result, Codes).

	test(generate_null, true(Result == null)) :-
		YAML = '@'(null),
		generate(codes(Codes), YAML),
		atom_codes(Result, Codes).

	test(generate_boolean_true, true(Result == true)) :-
		YAML = '@'(true),
		generate(codes(Codes), YAML),
		atom_codes(Result, Codes).

	test(generate_flow_mapping, true(atom(Result))) :-
		YAML = yaml([x-10, y-20]),
		generate(codes(Codes), YAML),
		atom_codes(Result, Codes).

	test(generate_flow_sequence, true(atom(Result))) :-
		YAML = [1, 2, 3],
		generate(codes(Codes), YAML),
		atom_codes(Result, Codes).

	test(parse_chars, true(YAML == yaml([key-value]))) :-
		atom_chars('{"key": "value"}', Chars),
		parse(chars(Chars), YAML).

	test(parse_atom, true(YAML == yaml([status-active]))) :-
		parse(atom('{"status": "active"}'), YAML).

	test(round_trip_simple, true(Parsed == Original)) :-
		Original = yaml([title-'Test', count-5]),
		generate(codes(Codes), Original),
		parse(codes(Codes), Parsed).

	test(round_trip_list, true(Parsed == Original)) :-
		Original = [item1, item2, item3],
		generate(codes(Codes), Original),
		parse(codes(Codes), Parsed).

	test(parse_with_comments, true(YAML == yaml([name-value]))) :-
		atom_codes('# This is a comment\nname: value', Codes),
		parse(codes(Codes), YAML).

	test(parse_empty_mapping, true(YAML == yaml([]))) :-
		parse(codes([0'{, 0'}]), YAML).

	test(parse_empty_sequence, true(YAML == [])) :-
		parse(codes([0'[, 0']]), YAML).

	test(parse_file_simple, true(YAML == yaml([name-'John Doe', age-30, city-'New York']))) :-
		^^file_path('simple/simple.yaml', Path),
		parse(file(Path), YAML).

	test(parse_file_list, true(YAML == [apple, banana, orange, grape])) :-
		^^file_path('simple/list.yaml', Path),
		parse(file(Path), YAML).

	test(generate_file, true(os::file_exists(Path))) :-
		YAML = yaml([test-value, number-42]),
		^^file_path('simple/output01.yaml', Path),
		generate(file(Path), YAML).

	test(instantiation_error_parse_var, error(instantiation_error)) :-
		parse(_, _).

	test(instantiation_error_generate_var_sink, error(instantiation_error)) :-
		generate(_, yaml(data)).

	test(instantiation_error_generate_var_yaml, error(instantiation_error)) :-
		generate(codes(_), _).

	test(domain_error_parse_invalid_source, error(domain_error(yaml_source, invalid_source))) :-
		parse(invalid_source, _).

	test(domain_error_generate_invalid_sink, error(domain_error(yaml_sink, invalid_sink))) :-
		generate(invalid_sink, yaml([data-value])).

	% Multi-document parsing tests

	test(parse_all_single_document, true(YAML == [yaml([a-1])])) :-
		parse_all(atom('a: 1'), YAML).

	test(parse_all_two_documents_with_separator, true(YAML == [yaml([a-1]), yaml([b-2])])) :-
		parse_all(atom('---\na: 1\n---\nb: 2'), YAML).

	test(parse_all_two_documents_with_end_marker, true(YAML == [yaml([a-1]), yaml([b-2])])) :-
		parse_all(atom('a: 1\n...\nb: 2'), YAML).

	test(parse_all_two_documents_with_both_markers, true(YAML == [yaml([a-1]), yaml([b-2])])) :-
		parse_all(atom('---\na: 1\n...\n---\nb: 2\n...'), YAML).

	test(parse_all_sequences, true(YAML == [[item1, item2], [item3, item4]])) :-
		parse_all(atom('---\n- item1\n- item2\n---\n- item3\n- item4'), YAML).

	% Multi-document generation tests

	test(generate_all_single_document, true(Atom == '{a:1}')) :-
		generate_all(atom(Atom), [yaml([a-1])]).

	test(generate_all_two_documents, true) :-
		generate_all(atom(Atom), [yaml([a-1]), yaml([b-2])]),
		atom_codes(Atom, Codes),
		atom_codes('---\n{a:1}\n---\n{b:2}\n', Expected),
		Codes == Expected.

	test(generate_all_empty_list, true(Atom == '')) :-
		generate_all(atom(Atom), []).

	% Round-trip tests for multi-document

	test(parse_all_generate_all_round_trip, true) :-
		Original = [yaml([a-1, b-2]), yaml([c-3, d-4])],
		generate_all(atom(Atom), Original),
		parse_all(atom(Atom), Parsed),
		Original == Parsed.

	% Anchor and alias tests

	test(parse_simple_anchor_alias, true(YAML == yaml([a-test, b-test]))) :-
		parse(atom('a: &myanchor test\nb: *myanchor\n'), YAML).

	test(parse_nested_anchor_alias, true(YAML == yaml([source-yaml([x-1, y-2]), copy-yaml([x-1, y-2])]))) :-
		parse(atom('source: &data\n  x: 1\n  y: 2\ncopy: *data\n'), YAML).

	test(parse_anchor_on_sequence, true(YAML == yaml([items-[a, b, c], more-[a, b, c]]))) :-
		parse(atom('items: &list\n  - a\n  - b\n  - c\nmore: *list\n'), YAML).

	% Merge key tests

	test(parse_simple_merge_key, true(YAML == yaml([defaults-yaml([timeout-30, retries-3]), server-yaml([timeout-30, retries-3, host-localhost])]))) :-
		parse(atom('defaults: &defaults\n  timeout: 30\n  retries: 3\nserver:\n  <<: *defaults\n  host: localhost\n'), YAML).

	test(parse_merge_key_override, true) :-
		parse(atom('defaults: &defaults\n  timeout: 30\nserver:\n  <<: *defaults\n  timeout: 60\n'), YAML),
		YAML = yaml([defaults-_, server-yaml(ServerPairs)]),
		% Last occurrence wins in YAML merge semantics
		member(timeout-60, ServerPairs).

:- end_object.
