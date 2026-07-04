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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-07-04,
		comment is 'Tests for the "json_pointer" library.'
	]).

	:- uses(json_pointer, [
		parse/2, generate/2, parse_fragment/2, generate_fragment/2, parse_relative/2, generate_relative/2, evaluate/3, evaluate_relative/4
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	cover(json_pointer(_)).
	cover(json_pointer).

	test(parse_empty_pointer, deterministic(Pointer == [])) :-
		parse(atom(''), Pointer).

	test(parse_error_var_source, error(instantiation_error)) :-
		parse(_, _).

	test(parse_error_invalid_source, error(domain_error(json_pointer_source, foo))) :-
		parse(foo, _).

	test(parse_chars_source, deterministic(Pointer == [foo])) :-
		parse(chars(['/', f, o, o]), Pointer).

	test(parse_codes_source, deterministic(Pointer == [foo])) :-
		parse(codes([0'/, 0'f, 0'o, 0'o]), Pointer).

	test(parse_root_member, deterministic(Pointer == [foo])) :-
		parse(atom('/foo'), Pointer).

	test(parse_empty_reference_token, deterministic(Pointer == [''])) :-
		parse(atom('/'), Pointer).

	test(parse_escaped_reference_token, deterministic(Pointer == ['a/b', 'm~n'])) :-
		parse(atom('/a~1b/m~0n'), Pointer).

	test(parse_invalid_pointer, fail) :-
		parse(atom('foo'), _).

	test(parse_invalid_escape, fail) :-
		parse(atom('/a~2b'), _).

	test(parse_relative_current_value, deterministic(RelativePointer == relative(0, 0, []))) :-
		parse_relative(atom('0'), RelativePointer).

	test(parse_relative_descend, deterministic(RelativePointer == relative(1, 0, [foo, bar]))) :-
		parse_relative(atom('1/foo/bar'), RelativePointer).

	test(parse_relative_hash, deterministic(RelativePointer == relative(0, 0, '#'))) :-
		parse_relative(atom('0#'), RelativePointer).

	test(parse_relative_shift, deterministic(RelativePointer == relative(0, -1, []))) :-
		parse_relative(atom('0-1'), RelativePointer).

	test(parse_relative_error_var_source, error(instantiation_error)) :-
		parse_relative(_, _).

	test(parse_relative_error_invalid_source, error(domain_error(json_relative_pointer_source, foo))) :-
		parse_relative(foo, _).

	test(parse_relative_invalid_syntax, fail) :-
		parse_relative(atom('/foo'), _).

	test(generate_empty_pointer, deterministic(Atom == '')) :-
		generate(atom(Atom), []).

	test(generate_error_var_sink, error(instantiation_error)) :-
		generate(_, []).

	test(generate_error_invalid_sink, error(domain_error(json_pointer_sink, foo))) :-
		generate(foo, []).

	test(generate_pointer, deterministic(Atom == '/a~1b/m~0n')) :-
		generate(atom(Atom), ['a/b', 'm~n']).

	test(generate_chars_sink, deterministic(Chars == ['/', a, '~', '1', b])) :-
		generate(chars(Chars), ['a/b']).

	test(generate_codes_sink, deterministic(Codes == [0'/, 0'a, 0'~, 0'1, 0'b])) :-
		generate(codes(Codes), ['a/b']).

	test(generate_error_invalid_pointer, error(type_error(list, foo))) :-
		generate(atom(_), foo).

	test(generate_error_improper_pointer, error(type_error(list, bar))) :-
		generate(atom(_), [foo| bar]).

	test(generate_relative_current_value, deterministic(Atom == '0')) :-
		generate_relative(atom(Atom), relative(0, 0, [])).

	test(generate_relative_descend, deterministic(Atom == '2/highly/nested/objects')) :-
		generate_relative(atom(Atom), relative(2, 0, [highly, nested, objects])).

	test(generate_relative_hash, deterministic(Atom == '0#')) :-
		generate_relative(atom(Atom), relative(0, 0, '#')).

	test(generate_relative_shift, deterministic(Atom == '0-1/child')) :-
		generate_relative(atom(Atom), relative(0, -1, [child])).

	test(generate_relative_error_var_sink, error(instantiation_error)) :-
		generate_relative(_, relative(0, 0, [])).

	test(generate_relative_error_invalid_sink, error(domain_error(json_relative_pointer_sink, foo))) :-
		generate_relative(foo, relative(0, 0, [])).

	test(generate_relative_error_invalid_pointer, error(domain_error(json_relative_pointer, foo))) :-
		generate_relative(atom(_), foo).

	test(generate_chars_representation, deterministic(Atom == '/foo')) :-
		json_pointer(chars)::generate(atom(Atom), [chars([f, o, o])]).

	test(generate_codes_representation, deterministic(Atom == '/foo')) :-
		json_pointer(codes)::generate(atom(Atom), [codes([0'f, 0'o, 0'o])]).

	test(parse_fragment_root, deterministic(Pointer == [])) :-
		parse_fragment(atom('#'), Pointer).

	test(parse_fragment_error_var_source, error(instantiation_error)) :-
		parse_fragment(_, _).

	test(parse_fragment_error_invalid_source, error(domain_error(json_pointer_fragment_source, foo))) :-
		parse_fragment(foo, _).

	test(parse_fragment_chars_source, deterministic(Pointer == [foo])) :-
		parse_fragment(chars(['#', '/', f, o, o]), Pointer).

	test(parse_fragment_codes_source, deterministic(Pointer == [foo])) :-
		parse_fragment(codes([0'#, 0'/, 0'f, 0'o, 0'o]), Pointer).

	test(parse_fragment_invalid_not_fragment, fail) :-
		parse_fragment(atom('/foo'), _).

	test(parse_fragment_invalid_escape, fail) :-
		parse_fragment(atom('#/%'), _).

	test(generate_fragment_root, deterministic(Atom == '#')) :-
		generate_fragment(atom(Atom), []).

	test(parse_fragment_pointer, deterministic(Pointer == ['a b', 'c/d'])) :-
		parse_fragment(atom('#/a%20b/c~1d'), Pointer).

	test(parse_fragment_percent_encoded_tilde, deterministic(Pointer == ['~'])) :-
		parse_fragment(atom('#/%7E0'), Pointer).

	test(parse_fragment_literal_tilde_escape, deterministic(Pointer == ['~'])) :-
		atom_codes(Fragment, [0'#, 0'/, 0'~, 0'0]),
		parse_fragment(atom(Fragment), Pointer).

	test(parse_fragment_lowercase_hex, deterministic(Pointer == [a, b])) :-
		parse_fragment(atom('#/a%2fb'), Pointer).

	test(generate_fragment_pointer, deterministic(Atom == '#/a%20b/c~1d')) :-
		generate_fragment(atom(Atom), ['a b', 'c/d']).

	test(generate_fragment_error_var_sink, error(instantiation_error)) :-
		generate_fragment(_, []).

	test(generate_fragment_error_invalid_sink, error(domain_error(json_pointer_fragment_sink, foo))) :-
		generate_fragment(foo, []).

	test(generate_fragment_percent_encoded_backslash, deterministic(Atom == '#/%5C')) :-
		Token = ('\\'),
		generate_fragment(atom(Atom), [Token]).

	test(generate_fragment_chars_sink, deterministic(Chars == ['#', '/', a])) :-
		generate_fragment(chars(Chars), [a]).

	test(generate_fragment_codes_sink, deterministic(Codes == [0'#, 0'/, 0'!, 0'$, 0'&, 0'\', 0'(, 0'), 0'*, 0'+, 0',, 0';, 0'=, 0':, 0'@, 0'?, 0'-, 0'., 0'_, 0'~, 0'0])) :-
		Token = '!$&''()*+,;=:@?-._~',
		generate_fragment(codes(Codes), [Token]).

	test(parse_chars_representation, deterministic(Pointer == [chars([f,o,o]), chars([/])])) :-
		json_pointer(chars)::parse(atom('/foo/~1'), Pointer).

	test(parse_codes_representation, deterministic(Pointer == [codes([102,111,111])])) :-
		json_pointer(codes)::parse(atom('/foo'), Pointer).

	test(parse_relative_chars_representation, deterministic(RelativePointer == relative(1, 0, [chars([f,o,o])]))) :-
		json_pointer(chars)::parse_relative(atom('1/foo'), RelativePointer).

	test(evaluate_root, deterministic(Value == {foo-bar})) :-
		evaluate([], {foo-bar}, Value).

	test(evaluate_error_var_pointer, error(instantiation_error)) :-
		evaluate(_, {}, _).

	test(evaluate_error_var_json, error(instantiation_error)) :-
		evaluate([], _, _).

	test(evaluate_error_invalid_pointer, error(type_error(list, foo))) :-
		evaluate(foo, {}, _).

	test(evaluate_object_member_curly, deterministic(Value == bar)) :-
		evaluate([foo], {foo-bar}, Value).

	test(evaluate_empty_object, fail) :-
		evaluate([foo], {}, _).

	test(evaluate_object_member_list, deterministic(Value == 1)) :-
		evaluate([foo], json([foo-1]), Value).

	test(evaluate_object_member_list_equal_pair, deterministic(Value == 1)) :-
		evaluate([foo], json([foo=1]), Value).

	test(evaluate_object_member_list_colon_pair, deterministic(Value == 2)) :-
		evaluate([foo], json([':'(foo, 2)]), Value).

	test(evaluate_nested_object, deterministic(Value == baz)) :-
		evaluate([foo, bar], {foo-{bar-baz}}, Value).

	test(evaluate_array_element, deterministic(Value == second)) :-
		evaluate([items, '1'], {items-[first, second, third]}, Value).

	test(evaluate_multi_digit_array_index, deterministic(Value == m)) :-
		evaluate([items, '12'], {items-[a, b, c, d, e, f, g, h, i, j, k, l, m]}, Value).

	test(evaluate_zero_array_index, deterministic(Value == first)) :-
		evaluate([items, '0'], {items-[first, second]}, Value).

	test(evaluate_array_index_with_leading_zero, fail) :-
		evaluate([items, '01'], {items-[first, second]}, _).

	test(evaluate_dash_array_index, fail) :-
		evaluate([items, '-'], {items-[first, second]}, _).

	test(evaluate_non_json_scalar, fail) :-
		evaluate([foo], 42, _).

	test(evaluate_missing_member, fail) :-
		evaluate([missing], {foo-bar}, _).

	test(evaluate_duplicate_member_fails, fail) :-
		evaluate([foo], {foo-1, foo-2}, _).

	test(evaluate_empty_member_name, deterministic(Value == 0)) :-
		evaluate([''], {''-0}, Value).

	test(evaluate_rfc_example_foo, deterministic(Value == [bar, baz])) :-
		evaluate([foo], {foo-[bar, baz], ''-0, 'a/b'-1, 'c%d'-2, 'e^f'-3, 'g|h'-4, 'i\\j'-5, 'k"l'-6, ' '-7, 'm~n'-8}, Value).

	test(evaluate_rfc_example_foo_0, deterministic(Value == bar)) :-
		evaluate([foo, '0'], {foo-[bar, baz], ''-0, 'a/b'-1, 'c%d'-2, 'e^f'-3, 'g|h'-4, 'i\\j'-5, 'k"l'-6, ' '-7, 'm~n'-8}, Value).

	test(evaluate_rfc_example_slash, deterministic(Value == 1)) :-
		evaluate(['a/b'], {foo-[bar, baz], ''-0, 'a/b'-1, 'c%d'-2, 'e^f'-3, 'g|h'-4, 'i\\j'-5, 'k"l'-6, ' '-7, 'm~n'-8}, Value).

	test(evaluate_rfc_example_percent, deterministic(Value == 2)) :-
		evaluate(['c%d'], {foo-[bar, baz], ''-0, 'a/b'-1, 'c%d'-2, 'e^f'-3, 'g|h'-4, 'i\\j'-5, 'k"l'-6, ' '-7, 'm~n'-8}, Value).

	test(evaluate_rfc_example_caret, deterministic(Value == 3)) :-
		evaluate(['e^f'], {foo-[bar, baz], ''-0, 'a/b'-1, 'c%d'-2, 'e^f'-3, 'g|h'-4, 'i\\j'-5, 'k"l'-6, ' '-7, 'm~n'-8}, Value).

	test(evaluate_rfc_example_pipe, deterministic(Value == 4)) :-
		evaluate(['g|h'], {foo-[bar, baz], ''-0, 'a/b'-1, 'c%d'-2, 'e^f'-3, 'g|h'-4, 'i\\j'-5, 'k"l'-6, ' '-7, 'm~n'-8}, Value).

	test(evaluate_rfc_example_backslash, deterministic(Value == 5)) :-
		evaluate(['i\\j'], {foo-[bar, baz], ''-0, 'a/b'-1, 'c%d'-2, 'e^f'-3, 'g|h'-4, 'i\\j'-5, 'k"l'-6, ' '-7, 'm~n'-8}, Value).

	test(evaluate_rfc_example_quote, deterministic(Value == 6)) :-
		evaluate(['k"l'], {foo-[bar, baz], ''-0, 'a/b'-1, 'c%d'-2, 'e^f'-3, 'g|h'-4, 'i\\j'-5, 'k"l'-6, ' '-7, 'm~n'-8}, Value).

	test(evaluate_rfc_example_space, deterministic(Value == 7)) :-
		evaluate([' '], {foo-[bar, baz], ''-0, 'a/b'-1, 'c%d'-2, 'e^f'-3, 'g|h'-4, 'i\\j'-5, 'k"l'-6, ' '-7, 'm~n'-8}, Value).

	test(evaluate_rfc_example_tilde, deterministic(Value == 8)) :-
		evaluate(['m~n'], {foo-[bar, baz], ''-0, 'a/b'-1, 'c%d'-2, 'e^f'-3, 'g|h'-4, 'i\\j'-5, 'k"l'-6, ' '-7, 'm~n'-8}, Value).

	test(evaluate_chars_pointer, deterministic(Value == chars([v,a,l,u,e]))) :-
		json_pointer(chars)::evaluate([chars([f,o,o])], json([chars([f,o,o])-chars([v,a,l,u,e])]), Value).

	test(evaluate_relative_current_value, deterministic(Value == baz)) :-
		evaluate_relative(relative(0, 0, []), [foo, '1'], {foo-[bar, baz], highly-{nested-{objects-true}}}, Value).

	test(evaluate_relative_up_and_descend, deterministic(Value == bar)) :-
		evaluate_relative(relative(1, 0, ['0']), [foo, '1'], {foo-[bar, baz], highly-{nested-{objects-true}}}, Value).

	test(evaluate_relative_shift, deterministic(Value == bar)) :-
		evaluate_relative(relative(0, -1, []), [foo, '1'], {foo-[bar, baz], highly-{nested-{objects-true}}}, Value).

	test(evaluate_relative_hash_array_index, deterministic(Value == 1)) :-
		evaluate_relative(relative(0, 0, '#'), [foo, '1'], {foo-[bar, baz], highly-{nested-{objects-true}}}, Value).

	test(evaluate_relative_hash_shifted_array_index, deterministic(Value == 0)) :-
		evaluate_relative(relative(0, -1, '#'), [foo, '1'], {foo-[bar, baz], highly-{nested-{objects-true}}}, Value).

	test(evaluate_relative_hash_object_member, deterministic(Value == nested)) :-
		evaluate_relative(relative(0, 0, '#'), [highly, nested], {foo-[bar, baz], highly-{nested-{objects-true}}}, Value).

	test(evaluate_relative_from_nested_object, deterministic(Value == true)) :-
		evaluate_relative(relative(0, 0, [objects]), [highly, nested], {foo-[bar, baz], highly-{nested-{objects-true}}}, Value).

	test(evaluate_relative_error_var_pointer, error(instantiation_error)) :-
		evaluate_relative(_, [], {}, _).

	test(evaluate_relative_error_var_context, error(instantiation_error)) :-
		evaluate_relative(relative(0, 0, []), _, {}, _).

	test(evaluate_relative_error_var_json, error(instantiation_error)) :-
		evaluate_relative(relative(0, 0, []), [], _, _).

	test(evaluate_relative_error_invalid_context, error(type_error(list, foo))) :-
		evaluate_relative(relative(0, 0, []), foo, {}, _).

	test(evaluate_relative_error_invalid_pointer, error(domain_error(json_relative_pointer, foo))) :-
		evaluate_relative(foo, [], {}, _).

	test(evaluate_relative_up_past_root, fail) :-
		evaluate_relative(relative(3, 0, []), [foo, '1'], {foo-[bar, baz]}, _).

	test(evaluate_relative_shift_on_object_member, fail) :-
		evaluate_relative(relative(0, 1, []), [highly, nested], {highly-{nested-{objects-true}}}, _).

	test(evaluate_relative_shift_on_numeric_object_member, fail) :-
		evaluate_relative(relative(0, 1, []), ['0'], {'0'-foo, '1'-bar}, _).

	test(evaluate_relative_chars_hash_member, deterministic(Value == chars([f,o,o]))) :-
		json_pointer(chars)::evaluate_relative(relative(0, 0, '#'), [chars([f,o,o])], json([chars([f,o,o])-1]), Value).

:- end_object.
