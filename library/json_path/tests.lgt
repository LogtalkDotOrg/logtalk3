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
		date is 2026-07-03,
		comment is 'Tests for the "json_path" library initial implementation.'
	]).

	:- uses(json_path, [
		parse/2, generate/2, evaluate/3, paths/3, nodes/3, query/3
	]).

	cover(json_path(_)).
	cover(json_path).

	test(parse_root, deterministic(Query == json_path([]))) :-
		parse(atom('$'), Query).

	test(parse_dot_name, deterministic(Query == json_path([child([name(store)]), child([name(book)]), child([index(0)]), child([name(title)])]))) :-
		parse(atom('$.store.book[0].title'), Query).

	test(parse_descendant_wildcard, deterministic(Query == json_path([descendant([wildcard])])) ) :-
		parse(atom('$..*'), Query).

	test(parse_slice, deterministic(Query == json_path([child([slice(1,5,2)])]))) :-
		parse(atom('$[1:5:2]'), Query).

	test(parse_filter_compare, deterministic(Query == json_path([child([filter(compare(lt, path(path(current, [child([name(price)])])), literal(10)))])]))) :-
		parse(atom('$[?@.price < 10]'), Query).

	test(parse_chars_representation, deterministic(Query == json_path([child([name(chars([s,t,o,r,e]))])])) ) :-
		json_path(chars)::parse(atom('$.store'), Query).

	test(parse_codes_representation, deterministic(Query == json_path([child([name(codes([115,116,111,114,101]))])])) ) :-
		json_path(codes)::parse(atom('$.store'), Query).

	test(parse_chars_source, deterministic(Query == json_path([child([name(store)])]))) :-
		parse(chars(['$', '.', s, t, o, r, e]), Query).

	test(parse_codes_source, deterministic(Query == json_path([child([name(store)])]))) :-
		parse(codes([36, 46, 115, 116, 111, 114, 101]), Query).

	test(parse_instantiation_error, error(instantiation_error)) :-
		parse(_, _).

	test(parse_invalid_source_error, error(domain_error(json_path_source, 42))) :-
		parse(42, _).

	test(generate_root, deterministic(Atom == '$')) :-
		Query = json_path([]),
		generate(atom(Atom), Query).

	test(generate_boolean_and_null_comparisons, deterministic(Atom == '$[?true != false || null <= null || 3 > 2 && 1 < 2]')) :-
		Query = json_path([child([filter(or(
			or(
				compare(ne, literal(@true), literal(@false)),
				compare(le, literal(@null), literal(@null))
			),
			and(
				compare(gt, literal(3), literal(2)),
				compare(lt, literal(1), literal(2))
			)
		))])]),
		generate(atom(Atom), Query).

	test(generate_chars_representation_sink, deterministic(Atom == '$[''store'']')) :-
		json_path(chars)::generate(atom(Atom), json_path([child([name(chars([s,t,o,r,e]))])])).

	test(generate_codes_representation_sink, deterministic(Atom == '$[''store'']')) :-
		json_path(codes)::generate(atom(Atom), json_path([child([name(codes([115,116,111,114,101]))])])).

	test(generate_escaped_name, deterministic(Codes == [36, 91, 39, 92, 92, 92, 39, 92, 98, 92, 116, 92, 110, 92, 102, 92, 114, 92, 117, 48, 48, 48, 98, 97, 39, 93])) :-
		atom_codes(Name, [92, 39, 8, 9, 10, 12, 13, 11, 97]),
		generate(codes(Codes), json_path([child([name(Name)])])).

	test(generate_instantiation_error, error(instantiation_error)) :-
		generate(_, json_path([])).

	test(generate_invalid_sink_error, error(domain_error(json_path_sink, foo))) :-
		generate(foo, json_path([])).

	test(generate_invalid_query_error, error(domain_error(json_path_query, foo))) :-
		generate(atom(_), foo).

	test(evaluate_root, deterministic(Values == [{store-{book-[{title-'Sayings of the Century'}, {title-'Sword of Honour'}]}}])) :-
		JSON = {store-{book-[{title-'Sayings of the Century'}, {title-'Sword of Honour'}]}},
		evaluate(json_path([]), JSON, Values).

	test(evaluate_name_index_name, deterministic(Values == ['Sayings of the Century'])) :-
		JSON = {store-{book-[{title-'Sayings of the Century'}, {title-'Sword of Honour'}]}},
		parse(atom('$.store.book[0].title'), Query),
		evaluate(Query, JSON, Values).

	test(evaluate_negative_index, deterministic(Values == [third])) :-
		JSON = [first, second, third],
		parse(atom('$[-1]'), Query),
		evaluate(Query, JSON, Values).

	test(evaluate_wildcard_array, deterministic(Values == [a, b, c])) :-
		parse(atom('$[*]'), Query),
		evaluate(Query, [a, b, c], Values).

	test(evaluate_slice, deterministic(Values == [b, d])) :-
		parse(atom('$[1:5:2]'), Query),
		evaluate(Query, [a, b, c, d, e, f, g], Values).

	test(evaluate_reverse_slice, deterministic(Values == [g, f, e, d, c, b, a])) :-
		parse(atom('$[::-1]'), Query),
		evaluate(Query, [a, b, c, d, e, f, g], Values).

	test(evaluate_filter_compare, deterministic(Values == ['Sayings of the Century', 'Moby Dick'])) :-
		JSON = {store-{book-[
			{title-'Sayings of the Century', price-8.95},
			{title-'Sword of Honour', price-12.99},
			{title-'Moby Dick', price-8.99}
		]}},
		query(atom('$.store.book[?@.price < 10].title'), JSON, Values).

	test(evaluate_filter_exists, deterministic(Values == [{title-'Moby Dick', isbn-'0-553-21311-3'}])) :-
		JSON = {store-{book-[
			{title-'Sayings of the Century'},
			{title-'Moby Dick', isbn-'0-553-21311-3'}
		]}},
		query(atom('$.store.book[?@.isbn]'), JSON, Values).

	test(evaluate_filter_logical_or, deterministic(Values == [1, {b-k}])) :-
		JSON = [3, 5, 1, 2, 4, 6, {b-j}, {b-k}, {b-{}}, {b-kilo}],
		query(atom('$[?@ < 2 || @.b == ''k'']'), JSON, Values).

	test(evaluate_filter_length_function, deterministic(Values == ['Sayings of the Century', 'Sword of Honour'])) :-
		JSON = {store-{book-[
			{title-'Sayings of the Century'},
			{title-'Sword of Honour'},
			{title-'Moby Dick'}
		]}},
		query(atom('$.store.book[?length(@.title) > 12].title'), JSON, Values).

	test(evaluate_filter_not_and_exists, deterministic(Values == [{keep-1}])) :-
		JSON = [{keep-1}, {keep-1, skip-1}, {skip-1}],
		query(atom('$[?!@.skip && @.keep]'), JSON, Values).

	test(evaluate_filter_root_path_compare, deterministic(Values == [1, 2])) :-
		JSON = {limit-2, items-[{n-1}, {n-2}, {n-3}]},
		query(atom('$.items[?@.n <= $.limit].n'), JSON, Values).

	test(evaluate_filter_missing_values_le, deterministic(Values == [1, 2])) :-
		JSON = {a-1, b-2},
		query(atom('$[?$.missing1 <= $.missing2]'), JSON, Values).

	test(evaluate_filter_count_function, deterministic(Values == [{authors-[a, b]}, {authors-[c, d, e]}])) :-
		JSON = [{authors-[a, b]}, {authors-[solo]}, {authors-[c, d, e]}],
		query(atom('$[?count(@.authors[*]) >= 2]'), JSON, Values).

	test(evaluate_filter_match_function, deterministic(Values == [{b-j}, {b-k}])) :-
		JSON = [{b-j}, {b-k}, {b-kilo}],
		query(atom('$[?match(@.b, ''[jk]'')]'), JSON, Values).

	test(evaluate_filter_search_function, deterministic(Values == [{b-j}, {b-k}, {b-kilo}])) :-
		JSON = [{b-j}, {b-k}, {b-kilo}],
		query(atom('$[?search(@.b, ''[jk]'')]'), JSON, Values).

	test(evaluate_filter_invalid_match_pattern, deterministic(Values == [])) :-
		JSON = [{b-j}, {b-k}],
		query(atom('$[?match(@.b, ''a{'')]'), JSON, Values).

	test(evaluate_filter_invalid_empty_class_pattern, deterministic(Values == [])) :-
		JSON = [{b-a}, {b-b}],
		query(atom('$[?match(@.b, ''[]'')]'), JSON, Values).

	test(evaluate_filter_invalid_nested_range_pattern, deterministic(Values == [])) :-
		JSON = [{b-abbbabbb}, {b-abbb}],
		query(atom('$[?match(@.b, ''(ab{2,3}){2,3}'')]'), JSON, Values).

	:- if((current_logtalk_flag(prolog_dialect, swi); current_logtalk_flag(prolog_dialect, xvm))).

		test(evaluate_filter_property_match_function, deterministic(Values == [{b-'5'}])) :-
			JSON = [{b-'5'}, {b-'A'}, {b-abc}],
			query(atom('$[?match(@.b, ''\\p{Nd}'')]'), JSON, Values).

		test(evaluate_filter_property_search_function, deterministic(Values == [{b-'A5'}, {b-'B7'}])) :-
			JSON = [{b-'A5'}, {b-'B7'}, {b-z}],
			query(atom('$[?search(@.b, ''[A-Z]\\p{Nd}+'')]'), JSON, Values).

		test(evaluate_filter_property_complement_function, deterministic(Values == [{b-'A'}, {b-z}])) :-
			JSON = [{b-'5'}, {b-'A'}, {b-z}],
			query(atom('$[?match(@.b, ''\\P{Nd}'')]'), JSON, Values).

	:- else.

		test(evaluate_filter_property_match_function, deterministic(Values == [])) :-
			JSON = [{b-'5'}, {b-'A'}, {b-abc}],
			query(atom('$[?match(@.b, ''\\p{Nd}'')]'), JSON, Values).

		test(evaluate_filter_property_search_function, deterministic(Values == [])) :-
			JSON = [{b-'A5'}, {b-'B7'}, {b-z}],
			query(atom('$[?search(@.b, ''[A-Z]\\p{Nd}+'')]'), JSON, Values).

		test(evaluate_filter_property_complement_function, deterministic(Values == [])) :-
			JSON = [{b-'5'}, {b-'A'}, {b-z}],
			query(atom('$[?match(@.b, ''\\P{Nd}'')]'), JSON, Values).

	:- endif.

	test(evaluate_filter_value_function, deterministic(Values == [{details-{color-red}}])) :-
		JSON = [{details-{color-red}}, {details-{color-blue}}, {details-{shade-red, accent-crimson}}],
		query(atom('$[?value(@..color) == ''red'']'), JSON, Values).

	test(evaluate_filter_value_multiple_nodes, deterministic(Values == [[a, b]])) :-
		JSON = [[a, b], [c], {x-1}],
		query(atom('$[?value(@[*]) == $.missing]'), JSON, Values).

	test(evaluate_filter_length_array_function, deterministic(Values == [[a, b]])) :-
		JSON = [[a, b], [c], foo],
		query(atom('$[?length(@) == 2]'), JSON, Values).

	test(evaluate_filter_length_object_function, deterministic(Values == [{a-1, b-2}])) :-
		JSON = [{a-1, b-2}, {a-1}, foo],
		query(atom('$[?length(@) == 2]'), JSON, Values).

	test(evaluate_filter_length_nothing_function, deterministic(Values == [{a-1}, {b-2}])) :-
		JSON = [{a-1}, {b-2}],
		query(atom('$[?length(@.missing) == $.also_missing]'), JSON, Values).

	test(evaluate_filter_array_equality, deterministic(Values == [[1, 2], [1, 2]])) :-
		JSON = [[1, 2], [1, 2], [1, 3]],
		query(atom('$[?@ == $[0]]'), JSON, Values).

	test(evaluate_filter_json_list_object_equality, deterministic(Values == [json([a=1, b=2]), json([':'(b, 2), ':'(a, 1)])])) :-
		JSON = [json([a=1, b=2]), json([':'(b, 2), ':'(a, 1)]), json([a=1, b=3])],
		query(atom('$[?@ == $[0]]'), JSON, Values).

	test(evaluate_filter_string_less_than, deterministic(Values == ['', a])) :-
		JSON = ['', a, b, @true],
		query(atom('$[?@ < ''b'']'), JSON, Values).

	test(evaluate_json_list_chars_key, deterministic(Values == [value])) :-
		JSON = json([chars([n, a, m, e])=value]),
		query(atom('$[''name'']'), JSON, Values).

	test(evaluate_json_list_codes_key, deterministic(Values == [value])) :-
		JSON = json([':'(codes([110, 97, 109, 101]), value)]),
		query(atom('$[''name'']'), JSON, Values).

	test(evaluate_descendant_name, deterministic(Values == [1, 4])) :-
		JSON = {o-{j-1, k-2}, a-[5, 3, [{j-4}, {k-6}]]},
		parse(atom('$..j'), Query),
		evaluate(Query, JSON, Values).

	test(paths_descendant_name, deterministic(Paths == ['$[''o''][''j'']', '$[''a''][2][0][''j'']'])) :-
		JSON = {o-{j-1, k-2}, a-[5, 3, [{j-4}, {k-6}]]},
		parse(atom('$..j'), Query),
		paths(Query, JSON, Paths).

	test(nodes_descendant_name, deterministic(Nodes == [node('$[''o''][''j'']', 1), node('$[''a''][2][0][''j'']', 4)])) :-
		JSON = {o-{j-1, k-2}, a-[5, 3, [{j-4}, {k-6}]]},
		parse(atom('$..j'), Query),
		nodes(Query, JSON, Nodes).

	test(query_convenience, deterministic(Values == [red])) :-
		JSON = {store-{bicycle-{color-red, price-399}}},
		query(atom('$.store.bicycle.color'), JSON, Values).

	test(evaluate_missing_member, deterministic(Values == [])) :-
		parse(atom('$.missing'), Query),
		evaluate(Query, {foo-bar}, Values).

	test(evaluate_out_of_range_index, deterministic(Values == [])) :-
		parse(atom('$[20]'), Query),
		evaluate(Query, [a, b], Values).

	test(paths_result, deterministic(Paths == ['$[''store''][''book'']'])) :-
		Query = json_path([child([wildcard]), child([wildcard])]),
		JSON = {store-{book-42}},
		paths(Query, JSON, Paths).

	test(paths_chars_representation, deterministic(Paths == [chars(['$', '[', '\'', s, t, o, r, e, '\'', ']'])])) :-
		Query = json_path([child([name(chars([s,t,o,r,e]))])]),
		JSON = {store-42},
		json_path(chars)::paths(Query, JSON, Paths).

	test(nodes_result, deterministic(Nodes == [node('$[''store'']', {book-42})])) :-
		Query = json_path([child([name(store)])]),
		JSON = {store-{book-42}},
		nodes(Query, JSON, Nodes).

	test(query_chars_source, deterministic(Values == [42])) :-
		JSON = {store-42},
		query(chars(['$', '.', s, t, o, r, e]), JSON, Values).

	test(query_codes_source, deterministic(Values == [42])) :-
		JSON = {store-42},
		query(codes([36, 46, 115, 116, 111, 114, 101]), JSON, Values).

	test(evaluate_query_instantiation_error, error(instantiation_error)) :-
		evaluate(_, [], _).

	test(evaluate_json_instantiation_error, error(instantiation_error)) :-
		evaluate(json_path([]), _, _).

	test(evaluate_invalid_query_error, error(domain_error(json_path_query, foo))) :-
		evaluate(foo, [], _).

	test(paths_query_instantiation_error, error(instantiation_error)) :-
		paths(_, [], _).

	test(paths_json_instantiation_error, error(instantiation_error)) :-
		paths(json_path([]), _, _).

	test(paths_invalid_query_error, error(domain_error(json_path_query, foo))) :-
		paths(foo, [], _).

	test(nodes_query_instantiation_error, error(instantiation_error)) :-
		nodes(_, [], _).

	test(nodes_json_instantiation_error, error(instantiation_error)) :-
		nodes(json_path([]), _, _).

	test(nodes_invalid_query_error, error(domain_error(json_path_query, foo))) :-
		nodes(foo, [], _).

	test(query_source_instantiation_error, error(instantiation_error)) :-
		query(_, [], _).

	test(query_json_instantiation_error, error(instantiation_error)) :-
		query(atom('$'), _, _).

	test(parse_invalid_query, false) :-
		parse(atom('$..'), _).

	test(parse_invalid_out_of_range_index, false) :-
		parse(atom('$[9007199254740992]'), _).

	test(parse_invalid_out_of_range_slice_step, false) :-
		parse(atom('$[0:10:9007199254740992]'), _).

	test(parse_invalid_out_of_range_singular_query_index, false) :-
		parse(atom('$[?$.a[9007199254740992] == 1]'), _).

	test(parse_invalid_leading_zero_index, false) :-
		parse(atom('$[01]'), _).

	test(parse_invalid_surrogate_codes_query, error(domain_error(json_path_source, codes([36, 91, 39, 55357, 39, 93])))) :-
		json_path(codes)::parse(codes([36, 91, 39, 55357, 39, 93]), _).

	test(generate_invalid_out_of_range_index_query, error(domain_error(json_path_query, json_path([child([index(9007199254740992)])])))) :-
		generate(atom(_), json_path([child([index(9007199254740992)])])).

	test(generate_invalid_surrogate_codes_name_query, error(domain_error(json_path_query, json_path([child([name(codes([55357]))])])))) :-
		json_path(codes)::generate(atom(_), json_path([child([name(codes([55357]))])])).

:- end_object.
