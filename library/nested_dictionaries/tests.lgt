%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paul Brown <pbrown@optimusprime.ai> and
%                      Paulo Moura <pmoura@logtalk.org>
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


:- object(tests(_DictionaryObject_),
	extends(lgtunit)).

	:- info([
		version is 0:1:0,
		author is 'Paul Brown and Paulo Moura',
		date is 2021-04-09,
		comment is 'Unit tests for the "nested_dictionaries" library.',
		parnames is ['DictionaryObject']
	]).

	:- uses(_DictionaryObject_, [
		as_nested_dictionary/2, as_curly_bracketed/2, new/1, empty/1,
		lookup_in/3, update_in/4, update_in/5, insert_in/4, delete_in/4
	]).

	:- uses(list, [
		valid/1 as is_list/1
	]).

	cover(_DictionaryObject_).

	% dictionaries for use in the tests

	test_dict(Dict) :-
		as_nested_dictionary(
			{	recipe-'Sponge Cake',
				ingredients-[
					{name-butter, measure-{value-125, unit-g}},
					{name-sugar,  measure-{value-125, unit-g}},
					{name-eggs,   measure-{value-2,   unit-count}},
					{name-flour,  measure-{value-125, unit-g}}
				],
				serves-8,
				time-{value-30, unit-minutes}
			},
			Dict
		).

	deep_dict(Dict) :-
		AlphabetNest = {a-{b-{c-{d-{e-{f-{g-{h-{i-{j-{k-{l-{m-{n-{o-{p-{q-{r-{s-{t-{u-{v-{w-{x-{y-{z-{}}}}}}}}}}}}}}}}}}}}}}}}}}},
		as_nested_dictionary(AlphabetNest, Dict).

	deep_dict_keys_full_path_to_empty_value([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z]).

	% new/1 tests

	test(nested_dictionaries_new_1, true(empty(Dict))) :-
		new(Dict).

	% as_nested_dictionary/2 tests

	test(nested_dictionaries_as_nested_dictionary_2_empty, true(empty(Dict))) :-
		as_nested_dictionary({}, Dict).

	test(nested_dictionaries_as_nested_dictionary_2_non_empty, true(Value == 8)) :-
		as_nested_dictionary({a-1, b-{c-3, d-{e-7,f-8}}}, Dict),
		lookup_in([b,d,f], Value, Dict).

	% as_curly_bracketed/2 tests

	test(nested_dictionaries_as_curly_bracketed_2_empty, true(Term == {})) :-
		new(Dict),
		as_curly_bracketed(Dict, Term).

	test(nested_dictionaries_as_curly_bracketed_2_non_empty, true(Term == {a-1, b-{c-3, d-{e-[7,8]}}})) :-
		as_nested_dictionary({a-1, b-{c-3, d-{e-[7,8]}}}, Dict),
		as_curly_bracketed(Dict, Term).

	% lookup_in/3 tests

	test(nested_dictionaries_lookup_in_3_empty_reflexive, deterministic(Ans == Dict)) :-
		test_dict(Dict),
		lookup_in([], Ans, Dict).

	test(nested_dictionaries_lookup_in_3_depth_one, deterministic([Name, Serves] == ['Sponge Cake', 8])) :-
		test_dict(Dict),
		lookup_in([recipe], Name, Dict),
		lookup_in([serves], Serves, Dict).

	test(nested_dictionaries_lookup_in_3_depth_two, deterministic([Value, Unit] == [30, minutes])) :-
		test_dict(Dict),
		lookup_in([time, value], Value, Dict),
		lookup_in([time, unit], Unit, Dict).

	test(nested_dictionaries_lookup_in_3_is_list, true(is_list(Ans))) :-
		test_dict(Dict),
		lookup_in([ingredients], Ans, Dict).

	test(nested_dictionaries_lookup_in_3_across_list, true) :-
		test_dict(Dict),
		lookup_in([ingredients], Ingredients, Dict),
		list::member(Ingredient, Ingredients),
		lookup_in([name], sugar, Ingredient).

	test(nested_dictionaries_lookup_in_3_through_list, true) :-
		test_dict(Dict),
		lookup_in([ingredients], Ingredients, Dict),
		list::member(Ingredient, Ingredients),
		lookup_in([measure, unit], g, Ingredient).

	test(nested_dictionaries_lookup_in_3_through_nondict, fail) :-
		test_dict(Dict),
		lookup_in([recipe, name], _, Dict).

	test(nested_dictionaries_obj_lookup_in_3_empty_reflexive, deterministic(Ans == Dict)) :-
		test_dict(Dict),
		lookup_in([], Ans, Dict).

	test(nested_dictionaries_obj_lookup_in_3_depth_one, deterministic([Name, Serves] == ['Sponge Cake', 8])) :-
		test_dict(Dict),
		lookup_in([recipe], Name, Dict),
		lookup_in([serves], Serves, Dict).

	test(nested_dictionaries_obj_lookup_in_3_depth_two, deterministic([Value, Unit] == [30, minutes])) :-
		test_dict(Dict),
		lookup_in([time, value], Value, Dict),
		lookup_in([time, unit], Unit, Dict).

	test(nested_dictionaries_obj_lookup_in_3_is_list, true(is_list(Ans))) :-
		test_dict(Dict),
		lookup_in([ingredients], Ans, Dict).

	test(nested_dictionaries_obj_lookup_in_3_across_list, true) :-
		test_dict(Dict),
		lookup_in([ingredients], Ingredients, Dict),
		list::member(Ingredient, Ingredients),
		lookup_in([name], sugar, Ingredient).

	test(nested_dictionaries_obj_lookup_in_3_through_list, true) :-
		test_dict(Dict),
		lookup_in([ingredients], Ingredients, Dict),
		list::member(Ingredient, Ingredients),
		lookup_in([measure, unit], g, Ingredient).

	test(nested_dictionaries_obj_lookup_in_3_through_nondict, fail) :-
		test_dict(Dict),
		lookup_in([recipe, name], _, Dict).

	test(nested_dictionaries_lookup_in_3_deep_dict, true) :-
		new(Empty),
		deep_dict(Dict),
		deep_dict_keys_full_path_to_empty_value(Path),
		lookup_in(Path, Empty, Dict).

	% update_in/4 tests

	test(nested_dictionaries_update_in_4_depth_one, deterministic([Name, Serves] == ['Cake', 6])) :-
		test_dict(Dict0),
		update_in(Dict0, [recipe], 'Cake', Dict1),
		update_in(Dict1, [serves], 6, Dict2),
		lookup_in([recipe], Name, Dict2),
		lookup_in([serves], Serves, Dict2).

	test(nested_dictionaries_update_in_4_depth_two, deterministic([Value, Unit] == [1, hour])) :-
		test_dict(Dict0),
		update_in(Dict0, [time, value], 1, Dict1),
		update_in(Dict1, [time, unit], hour, Dict2),
		lookup_in([time, value], Value, Dict2),
		lookup_in([time, unit], Unit, Dict2).

	test(nested_dictionaries_update_in_4_a_list, true(Ans == [])) :-
		test_dict(Dict0),
		update_in(Dict0, [ingredients], [], Dict),
		lookup_in([ingredients], Ans, Dict).

	test(nested_dictionaries_update_in_4_through_nondict, fail) :-
		test_dict(Dict),
		update_in(Dict, [recipe, name], any, _Dict).

	test(nested_dictionaries_obj_update_in_4_depth_one, deterministic([Name, Serves] == ['Cake', 6])) :-
		test_dict(Dict0),
		update_in(Dict0, [recipe], 'Cake', Dict1),
		update_in(Dict1, [serves], 6, Dict2),
		lookup_in([recipe], Name, Dict2),
		lookup_in([serves], Serves, Dict2).

	test(nested_dictionaries_obj_update_in_4_depth_two, deterministic([Value, Unit] == [1, hour])) :-
		test_dict(Dict0),
		update_in(Dict0, [time, value], 1, Dict1),
		update_in(Dict1, [time, unit], hour, Dict2),
		lookup_in([time, value], Value, Dict2),
		lookup_in([time, unit], Unit, Dict2).

	test(nested_dictionaries_obj_update_in_4_a_list, true(Ans = [])) :-
		test_dict(Dict0),
		update_in(Dict0, [ingredients], [], Dict),
		lookup_in([ingredients], Ans, Dict).

	test(nested_dictionaries_obj_update_in_4_through_nondict, fail) :-
		test_dict(Dict),
		update_in(Dict, [recipe, name], any, _Dict).

	test(nested_dictionaries_update_in_4_deep_dict, true(Value == Test)) :-
		test_dict(Test),
		deep_dict(Deep),
		deep_dict_keys_full_path_to_empty_value(Path),
		update_in(Deep, Path, Test, Deep0),
		lookup_in(Path, Value, Deep0).

	% update_in/5 tests

	test(nested_dictionaries_update_in_5_nested_matching, deterministic(Time == 1)) :-
		test_dict(Dict),
		update_in(Dict, [time, value], 30, 1, Dict1),
		lookup_in([time, value], Time, Dict1).

	test(nested_dictionaries_update_in_5_nested_not_matching, fail) :-
		test_dict(Dict),
		update_in(Dict, [time, value], 1, 1, _Dict1).

	test(nested_dictionaries_obj_update_in_5_nested_matching, deterministic(Time == 1)) :-
		test_dict(Dict),
		update_in(Dict, [time, value], 30, 1, Dict1),
		lookup_in([time, value], Time, Dict1).

	test(nested_dictionaries_obj_update_in_5_nested_not_matching, fail) :-
		test_dict(Dict),
		update_in(Dict, [time, value], 1, 1, _Dict1).

	test(nested_dictionaries_update_in_5_deep_dict, true(Value == Test)) :-
		test_dict(Test),
		deep_dict(Deep),
		new(Empty),
		deep_dict_keys_full_path_to_empty_value(Path),
		update_in(Deep, Path, Empty, Test, Deep0),
		lookup_in(Path, Value, Deep0).

	% insert_in/4 tests

	test(nested_dictionaries_insert_in_4_with_empty_keys, fail) :-
		new(Dictionary),
		insert_in(Dictionary, [], nowt, _NewDictionary).

	test(nested_dictionaries_insert_in_4_with_variable_key, fail) :-
		new(Dictionary),
		insert_in(Dictionary, [_Var], nowt, _NewDictionary).

	test(nested_dictionaries_insert_in_4_doesnt_create_missing_dict, fail) :-
		test_dict(Dictionary),
		insert_in(Dictionary, [time, zone, code], utc, _NewDictionary).

	test(nested_dictionaries_insert_in_4_in_empty_dictionary, true(Value == a)) :-
		new(Dictionary),
		insert_in(Dictionary, [1], a, NewDictionary),
		lookup_in([1], Value, NewDictionary).

	test(nested_dictionaries_insert_in_4_in_new_key, true(Zone == utc)) :-
		test_dict(Dictionary),
		insert_in(Dictionary, [time, zone], utc, NewDictionary),
		lookup_in([time, zone], Zone, NewDictionary).

	test(nested_dictionaries_insert_in_4_with_existing_key_clobbers, true(Value == 10)) :-
		test_dict(Dictionary),
		insert_in(Dictionary, [serves], 10, NewDictionary),
		lookup_in([serves], Value, NewDictionary).

	test(nested_dictionaries_insert_in_4_in_a_dictionary, true(Inserted == Dictionary)) :-
		test_dict(Dictionary),
		insert_in(Dictionary, [time, duplicate], Dictionary, WithDuplicate),
		lookup_in([time, duplicate], Inserted, WithDuplicate).

	test(nested_dictionaries_insert_in_4_in_deep_dict, true(Value == 1)) :-
		deep_dict(Deep),
		deep_dict_keys_full_path_to_empty_value(Path),
		list::append(Path, [foo], PathIntoEmpty),
		insert_in(Deep, PathIntoEmpty, 1, Deep0),
		lookup_in(PathIntoEmpty, Value, Deep0).

	% delete_in/4 tests

	test(nested_dictionaries_delete_in_4_no_keys, fail) :-
		test_dict(Dictionary),
		delete_in(Dictionary, [], _Value, _NewDictionary).

	test(nested_dictionaries_delete_in_4_wrong_keys, fail) :-
		test_dict(Dictionary),
		delete_in(Dictionary, [time, not_a_key], _Value, _NewDictionary).

	test(nested_dictionaries_delete_in_4_variable_keys, fail) :-
		test_dict(Dictionary),
		delete_in(Dictionary, [time, _Var], _Value, _NewDictionary).

	test(nested_dictionaries_delete_in_4_value_not_matching, fail) :-
		test_dict(Dictionary),
		delete_in(Dictionary, [serves], 100, _NewDictionary).

	test(nested_dictionaries_delete_in, true(Unit == minutes)) :-
		test_dict(Dictionary),
		delete_in(Dictionary, [time, unit], Unit, NewDictionary),
		\+ lookup_in([time, unit], _, NewDictionary).

	test(nested_dictionaries_delete_in_4_deep_dict, true(Value = Empty)) :-
		new(Empty),
		deep_dict(Deep),
		deep_dict_keys_full_path_to_empty_value(Path),
		delete_in(Deep, Path, Value, Deep0),
		\+ lookup_in(Path, _, Deep0).

:- end_object.
