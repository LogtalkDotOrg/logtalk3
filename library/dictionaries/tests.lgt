%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:10:0,
		author is 'Paulo Moura',
		date is 2024-10-02,
		comment is 'Unit tests for the "dictionaries" library.',
		parnames is ['DictionaryObject']
	]).

	:- uses(_DictionaryObject_, [
		as_dictionary/2, as_list/2, as_curly_bracketed/2,
		clone/3, clone/4, insert/4, delete/4, update/4, update/5, update/3, empty/1,
		lookup/3, lookup/2, previous/4, next/4, min/3, max/3, delete_min/4, delete_max/4,
		intersection/2, intersection/3, keys/2, values/2, map/2, map/3, apply/4, size/2,
		valid/1, new/1
	]).

	:- uses(list, [
		msort/2
	]).

	cover(_DictionaryObject_).

	% as_dictionary/2 and as_list/2 tests

	deterministic(dictionary_as_dictionary_2_01) :-
		as_dictionary([], Dictionary),
		as_list(Dictionary, Pairs),
		^^assertion(Pairs == []).

	deterministic(dictionary_as_dictionary_2_02) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		as_list(Dictionary, Pairs),
		^^assertion(Pairs == [a-1,b-2,c-3,d-4,e-5,f-6,g-7,h-8,i-9,j-0]).

	% as_curly_bracketed/2 tests

	deterministic(dictionary_as_curly_bracketed_2_01) :-
		as_dictionary([], Dictionary),
		as_curly_bracketed(Dictionary, Curly),
		^^assertion(Curly == {}).

	deterministic(dictionary_as_curly_bracketed_2_02) :-
		as_dictionary([a-1,b-2], Dictionary),
		as_curly_bracketed(Dictionary, Curly),
		^^assertion((Curly == {a-1,b-2}; Curly == {b-2,a-1})).

	% clone/3 tests

	deterministic(dictionary_clone_3_01) :-
		as_dictionary([], Dictionary),
		clone(Dictionary, Clone, ClonePairs),
		empty(Clone),
		^^assertion(ClonePairs == []).

	deterministic(dictionary_clone_3_02) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		clone(Dictionary, Clone, ClonePairs),
		keys(Clone, Keys),
		^^assertion(keys, Keys == [a,b,c,d,e,f,g,h,i,j]),
		^^assertion(pairs, lgtunit::variant(ClonePairs, [a-_,b-_,c-_,d-_,e-_,f-_,g-_,h-_,i-_,j-_])).

	% clone/4 tests

	deterministic(dictionary_clone_4_01) :-
		as_dictionary([], Dictionary),
		clone(Dictionary, DictionaryPairs, Clone, ClonePairs),
		empty(Clone),
		^^assertion(original_pairs, DictionaryPairs == []),
		^^assertion(clone_pairs, ClonePairs == []).

	deterministic(dictionary_clone_4_02) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		clone(Dictionary, DictionaryPairs, Clone, ClonePairs),
		keys(Clone, Keys),
		^^assertion(keys, Keys == [a,b,c,d,e,f,g,h,i,j]),
		^^assertion(original_pairs, DictionaryPairs == [a-1,b-2,c-3,d-4,e-5,f-6,g-7,h-8,i-9,j-0]),
		^^assertion(clone_pairs, lgtunit::variant(ClonePairs, [a-_,b-_,c-_,d-_,e-_,f-_,g-_,h-_,i-_,j-_])).

	% insert/4 tests

	% insert in empty dictionary
	deterministic(dictionary_insert_4_01) :-
		as_dictionary([], Dictionary),
		insert(Dictionary, b, 2, NewDictionary),
		lookup(b, Value, NewDictionary),
		^^assertion(new, Value == 2),
		as_list(NewDictionary, Pairs),
		^^assertion(pairs, Pairs == [b-2]).

	% insert smallest key
	deterministic(dictionary_insert_4_02) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,d-4], Dictionary),
		insert(Dictionary, a, 1, NewDictionary),
		lookup(a, Value, NewDictionary),
		^^assertion(new, Value == 1),
		as_list(NewDictionary, Pairs),
		^^assertion(pairs, Pairs == [a-1,b-2,c-3,d-4,e-5,f-6,g-7,h-8,i-9,j-0]).

	% insert second smallest key
	deterministic(dictionary_insert_4_03) :-
		as_dictionary([j-0,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		insert(Dictionary, b, 2, NewDictionary),
		lookup(b, Value, NewDictionary),
		^^assertion(new, Value == 2),
		as_list(NewDictionary, Pairs),
		^^assertion(pairs, Pairs == [a-1,b-2,c-3,d-4,e-5,f-6,g-7,h-8,i-9,j-0]).

	% insert middle key
	deterministic(dictionary_insert_4_04) :-
		as_dictionary([j-0,b-2,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		insert(Dictionary, e, 5, NewDictionary),
		lookup(e, Value, NewDictionary),
		^^assertion(new, Value == 5),
		as_list(NewDictionary, Pairs),
		^^assertion(pairs, Pairs == [a-1,b-2,c-3,d-4,e-5,f-6,g-7,h-8,i-9,j-0]).

	% insert key before largest key
	deterministic(dictionary_insert_4_05) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,h-8,f-6,a-1,d-4], Dictionary),
		insert(Dictionary, i, 9, NewDictionary),
		lookup(i, Value, NewDictionary),
		^^assertion(new, Value == 9),
		as_list(NewDictionary, Pairs),
		^^assertion(pairs, Pairs == [a-1,b-2,c-3,d-4,e-5,f-6,g-7,h-8,i-9,j-0]).

	% insert largest key
	deterministic(dictionary_insert_4_06) :-
		as_dictionary([e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		insert(Dictionary, j, 0, NewDictionary),
		lookup(j, Value, NewDictionary),
		^^assertion(Value == 0).

	% delete/4 tests

	% delete from empty dictionary
	test(dictionary_delete_4_01) :-
		as_dictionary([], Dictionary),
		\+ delete(Dictionary, b, _, _).

	% delete non-existing key
	test(dictionary_delete_4_02) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		\+ delete(Dictionary, x, _, _).

	% delete key with non-unifying value
	test(dictionary_delete_4_03) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		\+ delete(Dictionary, h, 9, _).

	% delete smallest key
	deterministic(dictionary_delete_4_04) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		delete(Dictionary, a, Value, NewDictionary),
		^^assertion(value, Value == 1),
		as_list(NewDictionary, Pairs),
		^^assertion(pairs, Pairs == [b-2,c-3,d-4,e-5,f-6,g-7,h-8,i-9,j-0]).

	% delete second smallest key
	deterministic(dictionary_delete_4_05) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		delete(Dictionary, b, Value, NewDictionary),
		^^assertion(value, Value == 2),
		as_list(NewDictionary, Pairs),
		^^assertion(pairs, Pairs == [a-1,c-3,d-4,e-5,f-6,g-7,h-8,i-9,j-0]).

	% delete middle key
	deterministic(dictionary_delete_4_06) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		delete(Dictionary, e, Value, NewDictionary),
		^^assertion(value, Value == 5),
		as_list(NewDictionary, Pairs),
		^^assertion(pairs, Pairs == [a-1,b-2,c-3,d-4,f-6,g-7,h-8,i-9,j-0]).

	% delete key before largest key
	deterministic(dictionary_delete_4_07) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		delete(Dictionary, i, Value, NewDictionary),
		^^assertion(value, Value == 9),
		as_list(NewDictionary, Pairs),
		^^assertion(pairs, Pairs == [a-1,b-2,c-3,d-4,e-5,f-6,g-7,h-8,j-0]).

	% delete largest key
	deterministic(dictionary_delete_4_08) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		delete(Dictionary, j, Value, NewDictionary),
		^^assertion(value, Value == 0),
		as_list(NewDictionary, Pairs),
		^^assertion(pairs, Pairs == [a-1,b-2,c-3,d-4,e-5,f-6,g-7,h-8,i-9]).

	% update/4 tests

	% update empty dictionary
	test(dictionary_update_4_01) :-
		as_dictionary([], Dictionary),
		\+ update(Dictionary, b, 42, _).

	% update non-existing key
	test(dictionary_update_4_02) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		\+ update(Dictionary, x, 42, _).

	% update existing key
	deterministic(dictionary_update_4_03) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		update(Dictionary, b, 22, NewDictionary),
		lookup(b, Value, NewDictionary),
		^^assertion(Value == 22).

	% update/5 tests

	% update empty dictionary
	test(dictionary_update_5_01) :-
		as_dictionary([], Dictionary),
		\+ update(Dictionary, b, _, _, _).

	% update non-existing key
	test(dictionary_update_5_02) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		\+ update(Dictionary, x, 42, 24, _).

	% delete key with non-unifying value
	test(dictionary_update_5_03) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		\+ update(Dictionary, h, 9, 10, _).

	% update existing key
	deterministic(dictionary_update_5_04) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		update(Dictionary, b, OldValue, 22, NewDictionary),
		^^assertion(old, OldValue == 2),
		lookup(b, Value, NewDictionary),
		^^assertion(new, Value == 22).

	% update/3 tests

	test(dictionary_update_3_01) :-
		as_dictionary([], Dictionary),
		\+ update(Dictionary, [b-42], _).

	deterministic(dictionary_update_3_02) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		update(Dictionary, [b-22, f-7], NewDictionary),
		lookup(b, B, NewDictionary),
		^^assertion(new_b, B == 22),
		lookup(f, F, NewDictionary),
		^^assertion(new_f, F == 7).

	% empty/1 tests

	deterministic(dictionary_empty_1_01) :-
		as_dictionary([], Dictionary),
		empty(Dictionary).

	test(dictionary_empty_1_02) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		\+ empty(Dictionary).

	% lookup/3 tests

	test(dictionary_lookup_3_01) :-
		as_dictionary([], Dictionary),
		\+ lookup(b, _, Dictionary).

	deterministic(dictionary_lookup_3_02) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		lookup(b, Value, Dictionary),
		^^assertion(Value == 2).

	deterministic(dictionary_lookup_3_03) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		findall(Key-Value, lookup(Key, Value, Dictionary), Pairs0),
		msort(Pairs0, Pairs),
		^^assertion(Pairs == [a-1,b-2,c-3,d-4,e-5,f-6,g-7,h-8,i-9,j-0]).

	% lookup/2 tests

	test(dictionary_lookup_2_01) :-
		as_dictionary([], Dictionary),
		\+ lookup([b-_], Dictionary).

	deterministic(dictionary_lookup_2_02) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		lookup([b-B, f-F], Dictionary),
		^^assertion(B-F == 2-6).

	% previous/4 tests

	test(dictionary_previous_4_01) :-
		as_dictionary([], Dictionary),
		\+ previous(Dictionary, _, _, _).

	deterministic(dictionary_previous_4_02) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		previous(Dictionary, b, Key, Value),
		^^assertion(Key-Value == a-1).

	% next/4 tests

	test(dictionary_next_4_01) :-
		as_dictionary([], Dictionary),
		\+ next(Dictionary, _, _, _).

	deterministic(dictionary_next_4_02) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		next(Dictionary, b, Key, Value),
		^^assertion(Key-Value == c-3).

	% min/3 tests

	test(dictionary_min_3_01) :-
		as_dictionary([], Dictionary),
		\+ min(Dictionary, _, _).

	deterministic(dictionary_min_3_02) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		min(Dictionary, Key, Value),
		^^assertion(Key-Value == a-1).

	% max/3 tests

	test(dictionary_max_3_01) :-
		as_dictionary([], Dictionary),
		\+ max(Dictionary, _, _).

	deterministic(dictionary_max_3_02) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		max(Dictionary, Key, Value),
		^^assertion(Key-Value == j-0).

	% delete_min/4 tests

	test(dictionary_delete_min_4_01) :-
		as_dictionary([], Dictionary),
		\+ delete_min(Dictionary, _, _, _).

	deterministic(dictionary_delete_min_4_02) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		delete_min(Dictionary, Key, Value, NewDictionary),
		^^assertion(pair, Key-Value == a-1),
		as_list(NewDictionary, OtherPairs),
		^^assertion(pairs, OtherPairs == [b-2,c-3,d-4,e-5,f-6,g-7,h-8,i-9,j-0]).

	% delete_max/4 tests

	test(dictionary_delete_max_4_01) :-
		as_dictionary([], Dictionary),
		\+ delete_max(Dictionary, _, _, _).

	deterministic(dictionary_delete_max_4_02) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		delete_max(Dictionary, Key, Value, NewDictionary),
		^^assertion(pair, Key-Value == j-0),
		as_list(NewDictionary, OtherPairs),
		^^assertion(pairs, OtherPairs == [a-1,b-2,c-3,d-4,e-5,f-6,g-7,h-8,i-9]).

	% keys/2 tests

	deterministic(dictionary_keys_2_01) :-
		as_dictionary([], Dictionary),
		keys(Dictionary, Keys),
		^^assertion(Keys == []).

	deterministic(dictionary_keys_2_02) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		keys(Dictionary, Keys),
		^^assertion(Keys == [a,b,c,d,e,f,g,h,i,j]).

	% values/2 tests

	deterministic(dictionary_values_2_01) :-
		as_dictionary([], Dictionary),
		values(Dictionary, Values),
		^^assertion(Values == []).

	deterministic(dictionary_values_2_02) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		values(Dictionary, Values),
		^^assertion(Values == [1,2,3,4,5,6,7,8,9,0]).

	% map/2 tests

	test(dictionary_map_2_01) :-
		as_dictionary([], Dictionary),
		map([_-Value]>>integer(Value), Dictionary).

	test(dictionary_map_2_02) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		map([_-Value]>>integer(Value), Dictionary).

	test(dictionary_map_2_03) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		\+ map([_-Value]>>atom(Value), Dictionary).

	% map/3 tests

	test(dictionary_map_3_01) :-
		as_dictionary([], Dictionary),
		map([Key-Value, Key-NewValue]>>(integer::succ(Value,NewValue)), Dictionary, NewDictionary),
		as_list(NewDictionary, Pairs),
		^^assertion(Pairs == []).

	test(dictionary_map_3_02) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		map([Key-Value, Key-NewValue]>>(integer::succ(Value,NewValue)), Dictionary, NewDictionary),
		as_list(NewDictionary, Pairs),
		^^assertion(Pairs == [a-2,b-3,c-4,d-5,e-6,f-7,g-8,h-9,i-10,j-1]).

	% apply/4 tests

	test(dictionary_apply_4_01) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		apply([Key-Value, Key-NewValue]>>(integer::succ(Value,NewValue)), Dictionary, b, NewDictionary),
		lookup(b, CurrentValue, NewDictionary),
		^^assertion(CurrentValue == 3).

	% size/2 tests

	deterministic(dictionary_size_2_01) :-
		as_dictionary([], Dictionary),
		size(Dictionary, Size),
		^^assertion(Size == 0).

	deterministic(dictionary_size_2_02) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		size(Dictionary, Size),
		^^assertion(Size == 10).

	% intersection/2 tests

	deterministic(dictionary_intersection_2_01) :-
		as_dictionary([], Dictionary1),
		as_dictionary([], Dictionary2),
		intersection(Dictionary1, Dictionary2).

	deterministic(dictionary_intersection_2_02) :-
		as_dictionary([], Dictionary1),
		as_dictionary([x-1], Dictionary2),
		intersection(Dictionary1, Dictionary2).

	deterministic(dictionary_intersection_2_03) :-
		as_dictionary([x-1], Dictionary1),
		as_dictionary([], Dictionary2),
		intersection(Dictionary1, Dictionary2).

	deterministic(dictionary_intersection_2_04) :-
		as_dictionary([x-1, y-Y], Dictionary1),
		as_dictionary([x-X, y-2], Dictionary2),
		intersection(Dictionary1, Dictionary2),
		^^assertion(X-Y == 1-2).

	deterministic(dictionary_intersection_2_05) :-
		as_dictionary([x-1, y-Y, z-3], Dictionary1),
		as_dictionary([x-X, y-2, t-4], Dictionary2),
		intersection(Dictionary1, Dictionary2),
		^^assertion(X-Y == 1-2).

	% intersection/3 tests

	deterministic(dictionary_intersection_3_01) :-
		as_dictionary([], Dictionary1),
		as_dictionary([], Dictionary2),
		intersection(Dictionary1, Dictionary2, Intersection),
		empty(Intersection).

	deterministic(dictionary_intersection_3_02) :-
		as_dictionary([], Dictionary1),
		as_dictionary([x-1], Dictionary2),
		intersection(Dictionary1, Dictionary2, Intersection),
		empty(Intersection).

	deterministic(dictionary_intersection_3_03) :-
		as_dictionary([x-1], Dictionary1),
		as_dictionary([], Dictionary2),
		intersection(Dictionary1, Dictionary2, Intersection),
		empty(Intersection).

	deterministic(dictionary_intersection_3_04) :-
		as_dictionary([x-1, y-Y], Dictionary1),
		as_dictionary([x-X, y-2], Dictionary2),
		intersection(Dictionary1, Dictionary2, Intersection),
		as_list(Intersection, Pairs),
		^^assertion(xy, X-Y == 1-2),
		^^assertion(pairs, Pairs == [x-1, y-2]).

	deterministic(dictionary_intersection_3_05) :-
		as_dictionary([x-1, y-Y, z-3], Dictionary1),
		as_dictionary([x-X, y-2, t-4], Dictionary2),
		intersection(Dictionary1, Dictionary2, Intersection),
		as_list(Intersection, Pairs),
		^^assertion(xy, X-Y == 1-2),
		^^assertion(pairs, Pairs == [x-1, y-2]).

	% valid/1 tests

	deterministic(dictionary_valid_1_01) :-
		as_dictionary([], Dictionary),
		valid(Dictionary).

	deterministic(dictionary_valid_1_02) :-
		as_dictionary([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Dictionary),
		valid(Dictionary).

	% new/1 tests

	deterministic(dictionary_new_1_01) :-
		new(Dictionary),
		valid(Dictionary).

:- end_object.
