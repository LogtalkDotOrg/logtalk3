%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2019/05/17,
		comment is 'Unit tests for the "dictionaries" library.',
		parnames is ['DictionaryObject']
	]).

	cover(_DictionaryObject_).

	% as_dictionary/2 and  as_list/2 tests

	test(dictionary_as_dictionary_2_01) :-
		_DictionaryObject_::as_dictionary([], Dictionary),
		_DictionaryObject_::as_list(Dictionary, Pairs),
		Pairs == [].

	test(dictionary_as_dictionary_2_02) :-
		_DictionaryObject_::as_dictionary([a-1,c-3,b-2], Dictionary),
		_DictionaryObject_::as_list(Dictionary, Pairs),
		Pairs == [a-1,b-2,c-3].

	% clone/3 tests

	test(dictionary_clone_3_01) :-
		_DictionaryObject_::as_dictionary([], Dictionary),
		_DictionaryObject_::clone(Dictionary, Clone, ClonePairs),
		_DictionaryObject_::empty(Clone),
		^^assertion(pairs, ClonePairs == []).

	test(dictionary_clone_3_02) :-
		_DictionaryObject_::as_dictionary([a-1,c-3,b-2], Dictionary),
		_DictionaryObject_::clone(Dictionary, Clone, ClonePairs),
		_DictionaryObject_::keys(Clone, Keys),
		^^assertion(keys, Keys == [a,b,c]),
		^^assertion(pairs, lgtunit::variant(ClonePairs, [a-_,b-_,c-_])).

	% clone/4 tests

	test(dictionary_clone_4_01) :-
		_DictionaryObject_::as_dictionary([], Dictionary),
		_DictionaryObject_::clone(Dictionary, DictionaryPairs, Clone, ClonePairs),
		_DictionaryObject_::empty(Clone),
		^^assertion(original_pairs, DictionaryPairs == []),
		^^assertion(clone_pairs, ClonePairs == []).

	test(dictionary_clone_4_02) :-
		_DictionaryObject_::as_dictionary([a-1,c-3,b-2], Dictionary),
		_DictionaryObject_::clone(Dictionary, DictionaryPairs, Clone, ClonePairs),
		_DictionaryObject_::keys(Clone, Keys),
		^^assertion(keys, Keys == [a,b,c]),
		^^assertion(original_pairs, DictionaryPairs == [a-1,b-2,c-3]),
		^^assertion(clone_pairs, lgtunit::variant(ClonePairs, [a-_,b-_,c-_])).

	% insert/4 tests

	test(dictionary_insert_4_01) :-
		_DictionaryObject_::as_dictionary([], Dictionary),
		_DictionaryObject_::insert(Dictionary, b, 2, NewDictionary),
		_DictionaryObject_::lookup(b, Value, NewDictionary),
		^^assertion(new, Value == 2).

	test(dictionary_insert_4_02) :-
		_DictionaryObject_::as_dictionary([a-1,c-3], Dictionary),
		_DictionaryObject_::insert(Dictionary, b, 2, NewDictionary),
		_DictionaryObject_::lookup(b, Value, NewDictionary),
		^^assertion(new, Value == 2).

	% delete/4 tests

	test(dictionary_delete_4_01) :-
		_DictionaryObject_::as_dictionary([], Dictionary),
		\+ _DictionaryObject_::delete(Dictionary, b, _, _).

	test(dictionary_delete_4_02) :-
		_DictionaryObject_::as_dictionary([a-1,c-3,b-2], Dictionary),
		_DictionaryObject_::delete(Dictionary, b, Value, NewDictionary),
		^^assertion(value, Value == 2),
		findall(OtherKey-OtherValue, _DictionaryObject_::lookup(OtherKey, OtherValue, NewDictionary), OtherPairs),
		^^assertion(pairs, OtherPairs == [a-1, c-3]).

	% update/4 tests

	test(dictionary_update_4_01) :-
		_DictionaryObject_::as_dictionary([], Dictionary),
		\+ _DictionaryObject_::update(Dictionary, b, _, _).

	test(dictionary_update_4_02) :-
		_DictionaryObject_::as_dictionary([a-1,c-3,b-2], Dictionary),
		_DictionaryObject_::update(Dictionary, b, 22, NewDictionary),
		_DictionaryObject_::lookup(b, Value, NewDictionary),
		^^assertion(new, Value == 22).

	% update/5 tests

	test(dictionary_update_5_01) :-
		_DictionaryObject_::as_dictionary([], Dictionary),
		\+ _DictionaryObject_::update(Dictionary, b, _, _, _).

	test(dictionary_update_5_02) :-
		_DictionaryObject_::as_dictionary([a-1,c-3,b-2], Dictionary),
		_DictionaryObject_::update(Dictionary, b, OldValue, 22, NewDictionary),
		^^assertion(old, OldValue == 2),
		_DictionaryObject_::lookup(b, Value, NewDictionary),
		^^assertion(new, Value == 22).

	% empty/1 tests

	test(dictionary_empty_1_01) :-
		_DictionaryObject_::as_dictionary([], Dictionary),
		_DictionaryObject_::empty(Dictionary).

	test(dictionary_empty_1_02) :-
		_DictionaryObject_::as_dictionary([a-1,c-3,b-2], Dictionary),
		\+ _DictionaryObject_::empty(Dictionary).

	% lookup/3 tests

	test(dictionary_lookup_3_01) :-
		_DictionaryObject_::as_dictionary([], Dictionary),
		\+ _DictionaryObject_::lookup(b, _, Dictionary).

	deterministic(dictionary_lookup_3_02) :-
		_DictionaryObject_::as_dictionary([a-1,c-3,b-2], Dictionary),
		_DictionaryObject_::lookup(b, Value, Dictionary),
		Value == 2.

	deterministic(dictionary_lookup_3_03) :-
		_DictionaryObject_::as_dictionary([a-1,c-3,b-2], Dictionary),
		findall(Key-Value, _DictionaryObject_::lookup(Key, Value, Dictionary), Pairs),
		Pairs == [a-1, b-2, c-3].

	% previous/4 tests

	test(dictionary_previous_4_01) :-
		_DictionaryObject_::as_dictionary([], Dictionary),
		\+ _DictionaryObject_::previous(Dictionary, _, _, _).

	test(dictionary_previous_4_02) :-
		_DictionaryObject_::as_dictionary([a-1,c-3,b-2], Dictionary),
		_DictionaryObject_::previous(Dictionary, b, Key, Value),
		^^assertion(pair, Key-Value == a-1).

	% next/4 tests

	test(dictionary_next_4_01) :-
		_DictionaryObject_::as_dictionary([], Dictionary),
		\+ _DictionaryObject_::next(Dictionary, _, _, _).

	test(dictionary_next_4_02) :-
		_DictionaryObject_::as_dictionary([a-1,c-3,b-2], Dictionary),
		_DictionaryObject_::next(Dictionary, b, Key, Value),
		^^assertion(pair, Key-Value == c-3).

	% min/3 tests

	test(dictionary_min_3_01) :-
		_DictionaryObject_::as_dictionary([], Dictionary),
		\+ _DictionaryObject_::min(Dictionary, _, _).

	test(dictionary_min_3_02) :-
		_DictionaryObject_::as_dictionary([a-1,c-3,b-2], Dictionary),
		_DictionaryObject_::min(Dictionary, Key, Value),
		^^assertion(pair, Key-Value == a-1).

	% max/3 tests

	test(dictionary_max_3_01) :-
		_DictionaryObject_::as_dictionary([], Dictionary),
		\+ _DictionaryObject_::max(Dictionary, _, _).

	test(dictionary_max_3_02) :-
		_DictionaryObject_::as_dictionary([a-1,c-3,b-2], Dictionary),
		_DictionaryObject_::max(Dictionary, Key, Value),
		^^assertion(pair, Key-Value == c-3).

	% delete_min/4 tests

	test(dictionary_delete_min_4_01) :-
		_DictionaryObject_::as_dictionary([], Dictionary),
		\+ _DictionaryObject_::delete_min(Dictionary, _, _, _).

	test(dictionary_delete_min_4_02) :-
		_DictionaryObject_::as_dictionary([a-1,c-3,b-2], Dictionary),
		_DictionaryObject_::delete_min(Dictionary, Key, Value, NewDictionary),
		^^assertion(pair, Key-Value == a-1),
		findall(OtherKey-OtherValue, _DictionaryObject_::lookup(OtherKey, OtherValue, NewDictionary), OtherPairs),
		^^assertion(pairs, OtherPairs == [b-2, c-3]).

	% delete_max/4 tests

	test(dictionary_delete_max_4_01) :-
		_DictionaryObject_::as_dictionary([], Dictionary),
		\+ _DictionaryObject_::delete_max(Dictionary, _, _, _).

	test(dictionary_delete_max_4_02) :-
		_DictionaryObject_::as_dictionary([a-1,c-3,b-2], Dictionary),
		_DictionaryObject_::delete_max(Dictionary, Key, Value, NewDictionary),
		^^assertion(pair, Key-Value == c-3),
		findall(OtherKey-OtherValue, _DictionaryObject_::lookup(OtherKey, OtherValue, NewDictionary), OtherPairs),
		^^assertion(pairs, OtherPairs == [a-1, b-2]).

	% keys/2 tests

	test(dictionary_keys_2_01) :-
		_DictionaryObject_::as_dictionary([], Dictionary),
		_DictionaryObject_::keys(Dictionary, Keys),
		Keys == [].

	test(dictionary_keys_2_02) :-
		_DictionaryObject_::as_dictionary([a-1,c-3,b-2], Dictionary),
		_DictionaryObject_::keys(Dictionary, Keys),
		Keys == [a, b, c].

	% values/2 tests

	test(dictionary_values_2_01) :-
		_DictionaryObject_::as_dictionary([], Dictionary),
		_DictionaryObject_::values(Dictionary, Values),
		Values == [].

	test(dictionary_values_2_02) :-
		_DictionaryObject_::as_dictionary([a-1,c-3,b-2], Dictionary),
		_DictionaryObject_::values(Dictionary, Values),
		Values == [1, 2, 3].

	% map/2 tests

	test(dictionary_map_2_01) :-
		_DictionaryObject_::as_dictionary([a-1,c-3,b-2], Dictionary),
		_DictionaryObject_::map([_-Value]>>integer(Value), Dictionary).

	test(dictionary_map_2_02) :-
		_DictionaryObject_::as_dictionary([a-1,c-3,b-2], Dictionary),
		\+ _DictionaryObject_::map([_-Value]>>atom(Value), Dictionary).

	% map/3 tests

	test(dictionary_map_3_01) :-
		_DictionaryObject_::as_dictionary([a-1,c-3,b-2], Dictionary),
		_DictionaryObject_::map([Key-Value, Key-NewValue]>>(integer::succ(Value,NewValue)), Dictionary, NewDictionary),
		findall(OtherKey-OtherValue, _DictionaryObject_::lookup(OtherKey, OtherValue, NewDictionary), OtherPairs),
		^^assertion(pairs, OtherPairs == [a-2, b-3, c-4]).

	% apply/4 tests

	test(dictionary_apply_4_01) :-
		_DictionaryObject_::as_dictionary([a-1,c-3,b-2], Dictionary),
		_DictionaryObject_::apply([Key-Value, Key-NewValue]>>(integer::succ(Value,NewValue)), Dictionary, b, NewDictionary),
		_DictionaryObject_::lookup(b, CurrentValue, NewDictionary),
		^^assertion(value, CurrentValue == 3).

	% size/2 tests

	test(dictionary_size_2_01) :-
		_DictionaryObject_::as_dictionary([], Dictionary),
		_DictionaryObject_::size(Dictionary, Size),
		Size == 0.

	test(dictionary_size_2_02) :-
		_DictionaryObject_::as_dictionary([a-1,c-3,b-2], Dictionary),
		_DictionaryObject_::size(Dictionary, Size),
		Size == 3.

:- end_object.
