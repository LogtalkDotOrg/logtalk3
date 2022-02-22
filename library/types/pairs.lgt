%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- object(pairs).

	:- info([
		version is 2:0:0,
		date is 2021-02-22,
		author is 'Paulo Moura',
		comment is 'Useful predicates over lists of pairs (key-value terms).',
		remarks is [
			'Usage' - 'This object can be loaded independently of other entities in the ``types`` library by using the goal ``logtalk_load(types(pairs))``.'
		]
	]).

	:- public(keys_values/3).
	:- mode(keys_values(+list(pair), -list, -list), one).
	:- mode(keys_values(-list(pair), +list, +list), one).
	:- info(keys_values/3, [
		comment is 'Converts between a list of pairs and lists of keys and values.',
		argnames is ['Pairs', 'Keys', 'Values']
	]).

	:- public(keys/2).
	:- mode(keys(+list(pair), -list), one).
	:- info(keys/2, [
		comment is 'Extracts a list of keys from a list of pairs.',
		argnames is ['Pairs', 'Keys']
	]).

	:- public(values/2).
	:- mode(values(+list(pair), -list), one).
	:- info(values/2, [
		comment is 'Extracts a list of values from a list of pairs.',
		argnames is ['Pairs', 'Values']
	]).

	:- public(transpose/2).
	:- mode(transpose(+list(pair), -list(pair)), one).
	:- info(transpose/2, [
		comment is 'Transposes a list of pairs by swapping each pair key and value. The relative order of the list elements is kept.',
		argnames is ['Pairs', 'TransposedPairs']
	]).

	:- public(group_sorted_by_key/2).
	:- mode(group_sorted_by_key(+list(pair), -list(pair)), one).
	:- info(group_sorted_by_key/2, [
		comment is 'Groups pairs by key by sorting them and then constructing new pairs by grouping all values for a given key in a list. Keys are compared using equality. Relative order of values per key is kept. Resulting list of pairs is sorted by key.',
		argnames is ['Pairs', 'Groups']
	]).

	:- public(group_consecutive_by_key/2).
	:- mode(group_consecutive_by_key(+list(pair), -list(pair)), one).
	:- info(group_consecutive_by_key/2, [
		comment is 'Groups pairs by constructing new pairs by grouping all values for consecutive key in a list. Keys are compared using equality. The relative order of the values for the same key is kept.',
		argnames is ['Pairs', 'Groups']
	]).

	:- public(group_by_key/2).
	:- mode(group_by_key(+list(pair), -list(pair)), one).
	:- info(group_by_key/2, [
		comment is 'Same as the ``group_sorted_by_key/2`` predicate. Deprecated.',
		argnames is ['Pairs', 'Groups']
	]).

	:- public(map/3).
	:- meta_predicate(map(2, *, *)).
	:- mode(map(@callable, +list, -list(pair)), one).
	:- info(map/3, [
		comment is 'Maps a list into pairs using a closure that applies to each list element to compute its key.',
		argnames is ['Closure', 'List', 'Pairs']
	]).

	keys_values(Pairs, Keys, Values) :-
		(	nonvar(Pairs) ->
			pairs_to_keys_values(Pairs, Keys, Values)
		;	nonvar(Keys), nonvar(Values),
			keys_values_to_pairs(Keys, Values, Pairs)
		).

	pairs_to_keys_values([], [], []).
	pairs_to_keys_values([Key-Value| Pairs], [Key| Keys], [Value| Values]) :-
		pairs_to_keys_values(Pairs, Keys, Values).

	keys_values_to_pairs([], [], []).
	keys_values_to_pairs([Key| Keys], [Value| Values], [Key-Value| Pairs]) :-
		keys_values_to_pairs(Keys, Values, Pairs).

	keys([], []).
	keys([Key-_| Pairs], [Key| Keys]) :-
		keys(Pairs, Keys).

	values([], []).
	values([_-Value| Pairs], [Value| Values]) :-
		values(Pairs, Values).

	transpose([], []).
	transpose([Key-Value| Pairs], [Value-Key| TransposedPairs]) :-
		transpose(Pairs, TransposedPairs).

	group_sorted_by_key(Pairs, Groups) :-
		keysort(Pairs, PairsSorted),
		group_sorted(PairsSorted, Groups).

	group_sorted([], []).
	group_sorted([Key-Value| Pairs], [Key-[Value| Values]| Groups]) :-
		group_sorted(Pairs, Key, Values, Groups).

	group_sorted([], _, [], []).
	group_sorted([Key-Value| Pairs], LookupKey, [Value| Values], Groups) :-
		Key == LookupKey,
		!,
		group_sorted(Pairs, LookupKey, Values, Groups).
	group_sorted([Key-Value| Pairs], _, [], [Key-[Value| Values]| Groups]) :-
		group_sorted(Pairs, Key, Values, Groups).

	group_consecutive_by_key([], []).
	group_consecutive_by_key([Key-Value| KeyValues0], [Key-[Value| Values]| KeyValues]) :-
        equal_key(Key, KeyValues0, Values, KeyValues1),
        group_consecutive_by_key(KeyValues1, KeyValues).

	equal_key(Key, [Key0-Value| KeyValues0], [Value| Values], KeyValues) :-
		Key == Key0,
		!,
		equal_key(Key, KeyValues0, Values, KeyValues).
	equal_key(_, KeyValues, [], KeyValues).

	% deprecated
	group_by_key(Pairs, Groups) :-
		group_sorted_by_key(Pairs, Groups).

	map(Closure, List, Pairs) :-
		map_(List, Closure, Pairs).

	:- meta_predicate(map_(*, 2, *)).
	map_([], _, []).
	map_([Item| Items], Closure, [Key-Item| Pairs]) :-
		call(Closure, Item, Key),
		map_(Items, Closure, Pairs).

:- end_object.
