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


:- object(lru_cache,
	implements(lru_cache_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-08,
		comment is 'Immutable LRU (Least Recently Used) caches.',
		see_also is [lru_cache_protocol]
	]).

	:- uses(avltree, [
		new/1 as new_dictionary/1, lookup/3 as dictionary_lookup/3, insert/4 as dictionary_insert/4,
		update/5 as dictionary_update/5, delete/4 as dictionary_delete/4, delete_min/4 as dictionary_delete_min/4,
		as_list/2 as dictionary_as_list/2
	]).

	:- uses(list, [
		reverse/2
	]).

	:- uses(type, [
		check/3
	]).

	new(Capacity, lru_cache(Capacity, 0, 0, Entries, Recency)) :-
		context(Context),
		check(non_negative_integer, Capacity, Context),
		new_dictionary(Entries),
		new_dictionary(Recency).

	capacity(lru_cache(Capacity, _, _, _, _), Capacity).

	size(lru_cache(_, Size, _, _, _), Size).

	empty(lru_cache(_, 0, _, _, _)).

	lookup(Key, Value, lru_cache(_, _, _, Entries, _)) :-
		check_key(Key),
		dictionary_lookup(Key, entry(Value, _), Entries).

	get(Key, Value, Cache, NewCache) :-
		check_key(Key),
		Cache = lru_cache(_, _, _, Entries, _),
		dictionary_lookup(Key, entry(Value, Stamp), Entries),
		promote(Key, Value, Stamp, Cache, NewCache).

	put(Key, Value, Cache, NewCache) :-
		check_key(Key),
		Cache = lru_cache(Capacity, _, _, Entries, _),
		(	Capacity =:= 0 ->
			NewCache = Cache
		;	dictionary_lookup(Key, entry(_, Stamp), Entries) ->
			promote(Key, Value, Stamp, Cache, NewCache)
		;	put_new(Key, Value, Cache, NewCache)
		).

	update(Key, OldValue, NewValue, Cache, NewCache) :-
		check_key(Key),
		Cache = lru_cache(_, _, _, Entries, _),
		dictionary_lookup(Key, entry(OldValue, Stamp), Entries),
		promote(Key, NewValue, Stamp, Cache, NewCache).

	delete(Key, Value, lru_cache(Capacity, Size0, NextStamp, Entries0, Recency0), lru_cache(Capacity, Size, NextStamp, Entries, Recency)) :-
		check_key(Key),
		dictionary_lookup(Key, entry(Value, Stamp), Entries0),
		dictionary_delete(Entries0, Key, entry(Value, Stamp), Entries),
		dictionary_delete(Recency0, Stamp, Key-Value, Recency),
		Size is Size0 - 1.

	evict(lru_cache(Capacity, Size0, NextStamp, Entries0, Recency0), Key-Value, lru_cache(Capacity, Size, NextStamp, Entries, Recency)) :-
		dictionary_delete_min(Recency0, _Stamp, Key-Value, Recency),
		dictionary_delete(Entries0, Key, entry(Value, _), Entries),
		Size is Size0 - 1.

	as_list(lru_cache(_, _, _, _, Recency), Pairs) :-
		dictionary_as_list(Recency, StampedPairs),
		reverse(StampedPairs, ReverseStampedPairs),
		strip_stamps(ReverseStampedPairs, Pairs).

	clear(lru_cache(Capacity, _, _, _, _), lru_cache(Capacity, 0, 0, Entries, Recency)) :-
		new_dictionary(Entries),
		new_dictionary(Recency).

	check_key(Key) :-
		context(Context),
		check(ground, Key, Context).

	promote(Key, Value, OldStamp, lru_cache(Capacity, Size, Stamp, Entries0, Recency0), lru_cache(Capacity, Size, NextStamp, Entries, Recency)) :-
		dictionary_lookup(OldStamp, Key-OldValue, Recency0),
		dictionary_delete(Recency0, OldStamp, Key-OldValue, Recency1),
		dictionary_insert(Recency1, Stamp, Key-Value, Recency),
		dictionary_update(Entries0, Key, entry(OldValue, OldStamp), entry(Value, Stamp), Entries),
		NextStamp is Stamp + 1.

	put_new(Key, Value, Cache0, Cache) :-
		Cache0 = lru_cache(Capacity, Size0, _, _, _),
		(	Size0 =:= Capacity ->
			evict(Cache0, _, Cache1)
		;	Cache1 = Cache0
		),
		insert_new(Key, Value, Cache1, Cache).

	insert_new(Key, Value, lru_cache(Capacity, Size0, Stamp, Entries0, Recency0), lru_cache(Capacity, Size, NextStamp, Entries, Recency)) :-
		dictionary_insert(Entries0, Key, entry(Value, Stamp), Entries),
		dictionary_insert(Recency0, Stamp, Key-Value, Recency),
		Size is Size0 + 1,
		NextStamp is Stamp + 1.

	strip_stamps([], []).
	strip_stamps([_-Pair| StampedPairs], [Pair| Pairs]) :-
		strip_stamps(StampedPairs, Pairs).

:- end_object.
