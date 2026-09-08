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
		date is 2026-09-08,
		comment is 'Unit tests for the "lru_caches" library.'
	]).

	:- uses(lru_cache, [
		new/2, capacity/2, size/2, empty/1, lookup/3, get/4,
		put/4, update/5, delete/4, evict/3, as_list/2, clear/2
	]).

	cover(lru_cache).

	test(lru_cache_new_2_01, deterministic(List == [])) :-
		new(2, Cache),
		capacity(Cache, 2),
		size(Cache, 0),
		empty(Cache),
		as_list(Cache, List).

	test(lru_cache_new_2_02, error(instantiation_error)) :-
		new(_, _).

	test(lru_cache_new_2_03, error(type_error(integer, two))) :-
		new(two, _).

	test(lru_cache_new_2_04, error(domain_error(non_negative_integer, -1))) :-
		new(-1, _).

	test(lru_cache_put_4_01, deterministic(List == [b-2,a-1])) :-
		new(2, Cache0),
		put(a, 1, Cache0, Cache1),
		put(b, 2, Cache1, Cache2),
		size(Cache2, 2),
		as_list(Cache2, List).

	test(lru_cache_put_4_02, deterministic(List == [a-3,b-2])) :-
		new(2, Cache0),
		put(a, 1, Cache0, Cache1),
		put(b, 2, Cache1, Cache2),
		put(a, 3, Cache2, Cache3),
		size(Cache3, 2),
		as_list(Cache3, List).

	test(lru_cache_put_4_03, deterministic(List == [c-3,b-2])) :-
		new(2, Cache0),
		put(a, 1, Cache0, Cache1),
		put(b, 2, Cache1, Cache2),
		put(c, 3, Cache2, Cache3),
		\+ lookup(a, _, Cache3),
		as_list(Cache3, List).

	test(lru_cache_put_4_04, deterministic(Cache == Cache0)) :-
		new(0, Cache0),
		put(a, 1, Cache0, Cache).

	test(lru_cache_put_4_05, deterministic(List == [b-2])) :-
		new(1, Cache0),
		put(a, 1, Cache0, Cache1),
		put(b, 2, Cache1, Cache2),
		as_list(Cache2, List).

	test(lru_cache_put_4_06, deterministic) :-
		new(2, Cache0),
		put(key(1), value(_), Cache0, Cache),
		lookup(key(1), value(_), Cache).

	test(lru_cache_put_4_07, error(instantiation_error)) :-
		new(1, Cache0),
		put(key(_), 1, Cache0, _).

	test(lru_cache_lookup_3_01, deterministic(List == [b-2,a-1])) :-
		new(2, Cache0),
		put(a, 1, Cache0, Cache1),
		put(b, 2, Cache1, Cache2),
		lookup(a, 1, Cache2),
		as_list(Cache2, List).

	test(lru_cache_get_4_01, deterministic(List == [a-1,b-2])) :-
		new(2, Cache0),
		put(a, 1, Cache0, Cache1),
		put(b, 2, Cache1, Cache2),
		get(a, 1, Cache2, Cache3),
		as_list(Cache3, List).

	test(lru_cache_get_4_02, false) :-
		new(1, Cache0),
		put(a, 1, Cache0, Cache),
		get(a, 2, Cache, _).

	test(lru_cache_get_4_03, false) :-
		new(1, Cache),
		get(a, _, Cache, _).

	test(lru_cache_update_5_01, deterministic(List == [a-3,b-2])) :-
		new(2, Cache0),
		put(a, 1, Cache0, Cache1),
		put(b, 2, Cache1, Cache2),
		update(a, 1, 3, Cache2, Cache3),
		as_list(Cache3, List).

	test(lru_cache_update_5_02, false) :-
		new(1, Cache0),
		put(a, 1, Cache0, Cache),
		update(a, 2, 3, Cache, _).

	test(lru_cache_update_5_03, false) :-
		new(1, Cache),
		update(a, _, 1, Cache, _).

	test(lru_cache_delete_4_01, deterministic(List == [b-2])) :-
		new(2, Cache0),
		put(a, 1, Cache0, Cache1),
		put(b, 2, Cache1, Cache2),
		delete(a, 1, Cache2, Cache3),
		size(Cache3, 1),
		as_list(Cache3, List).

	test(lru_cache_delete_4_02, false) :-
		new(1, Cache0),
		put(a, 1, Cache0, Cache),
		delete(a, 2, Cache, _).

	test(lru_cache_delete_4_03, false) :-
		new(1, Cache),
		delete(a, _, Cache, _).

	test(lru_cache_evict_3_01, deterministic(List == [b-2])) :-
		new(2, Cache0),
		put(a, 1, Cache0, Cache1),
		put(b, 2, Cache1, Cache2),
		evict(Cache2, Pair, Cache3),
		Pair == a-1,
		as_list(Cache3, List).

	test(lru_cache_evict_3_02, false) :-
		new(1, Cache),
		evict(Cache, _, _).

	test(lru_cache_clear_2_01, deterministic(empty(Cache))) :-
		new(2, Cache0),
		put(a, 1, Cache0, Cache1),
		clear(Cache1, Cache),
		capacity(Cache, 2).

	test(lru_cache_persistence_01, deterministic(List1-List2 == [a-1]-[b-2,a-1])) :-
		new(2, Cache0),
		put(a, 1, Cache0, Cache1),
		put(b, 2, Cache1, Cache2),
		as_list(Cache1, List1),
		as_list(Cache2, List2).

:- end_object.
