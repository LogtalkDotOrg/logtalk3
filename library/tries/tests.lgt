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


	cover(trie(_)).

	test(trie_new_1_01, deterministic) :-
		trie::new(Trie),
		trie::empty(Trie),
		trie::size(Trie, Size),
		^^assertion(Size == 0).

	test(trie_as_trie_2_01, deterministic) :-
		trie::as_trie(["cat"-1,"car"-2,"cat"-3,""-root], Trie),
		trie::as_list(Trie, Pairs),
		^^assertion(Pairs == [""-root,"car"-2,"cat"-3]).

	test(trie_strings_values_2_01, deterministic) :-
		trie::as_trie(["cat"-1,"car"-2,"dog"-3], Trie),
		trie::strings(Trie, Strings),
		trie::values(Trie, Values),
		^^assertion(strings, Strings == ["car","cat","dog"]),
		^^assertion(values, Values == [2,1,3]).

	test(trie_insert_4_01, deterministic) :-
		trie::new(Trie0),
		trie::insert(Trie0, "cat", 1, Trie1),
		trie::insert(Trie1, "cat", 2, Trie2),
		trie::lookup("cat", OldValue, Trie1),
		trie::lookup("cat", NewValue, Trie2),
		trie::size(Trie2, Size),
		^^assertion(old_value, OldValue == 1),
		^^assertion(new_value, NewValue == 2),
		^^assertion(size, Size == 1).

	test(trie_lookup_3_01, deterministic) :-
		trie::as_trie(["dog"-3,"car"-2,"cat"-1], Trie),
		findall(String-Value, trie::lookup(String, Value, Trie), Pairs),
		^^assertion(Pairs == ["car"-2,"cat"-1,"dog"-3]).

	test(trie_lookup_3_02, false) :-
		trie::new(Trie),
		trie::lookup("missing", _, Trie).

	test(trie_update_4_01, deterministic) :-
		trie::as_trie(["cat"-1], Trie0),
		trie::update(Trie0, "cat", 2, Trie),
		trie::lookup("cat", Value, Trie),
		^^assertion(Value == 2).

	test(trie_update_5_01, deterministic) :-
		trie::as_trie(["cat"-1], Trie0),
		trie::update(Trie0, "cat", OldValue, 2, Trie),
		trie::lookup("cat", NewValue, Trie),
		^^assertion(old_value, OldValue == 1),
		^^assertion(new_value, NewValue == 2).

	test(trie_update_5_02, false) :-
		trie::as_trie(["cat"-1], Trie),
		trie::update(Trie, "cat", 2, 3, _).

	test(trie_delete_4_01, deterministic) :-
		trie::as_trie(["car"-1,"cart"-2], Trie0),
		trie::delete(Trie0, "car", Value, Trie),
		\+ trie::lookup("car", _, Trie),
		trie::lookup("cart", DescendantValue, Trie),
		^^assertion(value, Value == 1),
		^^assertion(descendant_value, DescendantValue == 2).

	test(trie_delete_4_02, false) :-
		trie::new(Trie),
		trie::delete(Trie, "missing", _, _).

	test(trie_as_list_3_01, deterministic) :-
		trie::as_trie(["usage"-1,"users"-2,"users/me"-3,"cat"-4], Trie),
		trie::as_list(Trie, "us", Pairs),
		^^assertion(Pairs == ["usage"-1,"users"-2,"users/me"-3]).

	test(trie_as_list_3_02, false) :-
		trie::as_trie(["cat"-1], Trie),
		trie::as_list(Trie, "dog", _).

	test(trie_strings_values_3_01, deterministic) :-
		trie::as_trie(["usage"-1,"users"-2,"users/me"-3,"cat"-4], Trie),
		trie::strings(Trie, "users", Strings),
		trie::values(Trie, "users", Values),
		^^assertion(strings, Strings == ["users","users/me"]),
		^^assertion(values, Values == [2,3]).

	test(trie_lookup_prefix_4_01, deterministic) :-
		trie::as_trie(["usage"-1,"users"-2,"users/me"-3,"cat"-4], Trie),
		findall(String-Value, trie::lookup_prefix("us", String, Value, Trie), Pairs),
		^^assertion(Pairs == ["usage"-1,"users"-2,"users/me"-3]).

	test(trie_lookup_prefix_4_02, deterministic) :-
		trie::as_trie([
			"electroencephalogram"-'record of brain electrical activity',
			"electroencephalograph"-'instrument for recording brain electrical activity',
			"electroencephalographic"-'relating to electroencephalography',
			"electroencephalography"-'recording of brain electrical activity'
		], Trie),
		findall(Word-Definition, trie::lookup_prefix("electroencephalogra", Word, Definition, Trie), Pairs),
		^^assertion(Pairs == [
			"electroencephalogram"-'record of brain electrical activity',
			"electroencephalograph"-'instrument for recording brain electrical activity',
			"electroencephalographic"-'relating to electroencephalography',
			"electroencephalography"-'recording of brain electrical activity'
		]).

	test(trie_prefixes_3_01, deterministic) :-
		trie::as_trie([""-root,"users"-users,"users/me"-profile], Trie),
		trie::prefixes(Trie, "users/me/settings", Pairs),
		^^assertion(Pairs == [""-root,"users"-users,"users/me"-profile]).

	test(trie_prefixes_3_02, deterministic) :-
		trie::as_trie(["cat"-1], Trie),
		trie::prefixes(Trie, "dog", Pairs),
		^^assertion(Pairs == []).

	test(trie_longest_prefix_4_01, deterministic) :-
		trie::as_trie([""-root,"users"-users,"users/me"-profile], Trie),
		trie::longest_prefix(Trie, "users/me/settings", Prefix, Value),
		^^assertion(Prefix-Value == "users/me"-profile).

	test(trie_longest_prefix_4_02, false) :-
		trie::as_trie(["cat"-1], Trie),
		trie::longest_prefix(Trie, "dog", _, _).

	test(trie_delete_prefix_3_01, deterministic) :-
		trie::as_trie(["usage"-1,"users"-2,"users/me"-3,"cat"-4], Trie0),
		trie::delete_prefix(Trie0, "us", Trie),
		trie::as_list(Trie, Pairs),
		trie::size(Trie, Size),
		^^assertion(pairs, Pairs == ["cat"-4]),
		^^assertion(size, Size == 1).

	test(trie_delete_prefix_3_02, deterministic) :-
		trie::as_trie([""-root,"cat"-1], Trie0),
		trie::delete_prefix(Trie0, "", Trie),
		trie::empty(Trie).

	test(trie_delete_prefix_3_03, false) :-
		trie::as_trie(["cat"-1], Trie),
		trie::delete_prefix(Trie, "dog", _).
