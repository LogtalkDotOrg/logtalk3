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


	cover(n_grams(_)).

	test(n_grams_3_01, deterministic(NGrams == [["the", "quick"], ["quick", "brown"], ["brown", "fox"]])) :-
		n_grams::n_grams(2, ["the", "quick", "brown", "fox"], NGrams).

	test(n_grams_3_02, deterministic(NGrams == [])) :-
		n_grams::n_grams(3, ["a", "b"], NGrams).

	test(n_grams_3_03, deterministic(NGrams == [["a"]])) :-
		n_grams::n_grams(1, ["a"], NGrams).

	test(n_grams_4_01, deterministic(NGrams == [["<pad>", "a"], ["a", "b"], ["b", "<pad>"]])) :-
		n_grams::n_grams(2, ["a", "b"], NGrams, [padding(both("<pad>"))]).

	test(n_grams_4_02, deterministic(NGrams == [["a", "b"], ["c", "d"]])) :-
		n_grams::n_grams(2, ["a", "b", "c", "d", "e"], NGrams, [step(2)]).

	test(n_grams_4_03, deterministic(NGrams == [["<pad>", "a"], ["a", "b"]])) :-
		n_grams::n_grams(2, ["a", "b"], NGrams, [padding(left("<pad>"))]).

	test(n_grams_4_04, deterministic(NGrams == [["a", "b"], ["b", "<pad>"]])) :-
		n_grams::n_grams(2, ["a", "b"], NGrams, [padding(right("<pad>"))]).

	test(n_grams_4_05, deterministic(NGrams == [["a"], ["b"]])) :-
		n_grams::n_grams(1, ["a", "b"], NGrams, [padding(both("<pad>"))]).

	test(n_grams_4_06, deterministic(NGrams == [["a", "b"]])) :-
		n_grams::n_grams(2, ["a", "b", "c"], NGrams, [step(10)]).

	test(character_n_grams_3_01, deterministic(NGrams == ["hel", "ell", "llo"])) :-
		n_grams::character_n_grams(3, "hello", NGrams).

	test(character_n_grams_3_02, deterministic(NGrams == [])) :-
		n_grams::character_n_grams(2, "", NGrams).

	test(character_n_grams_4_01, deterministic(NGrams == ["^h", "hi", "i^"])) :-
		character_padding_marker(Marker),
		n_grams::character_n_grams(2, "hi", NGrams, [padding(both(Marker))]).

	test(character_n_grams_4_02, deterministic(NGrams == ["he", "ll"])) :-
		n_grams::character_n_grams(2, "hello", NGrams, [step(2)]).

	test(count_2_01, deterministic(Counts == ["ab"-2, "ba"-2, "ca"-1])) :-
		n_grams::count(["ab", "ba", "ab", "ca", "ba"], Counts).

	test(count_2_02, deterministic(Counts == [])) :-
		n_grams::count([], Counts).

	test(count_3_01, deterministic(Counts == ["ab"-2, "ba"-2, "ca"-1])) :-
		n_grams::count(standard, ["ca", "ba", "ab", "ba", "ab"], Counts).

	test(count_3_02, deterministic(Counts == ["ab"-2, "ba"-2, "ca"-1])) :-
		n_grams::count(frequency_descending, ["ca", "ba", "ab", "ba", "ab"], Counts).

	test(count_3_03, deterministic(Counts == [["a", "b"]-2, ["b", "a"]-1])) :-
		n_grams::count([["a", "b"], ["b", "a"], ["a", "b"]], Counts).

	test(count_3_04, deterministic(Counts == ["ca"-1, "ba"-2, "ab"-2])) :-
		n_grams::count(first_occurrence, ["ca", "ba", "ab", "ba", "ab"], Counts).

	test(bigrams_2_01, deterministic(Bigrams == [["a", "b"], ["b", "c"]])) :-
		n_grams::bigrams(["a", "b", "c"], Bigrams).

	test(trigrams_2_01, deterministic(Trigrams == [["a", "b", "c"]])) :-
		n_grams::trigrams(["a", "b", "c"], Trigrams).

	test(n_grams_invalid_n_01, ball(instantiation_error)) :-
		n_grams::n_grams(_, ["a"], _).

	test(n_grams_invalid_n_02, ball(type_error(integer, atom))) :-
		n_grams::n_grams(atom, ["a"], _).

	test(n_grams_invalid_n_03, ball(domain_error(positive_integer, 0))) :-
		n_grams::n_grams(0, ["a"], _).

	test(n_grams_invalid_options_01, error(instantiation_error)) :-
		n_grams::n_grams(2, ["a"], _, _).

	test(n_grams_invalid_options_02, error(domain_error(option, step(0)))) :-
		n_grams::n_grams(2, ["a"], _, [step(0)]).

	test(n_grams_invalid_options_03, error(domain_error(option, unknown(true)))) :-
		n_grams::n_grams(2, ["a"], _, [unknown(true)]).

	test(count_invalid_order_01, error(instantiation_error)) :-
		n_grams::count(_, [], _).

	test(count_invalid_order_02, error(domain_error(count_order, unsupported))) :-
		n_grams::count(unsupported, [], _).

	test(n_grams_invalid_representation_01, error(domain_error(text_representation, unsupported))) :-
		n_grams(unsupported)::n_grams(2, [], _).
