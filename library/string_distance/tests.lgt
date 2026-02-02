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


	cover(string_distance(_)).

	% levenshtein/3 tests

	test(levenshtein_3_01, true(Distance == 3)) :-
		string_distance::levenshtein("kitten", "sitting", Distance).

	test(levenshtein_3_02, true(Distance == 3)) :-
		string_distance::levenshtein("Saturday", "Sunday", Distance).

	test(levenshtein_3_03, true(Distance == 0)) :-
		string_distance::levenshtein("", "", Distance).

	test(levenshtein_3_04, true(Distance == 3)) :-
		string_distance::levenshtein("abc", "", Distance).

	test(levenshtein_3_05, true(Distance == 3)) :-
		string_distance::levenshtein("", "xyz", Distance).

	test(levenshtein_3_06, true(Distance == 3)) :-
		string_distance::levenshtein("cat", "dog", Distance).

	% damerau_levenshtein/3 tests

	test(damerau_levenshtein_3_01, true(Distance == 3)) :-
		string_distance::damerau_levenshtein("kitten", "sitting", Distance).

	test(damerau_levenshtein_3_02, true(Distance == 2)) :-
		string_distance::damerau_levenshtein("Saturday", "Sunday", Distance).

	% hamming/3 tests

	test(hamming_3_01, true(Distance == 3)) :-
		string_distance::hamming("kittens", "sitting", Distance).

	% longest_common_substring/3 tests

	test(longest_common_substring_3_01, true(Subsequence == "itt")) :-
		string_distance::longest_common_substring("kitten", "sitting", Subsequence).

	test(longest_common_substring_3_02, true(Subsequence == "a")) :-
		string_distance::longest_common_substring("abc", "axc", Subsequence).

	test(longest_common_substring_3_03, true(Subsequence == "bcde")) :-
		string_distance::longest_common_substring("abcdef", "xbcde", Subsequence).

	% longest_common_subsequence/3 tests

	test(longest_common_subsequence_3_01, true(Subsequence == "a")) :-
		string_distance::longest_common_subsequence("a", "a", Subsequence).

	test(longest_common_subsequence_3_02, true(Subsequence == "ab")) :-
		string_distance::longest_common_subsequence("ab", "ab", Subsequence).

	test(longest_common_subsequence_3_03, true(Subsequence == "a")) :-
		string_distance::longest_common_subsequence("ab", "ba", Subsequence).

	test(longest_common_subsequence_3_04, true(Subsequence == "c")) :-
		string_distance::longest_common_subsequence("abc", "axc", Subsequence).

	test(longest_common_subsequence_3_05, true(Subsequence == "aaa")) :-
		string_distance::longest_common_subsequence("aaa", "aaa", Subsequence).

	% cosine_similarity/3 tests

	test(cosine_similarity_3_01, true(Similarity =~= 0.7071067811865475)) :-
		string_distance::cosine_similarity(["Hello", "World"], ["Hello", "Hello"], Similarity).

	% jaccard_index/3 tests

	test(jaccard_index_3_01, true(Index =~= 0.25)) :-
		string_distance::jaccard_index(["This", "apple", "round", "and", "green"], ["The", "orange", "is", "round", "and", "orange"], Index).

	test(jaccard_index_3_02, true(Index =~= 0.44444444444444444)) :-
		string_distance::jaccard_index(["This", "apple", "round", "and", "is", "a", "fruit"], ["The", "orange", "fruit", "is", "round", "and", "orange"], Index).

	% jaro/3 tests

	test(jaro_3_01, true(Similarity =~= 0.746031746031746)) :-
		string_distance::jaro("kitten", "sitting", Similarity).

	% jaro_winkler/3 tests

	test(jaro_winkler_3_01, true(Similarity =~= 0.746031746031746)) :-
		string_distance::jaro_winkler("kitten", "sitting", Similarity).

	% edit_similarity/3 tests

	test(edit_similarity_3_01, true(Similarity =~= 0.5714285714285714)) :-
		string_distance::edit_similarity("kitten", "sitting", Similarity).

	% edit_similarity/4 tests

	test(edit_similarity_4_01, true(Similarity =~= 0.5714285714285714)) :-
		string_distance::edit_similarity(levenshtein, "kitten", "sitting", Similarity).

	test(edit_similarity_4_02, true(Similarity =~= 0.6666666666666667)) :-
		string_distance::edit_similarity(damerau_levenshtein, "Monday", "Sunday", Similarity).

	test(edit_similarity_4_03, true(Similarity =~= 1.0)) :-
		string_distance::edit_similarity(hamming, "kitten", "kitten", Similarity).

	test(edit_similarity_4_04, true(Similarity =~= 0.4285714285714286)) :-
		string_distance::edit_similarity(longest_common_subsequence, "kitten", "sitting", Similarity).

	% soundex/2 tests

	test(soundex_2_01, true(Code == "K350")) :-
		string_distance::soundex("kitten", Code).

	% metaphone/2 tests

	test(metaphone_2_01, true(Key == "KTTN")) :-
		string_distance::metaphone("kitten", Key).

	% soundex_match/2 tests

	test(soundex_match_2_01, true) :-
		string_distance::soundex_match("kittens", "kitting").

	% metaphone_match/2 tests

	test(metaphone_match_2_01, true) :-
		string_distance::metaphone_match("kitten", "sitting").
