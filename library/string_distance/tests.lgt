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


	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	cover(string_distance(_)).

	% levenshtein/3 tests

	test(levenshtein_3_01, deterministic(Distance == 3)) :-
		string_distance::levenshtein("kitten", "sitting", Distance).

	test(levenshtein_3_02, deterministic(Distance == 3)) :-
		string_distance::levenshtein("Saturday", "Sunday", Distance).

	test(levenshtein_3_03, deterministic(Distance == 0)) :-
		string_distance::levenshtein("", "", Distance).

	test(levenshtein_3_04, deterministic(Distance == 3)) :-
		string_distance::levenshtein("abc", "", Distance).

	test(levenshtein_3_05, deterministic(Distance == 3)) :-
		string_distance::levenshtein("", "xyz", Distance).

	test(levenshtein_3_06, deterministic(Distance == 3)) :-
		string_distance::levenshtein("cat", "dog", Distance).

	% damerau_levenshtein/3 tests

	test(damerau_levenshtein_3_01, deterministic(Distance == 3)) :-
		string_distance::damerau_levenshtein("kitten", "sitting", Distance).

	test(damerau_levenshtein_3_02, deterministic(Distance == 2)) :-
		string_distance::damerau_levenshtein("Saturday", "Sunday", Distance).

	% hamming/3 tests

	test(hamming_3_01, deterministic(Distance == 3)) :-
		string_distance::hamming("kittens", "sitting", Distance).

	% longest_common_substring/3 tests

	test(longest_common_substring_3_01, deterministic(Subsequence == "itt")) :-
		string_distance::longest_common_substring("kitten", "sitting", Subsequence).

	test(longest_common_substring_3_02, deterministic(Subsequence == "a")) :-
		string_distance::longest_common_substring("abc", "axc", Subsequence).

	test(longest_common_substring_3_03, deterministic(Subsequence == "bcde")) :-
		string_distance::longest_common_substring("abcdef", "xbcde", Subsequence).

	% longest_common_subsequence/3 tests

	test(longest_common_subsequence_3_01, deterministic(Subsequence == "a")) :-
		string_distance::longest_common_subsequence("a", "a", Subsequence).

	test(longest_common_subsequence_3_02, deterministic(Subsequence == "ab")) :-
		string_distance::longest_common_subsequence("ab", "ab", Subsequence).

	test(longest_common_subsequence_3_03, deterministic(Subsequence == "a")) :-
		string_distance::longest_common_subsequence("ab", "ba", Subsequence).

	test(longest_common_subsequence_3_04, deterministic(Subsequence == "c")) :-
		string_distance::longest_common_subsequence("abc", "axc", Subsequence).

	test(longest_common_subsequence_3_05, deterministic(Subsequence == "aaa")) :-
		string_distance::longest_common_subsequence("aaa", "aaa", Subsequence).

	% cosine_similarity/3 tests

	test(cosine_similarity_3_01, deterministic(Similarity =~= 0.7071067811865475)) :-
		string_distance::cosine_similarity(["Hello", "World"], ["Hello", "Hello"], Similarity).

	% jaccard_index/3 tests

	test(jaccard_index_3_01, deterministic(Index =~= 0.25)) :-
		string_distance::jaccard_index(["This", "apple", "round", "and", "green"], ["The", "orange", "is", "round", "and", "orange"], Index).

	test(jaccard_index_3_02, deterministic(Index =~= 0.44444444444444444)) :-
		string_distance::jaccard_index(["This", "apple", "round", "and", "is", "a", "fruit"], ["The", "orange", "fruit", "is", "round", "and", "orange"], Index).

	% jaro/3 tests

	test(jaro_3_01, deterministic(Similarity =~= 0.746031746031746)) :-
		string_distance::jaro("kitten", "sitting", Similarity).

	% jaro_winkler/3 tests

	test(jaro_winkler_3_01, deterministic(Similarity =~= 0.746031746031746)) :-
		string_distance::jaro_winkler("kitten", "sitting", Similarity).

	% edit_similarity/3 tests

	test(edit_similarity_3_01, deterministic(Similarity =~= 0.5714285714285714)) :-
		string_distance::edit_similarity("kitten", "sitting", Similarity).

	% edit_similarity/4 tests

	test(edit_similarity_4_01, deterministic(Similarity =~= 0.5714285714285714)) :-
		string_distance::edit_similarity(levenshtein, "kitten", "sitting", Similarity).

	test(edit_similarity_4_02, deterministic(Similarity =~= 0.6666666666666667)) :-
		string_distance::edit_similarity(damerau_levenshtein, "Monday", "Sunday", Similarity).

	test(edit_similarity_4_03, deterministic(Similarity =~= 1.0)) :-
		string_distance::edit_similarity(hamming, "kitten", "kitten", Similarity).

	test(edit_similarity_4_04, deterministic(Similarity =~= 0.4285714285714286)) :-
		string_distance::edit_similarity(longest_common_subsequence, "kitten", "sitting", Similarity).

	% soundex/2 tests

	test(soundex_2_01, deterministic(Code == "K350")) :-
		string_distance::soundex("kitten", Code).

	% nysiis/2 tests

	test(nysiis_2_01, deterministic(Key == "MCDANA")) :-
		string_distance::nysiis("Macdonald", Key).

	test(nysiis_2_02, deterministic(Key == "FASTAR")) :-
		string_distance::nysiis("Pfister", Key).

	test(nysiis_2_03, deterministic(Key == "SNAT")) :-
		string_distance::nysiis("Smith", Key).

	test(nysiis_2_04, deterministic(Key == "SNAD")) :-
		string_distance::nysiis("Schmidt", Key).

	test(nysiis_2_05, deterministic(Key == "")) :-
		string_distance::nysiis("", Key).

	test(nysiis_2_06, deterministic(Key == "ODANAL")) :-
		string_distance::nysiis("O'Daniel", Key).

	test(nysiis_2_07, deterministic(Key == "WASTAR")) :-
		string_distance::nysiis("Westerlund", Key).

	test(nysiis_2_08, deterministic(Key == "CARY")) :-
		string_distance::nysiis("Carraway", Key).

	test(nysiis_2_09, deterministic(Key == "DAD")) :-
		string_distance::nysiis("Dent", Key).

	test(nysiis_2_10, deterministic(Key == "MCANT")) :-
		string_distance::nysiis("Macintosh", Key).

	test(nysiis_2_11, deterministic(Key == "NX")) :-
		string_distance::nysiis("Knx", Key).

	test(nysiis_2_12, deterministic(Key == "FX")) :-
		string_distance::nysiis("Phx", Key).

	test(nysiis_2_13, deterministic(Key == "XY")) :-
		string_distance::nysiis("Xee", Key).

	test(nysiis_2_14, deterministic(Key == "XAF")) :-
		string_distance::nysiis("Xev", Key).

	test(nysiis_2_15, deterministic(Key == "XG")) :-
		string_distance::nysiis("Xq", Key).

	test(nysiis_2_16, deterministic(Key == "X")) :-
		string_distance::nysiis("Xz", Key).

	% metaphone/2 tests

	test(metaphone_2_01, deterministic(Key == "JS")) :-
		string_distance::metaphone("jose", Key).

	test(metaphone_2_02, deterministic(Key == "HS")) :-
		string_distance::metaphone("hose", Key).

	test(metaphone_2_03, deterministic(Key == "KTN")) :-
		string_distance::metaphone("kitten", Key).

	test(metaphone_2_04, deterministic(Key == "APL")) :-
		string_distance::metaphone("apple", Key).

	test(metaphone_2_05, deterministic(Key == "BK")) :-
		string_distance::metaphone("back", Key).

	test(metaphone_2_06, deterministic(Key == "TM")) :-
		string_distance::metaphone("dumb", Key).

	test(metaphone_2_07, deterministic(Key == "KX")) :-
		string_distance::metaphone("catch", Key).

	test(metaphone_2_08, deterministic(Key == "SFR")) :-
		string_distance::metaphone("xavier", Key).

	% double_metaphone/3 tests
	% Expected encodings are derived from the Philips/Atkinson C++ reference
	% implementation. Other implementations are secondary cross-checks only.

	test(double_metaphone_3_01, deterministic((Primary == "HS", Alternative == "HS"))) :-
		string_distance::double_metaphone("jose", Primary, Alternative).

	test(double_metaphone_3_02, deterministic((Primary == "HS", Alternative == "HS"))) :-
		string_distance::double_metaphone("hose", Primary, Alternative).

	test(double_metaphone_3_03, deterministic((Primary == "KTN", Alternative == "KTN"))) :-
		string_distance::double_metaphone("kitten", Primary, Alternative).

	test(double_metaphone_3_04, deterministic((Primary == "JN", Alternative == "AN"))) :-
		string_distance::double_metaphone("john", Primary, Alternative).

	test(double_metaphone_3_05, deterministic((Primary == "XMT", Alternative == "SMT"))) :-
		string_distance::double_metaphone("schmidt", Primary, Alternative).

	test(double_metaphone_3_06, deterministic((Primary == "K0RN", Alternative == "KTRN"))) :-
		string_distance::double_metaphone("catherine", Primary, Alternative).

	test(double_metaphone_3_07, deterministic((Primary == "K0RN", Alternative == "KTRN"))) :-
		string_distance::double_metaphone("katherine", Primary, Alternative).

	test(double_metaphone_3_08, deterministic((Primary == "STFN", Alternative == "STFN"))) :-
		string_distance::double_metaphone("steven", Primary, Alternative).

	test(double_metaphone_3_09, deterministic((Primary == "STFN", Alternative == "STFN"))) :-
		string_distance::double_metaphone("stephen", Primary, Alternative).

	test(double_metaphone_3_10, deterministic((Primary == "AP", Alternative == "AP"))) :-
		string_distance::double_metaphone("Abbe", Primary, Alternative).

	test(double_metaphone_3_11, deterministic((Primary == "SM0", Alternative == "XMT"))) :-
		string_distance::double_metaphone("Smith", Primary, Alternative).

	test(double_metaphone_3_12, deterministic((Primary == "XMT", Alternative == "SMT"))) :-
		string_distance::double_metaphone("Schmidt", Primary, Alternative).

	test(double_metaphone_3_13, deterministic((Primary == "AKXN", Alternative == "AKXN"))) :-
		string_distance::double_metaphone("action", Primary, Alternative).

	% soundex_match/2 tests

	test(soundex_match_2_01, true) :-
		string_distance::soundex_match("kittens", "kitting").

	% nysiis_match/2 tests

	test(nysiis_match_2_01, true) :-
		string_distance::nysiis_match("Smith", "Schmit").

	test(nysiis_match_2_02, false) :-
		string_distance::nysiis_match("Smith", "Schmidt").

	% metaphone_match/2 tests

	test(metaphone_match_2_01, false) :-
		string_distance::metaphone_match("kitten", "sitting").

	test(metaphone_match_2_02, false) :-
		string_distance::metaphone_match("jose", "hose").

	% double_metaphone_match/2 tests

	test(double_metaphone_match_2_01, true) :-
		string_distance::double_metaphone_match("jose", "hose").

	test(double_metaphone_match_2_02, true) :-
		string_distance::double_metaphone_match("catherine", "katherine").

	test(double_metaphone_match_2_03, true) :-
		string_distance::double_metaphone_match("steven", "stephen").
