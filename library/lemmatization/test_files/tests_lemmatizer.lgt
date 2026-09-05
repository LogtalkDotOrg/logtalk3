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


	cover(lemmatizer(_, _)).

	test(lemmatizer_lemma_2_irregular_noun, deterministic(Lemma == "mouse")) :-
		lemmatizer::lemma("mice", Lemma).

	test(lemmatizer_lemma_2_doubled_consonant, deterministic(Lemma == "run")) :-
		lemmatizer::lemma("running", Lemma).

	test(lemmatizer_lemma_2_silent_e, deterministic(Lemma == "make")) :-
		lemmatizer::lemma("making", Lemma).

	test(lemmatizer_lemma_2_y_restoration, deterministic(Lemma == "study")) :-
		lemmatizer::lemma("studied", Lemma).

	test(lemmatizer_lemma_2_case_normalization, deterministic(Lemma == "run")) :-
		lemmatizer::lemma("RUNNING", Lemma).

	test(lemmatizer_lemma_2_unknown_identity, deterministic(Lemma == "unknown")) :-
		lemmatizer::lemma("UNKNOWN", Lemma).

	test(lemmatizer_lemma_2_ambiguity, deterministic(Lemmas == ["saw", "see"])) :-
		findall(Lemma, tiny_lemmatizer::lemma("saw", Lemma), Lemmas).

	test(lemmatizer_lemma_3_empty_options, deterministic(Lemmas == ["saw", "see"])) :-
		findall(Lemma, tiny_lemmatizer::lemma("saw", Lemma, []), Lemmas).

	test(lemmatizer_lemma_3_pos_noun, deterministic(Lemma == "saw")) :-
		tiny_lemmatizer::lemma("saw", Lemma, [part_of_speech(noun)]).

	test(lemmatizer_lemma_3_pos_verb, deterministic(Lemma == "see")) :-
		tiny_lemmatizer::lemma("saw", Lemma, [part_of_speech(verb)]).

	test(lemmatizer_lemma_3_first, deterministic(Lemma == "saw")) :-
		tiny_lemmatizer::lemma("saw", Lemma, [ambiguity(first)]).

	test(lemmatizer_lemma_3_unknown_fail, false) :-
		tiny_lemmatizer::lemma("unknown", _, [unknown(fail)]).

	test(lemmatizer_lemma_3_scalar_parts_of_speech, error(domain_error(option, parts_of_speech(_)))) :-
		tiny_lemmatizer::lemma("saw", _, [parts_of_speech([noun])]).

	test(lemmatizer_lemma_3_invalid_ambiguity, error(domain_error(option, ambiguity(random)))) :-
		tiny_lemmatizer::lemma("saw", _, [ambiguity(random)]).

	test(lemmatizer_lemma_3_invalid_unknown, error(domain_error(option, unknown(error)))) :-
		tiny_lemmatizer::lemma("saw", _, [unknown(error)]).

	test(lemmatizer_lemma_3_variable_options, error(instantiation_error)) :-
		tiny_lemmatizer::lemma("saw", _, _).

	test(lemmatizer_lemmas_2_default, deterministic(Lemmas == ["the", "child", "be", "run", "good"])) :-
		lemmatizer::lemmas(["the", "children", "were", "running", "better"], Lemmas).

	test(lemmatizer_lemmas_2_empty, deterministic(Lemmas == [])) :-
		lemmatizer::lemmas([], Lemmas).

	test(lemmatizer_lemmas_2_unknown_normalized, deterministic(Lemmas == ["unknown"])) :-
		tiny_lemmatizer::lemmas(["UNKNOWN"], Lemmas).

	test(lemmatizer_lemmas_3_per_word_pos, deterministic(Lemmas == ["see", "good"])) :-
		tiny_lemmatizer::lemmas(
			["saw", "better"], Lemmas,
			[parts_of_speech([verb, adjective])]
		).

	test(lemmatizer_lemmas_3_shared_pos, deterministic(Lemmas == ["saw", "mouse"])) :-
		tiny_lemmatizer::lemmas(
			["saw", "mice"], Lemmas,
			[part_of_speech(noun)]
		).

	test(lemmatizer_lemmas_3_all, deterministic(All == [
		["saw", "good"], ["saw", "well"], ["see", "good"], ["see", "well"]
	])) :-
		findall(
			Lemmas,
			tiny_lemmatizer::lemmas(["saw", "better"], Lemmas, [ambiguity(all)]),
			All
		).

	test(lemmatizer_lemmas_3_unknown_fail, false) :-
		tiny_lemmatizer::lemmas(["mice", "unknown"], _, [unknown(fail)]).

	test(lemmatizer_lemmas_3_unknown_preserve, deterministic(Lemmas == ["mouse", "UNKNOWN"])) :-
		tiny_lemmatizer::lemmas(["mice", "UNKNOWN"], Lemmas, [unknown(preserve)]).

	test(lemmatizer_lemma_3_unknown_preserve, deterministic(Lemma == "UNKNOWN")) :-
		tiny_lemmatizer::lemma("UNKNOWN", Lemma, [unknown(preserve)]).

	test(lemmatizer_lemmas_3_dimension_mismatch, error(consistency_error(same_length, ["saw", "mice"], [noun]))) :-
		tiny_lemmatizer::lemmas(["saw", "mice"], _, [parts_of_speech([noun])]).

	test(lemmatizer_lemmas_3_mutually_exclusive_pos, error(consistency_error(mutually_exclusive_options, part_of_speech(noun), parts_of_speech([noun])))) :-
		tiny_lemmatizer::lemmas(
			["saw"], _,
			[part_of_speech(noun), parts_of_speech([noun])]
		).

	test(lemmatizer_lemma_2_invalid_representation, error(domain_error(text_representation, unsupported))) :-
		lemmatizer(unsupported, english_lemmatizer)::lemma("word", _).
