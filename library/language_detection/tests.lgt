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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-10-15,
		comment is 'tests for the "language_detection" library.'
	]).

	:- uses(list, [
		member/2, msort/2
	]).

	:- uses(lgtunit, [
		assertion/1, op(700, xfx, =~=), (=~=)/2
	]).

	cover(language_profiles).
	cover(language_detection_scripts).
	cover(language_detector(_, _)).
	cover(test_language_detection_strategy).
	cover(language_profile_common).
	cover(language_profile_expansion).
	cover(language_detection_scoring).
	cover(language_detection_tokenizer).
	cover(ngram_language_detector).
	cover(stopword_language_detector).
	cover(script_language_detector).
	cover(hybrid_language_detector).

	test(language_profiles_2_01, deterministic(Languages == [af, ar, bg, bn, br, ca, cs, da, de, el, en, eo, es, et, eu, fa, fi, fr, ga, gl, ha, he, hi, hr, hu, hy, id, it, ja, ko, ku, la, lt, lv, mr, ms, nl, no, pl, pt, ro, ru, sk, sl, so, st, sv, sw, th, tl, tr, uk, ur, vi, yo, zh, zu])) :-
		findall(Language, language_profiles::profile(Language, _), Languages0),
		msort(Languages0, Languages).

	test(language_profiles_2_02, deterministic(Profile == custom_en_language_profile)) :-
		language_profiles::profile(en, Profile).

	test(language_profiles_2_03, fail) :-
		language_profiles::profile(en, en_language_profile).

	test(language_profiles_2_04, deterministic(Profile == de_language_profile)) :-
		language_profiles::profile(de, Profile).

	test(language_scripts_2_01, deterministic(Scripts == ['Greek', 'Latin'])) :-
		language_detection_scripts::language_scripts(en, Scripts).

	test(language_scripts_2_02, fail) :-
		language_detection_scripts::language_scripts(en, ['Latin']).

	test(language_scripts_2_03, deterministic(Count == 57)) :-
		findall(Language, language_profiles::profile(Language, _), Languages),
		findall(Language, language_detection_scripts::language_scripts(Language, _), ScriptLanguages),
		sort(Languages, SortedLanguages),
		sort(ScriptLanguages, SortedLanguages),
		length(SortedLanguages, Count).

	test(script_ratios_2_01, deterministic(Ratios == ['Latin'-0.5, 'Common'-0.25, 'Cyrillic'-0.25])) :-
		language_detection_scripts::script_ratios([65, 66, 32, 1040], Ratios).

	test(script_ratios_2_02, deterministic(Ratios == [])) :-
		language_detection_scripts::script_ratios([], Ratios).

	test(scripts_to_languages_2_01, deterministic(Languages == [bg, ru, uk])) :-
		language_detection_scripts::scripts_to_languages(['Common', 'Cyrillic', 'Zzzz'], Languages).

	test(detect_all_3_01, deterministic(Scores == [en-1.0, pt-1.0])) :-
		language_detector(atom, test_language_detection_strategy)::detect_all('A sufficiently long sample.', Scores, [candidates([en, pt]), min_margin(0.0)]).

	test(detect_all_short_01, deterministic(Scores == [])) :-
		language_detector(atom, test_language_detection_strategy)::detect_all(short, Scores).

	test(invalid_strategy_01, error(domain_error(language_detection_strategy_protocol, list))) :-
		language_detector(atom, list)::detect_all('A sufficiently long sample.', _).

	test(variable_strategy_01, error(instantiation_error)) :-
		language_detector(atom, _)::detect_all('A sufficiently long sample.', _).

	test(invalid_method_option_01, error(domain_error(option, method(ngrams)))) :-
		language_detector(atom, test_language_detection_strategy)::detect_all('A sufficiently long sample.', _, [method(ngrams)]).

	test(stopword_english_01, deterministic(Language == en)) :-
		language_detector(atom, stopword_language_detector)::detect(
			'The quick fox and the lazy dog are in the garden.', Language, _, [min_score(0.0), min_margin(0.0)]
		).

	test(stopword_custom_profile_override_01, deterministic(Language == en)) :-
		language_detector(atom, stopword_language_detector)::detect(
			'Customword customword customword customword.', Language, _, [candidates([de, en]), min_score(0.0), min_margin(0.0)]
		).

	test(stopword_italian_01, deterministic(Language == it)) :-
		language_detector(atom, stopword_language_detector)::detect(
			'Questo testo italiano contiene delle parole che sono molto comuni.', Language, _, [min_score(0.0), min_margin(0.0)]
		).

	test(script_scores_cross_script_01, deterministic(Scores == [en-0.0, ru-1.0])) :-
		script_language_detector::scores([1040, 1041], [en, ru], Scores).

	test(script_scores_same_script_01, deterministic(Scores == [de-0.5, en-0.5])) :-
		script_language_detector::scores([65, 66], [de, en], Scores).

	test(script_scores_mixed_script_01, deterministic(Scores == [en-0.5, ru-0.5])) :-
		script_language_detector::scores([65, 1040], [en, ru], Scores).

	test(script_scores_common_only_01, deterministic(Scores == [])) :-
		script_language_detector::scores([32, 33, 49], [en, ru], Scores).

	test(script_representations_01, deterministic((AtomLanguage == ru, CharsLanguage == ru, CodesLanguage == ru))) :-
		Codes = [1040, 1041, 1042, 1043],
		atom_codes(Atom, Codes),
		atom_chars(Atom, Chars),
		Options = [candidates([en, ru]), min_length(0), min_score(0.0), min_margin(0.0)],
		language_detector(atom,  script_language_detector)::detect(Atom,  AtomLanguage,  _, Options),
		language_detector(chars, script_language_detector)::detect(Chars, CharsLanguage, _, Options),
		language_detector(codes, script_language_detector)::detect(Codes, CodesLanguage, _, Options).

	test(profile_trigrams_01, deterministic(Counts \== [])) :-
		en_language_profile::trigram_counts(Counts).

	test(ngram_english_01, deterministic(Language == en)) :-
		language_detector(atom, ngram_language_detector)::detect(
			'The quick brown fox jumps over the lazy dog in the garden.', Language, _, [min_score(0.0), min_margin(0.0)]
		).

	test(ngram_italian_01, deterministic(Language == it)) :-
		language_detector(atom, ngram_language_detector)::detect(
			'Questo testo italiano contiene parole comuni della nostra lingua.', Language, _, [min_score(0.0), min_margin(0.0)]
		).

	test(hybrid_italian_01, deterministic(Language == it)) :-
		language_detector(atom, hybrid_language_detector)::detect(
			'Questo testo italiano contiene delle parole che sono molto comuni.', Language, _, [min_score(0.0), min_margin(0.0)]
		).

	test(hybrid_weights_01, deterministic((DifferenceEn < 0.000000000001, DifferenceIt < 0.000000000001))) :-
		atom_codes('questo testo italiano contiene delle parole che sono molto comuni', Codes),
		Candidates = [en, it],
		ngram_language_detector::scores(Codes, Candidates, [en-NGramEn, it-NGramIt]),
		stopword_language_detector::scores(Codes, Candidates, [en-StopWordEn, it-StopWordIt]),
		hybrid_language_detector::scores(Codes, Candidates, [en-HybridEn, it-HybridIt]),
		ExpectedEn is 0.80 * (0.75 * NGramEn + 0.25 * StopWordEn) + 0.10,
		ExpectedIt is 0.80 * (0.75 * NGramIt + 0.25 * StopWordIt) + 0.10,
		DifferenceEn is abs(HybridEn - ExpectedEn),
		DifferenceIt is abs(HybridIt - ExpectedIt).

	test(ngram_six_languages_01, deterministic(Languages == [de, en, es, fr, it, pt])) :-
		detected_languages(ngram_language_detector, Languages).

	test(stopword_six_languages_01, deterministic(Languages == [de, en, es, fr, it, pt])) :-
		detected_languages(stopword_language_detector, Languages).

	test(hybrid_six_languages_01, deterministic(Languages == [de, en, es, fr, it, pt])) :-
		detected_languages(hybrid_language_detector, Languages).

	test(representations_01, deterministic((AtomScores == CharsScores, AtomScores == CodesScores))) :-
		language_fixture(en, Text),
		atom_chars(Text, Chars),
		atom_codes(Text, Codes),
		Options = [candidates([en, it]), min_score(0.0), min_margin(0.0)],
		language_detector(atom, hybrid_language_detector)::detect_all(Text, AtomScores, Options),
		language_detector(chars, hybrid_language_detector)::detect_all(Chars, CharsScores, Options),
		language_detector(codes, hybrid_language_detector)::detect_all(Codes, CodesScores, Options).

	test(duplicate_candidates_01, error(domain_error(language_candidates, [en, pt, en]))) :-
		language_detector(atom, stopword_language_detector)::detect_all(
			'A sufficiently long language sample.', _, [candidates([en, pt, en])]
		).

	test(unsupported_candidate_01, error(domain_error(language, cy))) :-
		language_detector(atom, stopword_language_detector)::detect_all(
			'A sufficiently long language sample.', _, [candidates([en, cy])]
		).

	test(language_detection_documented_english, deterministic(Language == en)) :-
		language_detector(atom, ngram_language_detector)::detect(
			'The quick brown fox jumps over the lazy dog.', Language, _, [candidates([en, it])]
		).

	test(language_detection_documented_italian, deterministic(Language == it)) :-
		language_detector(atom, hybrid_language_detector)::detect(
			'Questo testo è scritto in italiano.', Language, _, [min_length(10), candidates([en, it])]
		).

	test(language_detection_detect_2, deterministic(Language == en)) :-
		language_detector(atom, confident_language_detection_strategy)::detect('A sufficiently long sample.', Language).

	test(language_detection_detect_3, deterministic((Language == en, Score =~= 1.0))) :-
		language_detector(atom, confident_language_detection_strategy)::detect('A sufficiently long sample.', Language, Score).

	test(language_detection_empty_evidence, deterministic(Scores == [])) :-
		fixed_scores(empty, [], Scores).

	test(language_detection_default_ranking, deterministic(Scores == [en-0.75, pt-0.25])) :-
		fixed_scores(ranked, [], Scores).

	test(language_detection_default_margin_rejection, deterministic(Scores == [])) :-
		fixed_scores(low_margin, [], Scores).

	test(language_detection_score_rejection, deterministic(Scores == [])) :-
		fixed_scores(ranked, [min_score(0.8)], Scores).

	test(language_detection_threshold_equality, deterministic(Scores == [en-0.5, pt-0.25])) :-
		fixed_scores(boundary, [min_score(0.5), min_margin(0.25)], Scores).

	test(language_detection_margin_rejection, deterministic(Scores == [])) :-
		fixed_scores(boundary, [min_margin(0.5)], Scores).

	test(language_detection_lexical_tie, deterministic(Scores == [en-0.5, pt-0.5])) :-
		fixed_scores(tie, [min_margin(0.0)], Scores).

	test(language_detection_missing_score, error(domain_error(language_detection_scores, [en-0.5]))) :-
		fixed_scores(missing, [], _).

	test(language_detection_duplicate_score, error(domain_error(language_detection_scores, [en-0.5, en-0.5]))) :-
		fixed_scores(duplicate, [], _).

	test(language_detection_unexpected_score, error(domain_error(language_detection_scores, [en-0.5, xx-0.5]))) :-
		fixed_scores(unexpected, [], _).

	test(language_detection_malformed_score, error(domain_error(language_detection_scores, [invalid]))) :-
		fixed_scores(malformed, [], _).

	throws(language_detection_non_list_scores, [type_error(list, invalid)]) :-
		fixed_scores(non_list, [], _).

	throws(language_detection_non_number_score, [type_error(number, invalid)]) :-
		fixed_scores(non_number, [], _).

	test(language_detection_negative_score, error(domain_error(language_detection_score, -0.1))) :-
		fixed_scores(negative, [], _).

	test(language_detection_large_score, error(domain_error(language_detection_score, 1.1))) :-
		fixed_scores(too_large, [], _).

	test(language_detection_cosine_vectors, deterministic) :-
		forall(member(Left-Right-Expected, [
			[]-[]-0.0,
			[a-1]-[]-0.0,
			[]-[a-1]-0.0,
			[a-1]-[a-1]-1.0,
			[a-1]-[b-1]-0.0,
			[b-1]-[a-1]-0.0,
			[a-3, c-4]-[b-3, c-4]-0.64
		]), (
			test_language_detection_scoring::cosine(Left, Right, Actual),
			assertion(Actual =~= Expected)
		)).

	test(language_detection_normalize_zero, deterministic(Scores == [])) :-
		test_language_detection_scoring::normalize([en-0.0], Scores).

	test(language_detection_hybrid_fallbacks, deterministic) :-
		forall(member(Left-Right-Expected, [
			[]-[]-[],
			[]-[en-1.0]-[en-1.0],
			[en-1.0]-[]-[en-1.0]
		]), (
			hybrid_language_detector << combine_lexical_scores(Left, Right, Lexical),
			assertion(Lexical == Expected),
			hybrid_language_detector << combine_scores(Left, Right, Combined),
			assertion(Combined == Expected)
		)).

	test(language_detection_tokenizer_punctuation, deterministic) :-
		forall(member(Code, [39, 8217, 45, 8208, 8209]), (
			Input = [97, Code, 98],
			tokenizer(codes, language_detection_tokenizer)::tokenize(Input, Tokens, [
				keep_punctuation(false), normalize_quotes(false), normalize_dashes(false)
			]),
			assertion(Tokens == [Input])
		)).

	test(language_detection_all_profile_vectors, deterministic) :-
		forall(language_profiles::default_profile(_, Profile), (
			Profile::trigram_counts(Counts),
			Profile::trigram_counts(StoredCounts, SquareSum),
			assertion(StoredCounts == Counts),
			findall(Square, (
				member(_-Count, Counts),
				Square is Count * Count
			), Squares),
			numberlist::sum(Squares, ExpectedSquareSum),
			assertion(SquareSum =~= ExpectedSquareSum),
			assertion(Counts \== []),
			assertion(type::valid(list(pair(atom, positive_integer)), Counts)),
			findall(Gram, member(Gram-_, Counts), Grams),
			sort(Grams, SortedGrams),
			assertion(Grams == SortedGrams)
		)).

	test(language_detection_derived_profile_vector, deterministic(Counts == [' a '-1, ' th'-1, 'he '-1, the-1])) :-
		derived_language_profile::trigram_counts(Counts).

	test(language_detection_derived_profile_norm, deterministic) :-
		derived_language_profile::trigram_counts(Counts, SquareSum),
		assertion(Counts == [' a '-1, ' th'-1, 'he '-1, the-1]),
		assertion(SquareSum =~= 4.0).

	test(language_detection_cached_profile, deterministic) :-
		cached_language_profile::trigram_counts(Counts),
		assertion(Counts == [cached-3]),
		cached_language_profile::trigram_counts(StoredCounts, SquareSum),
		assertion(StoredCounts == Counts),
		assertion(SquareSum =~= 9.0).

	test(language_detection_overridden_profile_norm, deterministic) :-
		overridden_language_profile::trigram_counts(Counts, SquareSum),
		assertion(Counts == [custom-2]),
		assertion(SquareSum =~= 4.0).

	test(language_detection_precomputed_norm_scores, deterministic) :-
		forall(member(Text-Candidates, [
			''-[en, it],
			a-[it, en],
			'the english language'-[en, it],
			'questo testo italiano'-[it, en],
			'12345 !?'-[en, it]
		]), (
			atom_codes(Text, Codes),
			ngram_language_detector::scores(Codes, Candidates, Scores),
			n_grams(atom)::character_n_grams(3, Text, Grams),
			n_grams(atom)::count(standard, Grams, Counts),
			reference_ngram_scores(Candidates, Counts, RawScores),
			test_language_detection_scoring::normalize(RawScores, Expected),
			assertion(Scores == Expected)
		)).

	test(language_detection_memoized_stopword_scores, deterministic) :-
		forall(member(Text, [
			'', 'qzx qzx', 'the the the', 'de de de',
			'customword customword the questo de qzx',
			'the questo de o and un uma a'
		]), (
			atom_codes(Text, Codes),
			forall(member(Candidates, [[en, it, pt, de], [pt, de, it, en], [en], []]), (
				stopword_language_detector::scores(Codes, Candidates, Scores),
				reference_stopword_scores(Codes, Candidates, Expected),
				assertion(Scores == Expected)
			))
		)).

	test(language_detection_profile_expansion, deterministic) :-
		language_profile_expansion::term_expansion(
			(stop_word(Word) :- derived_language_profile::stop_word(Word)),
			Expanded
		),
		Expected = [
			(stop_word(Word) :- derived_language_profile::stop_word(Word)),
			(trigram_counts_(Counts, SquareSum) :-
				Counts = [' a '-1, ' th'-1, 'he '-1, the-1],
				SquareSum = 4.0
			)
		],
		assertion(lgtunit::variant(Expanded, Expected)),
		language_profile_expansion << word_trigrams([a, the], Trigrams, []),
		assertion(Trigrams == [' a ', ' th', the, 'he ']).

	% auxiliary predicates

	reference_stopword_scores(Codes, Candidates, Scores) :-
		tokenizer(codes, language_detection_tokenizer)::tokenize(Codes, Tokens, [keep_punctuation(false), lowercase(true)]),
		findall(Language-0.0, member(Language, Candidates), InitialScores),
		reference_stopword_tokens(Tokens, Candidates, InitialScores, RawScores),
		test_language_detection_scoring::normalize(RawScores, Scores).

	reference_stopword_tokens([], _, Scores, Scores).
	reference_stopword_tokens([Token| Tokens], Candidates, Scores0, Scores) :-
		atom_codes(Word, Token),
		findall(Language, (
			member(Language, Candidates),
			language_profiles::profile(Language, Profile),
			( Profile::stop_word(Word) ->
				true
			; fail
			)
		), Matches),
		( Matches == [] ->
			Scores1 = Scores0
		; length(Matches, Count),
			Weight is 1.0 / Count,
			reference_stopword_evidence(Scores0, Matches, Weight, Scores1)
		),
		reference_stopword_tokens(Tokens, Candidates, Scores1, Scores).

	reference_stopword_evidence([], _, _, []).
	reference_stopword_evidence([Language-Score0| Scores0], Matches, Weight, [Language-Score| Scores]) :-
		( member(Language, Matches) ->
			Score is Score0 + Weight
		; Score = Score0
		),
		reference_stopword_evidence(Scores0, Matches, Weight, Scores).

	reference_ngram_scores([], _, []).
	reference_ngram_scores([Language| Languages], Counts, [Language-Score| Scores]) :-
		language_profiles::profile(Language, Profile),
		Profile::trigram_counts(ProfileCounts),
		test_language_detection_scoring::cosine(Counts, ProfileCounts, Score),
		reference_ngram_scores(Languages, Counts, Scores).

	fixed_scores(Case, Options, Scores) :-
		language_detector(atom, fixed_language_detection_strategy(Case))::detect_all(
			'A sufficiently long sample.', Scores, [candidates([en, pt])| Options]
		).

	detected_languages(Strategy, Languages) :-
		findall(
			Language,
			(	language_fixture(_, Text),
				language_detector(atom, Strategy)::detect(Text, Language, _, [candidates([de, en, es, fr, it, pt]), min_score(0.0), min_margin(0.0)])
			),
			Languages
		).

	language_fixture(de, 'Dies ist ein deutscher Text mit einigen sehr häufigen Wörtern der deutschen Sprache.').
	language_fixture(en, 'This is an English text with several very common words in the English language.').
	language_fixture(es, 'Este texto está escrito en español y contiene algunas palabras muy comunes de nuestra lengua.').
	language_fixture(fr, 'Ce texte est écrit en français et contient plusieurs mots très courants de notre langue.').
	language_fixture(it, 'Questo testo è scritto in italiano e contiene alcune parole molto comuni della nostra lingua.').
	language_fixture(pt, 'Este texto está escrito em português e contém algumas palavras muito comuns da nossa língua.').

:- end_object.
