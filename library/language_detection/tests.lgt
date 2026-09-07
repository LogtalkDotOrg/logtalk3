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
		date is 2026-09-07,
		comment is 'tests for the "language_detection" library.'
	]).

	:- uses(list, [
		msort/2
	]).

	cover(language_profiles).
	cover(language_detection_scripts).
	cover(language_detector(_, _)).
	cover(test_language_detection_strategy).

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

	test(invalid_strategy_01, error(domain_error(language_detection_strategy, list))) :-
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

	test(representations_01, deterministic((AtomLanguage == en, CharsLanguage == en, CodesLanguage == en))) :-
		language_fixture(en, Text),
		atom_chars(Text, Chars),
		atom_codes(Text, Codes),
		Options = [min_score(0.0), min_margin(0.0)],
		language_detector(atom,  hybrid_language_detector)::detect(Text,  AtomLanguage,  _, Options),
		language_detector(chars, hybrid_language_detector)::detect(Chars, CharsLanguage, _, Options),
		language_detector(codes, hybrid_language_detector)::detect(Codes, CodesLanguage, _, Options).

	test(duplicate_candidates_01, error(domain_error(language_candidates, [en, pt, en]))) :-
		language_detector(atom, stopword_language_detector)::detect_all(
			'A sufficiently long language sample.', _, [candidates([en, pt, en])]
		).

	test(unsupported_candidate_01, error(domain_error(language, cy))) :-
		language_detector(atom, stopword_language_detector)::detect_all(
			'A sufficiently long language sample.', _, [candidates([en, cy])]
		).

	% auxiliary predicates

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
