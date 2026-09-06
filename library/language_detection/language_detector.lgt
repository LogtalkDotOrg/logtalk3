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


:- object(language_detector(_Representation_, _Strategy_),
	implements(language_detector_protocol),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-06,
		comment is 'Language detector parameterized by text representation and detection strategy.',
		parameters is [
			'Representation' - 'Text representation. Valid values are ``atom``, ``chars``, and ``codes``.',
			'Strategy' - 'Object implementing the ``language_detection_strategy_protocol`` protocol.'
		],
		see_also is [language_detector_protocol, language_detection_strategy_protocol]
	]).

	:- uses(list, [
		length/2, member/2, sort/2, sort/4
	]).

	:- uses(type, [
		check/2, valid/2
	]).

	detect(Text, Language) :-
		detect(Text, Language, _).

	detect(Text, Language, Score) :-
		detect(Text, Language, Score, []).

	detect(Text, Language, Score, UserOptions) :-
		detect_all(Text, ScoredLanguages, UserOptions),
		ScoredLanguages = [Language-Score| _].

	detect_all(Text, ScoredLanguages) :-
		detect_all(Text, ScoredLanguages, []).

	detect_all(Text, ScoredLanguages, UserOptions) :-
		prepare(Text, Codes, Candidates, MinLength, MinScore, MinMargin, UserOptions),
		length(Codes, Length),
		detect_all(Length, MinLength, Codes, Candidates, MinScore, MinMargin, ScoredLanguages).

	detect_all(Length, MinLength, _, _, _, _, []) :-
		Length < MinLength,
		!.
	detect_all(_, _, Codes, Candidates, MinScore, MinMargin, ScoredLanguages) :-
		_Strategy_::scores(Codes, Candidates, Scores),
		process_scores(Scores, Candidates, MinScore, MinMargin, ScoredLanguages).

	process_scores([], _, _, _, []) :-
		!.
	process_scores(Scores, Candidates, MinScore, MinMargin, ScoredLanguages) :-
		validate_scores(Scores, Candidates),
		rank_scores(Scores, RankedScores),
		accept_scores(RankedScores, MinScore, MinMargin, ScoredLanguages).

	default_option(min_length(20)).
	default_option(candidates(all)).
	default_option(min_score(0.30)).
	default_option(min_margin(0.05)).

	valid_option(min_length(MinLength)) :-
		valid(non_negative_integer, MinLength).
	valid_option(candidates(Candidates)) :-
		(	Candidates == all ->
			true
		;	valid(list(atom), Candidates)
		).
	valid_option(min_score(Score)) :-
		valid(between(number, 0.0, 1.0), Score).
	valid_option(min_margin(Margin)) :-
		valid(between(number, 0.0, 1.0), Margin).

	prepare(Text, Codes, Candidates, MinLength, MinScore, MinMargin, UserOptions) :-
		check_representation,
		check_strategy,
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		check_text(Text),
		text_codes(Text, RawCodes),
		text_normalizer(codes, default_text_normalization)::clean(RawCodes, Codes, [
			unicode(nfc), entities(false), diacritics(none), case(lower),
			whitespace(true), trim(true), collapse(all), line_endings(lf), controls(remove)
		]),
		^^option(min_length(MinLength), Options),
		^^option(min_score(MinScore), Options),
		^^option(min_margin(MinMargin), Options),
		^^option(candidates(CandidateOption), Options),
		resolve_candidates(CandidateOption, Candidates).

	check_representation :-
		(	var(_Representation_) ->
			instantiation_error
		;	member(_Representation_, [atom, chars, codes]) ->
			true
		;	domain_error(text_representation, _Representation_)
		).

	check_strategy :-
		(	var(_Strategy_) ->
			instantiation_error
		;	conforms_to_protocol(_Strategy_, language_detection_strategy_protocol) ->
			true
		;	domain_error(language_detection_strategy, _Strategy_)
		).

	check_text(Text) :-
		check_text(_Representation_, Text).

	check_text(atom, Text) :-
		check(atom, Text).
	check_text(chars, Text) :-
		check(chars, Text).
	check_text(codes, Text) :-
		check(codes, Text).

	text_codes(Text, Codes) :-
		text_codes(_Representation_, Text, Codes).

	text_codes(atom, Atom, Codes) :-
		atom_codes(Atom, Codes).
	text_codes(chars, Chars, Codes) :-
		atom_chars(Atom, Chars),
		atom_codes(Atom, Codes).
	text_codes(codes, Codes, Codes).

	resolve_candidates(all, Candidates) :-
		!,
		findall(Language, language_profiles::profile(Language, _), Candidates).
	resolve_candidates(Candidates, SortedCandidates) :-
		Candidates \== all,
		(	Candidates == [] ->
			domain_error(language_candidates, Candidates)
		;	check_candidates(Candidates)
		),
		sort(Candidates, SortedCandidates),
		length(Candidates, Length),
		length(SortedCandidates, Length),
		!.
	resolve_candidates(Candidates, _) :-
		domain_error(language_candidates, Candidates).

	check_candidates([]).
	check_candidates([Language| Languages]) :-
		(	language_profiles::profile(Language, _) ->
			check_candidates(Languages)
		;	domain_error(language, Language)
		).

	validate_scores(Scores, Candidates) :-
		check(list, Scores),
		check_score_pairs(Scores, Languages),
		sort(Languages, SortedLanguages),
		sort(Candidates, SortedCandidates),
		(	SortedLanguages == SortedCandidates ->
			length(Languages, Length),
			length(SortedLanguages, Length)
		;	domain_error(language_detection_scores, Scores)
		),
		!.
	validate_scores(Scores, _) :-
		domain_error(language_detection_scores, Scores).

	check_score_pairs([], []).
	check_score_pairs([Language-Score| Scores], [Language| Languages]) :-
		check(atom, Language),
		check(number, Score),
		(	Score >= 0.0, Score =< 1.0 ->
			check_score_pairs(Scores, Languages)
		;	domain_error(language_detection_score, Score)
		).

	rank_scores(Scores, RankedScores) :-
		decorate_scores(Scores, DecoratedScores),
		sort(1, @=<, DecoratedScores, SortedDecoratedScores),
		undecorate_scores(SortedDecoratedScores, RankedScores).

	decorate_scores([], []).
	decorate_scores([Language-Score| Scores], [pair(NegativeScore, Language)-(Language-Score)| DecoratedScores]) :-
		NegativeScore is -Score,
		decorate_scores(Scores, DecoratedScores).

	undecorate_scores([], []).
	undecorate_scores([_Key-Score| DecoratedScores], [Score| Scores]) :-
		undecorate_scores(DecoratedScores, Scores).

	accept_scores([], _, _, []).
	accept_scores(RankedScores, MinScore, MinMargin, AcceptedScores) :-
		RankedScores = [_-FirstScore| RemainingScores],
		(	FirstScore < MinScore ->
			AcceptedScores = []
		;	RemainingScores = [_-SecondScore| _], FirstScore - SecondScore < MinMargin ->
			AcceptedScores = []
		;	AcceptedScores = RankedScores
		).

:- end_object.
