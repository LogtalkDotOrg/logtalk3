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


:- category(score_ranker_model_common,
	extends(score_ranker_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-26,
		comment is 'Shared predicates for score-ranker term construction, validation, ranking, export, and pretty-printing.'
	]).

	:- protected(score_ranker_model/1).
	:- mode(score_ranker_model(-atom), one).
	:- info(score_ranker_model/1, [
		comment is 'Hook predicate that importing rankers must define to identify the learned score-ranker model.',
		argnames is ['Model']
	]).

	:- protected(score_ranker_label/1).
	:- mode(score_ranker_label(-atom), one).
	:- info(score_ranker_label/1, [
		comment is 'Hook predicate that importing rankers must define to provide a human-readable label used by pretty-printing helpers.',
		argnames is ['Label']
	]).

	:- protected(score_ranker_term/4).
	:- mode(score_ranker_term(?list, ?list, ?list(compound), ?compound), zero_or_one).
	:- info(score_ranker_term/4, [
		comment is 'Hook predicate that importing rankers must define to map between the ranker term representation and its items, scores, and diagnostics payloads.',
		argnames is ['Items', 'Scores', 'Diagnostics', 'Ranker']
	]).

	:- protected(valid_score/1).
	:- mode(valid_score(+number), zero_or_one).
	:- info(valid_score/1, [
		comment is 'Hook predicate that importing rankers must define to validate individual learned score values.',
		argnames is ['Score']
	]).

	:- protected(build_score_ranker/5).
	:- mode(build_score_ranker(+list, +list(pair), +list(compound), +list(compound), -compound), one).
	:- info(build_score_ranker/5, [
		comment is 'Builds a self-describing score-ranker term from ordered items, score pairs, effective options, and dataset summary metadata.',
		argnames is ['Items', 'Scores', 'Options', 'DatasetSummary', 'Ranker']
	]).

	:- protected(score_ranker_data/4).
	:- mode(score_ranker_data(+compound, -list, -list(pair), -list(compound)), one).
	:- info(score_ranker_data/4, [
		comment is 'Validates and destructures a score-ranker term into its items, score pairs, and diagnostics payloads.',
		argnames is ['Ranker', 'Items', 'Scores', 'Diagnostics']
	]).

	:- protected(initialize_scores/2).
	:- mode(initialize_scores(+list, -compound), one).
	:- info(initialize_scores/2, [
		comment is 'Initializes a score dictionary with zero scores for the given ordered items.',
		argnames is ['Items', 'Scores']
	]).

	:- protected(accumulate_item_scores/3).
	:- mode(accumulate_item_scores(+list(pair), +compound, -compound), one).
	:- info(accumulate_item_scores/3, [
		comment is 'Accumulates item score deltas into a score dictionary.',
		argnames is ['ItemDeltas', 'Scores', 'UpdatedScores']
	]).

	:- protected(update_score/4).
	:- mode(update_score(+compound, +term, +number, -compound), one).
	:- info(update_score/4, [
		comment is 'Updates the score of a single item in a score dictionary by adding the given delta.',
		argnames is ['Scores', 'Item', 'Delta', 'UpdatedScores']
	]).

	:- protected(ordered_scores/3).
	:- mode(ordered_scores(+list, +compound, -list(pair)), one).
	:- info(ordered_scores/3, [
		comment is 'Materializes ordered ``Item-Score`` pairs from a score dictionary using the given item order.',
		argnames is ['Items', 'ScoresDictionary', 'Scores']
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		length/2, member/2
	]).

	:- uses(avltree, [
		insert/4 as dictionary_insert/4, lookup/3 as dictionary_lookup/3, new/1 as dictionary_new/1
	]).

	ranker_scores_data(Ranker, Scores) :-
		::score_ranker_data(Ranker, _Items, Scores, _Diagnostics).

	ranker_diagnostics_data(Ranker, Diagnostics) :-
		::score_ranker_data(Ranker, _Items, _Scores, Diagnostics).

	ranker_export_template(_Dataset, _Ranker, Functor, Template) :-
		Template =.. [Functor, 'Ranker'].

	ranker_term_template(_Ranker, Template) :-
		::score_ranker_term('Items', 'Scores', 'Diagnostics', Template).

	export_to_clauses(_Dataset, Ranker, Functor, [Clause]) :-
		Clause =.. [Functor, Ranker].

	print_ranker(Ranker) :-
		::score_ranker_data(Ranker, Items, Scores, Diagnostics),
		::score_ranker_label(Label),
		::ranker_term_template(Ranker, Template),
		length(Items, Count),
		format('~w ranker for ~d items~n', [Label, Count]),
		format('Template: ~w~n', [Template]),
		forall(
			member(Item-Score, Scores),
			format('  ~q: ~w~n', [Item, Score])
		),
		format('Diagnostics: ~q~n', [Diagnostics]).

	build_score_ranker(Items, Scores, Options, DatasetSummary, Ranker) :-
		::score_ranker_model(Model),
		Diagnostics = [
			model(Model),
			options(Options),
			dataset_summary(DatasetSummary)
		],
		::score_ranker_term(Items, Scores, Diagnostics, Ranker).

	score_ranker_data(Ranker, Items, Scores, Diagnostics) :-
		::score_ranker_term('Items', 'Scores', 'Diagnostics', Template),
		functor(Template, Domain, 3),
		(   var(Ranker) ->
			instantiation_error
		;   ::score_ranker_term(Items, Scores, Diagnostics, Ranker),
			valid_core_ranker_data(Items, Scores) ->
			true
		;   domain_error(Domain, Ranker)
		).

	valid_core_ranker_data(Items, Scores) :-
		dictionary_new(SeenItems),
		validate_item_scores(Items, Scores, SeenItems).

	validate_item_scores(Items, Scores, _SeenItems) :-
		(   var(Items); var(Scores) ),
		!,
		fail.
	validate_item_scores([], [], _SeenItems).
	validate_item_scores([Item| Items], [ScoreItem-Score| Scores], SeenItems0) :-
		nonvar(Item),
		Item == ScoreItem,
		::valid_score(Score),
		\+ dictionary_lookup(Item, _Seen, SeenItems0),
		dictionary_insert(SeenItems0, Item, true, SeenItems),
		validate_item_scores(Items, Scores, SeenItems).

	initialize_scores(Items, Scores) :-
		dictionary_new(Scores0),
		initialize_scores(Items, Scores0, Scores).

	initialize_scores([], Scores, Scores).
	initialize_scores([Item| Items], Scores0, Scores) :-
		dictionary_insert(Scores0, Item, 0, Scores1),
		initialize_scores(Items, Scores1, Scores).

	accumulate_item_scores([], Scores, Scores).
	accumulate_item_scores([Item-Delta| ItemDeltas], Scores0, Scores) :-
		update_score(Scores0, Item, Delta, Scores1),
		accumulate_item_scores(ItemDeltas, Scores1, Scores).

	update_score(Scores0, Item, Delta, Scores) :-
		dictionary_lookup(Item, Score0, Scores0),
		Score is Score0 + Delta,
		dictionary_insert(Scores0, Item, Score, Scores).

	ordered_scores([], _ScoresDictionary, []).
	ordered_scores([Item| Items], ScoresDictionary, [Item-Score| Scores]) :-
		dictionary_lookup(Item, Score, ScoresDictionary),
		ordered_scores(Items, ScoresDictionary, Scores).

:- end_category.
