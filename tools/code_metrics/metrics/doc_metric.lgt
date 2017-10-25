%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 2017 Paulo Moura <pmoura@logtalk.org>
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


:- object(doc_metric,
	implements(code_metrics_protocol),
	imports(code_metrics_utilities)).

	:- info([
		version is 0.2,
		author is 'Paulo Moura',
		date is 2017/10/25,
		comment is 'Entity and entity predicates documentation score.',
		remarks is [
			'Score range' - 'Score is a percentage where a 100% score means that all expected documentation information is present.',
			'Score weights' - 'The score is split by default between 20% for the entity documentation and 80% for the entity predicates documentation, Can be customizewd using the predicate entity_predicates_weights_hook/2.',
			'Score customization' - 'The individual scores of entity info/1 pairs and predicate info/2 pairs can be customized using the entity_info_pair_score_hook/3 and predicate_info_pair_score_hook/4 predicates.'
		]
	]).

	:- public(entity_predicates_weights_hook/2).
	:- multifile(entity_predicates_weights_hook/2).
	:- dynamic(entity_predicates_weights_hook/2).
	:- mode(entity_predicates_weights_hook(?integer, ?integer), zero_or_one).
	:- info(entity_predicates_weights_hook/2, [
		comment is 'Relative weight between entity documentation and predicates documentation. The sum of the two values must be equal to 100.',
		argnames is ['EntityWeight', 'PredicatesWeight']
	]).

	:- public(entity_info_score_hook/2).
	:- multifile(entity_info_score_hook/2).
	:- dynamic(entity_info_score_hook/2).
	:- mode(entity_info_score_hook(?term, ?integer), zero_or_one).
	:- info(entity_info_score_hook/2, [
		comment is 'Maximum score for entity info/1 directives.',
		argnames is ['Entity', 'MaximumScore']
	]).

	:- public(entity_info_pair_score_hook/3).
	:- multifile(entity_info_pair_score_hook/3).
	:- dynamic(entity_info_pair_score_hook/3).
	:- mode(entity_info_pair_score_hook(?callable, ?term, ?integer), zero_or_more).
	:- info(entity_info_pair_score_hook/3, [
		comment is 'Score for relevant entity info/1 directive pairs.',
		argnames is ['Pair', 'Entity', 'Score']
	]).

	:- public(predicate_mode_score_hook/3).
	:- multifile(predicate_mode_score_hook/3).
	:- dynamic(predicate_mode_score_hook/3).
	:- mode(predicate_mode_score_hook(?term, ?term, ?integer), zero_or_more).
	:- info(predicate_mode_score_hook/3, [
		comment is 'Maximum score for predicate mode/2 directives.',
		argnames is ['Entity', 'Predicate', 'MaximumScore']
	]).

	:- public(predicate_mode_score_hook/5).
	:- multifile(predicate_mode_score_hook/5).
	:- dynamic(predicate_mode_score_hook/5).
	:- mode(predicate_mode_score_hook(?term, ?term, ?term, ?term, ?integer), zero_or_one).
	:- info(predicate_mode_score_hook/5, [
		comment is 'Score for a predicate mode/2 directives.',
		argnames is ['Template', 'Solutions', 'Entity', 'Predicate', 'Score']
	]).

	:- public(predicate_info_score_hook/3).
	:- multifile(predicate_info_score_hook/3).
	:- dynamic(predicate_info_score_hook/3).
	:- mode(predicate_info_score_hook(?term, ?term, ?integer), zero_or_one).
	:- info(predicate_info_score_hook/3, [
		comment is 'Maximum score for predicate info/2 directives.',
		argnames is ['Entity', 'Predicate', 'MaximumScore']
	]).

	:- public(predicate_info_pair_score_hook/4).
	:- multifile(predicate_info_pair_score_hook/4).
	:- dynamic(predicate_info_pair_score_hook/4).
	:- mode(predicate_info_pair_score_hook(?callable, ?term, ?term, ?integer), zero_or_more).
	:- info(predicate_info_pair_score_hook/4, [
		comment is 'Score for a predicate info/2 directive pairs.',
		argnames is ['Pair', 'Entity', 'Predicate', 'Score']
	]).

	:- uses(list, [
		length/2, member/2
	]).

	:- uses(numberlist, [
		sum/2
	]).

	entity_score(Entity, Score) :-
		^^current_entity(Entity),
		score(Entity, Score).

	score(Entity, Score) :-
		info_1_score(Entity, Info1Score),
		info_2_score(Entity, Info2Score),
		(	entity_predicates_weights_hook(EntityWeight, PredicatesWeight) ->
			true
		;	entity_predicates_weights(EntityWeight, PredicatesWeight)
		),
		Score is Info1Score * EntityWeight / 100 + Info2Score * PredicatesWeight / 100.

	% defauls
	entity_predicates_weights(20, 80).

	info_1_score(Entity, Score) :-
		(	^^entity_property(Entity, info(Info)) ->
			info_1_score(Entity, Info, Score)
		;	Score = 0
		).

	info_1_score(Entity, Info, Score) :-
		info_1_score(Info, Entity, 0, Score0),
		(	entity_info_score_hook(Entity, MaximumScore) ->
			true
		;	entity_info_score(Entity, MaximumScore)
		),
		Score is Score0 * 100 / MaximumScore.

	info_1_score([], _, Score, Score).
	info_1_score([Pair| Pairs], Entity, Score0, Score) :-
		(	entity_info_pair_score_hook(Pair, Entity, PairScore) ->
			Score1 is Score0 + PairScore
		;	entity_info_pair_score(Pair, Entity, PairScore) ->
			Score1 is Score0 + PairScore
		;	Score1 is Score0
		),
		info_1_score(Pairs, Entity, Score1, Score).

	info_2_score(Entity, Score) :-
		findall(
			PredicateScore,
			info_2_predicate_score(Entity, PredicateScore),
			PredicateScores
		),
		length(PredicateScores, NumberOfPredicates),
		(	NumberOfPredicates > 0 ->
			sum(PredicateScores, SumScores),
			Score is SumScores / NumberOfPredicates
		;	Score is 100
		).

	info_2_predicate_score(Entity, Score) :-
		^^entity_property(Entity, declares(Predicate, Properties)),
		(	member(mode(Template,Solutions), Properties) ->
			(	predicate_mode_score_hook(Template, Solutions, Entity, Predicate, ModeScore) ->
				true
			;	predicate_mode_score(Template, Solutions, Entity, Predicate, ModeScore) ->
				true
			;	ModeScore is 0
			)
		;	ModeScore is 0
		),
		(	predicate_mode_score_hook(Entity, Predicate, MaximumModeScore) ->
			true
		;	predicate_mode_score(Entity, Predicate, MaximumModeScore)
		),
		(	member(info(Pairs), Properties) ->
			(	info_2_pairs_score(Pairs, Entity, Predicate, 0, InfoScore) ->
				true
			;	InfoScore is 0
			)
		;	InfoScore is 0
		),
		(	predicate_info_score_hook(Entity, Predicate, MaximumInfoScore) ->
			true
		;	predicate_info_score(Entity, Predicate, MaximumInfoScore)
		),
		Score is (ModeScore + InfoScore) * 100 / (MaximumModeScore + MaximumInfoScore).

	info_2_pairs_score([], _, _, Score, Score).
	info_2_pairs_score([Pair| Pairs], Entity, Predicate, Score0, Score) :-
		(	predicate_info_pair_score_hook(Pair, Entity, Predicate, PairScore) ->
			Score1 is Score0 + PairScore
		;	predicate_info_pair_score(Pair, Entity, Predicate, PairScore) ->
			Score1 is Score0 + PairScore
		;	Score1 is Score0
		),
		info_2_pairs_score(Pairs, Entity, Predicate, Score1, Score).

	% entity info/1 defauls

	entity_info_score(Entity, MaximumScore) :-
		(	atom(Entity) ->
			MaximumScore is 40
		;	% parametric entity
			MaximumScore is 50
		).

	entity_info_pair_score(author(Author), _, 10) :-
		atom_length(Author, Length), Length >= 2.
	entity_info_pair_score(version(_), _, 10).
	entity_info_pair_score(date(_), _, 10).
	entity_info_pair_score(comment(Comment), _, 10) :-
		atom_length(Comment, Length), Length >= 10.
	entity_info_pair_score(parnames(_), _, 10).
	entity_info_pair_score(parameters(_), _, 10).

	% predicate mode/2 directive defaults

	predicate_mode_score(_Entity, _Predicate, 10).

	predicate_mode_score(_Template, _Solutions, _Entity, _Predicate, 10).

	% predicate info/2 directive defaults

	predicate_info_score(_Entity, _Predicate, 20).

	predicate_info_pair_score(comment(Comment), _, _, 10) :-
		atom_length(Comment, Length), Length >= 10.
	predicate_info_pair_score(argnames(_), _, _, 10).
	predicate_info_pair_score(arguments(_), _, _, 10).

	metric_label('Documentation').

:- end_object.
