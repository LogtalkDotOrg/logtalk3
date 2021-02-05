%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2017-2018 Paulo Moura <pmoura@logtalk.org>
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


:- object(doc_metric,
	imports((code_metrics_utilities, code_metric))).

	:- info([
		version is 0:12:0,
		author is 'Paulo Moura',
		date is 2019-09-23,
		comment is 'Entity and entity predicates documentation score.',
		remarks is [
			'Score range' - 'Score is a integer percentage where a 100% score means that all expected documentation information is present.',
			'Score weights' - 'The score is split by default between 20% for the entity documentation and 80% for the entity predicates documentation, Can be customized using the predicate ``entity_predicates_weights_hook/2``.',
			'Score customization' - 'The individual scores of entity ``info/1`` pairs and predicate ``info/2`` pairs can be customized using the ``entity_info_pair_score_hook/3`` and ``predicate_info_pair_score_hook/4`` predicates.'
		]
	]).

	:- public(entity_predicates_weights_hook/2).
	:- multifile(entity_predicates_weights_hook/2).
	:- dynamic(entity_predicates_weights_hook/2).
	:- mode(entity_predicates_weights_hook(?integer, ?integer), zero_or_one).
	:- info(entity_predicates_weights_hook/2, [
		comment is 'Relative weight between entity documentation and predicates documentation in percentage. The sum of the two values must be equal to 100.',
		argnames is ['EntityWeight', 'PredicatesWeight']
	]).

	:- public(entity_info_score_hook/2).
	:- multifile(entity_info_score_hook/2).
	:- dynamic(entity_info_score_hook/2).
	:- mode(entity_info_score_hook(?term, ?integer), zero_or_one).
	:- info(entity_info_score_hook/2, [
		comment is 'Maximum score for entity ``info/1`` directives.',
		argnames is ['Entity', 'MaximumScore']
	]).

	:- public(entity_info_pair_score_hook/3).
	:- multifile(entity_info_pair_score_hook/3).
	:- dynamic(entity_info_pair_score_hook/3).
	:- mode(entity_info_pair_score_hook(?callable, ?term, ?integer), zero_or_more).
	:- info(entity_info_pair_score_hook/3, [
		comment is 'Score for relevant entity ``info/1`` directive pairs. If defined, the ``entity_info_score_hook/2`` predicate should be defined accordingly.',
		argnames is ['Pair', 'Entity', 'Score']
	]).

	:- public(predicate_mode_score_hook/3).
	:- multifile(predicate_mode_score_hook/3).
	:- dynamic(predicate_mode_score_hook/3).
	:- mode(predicate_mode_score_hook(?term, ?term, ?integer), zero_or_more).
	:- info(predicate_mode_score_hook/3, [
		comment is 'Maximum score for predicate ``mode/2`` directives.',
		argnames is ['Entity', 'Predicate', 'MaximumScore']
	]).

	:- public(predicate_mode_score_hook/5).
	:- multifile(predicate_mode_score_hook/5).
	:- dynamic(predicate_mode_score_hook/5).
	:- mode(predicate_mode_score_hook(?term, ?term, ?term, ?term, ?integer), zero_or_one).
	:- info(predicate_mode_score_hook/5, [
		comment is 'Score for a predicate ``mode/2`` directive. If defined, the ``predicate_mode_score_hook/3`` predicate should be defined accordingly.',
		argnames is ['Template', 'Solutions', 'Entity', 'Predicate', 'Score']
	]).

	:- public(predicate_info_score_hook/3).
	:- multifile(predicate_info_score_hook/3).
	:- dynamic(predicate_info_score_hook/3).
	:- mode(predicate_info_score_hook(?term, ?term, ?integer), zero_or_one).
	:- info(predicate_info_score_hook/3, [
		comment is 'Maximum score for predicate ``info/2`` directives.',
		argnames is ['Entity', 'Predicate', 'MaximumScore']
	]).

	:- public(predicate_info_pair_score_hook/4).
	:- multifile(predicate_info_pair_score_hook/4).
	:- dynamic(predicate_info_pair_score_hook/4).
	:- mode(predicate_info_pair_score_hook(?callable, ?term, ?term, ?integer), zero_or_more).
	:- info(predicate_info_pair_score_hook/4, [
		comment is 'Score for a predicate ``info/2`` directive pairs. If defined, the ``predicate_info_score_hook/3`` predicate should be defined accordingly.',
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
		(	Info2Score =:= -1 ->
			% no declared predicates
			Score is round(Info1Score)
		;	Score is round(Info1Score * EntityWeight / 100 + Info2Score * PredicatesWeight / 100)
		).

	info_1_score(Entity, Score) :-
		(	^^entity_property(Entity, info(Info)) ->
			info_1_score(Entity, Info, Score)
		;	Score = 0.0
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
			info_2_predicate_score(Entity, _, PredicateScore),
			PredicateScores
		),
		length(PredicateScores, NumberOfPredicates),
		(	NumberOfPredicates > 0 ->
			sum(PredicateScores, SumScores),
			Score is SumScores / NumberOfPredicates
		;	Score is -1
		).

	info_2_predicate_score(Entity, Predicate, Score) :-
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

	% default for relative weights between entity
	% documentation and predicates documentation

	entity_predicates_weights(20, 80).

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
	entity_info_pair_score(parameters(Parameters), _, Score) :-
		parameters_with_descriptions(Parameters, 0, NumberOfParameters, 0, NumberOfDescriptions),
		(	NumberOfDescriptions =:= 0 ->
			% no parameter descriptions present
			Score is 3
		;	% parameter description present for at least one of the parameters
			Ratio is NumberOfParameters / NumberOfDescriptions,
			Score is 3 + 7 / Ratio
		).

	parameters_with_descriptions([], NumberOfParameters, NumberOfParameters, NumberOfDescriptions, NumberOfDescriptions).
	parameters_with_descriptions([_-Description| Parameters], NumberOfParameters0, NumberOfParameters, NumberOfDescriptions0, NumberOfDescriptions) :-
		NumberOfParameters1 is NumberOfParameters0 + 1,
		(	atom_length(Description, Length), Length >= 10 ->
			NumberOfDescriptions1 is NumberOfDescriptions0 + 1
		;	NumberOfDescriptions1 is NumberOfDescriptions0
		),
		parameters_with_descriptions(Parameters, NumberOfParameters1, NumberOfParameters, NumberOfDescriptions1, NumberOfDescriptions).

	% predicate mode/2 directive defaults

	predicate_mode_score(_Entity, _Predicate, 10).

	predicate_mode_score(Template, _Solutions, _Entity, _Predicate, Score) :-
		Template =.. [_| Arguments],
		arguments_with_type_information(Arguments, 0, NumberOfArguments, 0, NumberOfCompoundArguments),
		(	NumberOfArguments =:= 0 ->
			% predicate with no arguments
			Score is 10
		;	NumberOfCompoundArguments =:= 0 ->
			% no type information present
			Score is 3
		;	% type information present for at least one of the arguments
			Ratio is NumberOfArguments / NumberOfCompoundArguments,
			Score is 3 + 7 / Ratio
		).

	arguments_with_type_information([], NumberOfArguments, NumberOfArguments, NumberOfCompoundArguments, NumberOfCompoundArguments).
	arguments_with_type_information([Argument| Arguments], NumberOfArguments0, NumberOfArguments, NumberOfCompoundArguments0, NumberOfCompoundArguments) :-
		NumberOfArguments1 is NumberOfArguments0 + 1,
		(	compound(Argument) ->
			NumberOfCompoundArguments1 is NumberOfCompoundArguments0 + 1
		;	NumberOfCompoundArguments1 is NumberOfCompoundArguments0
		),
		arguments_with_type_information(Arguments, NumberOfArguments1, NumberOfArguments, NumberOfCompoundArguments1, NumberOfCompoundArguments).

	% predicate info/2 directive defaults

	predicate_info_score(_Entity, _/Arity, Score) :-
		(	Arity =:= 0 ->
			Score is 10
		;	Score is 20
		).

	predicate_info_pair_score(comment(Comment), _, _, 10) :-
		atom_length(Comment, Length), Length >= 10.
	predicate_info_pair_score(argnames(_), _, _, 10).
	predicate_info_pair_score(arguments(Arguments), _, _, Score) :-
		arguments_with_descriptions(Arguments, 0, NumberOfArguments, 0, NumberOfDescriptions),
		(	NumberOfDescriptions =:= 0 ->
			% no argument descriptions present
			Score is 3
		;	% argument description present for at least one of the arguments
			Ratio is NumberOfArguments / NumberOfDescriptions,
			Score is 3 + 7 / Ratio
		).

	arguments_with_descriptions([], NumberOfArguments, NumberOfArguments, NumberOfDescriptions, NumberOfDescriptions).
	arguments_with_descriptions([_-Description| Arguments], NumberOfArguments0, NumberOfArguments, NumberOfDescriptions0, NumberOfDescriptions) :-
		NumberOfArguments1 is NumberOfArguments0 + 1,
		(	atom_length(Description, Length), Length >= 10 ->
			NumberOfDescriptions1 is NumberOfDescriptions0 + 1
		;	NumberOfDescriptions1 is NumberOfDescriptions0
		),
		arguments_with_descriptions(Arguments, NumberOfArguments1, NumberOfArguments, NumberOfDescriptions1, NumberOfDescriptions).

	entity_score(_Entity, Score) -->
		['Documentation: ~w%'-[Score], nl].

:- end_object.
