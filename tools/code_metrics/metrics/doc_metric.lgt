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
		version is 0.1,
		author is 'Paulo Moura',
		date is 2017/10/23,
		comment is 'Entity and entity predicates documentation score.',
		remarks is [
			'Score range' - 'Score is a percentage where a 100% score means that all expected documentation information is present.',
			'Score heights' - 'The score is split between 20% for the entity documentation and 80% for the entity predicates documentation.'
		]
	]).

	:- uses(list, [
		length/2, select/3
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
		Score is Info1Score * 20 / 100 + Info2Score * 80 / 100.

	info_1_score(Entity, Score) :-
		(	^^entity_property(Entity, info(Info)) ->
			info_1_score(Entity, Info, Score)
		;	Score = 0
		).

	info_1_score(Entity, Info, Score) :-
		info_1_score(Info, Entity, 0, Score0),
		(	atom(Entity) ->
			Score is Score0 * 100 / 40
		;	% parametric entity
			Score is Score0 * 100 / 50
		).

	info_1_score([], _, Score, Score).
	info_1_score([Pair| Pairs], Entity, Score0, Score) :-
		(	info_1_pair_score(Pair, Entity, PairScore) ->
			Score1 is Score0 + PairScore
		;	Score1 is Score0
		),
		info_1_score(Pairs, Entity, Score1, Score).

	info_1_pair_score(author(Author), _, 10) :-
		atom_length(Author, Length), Length >= 2.
	info_1_pair_score(version(_), _, 10).
	info_1_pair_score(date(_), _, 10).
	info_1_pair_score(comment(Comment), _, 10) :-
		atom_length(Comment, Length), Length >= 10.
	info_1_pair_score(parnames(_), _, 10).
	info_1_pair_score(parameters(_), _, 10).

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
		(	select(mode(_,_), Properties, OtherProperties) ->
			info_2_predicate_score(OtherProperties, Predicate, 10, Score0)
		;	info_2_predicate_score(Properties, Predicate, 0, Score0)
		),
		(	Predicate = _/0 ->
			Score is Score0 * 100 / 20
		;	Score is Score0 * 100 / 30
		).

	info_2_predicate_score([], _, Score, Score).
	info_2_predicate_score([Pair| Pairs], Predicate, Score0, Score) :-
		(	info_2_pair_score(Pair, Predicate, PairScore) ->
			Score1 is Score0 + PairScore
		;	Score1 is Score0
		),
		info_2_predicate_score(Pairs, Predicate, Score1, Score).

	info_2_pair_score(info(Pairs), Predicate, Score) :-
		info_2_info_score(Pairs, Predicate, 0, Score).

	info_2_info_score([], _, Score, Score).
	info_2_info_score([Pair| Pairs], Predicate, Score0, Score) :-
		(	info_2_info_score(Pair, Predicate, PairScore) ->
			Score1 is Score0 + PairScore
		;	Score1 is Score0
		),
		info_2_info_score(Pairs, Predicate, Score1, Score).

	info_2_info_score(comment(Comment), _, 10) :-
		atom_length(Comment, Length), Length >= 10.
	info_2_info_score(argnames(_), _, 10).
	info_2_info_score(arguments(_), _, 10).

	metric_label('Documentation').

:- end_object.
