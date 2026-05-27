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


:- object(interval_relation_set,
	implements(interval_relation_set_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-13,
		comment is 'Allen interval relation-set operations using canonical ordered duplicate-free lists of base relation atoms.'
	]).

	:- uses(list, [
		append/3, memberchk/2
	]).

	relation_set(RelationSet) :-
		(	var(RelationSet) ->
			relation_order(Order),
			relation_subset(Order, RelationSet)
		;	normalize(RelationSet, RelationSet)
		).

	empty([]).

	universal(RelationSet) :-
		relation_order(RelationSet).

	singleton(Relation, [Relation]) :-
		interval_algebra::relation(Relation).

	normalize(Relations, RelationSet) :-
		relation_list(Relations),
		relation_order(Order),
		canonical_relations(Order, Relations, RelationSet).

	member(Relation, RelationSet) :-
		relation_set(RelationSet),
		memberchk(Relation, RelationSet).

	subset(RelationSet1, RelationSet2) :-
		relation_set(RelationSet1),
		relation_set(RelationSet2),
		subset_relations(RelationSet1, RelationSet2).

	intersection(RelationSet1, RelationSet2, Intersection) :-
		relation_set(RelationSet1),
		relation_set(RelationSet2),
		relation_order(Order),
		intersection_relations(Order, RelationSet1, RelationSet2, Intersection).

	union(RelationSet1, RelationSet2, Union) :-
		relation_set(RelationSet1),
		relation_set(RelationSet2),
		append(RelationSet1, RelationSet2, Relations),
		normalize(Relations, Union).

	converse(RelationSet, ConverseSet) :-
		relation_set(RelationSet),
		converse_relations(RelationSet, Converses),
		normalize(Converses, ConverseSet).

	compose(RelationSet1, RelationSet2, Composition) :-
		relation_set(RelationSet1),
		relation_set(RelationSet2),
		compose_relations(RelationSet1, RelationSet2, Relations),
		normalize(Relations, Composition).

	relation_order(Order) :-
		findall(Relation, interval_algebra::relation(Relation), Order).

	relation_list([]).
	relation_list([Relation| Relations]) :-
		interval_algebra::relation(Relation),
		relation_list(Relations).

	relation_subset([], []).
	relation_subset([_Relation| Order], RelationSet) :-
		relation_subset(Order, RelationSet).
	relation_subset([Relation| Order], [Relation| RelationSet]) :-
		relation_subset(Order, RelationSet).

	canonical_relations([], _Relations0, []).
	canonical_relations([Relation| Order], Relations0, [Relation| Relations]) :-
		memberchk(Relation, Relations0),
		!,
		canonical_relations(Order, Relations0, Relations).
	canonical_relations([_Relation| Order], Relations0, Relations) :-
		canonical_relations(Order, Relations0, Relations).

	subset_relations([], _RelationSet2).
	subset_relations([Relation| RelationSet1], RelationSet2) :-
		memberchk(Relation, RelationSet2),
		subset_relations(RelationSet1, RelationSet2).

	intersection_relations([], _RelationSet1, _RelationSet2, []).
	intersection_relations([Relation| Order], RelationSet1, RelationSet2, [Relation| Intersection]) :-
		memberchk(Relation, RelationSet1),
		memberchk(Relation, RelationSet2),
		!,
		intersection_relations(Order, RelationSet1, RelationSet2, Intersection).
	intersection_relations([_Relation| Order], RelationSet1, RelationSet2, Intersection) :-
		intersection_relations(Order, RelationSet1, RelationSet2, Intersection).

	converse_relations([], []).
	converse_relations([Relation| Relations], [Converse| Converses]) :-
		interval_algebra::converse(Relation, Converse),
		converse_relations(Relations, Converses).

	compose_relations([], _RelationSet2, []).
	compose_relations([Relation1| RelationSet1], RelationSet2, Relations) :-
		compose_with_relation(RelationSet2, Relation1, Relations1),
		compose_relations(RelationSet1, RelationSet2, Relations2),
		append(Relations1, Relations2, Relations).

	compose_with_relation([], _Relation1, []).
	compose_with_relation([Relation2| RelationSet2], Relation1, Relations) :-
		interval_algebra::compose(Relation1, Relation2, Relations1),
		compose_with_relation(RelationSet2, Relation1, Relations2),
		append(Relations1, Relations2, Relations).

:- end_object.
