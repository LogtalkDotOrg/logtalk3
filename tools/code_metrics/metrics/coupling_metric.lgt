%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com>
%                 Paulo Moura         <pmoura@logtalk.org>
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


:- object(coupling_metric,
	implements(code_metrics_protocol),
	imports(code_metrics_utilities)).

	:- info([
		version is 0.1,
		author is 'Ebrahim Azarisooreh',
		date is 2017/1/2,
		comment is 'Analyzes the coupling score for objects, categories, and protocols.',
		remarks is [
			'Calls and Updates'-'Any calls or dynamic updates to predicates in external objects or categories increments the coupling score.',
			'Ancestors'-'Any direct inheritance relations to the entity in question will also increment the score. Duplicate entity couplings will not be scored multiple times.',
			'Interpretation'-'Generally speaking, lower scores are better.'
		]
	]).

	:- uses(list, [memberchk/2]).

	item_score(Entity, Score) :-
		^^current_entity(Entity),
		score(Entity, Score).

	score(Entity, Score) :-
		^^entity_kind(Entity, EntityKind),
		(   EntityKind == object
		;   EntityKind == category
		),
		!,
		coupling_score(EntityKind, Entity, Score).

	score(Protocol, Score) :-
		^^entity_kind(Protocol, protocol),
		coupling_score_protocol(Protocol, 0, Score, []).

	% Measure the coupling scores for objects and categories
	coupling_score(EntityKind, Entity, Score) :-
		coupling_score(EntityKind, Entity, 0, Score, []).

	coupling_score_protocol(Protocol, Score0, Score, LoggedEntities) :-
		(   unvisited_ancestor(protocol, Protocol, Ancestor, LoggedEntities)
		->  Score1 is Score0 + 1,
			coupling_score_protocol(Protocol, Score1, Score, [Ancestor|LoggedEntities])
		;   Score0 = Score
		).

	coupling_score(EntityKind, Entity, Score0, Score, LoggedEntities) :-
		(   unvisited_ancestor(EntityKind, Entity, Ancestor, LoggedEntities)
		->  Score1 is Score0 + 1,
			coupling_score(EntityKind, Entity, Score1, Score, [Ancestor|LoggedEntities])
		;   unvisited_call(Entity, Entity2, LoggedEntities)
		->  Score1 is Score0 + 1,
			coupling_score(EntityKind, Entity, Score1, Score, [Entity2|LoggedEntities])
		;   Score0 = Score
		).

	% Increment the score if there are any direct ancestors
	unvisited_ancestor(EntityKind, Entity, Ancestor, LoggedEntities) :-
		^^ancestor(EntityKind, Entity, _, Ancestor),
		\+ memberchk(Ancestor, LoggedEntities).

	% score any calls external to the entity being measured
	unvisited_call(Entity, Entity2, LoggedEntities) :-
		external_call(Entity, Entity2, _Caller, _Predicate),
		\+ memberchk(Entity2, LoggedEntities),
		!.

	external_call(Entity, Entity2, Caller, Predicate) :-
		^^entity_calls(Entity, Caller, Predicate),
		external_call_(Entity, Entity2, Predicate).

	external_call(Entity, Entity2, Caller, Predicate) :-
		^^entity_updates(Entity, Caller, Predicate),
		external_call_(Entity, Entity2, Predicate).

	external_call_(Entity, Obj, Obj::_Functor/_N) :-
		Entity \== Obj.

	external_call_(Entity, Entity2, Predicate) :-
		(   Predicate = Functor/N
		;   Predicate = ^^Functor/N
		),
		\+ ^^defines_predicate(Entity, Predicate),
		^^current_entity(Entity2),
		Entity \== Entity2,
		^^defines_predicate(Entity2, Predicate).

:- end_object.
