%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2017-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-FileCopyrightText: 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com>
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


:- object(lcom_metric,
	imports((code_metrics_utilities, code_metric))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-13,
		comment is 'Lack of Cohesion Of Methods metric (LCOM4).',
		remarks is [
			'Score computation' - 'Computes the number of connected components in the undirected graph whose nodes are the locally defined predicates and whose edges represent direct internal calls between them.',
			'Score interpretation' - 'A score of 1 indicates a fully cohesive entity. Higher scores indicate that the entity may benefit from being split. Protocols are not scored as they cannot define predicates.',
			'Score representation' - 'The score is represented by the compound term ``lcom(Components, Predicates)`` where ``Components`` is the number of connected components and ``Predicates`` is the total number of locally defined (non-auxiliary) predicates.'
		]
	]).

	:- uses(list, [
		append/3, member/2, memberchk/2, length/2, subtract/3
	]).

	:- uses(logtalk, [
		print_message/3
	]).

	entity_score(Entity, lcom(Components, Total)) :-
		(	var(Entity) ->
			^^current_entity(Entity)
		;	true
		),
		^^entity_kind(Entity, Kind),
		Kind \== protocol,
		entity_score_(Kind, Entity, Components, Total).

	entity_score_(Kind, Entity, Components, Total) :-
		defined_predicates(Kind, Entity, Defined),
		length(Defined, Total),
		findall(
			Caller-Callee,
			internal_edge(Kind, Entity, Defined, Caller, Callee),
			RawEdges
		),
		sort(RawEdges, Edges),
		connected_components(Defined, Edges, Components).

	defined_predicates(object, Entity, Defined) :-
		findall(
			Predicate,
			(	object_property(Entity, defines(Predicate, Properties)),
				\+ memberchk(auxiliary, Properties)
			),
			Bag
		),
		sort(Bag, Defined).
	defined_predicates(category, Entity, Defined) :-
		findall(
			Predicate,
			(	category_property(Entity, defines(Predicate, Properties)),
				\+ memberchk(auxiliary, Properties)
			),
			Bag
		),
		sort(Bag, Defined).

	internal_edge(object, Entity, Defined, Caller, Callee) :-
		object_property(Entity, calls(Callee, Properties)),
		ground(Callee),
		Callee = _/_,
		memberchk(caller(Caller), Properties),
		member(Callee, Defined).
	internal_edge(object, Entity, Defined, Caller, Callee) :-
		object_property(Entity, updates(Callee, Properties)),
		ground(Callee),
		Callee = _/_,
		memberchk(updater(Caller), Properties),
		member(Callee, Defined).
	internal_edge(category, Entity, Defined, Caller, Callee) :-
		category_property(Entity, calls(Callee, Properties)),
		ground(Callee),
		Callee = _/_,
		memberchk(caller(Caller), Properties),
		member(Callee, Defined).
	internal_edge(category, Entity, Defined, Caller, Callee) :-
		category_property(Entity, updates(Callee, Properties)),
		ground(Callee),
		Callee = _/_,
		memberchk(updater(Caller), Properties),
		member(Callee, Defined).

	% connected_components(+Nodes, +Edges, -Count)
	% Count = number of connected components in the undirected graph
	connected_components(Nodes, Edges, Count) :-
		connected_components_(Nodes, Edges, 0, Count).

	connected_components_([], _, Count, Count).
	connected_components_([Node| Rest], Edges, Count0, Count) :-
		reachable(Node, Edges, Reachable),
		subtract(Rest, Reachable, Remaining),
		Count1 is Count0 + 1,
		connected_components_(Remaining, Edges, Count1, Count).

	% reachable(+Start, +Edges, -Reachable)
	% Reachable = all nodes reachable from Start via undirected edges (excluding Start itself)
	reachable(Start, Edges, Reachable) :-
		reachable_([Start], Edges, [Start], Visited),
		subtract(Visited, [Start], Reachable).

	reachable_([], _, Visited, Visited).
	reachable_([Node| Worklist], Edges, Visited0, Visited) :-
		findall(
			Neighbor,
			(	adjacent(Node, Edges, Neighbor),
				\+ member(Neighbor, Visited0)
			),
			NewNodes
		),
		append(Worklist, NewNodes, Worklist1),
		append(Visited0, NewNodes, Visited1),
		reachable_(Worklist1, Edges, Visited1, Visited).

	adjacent(A, Edges, B) :-
		member(A-B, Edges).
	adjacent(A, Edges, B) :-
		member(B-A, Edges).

	process_entity(Kind, Entity) :-
		entity_score_(Kind, Entity, Components, Total),
		print_message(information, code_metrics, lcom(Components, Total)).

	format_entity_score(_Entity, lcom(Components, Total)) -->
		['LCOM4: ~w'-[Components], nl],
		['Number of predicates: ~w'-[Total], nl].

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(lcom(Components, Total), code_metrics) -->
		['LCOM4: ~w'-[Components], nl],
		['Number of predicates: ~w'-[Total], nl].

:- end_object.
