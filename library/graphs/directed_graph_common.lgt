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


:- category(directed_graph_common,
	implements(directed_graph_protocol),
	extends(graph_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-25,
		comment is 'Common directed graph predicates shared by directed graph objects. Uses self-dispatch to call object-specific predicates and avltree for internal bookkeeping.'
	]).

	:- uses(avltree, [
		new/1 as scc_new/1,
		insert/4 as scc_insert/4,
		lookup/3 as scc_lookup/3
	]).

	:- uses(list, [
		length/2, member/2, reverse/2, subtract/3
	]).

	% === Degree queries ===

	in_degree(Vertex, Graph, Degree) :-
		::neighbors(Vertex, Graph, _),
		::vertices(Graph, Vertices),
		count_incoming(Vertices, Vertex, Graph, 0, Degree).

	out_degree(Vertex, Graph, Degree) :-
		::neighbors(Vertex, Graph, Neighbors),
		length(Neighbors, Degree).

	% === DAG test ===

	is_acyclic(Graph) :-
		::topological_sort(Graph, Sorted),
		::vertices(Graph, Vertices),
		length(Sorted, N1),
		length(Vertices, N2),
		N1 =:= N2.

	has_cycle(Graph) :-
		cycle(Graph, _),
		!.

	cycle(Graph, Cycle) :-
		::vertices(Graph, Vertices),
		member(Start, Vertices),
		directed_cycle_from(Start, Start, Graph, [Start], ReverseCycle),
		reverse(ReverseCycle, Cycle).

	% === Strongly Connected Components (Tarjan's algorithm) ===

	strongly_connected_components(Graph, SCCs) :-
		::vertices(Graph, Vertices),
		scc_new(Info0),
		tarjan_all(Vertices, Graph, Info0, 0, [], [], _Info, _Index, _Stack, SCCs).

	% === Weakly Connected Components ===

	weakly_connected_components(Graph, Components) :-
		::vertices(Graph, Vertices),
		::symmetric_closure(Graph, Closure),
		weakly_connected_components_loop(Vertices, Closure, [], Components).

	% ===========================================================
	% Auxiliary predicates
	% ===========================================================

	% --- In-degree predicates ---

	count_incoming([], _, _, Degree, Degree).
	count_incoming([Vertex| Vertices], Target, Graph, Degree0, Degree) :-
		(	::neighbors(Vertex, Graph, Neighbors),
			member(Target, Neighbors) ->
			Degree1 is Degree0 + 1
		;	Degree1 = Degree0
		),
		count_incoming(Vertices, Target, Graph, Degree1, Degree).

	% --- Tarjan's SCC ---

	directed_cycle_from(Current, Start, Graph, Visited, ReverseCycle) :-
		::neighbors(Current, Graph, Neighbors),
		member(Next, Neighbors),
		( Next == Start ->
			ReverseCycle = [Start| Visited]
		; \+ member(Next, Visited),
			directed_cycle_from(Next, Start, Graph, [Next| Visited], ReverseCycle)
		).

	weakly_connected_components_loop([], _, Components, Components).
	weakly_connected_components_loop([Vertex| Vertices], Graph, Components0, Components) :-
		( vertex_in_component(Vertex, Components0) ->
			weakly_connected_components_loop(Vertices, Graph, Components0, Components)
		;	::reachable(Vertex, Graph, Reachable),
			subtract(Vertices, Reachable, Remaining),
			weakly_connected_components_loop(Remaining, Graph, [Reachable| Components0], Components)
		).

	vertex_in_component(Vertex, [Component| Components]) :-
		( member(Vertex, Component) ->
			true
		;	vertex_in_component(Vertex, Components)
		).

	tarjan_all([], _, Info, Index, Stack, SCCs, Info, Index, Stack, SCCs).
	tarjan_all([Vertex| Vertices], Graph, Info0, Index0, Stack0, SCCs0, Info, Index, Stack, SCCs) :-
		(	scc_lookup(Vertex, _, Info0) ->
			tarjan_all(Vertices, Graph, Info0, Index0, Stack0, SCCs0, Info, Index, Stack, SCCs)
		;	strongconnect(Vertex, Graph, Info0, Index0, Stack0, SCCs0, Info1, Index1, Stack1, SCCs1),
			tarjan_all(Vertices, Graph, Info1, Index1, Stack1, SCCs1, Info, Index, Stack, SCCs)
		).

	strongconnect(Vertex, Graph, Info0, Index0, Stack0, SCCs0, Info, Index, Stack, SCCs) :-
		scc_insert(Info0, Vertex, scc(Index0, Index0, true), Info1),
		Index1 is Index0 + 1,
		Stack1 = [Vertex| Stack0],
		::neighbors(Vertex, Graph, Neighbors),
		process_scc_neighbors(Neighbors, Vertex, Graph, Info1, Index1, Stack1, SCCs0, Info2, Index2, Stack2, SCCs1),
		scc_lookup(Vertex, scc(VIndex, VLowlink, _), Info2),
		(	VIndex =:= VLowlink ->
			pop_scc(Stack2, Vertex, [], SCC0, Stack3),
			sort(SCC0, SCC),
			mark_off_stack(SCC, Info2, Info3),
			Info = Info3, Index = Index2, Stack = Stack3, SCCs = [SCC| SCCs1]
		;	Info = Info2, Index = Index2, Stack = Stack2, SCCs = SCCs1
		).

	process_scc_neighbors([], _, _, Info, Index, Stack, SCCs, Info, Index, Stack, SCCs).
	process_scc_neighbors([Neighbor| Neighbors], Vertex, Graph, Info0, Index0, Stack0, SCCs0, Info, Index, Stack, SCCs) :-
		(	scc_lookup(Neighbor, scc(WIndex, _, WOnStack), Info0) ->
			(	WOnStack == true ->
				scc_lookup(Vertex, scc(VIndex, VLowlink, VOnStack), Info0),
				NewVLowlink is min(VLowlink, WIndex),
				scc_insert(Info0, Vertex, scc(VIndex, NewVLowlink, VOnStack), Info1),
				process_scc_neighbors(Neighbors, Vertex, Graph, Info1, Index0, Stack0, SCCs0, Info, Index, Stack, SCCs)
			;	process_scc_neighbors(Neighbors, Vertex, Graph, Info0, Index0, Stack0, SCCs0, Info, Index, Stack, SCCs)
			)
		;	strongconnect(Neighbor, Graph, Info0, Index0, Stack0, SCCs0, Info1, Index1, Stack1, SCCs1),
			scc_lookup(Vertex, scc(VIndex, VLowlink, VOnStack), Info1),
			scc_lookup(Neighbor, scc(_, WLowlink, _), Info1),
			NewVLowlink is min(VLowlink, WLowlink),
			scc_insert(Info1, Vertex, scc(VIndex, NewVLowlink, VOnStack), Info2),
			process_scc_neighbors(Neighbors, Vertex, Graph, Info2, Index1, Stack1, SCCs1, Info, Index, Stack, SCCs)
		).

	pop_scc([W| Stack], Target, Acc, SCC, RestStack) :-
		(	W == Target ->
			SCC = [W| Acc],
			RestStack = Stack
		;	pop_scc(Stack, Target, [W| Acc], SCC, RestStack)
		).

	mark_off_stack([], Info, Info).
	mark_off_stack([Vertex| Vertices], Info0, Info) :-
		scc_lookup(Vertex, scc(VIndex, VLowlink, _), Info0),
		scc_insert(Info0, Vertex, scc(VIndex, VLowlink, false), Info1),
		mark_off_stack(Vertices, Info1, Info).

:- end_category.
