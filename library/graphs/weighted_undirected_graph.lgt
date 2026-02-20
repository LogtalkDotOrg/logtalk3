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


:- object(weighted_undirected_graph(_Dictionary_),
	implements(weighted_graph_protocol),
	imports((graph_common, undirected_graph_common))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-20,
		comment is 'Weighted undirected graph predicates using a dictionary representation. Each edge is stored in both directions. Edge weights use a pair representation (Vertex-Weight in neighbor lists, (V1-V2)-Weight for edge lists). The parametric object parameter is the dictionary to use for the graph representation.',
		parnames is ['Dictionary']
	]).

	% Additional public predicates specific to weighted undirected graphs

	:- public(degree/3).
	:- mode(degree(+vertex, +graph, -integer), zero_or_one).
	:- info(degree/3, [
		comment is 'Returns the degree (number of edges incident to the vertex) of ``Vertex`` in ``Graph``.',
		argnames is ['Vertex', 'Graph', 'Degree']
	]).

	:- public(is_connected/1).
	:- mode(is_connected(+graph), zero_or_one).
	:- info(is_connected/1, [
		comment is 'True if ``Graph`` is connected (all vertices are reachable from any vertex).',
		argnames is ['Graph']
	]).

	:- public(connected_components/2).
	:- mode(connected_components(+graph, -list(list)), one).
	:- info(connected_components/2, [
		comment is 'Returns the list of connected components (each a list of vertices).',
		argnames is ['Graph', 'Components']
	]).

	:- public(min_tree/3).
	:- mode(min_tree(+graph, -graph, -number), zero_or_one).
	:- info(min_tree/3, [
		comment is 'Constructs a minimum spanning tree and returns its total weight.',
		argnames is ['Graph', 'Tree', 'Cost']
	]).

	:- public(max_tree/3).
	:- mode(max_tree(+graph, -graph, -number), zero_or_one).
	:- info(max_tree/3, [
		comment is 'Constructs a maximum spanning tree and returns its total weight.',
		argnames is ['Graph', 'Tree', 'Cost']
	]).

	% Dictionary operations

	:- uses(_Dictionary_, [
		new/1 as dict_new/1,
		empty/1 as dict_empty/1,
		insert/4 as dict_insert/4,
		lookup/3 as dict_lookup/3,
		delete/4 as dict_delete/4,
		keys/2 as dict_keys/2,
		as_list/2 as dict_as_list/2,
		as_dictionary/2 as dict_as_dictionary/2
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2, msort/2, reverse/2, subtract/3
	]).

	% === Graph creation (new/1 only; new/2, new/3 from graph_common) ===

	new(Graph) :-
		dict_new(Graph).

	% === Basic queries ===

	empty(Graph) :-
		dict_empty(Graph).

	vertices(Graph, Vertices) :-
		dict_keys(Graph, Vertices).

	edges(Graph, Edges) :-
		dict_as_list(Graph, Pairs),
		^^wpairs_to_edges(Pairs, AllEdges),
		canonical_wedges(AllEdges, Edges).

	% === Vertex operations (add_vertices/3, delete_vertices/3 from graph_common) ===

	add_vertex(Graph, Vertex, NewGraph) :-
		(	dict_lookup(Vertex, _, Graph) ->
			NewGraph = Graph
		;	dict_insert(Graph, Vertex, [], NewGraph)
		).

	delete_vertex(Graph, Vertex, NewGraph) :-
		(	dict_delete(Graph, Vertex, _, G1) ->
			dict_as_list(G1, Pairs),
			wremove_vertex_from_all(Pairs, Vertex, NewPairs),
			dict_as_dictionary(NewPairs, NewGraph)
		;	NewGraph = Graph
		).

	% === Edge operations (undirected: edges stored in both directions) ===

	edge(Vertex1, Vertex2, Weight, Graph) :-
		dict_lookup(Vertex1, WNeighbors, Graph),
		wfind(WNeighbors, Vertex2, Weight).

	add_edge(Graph, Vertex1, Vertex2, Weight, NewGraph) :-
		% Ensure Vertex1 is in graph
		(	dict_lookup(Vertex1, WNs1, Graph) ->
			winsert_neighbor(WNs1, Vertex2, Weight, NewWNs1)
		;	NewWNs1 = [Vertex2-Weight]
		),
		dict_insert(Graph, Vertex1, NewWNs1, G1),
		% Also add reverse direction
		(	dict_lookup(Vertex2, WNs2, G1) ->
			winsert_neighbor(WNs2, Vertex1, Weight, NewWNs2)
		;	NewWNs2 = [Vertex1-Weight]
		),
		dict_insert(G1, Vertex2, NewWNs2, NewGraph).

	add_edges(Graph, [], Graph).
	add_edges(Graph, [(Vertex1-Vertex2)-Weight| Edges], NewGraph) :-
		add_edge(Graph, Vertex1, Vertex2, Weight, G1),
		add_edges(G1, Edges, NewGraph).

	delete_edge(Graph, Vertex1, Vertex2, Weight, NewGraph) :-
		(	dict_lookup(Vertex1, WNs1, Graph),
			wremove_neighbor(WNs1, Vertex2, Weight, NewWNs1) ->
			dict_insert(Graph, Vertex1, NewWNs1, G1),
			(	dict_lookup(Vertex2, WNs2, G1),
				wremove_neighbor(WNs2, Vertex1, _, NewWNs2) ->
				dict_insert(G1, Vertex2, NewWNs2, NewGraph)
			;	NewGraph = G1
			)
		;	NewGraph = Graph
		).

	delete_edges(Graph, [], Graph).
	delete_edges(Graph, [(Vertex1-Vertex2)-Weight| Edges], NewGraph) :-
		(	delete_edge(Graph, Vertex1, Vertex2, Weight, G1) ->
			true
		;	G1 = Graph
		),
		delete_edges(G1, Edges, NewGraph).

	% === Neighbor queries ===

	neighbors(Vertex, Graph, Neighbors) :-
		dict_lookup(Vertex, WNeighbors, Graph),
		strip_weights(WNeighbors, Neighbors).

	wneighbors(Vertex, Graph, WNeighbors) :-
		dict_lookup(Vertex, WNeighbors, Graph).

	% === Degree ===

	degree(Vertex, Graph, Degree) :-
		dict_lookup(Vertex, WNeighbors, Graph),
		length(WNeighbors, Degree).

	% === Connectivity ===

	is_connected(Graph) :-
		dict_keys(Graph, Vertices),
		Vertices = [Vertex| _],
		^^reachable(Vertex, Graph, Reachable),
		length(Vertices, NV),
		length(Reachable, NR),
		NV =:= NR.

	connected_components(Graph, Components) :-
		dict_keys(Graph, Vertices),
		connected_components_loop(Vertices, Graph, [], Components).

	% === Min Path (Dijkstra) ===

	min_path(V1, V2, Graph, Path, Cost) :-
		(	V1 == V2 ->
			neighbors(V1, Graph, _),
			Path = [V1], Cost = 0
		;	dict_new(Dist0),
			dict_insert(Dist0, V1, 0-none, Dist1),
			dijkstra([0-V1], V2, Graph, Dist1, FinalDist),
			dict_lookup(V2, Cost-_, FinalDist),
			trace_back(V2, V1, FinalDist, [V2], Path)
		).

	% === Max Path (DFS exploring all simple paths) ===

	max_path(V1, V2, Graph, Path, Cost) :-
		(	V1 == V2 ->
			neighbors(V1, Graph, _),
			Path = [V1], Cost = 0
		;	max_wpath_dfs(V1, V2, Graph, [V1], 0, none, none, FinalPath, FinalCost),
			FinalPath \== none,
			Path = FinalPath,
			Cost = FinalCost
		).

	% === Minimum Spanning Tree (Kruskal) ===

	min_tree(Graph, Tree, TotalCost) :-
		edges(Graph, Edges),
		msort_wedges(Edges, SortedEdges),
		dict_keys(Graph, Vertices),
		init_uf(Vertices, UF0),
		kruskal(SortedEdges, UF0, [], TotalCost, 0, TreeEdges),
		new(Tree0),
		add_vertices_from_list(Vertices, Tree0, Tree1),
		add_tree_edges(TreeEdges, Tree1, Tree).

	% === Maximum Spanning Tree (Kruskal with reversed sort) ===

	max_tree(Graph, Tree, TotalCost) :-
		edges(Graph, Edges),
		msort_wedges_desc(Edges, SortedEdges),
		dict_keys(Graph, Vertices),
		init_uf(Vertices, UF0),
		kruskal(SortedEdges, UF0, [], TotalCost, 0, TreeEdges),
		new(Tree0),
		add_vertices_from_list(Vertices, Tree0, Tree1),
		add_tree_edges(TreeEdges, Tree1, Tree).

	% ===========================================================
	% Auxiliary predicates
	% ===========================================================

	% --- Weighted neighbor list operations ---

	winsert_neighbor([], V, W, [V-W]).
	winsert_neighbor([V0-W0| Rest], V, W, Result) :-
		compare(Order, V, V0),
		(	Order == (<) ->
			Result = [V-W, V0-W0| Rest]
		;	Order == (=) ->
			Result = [V-W| Rest]
		;	Result = [V0-W0| Rest1],
			winsert_neighbor(Rest, V, W, Rest1)
		).

	wremove_neighbor([V0-W0| Rest], V, W, Result) :-
		compare(Order, V0, V),
		(	Order == (=) ->
			W = W0,
			Result = Rest
		;	Order == (<) ->
			Result = [V0-W0| Rest1],
			wremove_neighbor(Rest, V, W, Rest1)
		;	fail
		).

	wfind([V0-W0|_], V, W) :-
		V0 == V,
		!,
		W = W0.
	wfind([_| Rest], V, W) :-
		wfind(Rest, V, W).

	strip_weights([], []).
	strip_weights([Vertex-_| WNeighbors], [Vertex| Neighbors]) :-
		strip_weights(WNeighbors, Neighbors).

	% Remove duplicate undirected edges: keep only V1-(V2-W) where V1 @< V2
	canonical_wedges([], []).
	canonical_wedges([(V1-V2)-W|Edges], Result) :-
		(	V1 @< V2 ->
			Result = [(V1-V2)-W|Rest],
			canonical_wedges(Edges, Rest)
		;	canonical_wedges(Edges, Result)
		).

	% --- Remove vertex from all weighted neighbor lists ---

	wremove_vertex_from_all([], _, []).
	wremove_vertex_from_all([V-WNs|Pairs], Vertex, [V-NewWNs|NewPairs]) :-
		wsubtract_vertex(WNs, Vertex, NewWNs),
		wremove_vertex_from_all(Pairs, Vertex, NewPairs).

	wsubtract_vertex([], _, []).
	wsubtract_vertex([V0-W0|Rest], V, Result) :-
		compare(Order, V0, V),
		(	Order == (=) ->
			wsubtract_vertex(Rest, V, Result)
		;	Order == (<) ->
			Result = [V0-W0|Rest1],
			wsubtract_vertex(Rest, V, Rest1)
		;	Result = [V0-W0|Rest]
		).

	% --- Connected components ---

	connected_components_loop([], _, Components, Components).
	connected_components_loop([Vertex| Vertices], Graph, Components0, Components) :-
		(	vertex_in_component(Vertex, Components0) ->
			connected_components_loop(Vertices, Graph, Components0, Components)
		;	^^reachable(Vertex, Graph, Reachable),
			subtract(Vertices, Reachable, Remaining),
			connected_components_loop(Remaining, Graph, [Reachable| Components0], Components)
		).

	vertex_in_component(Vertex, [Component| Components]) :-
		(	member(Vertex, Component) ->
			true
		;	vertex_in_component(Vertex, Components)
		).

	% --- Dijkstra (min path) ---

	dijkstra([], _, _, _, _) :-
		fail.
	dijkstra([_D-V|_Queue], Target, _Graph, Dist, Dist) :-
		V == Target,
		!.
	dijkstra([D-V|Queue], Target, Graph, Dist, FinalDist) :-
		dict_lookup(V, BestD-_, Dist),
		(	D > BestD ->
			dijkstra(Queue, Target, Graph, Dist, FinalDist)
		;	(	dict_lookup(V, WNeighbors, Graph) ->
				relax_neighbors(WNeighbors, V, D, Queue, Dist, NewQueue, NewDist)
			;	NewQueue = Queue,
				NewDist = Dist
			),
			dijkstra(NewQueue, Target, Graph, NewDist, FinalDist)
		).

	relax_neighbors([], _, _, Queue, Dist, Queue, Dist).
	relax_neighbors([N-W|WNs], V, D, Queue, Dist, FinalQueue, FinalDist) :-
		NewD is D + W,
		(	dict_lookup(N, OldD-_, Dist),
			OldD =< NewD ->
			relax_neighbors(WNs, V, D, Queue, Dist, FinalQueue, FinalDist)
		;	dict_insert(Dist, N, NewD-V, Dist1),
			pq_insert(Queue, NewD-N, Queue1),
			relax_neighbors(WNs, V, D, Queue1, Dist1, FinalQueue, FinalDist)
		).

	pq_insert([], Item, [Item]).
	pq_insert([D2-V2|Rest], D1-V1, Result) :-
		(	D1 =< D2 ->
			Result = [D1-V1, D2-V2|Rest]
		;	Result = [D2-V2|Rest1],
			pq_insert(Rest, D1-V1, Rest1)
		).

	trace_back(V, V, _, Path, Path) :- !.
	trace_back(V, Start, Dist, Acc, Path) :-
		dict_lookup(V, _-Prev, Dist),
		trace_back(Prev, Start, Dist, [Prev|Acc], Path).

	% --- DFS max_path (weighted) ---

	max_wpath_dfs(V, Target, _Graph, Visited, CurrCost, BestPath0, BestCost0, BestPath, BestCost) :-
		V == Target,
		!,
		(	BestCost0 == none ->
			BestCost = CurrCost,
			reverse(Visited, BestPath)
		;	CurrCost > BestCost0 ->
			BestCost = CurrCost,
			reverse(Visited, BestPath)
		;	BestCost = BestCost0,
			BestPath = BestPath0
		).
	max_wpath_dfs(V, Target, Graph, Visited, CurrCost, BestPath0, BestCost0, BestPath, BestCost) :-
		dict_lookup(V, WNeighbors, Graph),
		max_wpath_try(WNeighbors, Target, Graph, Visited, CurrCost, BestPath0, BestCost0, BestPath, BestCost).

	max_wpath_try([], _, _, _, _, BestPath, BestCost, BestPath, BestCost).
	max_wpath_try([N-W|WNs], Target, Graph, Visited, CurrCost, BestPath0, BestCost0, BestPath, BestCost) :-
		(	memberchk(N, Visited) ->
			max_wpath_try(WNs, Target, Graph, Visited, CurrCost, BestPath0, BestCost0, BestPath, BestCost)
		;	NewCost is CurrCost + W,
			max_wpath_dfs(N, Target, Graph, [N|Visited], NewCost, BestPath0, BestCost0, BestPath1, BestCost1),
			max_wpath_try(WNs, Target, Graph, Visited, CurrCost, BestPath1, BestCost1, BestPath, BestCost)
		).

	% --- Kruskal's algorithm ---

	% Sort edges by weight (ascending for min, descending for max)
	msort_wedges(Edges, Sorted) :-
		wattach_weights(Edges, WeightedEdges),
		msort(WeightedEdges, SortedWeighted),
		wdetach_weights(SortedWeighted, Sorted).

	msort_wedges_desc(Edges, Sorted) :-
		wattach_neg_weights(Edges, WeightedEdges),
		msort(WeightedEdges, SortedWeighted),
		wdetach_neg_weights(SortedWeighted, Sorted).

	wattach_weights([], []).
	wattach_weights([(Vertex1-Vertex2)-Weight| Edges], [Weight-((Vertex1-Vertex2)-Weight)| Weighted]) :-
		wattach_weights(Edges, Weighted).

	wdetach_weights([], []).
	wdetach_weights([_-Edge| Weighted], [Edge| Edges]) :-
		wdetach_weights(Weighted, Edges).

	wattach_neg_weights([], []).
	wattach_neg_weights([(Vertex1-Vertex2)-Weight| Edges], [NegWeight-((Vertex1-Vertex2)-Weight)| Weighted]) :-
		NegWeight is -Weight,
		wattach_neg_weights(Edges, Weighted).

	wdetach_neg_weights([], []).
	wdetach_neg_weights([_-Edge| Weighted], [Edge| Edges]) :-
		wdetach_neg_weights(Weighted, Edges).

	kruskal([], _, TreeEdges, Cost, Cost, TreeEdges).
	kruskal([(Vertex1-Vertex2)-Weight| Edges], UF0, Acc, TotalCost, CostAcc, TreeEdges) :-
		find(Vertex1, UF0, Root1, UF1),
		find(Vertex2, UF1, Root2, UF2),
		(	Root1 == Root2 ->
			kruskal(Edges, UF2, Acc, TotalCost, CostAcc, TreeEdges)
		;	union(Root1, Root2, UF2, UF3),
			NewCostAcc is CostAcc + Weight,
			kruskal(Edges, UF3, [(Vertex1-Vertex2)-Weight| Acc], TotalCost, NewCostAcc, TreeEdges)
		).

	% --- Union-Find data structure ---

	init_uf(Vertices, UF) :-
		dict_new(UF0),
		uf_init_vertices(Vertices, UF0, UF).

	uf_init_vertices([], UF, UF).
	uf_init_vertices([Vertex| Vertices], UF0, UF) :-
		dict_insert(UF0, Vertex, Vertex-0, UF1),
		uf_init_vertices(Vertices, UF1, UF).

	find(V, UF0, Root, UF) :-
		dict_lookup(V, Parent-Rank, UF0),
		(	Parent == V ->
			Root = V,
			UF = UF0
		;	find(Parent, UF0, Root, UF1),
			dict_insert(UF1, V, Root-Rank, UF)
		).

	union(Root1, Root2, UF0, UF) :-
		dict_lookup(Root1, Root1-Rank1, UF0),
		dict_lookup(Root2, Root2-Rank2, UF0),
		(	Rank1 < Rank2 ->
			dict_insert(UF0, Root1, Root2-Rank1, UF)
		;	Rank1 > Rank2 ->
			dict_insert(UF0, Root2, Root1-Rank2, UF)
		;	NewRank is Rank1 + 1,
			dict_insert(UF0, Root2, Root1-Rank2, UF1),
			dict_insert(UF1, Root1, Root1-NewRank, UF)
		).

	% --- Build tree from edges ---

	add_vertices_from_list([], Tree, Tree).
	add_vertices_from_list([Vertex| Vertices], Tree0, Tree) :-
		add_vertex(Tree0, Vertex, T1),
		add_vertices_from_list(Vertices, T1, Tree).

	add_tree_edges([], Tree, Tree).
	add_tree_edges([(Vertex1-Vertex2)-Weight| Edges], Tree0, Tree) :-
		add_edge(Tree0, Vertex1, Vertex2, Weight, Tree1),
		add_tree_edges(Edges, Tree1, Tree).

:- end_object.


:- object(weighted_undirected_graph,
	extends(weighted_undirected_graph(avltree))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'Weighted undirected graph predicates using the AVL tree dictionary representation. Each edge is stored in both directions. Edge weights use a pair representation (Vertex-Weight in neighbor lists, (V1-V2)-Weight for edge lists).'
	]).

:- end_object.
