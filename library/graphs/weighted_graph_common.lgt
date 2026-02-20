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


:- category(weighted_graph_common(_Dictionary_),
	implements(weighted_graph_protocol),
	extends(graph_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-20,
		comment is 'Common weighted graph predicates shared by both weighted directed and weighted undirected graph objects. Uses self-dispatch to call object-specific predicates such as ``add_edge/5``, ``delete_edge/5``, and ``edges/2``.',
		parnames is ['Dictionary']
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
		member/2, reverse/2
	]).

	:- uses(pairs, [
		keys/2 as strip_weights/2
	]).

	% Protected predicates for use by importing objects

	:- protected(winsert_neighbor/4).
	:- mode(winsert_neighbor(+list, +vertex, +number, -list), one).
	:- info(winsert_neighbor/4, [
		comment is 'Inserts a weighted neighbor into a sorted weighted neighbor list, replacing any existing entry for the same vertex.',
		argnames is ['WNeighbors', 'Vertex', 'Weight', 'NewWNeighbors']
	]).

	:- protected(wremove_neighbor/4).
	:- mode(wremove_neighbor(+list, +vertex, -number, -list), zero_or_one).
	:- info(wremove_neighbor/4, [
		comment is 'Removes a vertex from a sorted weighted neighbor list, unifying the weight. Fails if the vertex is not found.',
		argnames is ['WNeighbors', 'Vertex', 'Weight', 'NewWNeighbors']
	]).

	:- protected(wfind/3).
	:- mode(wfind(+list, +vertex, -number), zero_or_one).
	:- info(wfind/3, [
		comment is 'Finds the weight associated with a vertex in a weighted neighbor list.',
		argnames is ['WNeighbors', 'Vertex', 'Weight']
	]).

	:- protected(wremove_vertex_from_all/3).
	:- mode(wremove_vertex_from_all(+list(pair), +vertex, -list(pair)), one).
	:- info(wremove_vertex_from_all/3, [
		comment is 'Removes a vertex from all weighted neighbor lists in a list of vertex-neighbors pairs.',
		argnames is ['Pairs', 'Vertex', 'NewPairs']
	]).

	:- protected(relax_neighbors/7).
	:- mode(relax_neighbors(+list, +vertex, +number, +list, +dictionary, -list, -dictionary), one).
	:- info(relax_neighbors/7, [
		comment is 'Relaxes neighbors during Dijkstra shortest path computation, updating distances and priority queue.',
		argnames is ['WNeighbors', 'Vertex', 'Distance', 'Queue', 'Dist', 'NewQueue', 'NewDist']
	]).

	:- protected(pq_insert/3).
	:- mode(pq_insert(+list, +pair, -list), one).
	:- info(pq_insert/3, [
		comment is 'Inserts an element into a sorted priority queue (list of Distance-Vertex pairs).',
		argnames is ['Queue', 'Item', 'NewQueue']
	]).

	% === Graph creation (new/1 only; new/2, new/3 from graph_common) ===

	new(Graph) :-
		dict_new(Graph).

	% === Basic queries ===

	empty(Graph) :-
		dict_empty(Graph).

	vertices(Graph, Vertices) :-
		dict_keys(Graph, Vertices).

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

	% === Edge operations (add_edge/5 and delete_edge/5 are object-specific) ===

	edge(Vertex1, Vertex2, Weight, Graph) :-
		dict_lookup(Vertex1, WNeighbors, Graph),
		wfind(WNeighbors, Vertex2, Weight).

	add_edges(Graph, [], Graph).
	add_edges(Graph, [(Vertex1-Vertex2)-Weight| Edges], NewGraph) :-
		::add_edge(Graph, Vertex1, Vertex2, Weight, NewGraph0),
		add_edges(NewGraph0, Edges, NewGraph).

	delete_edges(Graph, [], Graph).
	delete_edges(Graph, [(Vertex1-Vertex2)-Weight| Edges], NewGraph) :-
		(	::delete_edge(Graph, Vertex1, Vertex2, Weight, NewGraph0) ->
			true
		;	NewGraph0 = Graph
		),
		delete_edges(NewGraph0, Edges, NewGraph).

	% === Neighbor queries ===

	% neighbors/3 returns just vertex names (without weights)
	neighbors(Vertex, Graph, Neighbors) :-
		dict_lookup(Vertex, WNeighbors, Graph),
		strip_weights(WNeighbors, Neighbors).

	% wneighbors/3 returns Vertex-Weight pairs
	wneighbors(Vertex, Graph, WNeighbors) :-
		dict_lookup(Vertex, WNeighbors, Graph).

	% === Min Path (Dijkstra) ===

	min_path(Vertex1, Vertex2, Graph, Path, Cost) :-
		(	Vertex1 == Vertex2 ->
			neighbors(Vertex1, Graph, _),
			Path = [Vertex1], Cost = 0
		;	dict_new(Dist0),
			dict_insert(Dist0, Vertex1, 0-none, Dist1),
			dijkstra([0-Vertex1], Vertex2, Graph, Dist1, FinalDist),
			dict_lookup(Vertex2, Cost-_, FinalDist),
			trace_back(Vertex2, Vertex1, FinalDist, [Vertex2], Path)
		).

	% === Max Path (DFS exploring all simple paths) ===

	max_path(Vertex1, Vertex2, Graph, Path, Cost) :-
		(	Vertex1 == Vertex2 ->
			neighbors(Vertex1, Graph, _),
			Path = [Vertex1], Cost = 0
		;	max_wpath_dfs(Vertex1, Vertex2, Graph, [Vertex1], 0, none, none, FinalPath, FinalCost),
			FinalPath \== none,
			Path = FinalPath,
			Cost = FinalCost
		).

	% ===========================================================
	% Auxiliary predicates
	% ===========================================================

	% --- Weighted neighbor list operations ---

	winsert_neighbor([], Vertex, Weight, [Vertex-Weight]).
	winsert_neighbor([Vertex0-Weight0| Rest], Vertex, Weight, Result) :-
		compare(Order, Vertex, Vertex0),
		(	Order == (<) ->
			Result = [Vertex-Weight, Vertex0-Weight0| Rest]
		;	Order == (=) ->
			Result = [Vertex-Weight| Rest]
		;	Result = [Vertex0-Weight0| Rest1],
			winsert_neighbor(Rest, Vertex, Weight, Rest1)
		).

	wremove_neighbor([Vertex0-Weight0|Rest], Vertex, Weight, Result) :-
		compare(Order, Vertex0, Vertex),
		(	Order == (=) ->
			Weight = Weight0,
			Result = Rest
		;	Order == (<) ->
			Result = [Vertex0-Weight0| Rest1],
			wremove_neighbor(Rest, Vertex, Weight, Rest1)
		;	fail
		).

	wfind([Vertex0-Weight0| _], Vertex, Weight) :-
		Vertex0 == Vertex,
		!,
		Weight = Weight0.
	wfind([_| Rest], Vertex, Weight) :-
		wfind(Rest, Vertex, Weight).

	% --- Remove vertex from all weighted neighbor lists ---

	wremove_vertex_from_all([], _, []).
	wremove_vertex_from_all([Vertex0-WNs| Pairs], Vertex, [Vertex0-NewWNs| NewPairs]) :-
		wsubtract_vertex(WNs, Vertex, NewWNs),
		wremove_vertex_from_all(Pairs, Vertex, NewPairs).

	wsubtract_vertex([], _, []).
	wsubtract_vertex([Vertex0-Weight0| Rest], Vertex, Result) :-
		compare(Order, Vertex0, Vertex),
		(	Order == (=) ->
			wsubtract_vertex(Rest, Vertex, Result)
		;	Order == (<) ->
			Result = [Vertex0-Weight0| Rest1],
			wsubtract_vertex(Rest, Vertex, Rest1)
		;	Result = [Vertex0-Weight0| Rest]
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
	relax_neighbors([Neighbor-Weight| WNeighbors], V, D, Queue, Dist, FinalQueue, FinalDist) :-
		NewD is D + Weight,
		(	dict_lookup(Neighbor, OldD-_, Dist),
			OldD =< NewD ->
			relax_neighbors(WNeighbors, V, D, Queue, Dist, FinalQueue, FinalDist)
		;	dict_insert(Dist, Neighbor, NewD-V, Dist1),
			pq_insert(Queue, NewD-Neighbor, Queue1),
			relax_neighbors(WNeighbors, V, D, Queue1, Dist1, FinalQueue, FinalDist)
		).

	pq_insert([], Item, [Item]).
	pq_insert([D2-Vertex2| Rest], D1-Vertex1, Result) :-
		(	D1 =< D2 ->
			Result = [D1-Vertex1, D2-Vertex2| Rest]
		;	Result = [D2-Vertex2| Rest1],
			pq_insert(Rest, D1-Vertex1, Rest1)
		).

	trace_back(Vertex, Vertex, _, Path, Path) :-
		!.
	trace_back(Vertex, Start, Dist, Acc, Path) :-
		dict_lookup(Vertex, _-Previous, Dist),
		trace_back(Previous, Start, Dist, [Previous| Acc], Path).

	% --- DFS max_path (weighted) ---

	max_wpath_dfs(Vertex, Target, _Graph, Visited, CurrCost, BestPath0, BestCost0, BestPath, BestCost) :-
		Vertex == Target,
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
	max_wpath_dfs(Vertex, Target, Graph, Visited, CurrCost, BestPath0, BestCost0, BestPath, BestCost) :-
		dict_lookup(Vertex, WNeighbors, Graph),
		max_wpath_try(WNeighbors, Target, Graph, Visited, CurrCost, BestPath0, BestCost0, BestPath, BestCost).

	max_wpath_try([], _, _, _, _, BestPath, BestCost, BestPath, BestCost).
	max_wpath_try([Neighbor-Weight| WNeighbors], Target, Graph, Visited, CurrCost, BestPath0, BestCost0, BestPath, BestCost) :-
		(	member(Neighbor, Visited) ->
			max_wpath_try(WNeighbors, Target, Graph, Visited, CurrCost, BestPath0, BestCost0, BestPath, BestCost)
		;	NewCost is CurrCost + Weight,
			max_wpath_dfs(Neighbor, Target, Graph, [Neighbor| Visited], NewCost, BestPath0, BestCost0, BestPath1, BestCost1),
			max_wpath_try(WNeighbors, Target, Graph, Visited, CurrCost, BestPath1, BestCost1, BestPath, BestCost)
		).

:- end_category.
