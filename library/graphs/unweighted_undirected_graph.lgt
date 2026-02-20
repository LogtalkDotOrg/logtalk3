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


:- object(unweighted_undirected_graph(_Dictionary_),
	imports((unweighted_graph_common(_Dictionary_), undirected_graph_common))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-20,
		comment is 'Unweighted undirected graph predicates using a dictionary representation. Undirected edges are stored as two directed edges. The parametric object parameter is the dictionary to use for the graph representation.',
		parnames is ['Dictionary']
	]).

	% Additional public predicates specific to undirected graphs

	:- public(degree/3).
	:- mode(degree(+vertex, +graph, -integer), zero_or_one).
	:- info(degree/3, [
		comment is 'Unifies ``Degree`` with the number of edges incident to ``Vertex``. Fails if ``Vertex`` is not in the graph.',
		argnames is ['Vertex', 'Graph', 'Degree']
	]).

	:- public(is_connected/1).
	:- mode(is_connected(+graph), zero_or_one).
	:- info(is_connected/1, [
		comment is 'True iff the graph is connected (every vertex is reachable from every other vertex).',
		argnames is ['Graph']
	]).

	:- public(connected_components/2).
	:- mode(connected_components(+graph, -list(list(vertex))), one).
	:- info(connected_components/2, [
		comment is 'Unifies ``Components`` with a list of connected components. Each component is a sorted list of vertices.',
		argnames is ['Graph', 'Components']
	]).

	% Dictionary operations

	:- uses(_Dictionary_, [
		insert/4 as dict_insert/4,
		lookup/3 as dict_lookup/3,
		keys/2 as dict_keys/2,
		as_list/2 as dict_as_list/2
	]).

	:- uses(set, [
		insert/3 as set_insert/3,
		subtract/3 as set_subtract/3
	]).

	:- uses(list, [
		length/2, subtract/3
	]).

	% === Edges (edges/2 only; edge/3, add_edges/3, delete_edges/3 from unweighted_graph_common) ===

	% For undirected graphs, return each edge only once (Vertex1 @< Vertex2 or Vertex1 == Vertex2)
	edges(Graph, Edges) :-
		dict_as_list(Graph, Pairs),
		^^pairs_to_edges(Pairs, AllEdges),
		remove_duplicate_edges(AllEdges, Edges).

	% === Edge operations (add_edge/4, delete_edge/4 only) ===

	add_edge(Graph, Vertex1, Vertex2, NewGraph) :-
		add_directed_edge(Graph, Vertex1, Vertex2, NewGraph0),
		add_directed_edge(NewGraph0, Vertex2, Vertex1, NewGraph).

	delete_edge(Graph, Vertex1, Vertex2, NewGraph) :-
		delete_directed_edge(Graph, Vertex1, Vertex2, NewGraph0),
		delete_directed_edge(NewGraph0, Vertex2, Vertex1, NewGraph).

	% === Neighbor queries (exclude self-loops) ===

	neighbors(Vertex, Graph, Neighbors) :-
		dict_lookup(Vertex, AllNeighbors, Graph),
		set_subtract(AllNeighbors, [Vertex], Neighbors).

	% === Degree ===

	degree(Vertex, Graph, Degree) :-
		neighbors(Vertex, Graph, Ns),
		length(Ns, Degree).

	% === Connectivity ===

	is_connected(Graph) :-
		dict_keys(Graph, Vertices),
		(	Vertices == [] ->
			true
		;	Vertices = [Vertex| _],
			^^reachable(Vertex, Graph, Reachable),
			length(Vertices, N),
			length(Reachable, N)
		).

	connected_components(Graph, Components) :-
		dict_keys(Graph, Vertices),
		find_components(Vertices, Graph, Components).

	% ===========================================================
	% Auxiliary predicates
	% ===========================================================

	% --- Directed edge predicates (internal) ---

	add_directed_edge(Graph, Vertex1, Vertex2, NewGraph) :-
		(	dict_lookup(Vertex1, Neighbors, Graph) ->
			set_insert(Neighbors, Vertex2, NewNeighbors)
		;	NewNeighbors = [Vertex2]
		),
		dict_insert(Graph, Vertex1, NewNeighbors, NewGraph0),
		(	dict_lookup(Vertex2, _, NewGraph0) ->
			NewGraph = NewGraph0
		;	dict_insert(NewGraph0, Vertex2, [], NewGraph)
		).

	delete_directed_edge(Graph, Vertex1, Vertex2, NewGraph) :-
		(	dict_lookup(Vertex1, Neighbors, Graph) ->
			set_subtract(Neighbors, [Vertex2], NewNeighbors),
			dict_insert(Graph, Vertex1, NewNeighbors, NewGraph)
		;	NewGraph = Graph
		).

	% Only keep edges where Vertex1 @< Vertex2 or Vertex1 == Vertex2 (self-loops)
	remove_duplicate_edges([], []).
	remove_duplicate_edges([Vertex1-Vertex2| Edges], Result) :-
		(	Vertex1 @> Vertex2 ->
			Result = Rest
		;	Result = [Vertex1-Vertex2| Rest]
		),
		remove_duplicate_edges(Edges, Rest).

	% --- Connected components ---

	find_components([], _, []).
	find_components([Vertex| Vertices], Graph, [Component| Components]) :-
		^^reachable(Vertex, Graph, Component),
		subtract(Vertices, Component, Remaining),
		find_components(Remaining, Graph, Components).

:- end_object.


:- object(unweighted_undirected_graph,
	extends(unweighted_undirected_graph(avltree))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'Unweighted undirected graph predicates using the AVL tree dictionary representation.'
	]).

:- end_object.
