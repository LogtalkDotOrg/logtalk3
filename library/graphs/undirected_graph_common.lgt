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


:- category(undirected_graph_common,
	extends(graph_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-25,
		comment is 'Common predicates shared by undirected graph objects. Uses self-dispatch to call object-specific predicates such as ``is_connected/1``, ``vertices/2``, ``edges/2``, and ``neighbors/3``.'
	]).

	:- uses(list, [
		length/2, member/2, msort/2, reverse/2
	]).

	:- uses(avltree, [
		new/1 as dfs_new/1,
		insert/4 as dfs_insert/4,
		lookup/3 as dfs_lookup/3
	]).

	:- uses(set, [
		delete/3 as set_delete/3,
		empty/1 as set_empty/1,
		insert/3 as set_insert/3,
		intersection/3 as set_intersection/3,
		select/3 as set_select/3,
		size/2 as set_size/2,
		subtract/3 as set_subtract/3,
		union/3 as set_union/3
	]).

	:- public(is_tree/1).
	:- mode(is_tree(+graph), zero_or_one).
	:- info(is_tree/1, [
		comment is 'True iff the graph is a tree, i.e. it is connected and has exactly ``|V| - 1`` edges.',
		argnames is ['Graph']
	]).

	:- public(has_cycle/1).
	:- mode(has_cycle(+graph), zero_or_one).
	:- info(has_cycle/1, [
		comment is 'True iff the graph contains at least one cycle.',
		argnames is ['Graph']
	]).

	:- public(cycle/2).
	:- mode(cycle(+graph, -list(vertex)), zero_or_more).
	:- info(cycle/2, [
		comment is 'Enumerates cycles as lists of vertices where the first and last vertices are the same.',
		argnames is ['Graph', 'Cycle']
	]).

	:- public(graph_coloring/3).
	:- mode(graph_coloring(+graph, -list(pair), -integer), one).
	:- info(graph_coloring/3, [
		comment is 'Computes a greedy vertex coloring of the graph. ``Coloring`` is a list of ``Vertex-Color`` pairs where colors are integers starting from 1. ``NumberOfColors`` is the total number of colors used.',
		argnames is ['Graph', 'Coloring', 'NumberOfColors']
	]).

	:- public(articulation_points/2).
	:- mode(articulation_points(+graph, -list(vertex)), one).
	:- info(articulation_points/2, [
		comment is 'Computes all articulation points (cut vertices) of the graph. The result is a sorted list of vertices.',
		argnames is ['Graph', 'Points']
	]).

	:- public(bridges/2).
	:- mode(bridges(+graph, -list(edge)), one).
	:- info(bridges/2, [
		comment is 'Computes all bridges (cut edges) of the graph. Each bridge is returned once as ``Vertex1-Vertex2`` with ``Vertex1 @< Vertex2``.',
		argnames is ['Graph', 'Bridges']
	]).

	:- public(maximal_cliques/2).
	:- mode(maximal_cliques(+graph, -list(list(vertex))), one).
	:- info(maximal_cliques/2, [
		comment is 'Computes all maximal cliques of the graph using the Bron-Kerbosch algorithm with pivoting. A maximal clique is a clique that cannot be extended by adding another adjacent vertex. Each clique is a sorted list of vertices. The list of cliques is sorted in standard order.',
		argnames is ['Graph', 'Cliques']
	]).

	:- public(maximum_cliques/2).
	:- mode(maximum_cliques(+graph, -list(list(vertex))), one).
	:- info(maximum_cliques/2, [
		comment is 'Computes all maximum cliques of the graph, i.e. the largest maximal cliques. Each clique is a sorted list of vertices. The list of cliques is sorted in standard order. For an empty graph, returns an empty list.',
		argnames is ['Graph', 'Cliques']
	]).

	% === Tree check ===

	is_tree(Graph) :-
		::is_connected(Graph),
		::vertices(Graph, Vertices),
		length(Vertices, V),
		::edges(Graph, Edges),
		length(Edges, E),
		E =:= V - 1.

	has_cycle(Graph) :-
		cycle(Graph, _),
		!.

	cycle(Graph, Cycle) :-
		::vertices(Graph, Vertices),
		member(Start, Vertices),
		undirected_cycle_from(Start, none, Start, Graph, [Start], ReverseCycle),
		reverse(ReverseCycle, Cycle).

	articulation_points(Graph, Points) :-
		::vertices(Graph, Vertices),
		dfs_new(Disc0),
		dfs_new(Low0),
		ap_all(Vertices, Graph, 0, Disc0, Low0, [], [], _Disc, _Low, Points).

	bridges(Graph, Bridges) :-
		::vertices(Graph, Vertices),
		dfs_new(Disc0),
		dfs_new(Low0),
		bridges_all(Vertices, Graph, 0, Disc0, Low0, [], [], _Disc, _Low, Bridges).

	% === Graph coloring (greedy) ===

	graph_coloring(Graph, Coloring, NumberOfColors) :-
		::vertices(Graph, Vertices),
		color_vertices(Vertices, Graph, [], Coloring, 0, NumberOfColors).

	% --- Graph coloring helpers ---

	undirected_cycle_from(Current, Parent, Start, Graph, Visited, ReverseCycle) :-
		::neighbors(Current, Graph, Neighbors),
		member(Next, Neighbors),
		( Next == Parent ->
			fail
		; Next == Start ->
			length(Visited, N),
			N >= 3,
			ReverseCycle = [Start| Visited]
		; \+ member(Next, Visited),
			undirected_cycle_from(Next, Current, Start, Graph, [Next| Visited], ReverseCycle)
		).

	ap_all([], _, _, Disc, Low, Points, _, Disc, Low, Points).
	ap_all([Vertex| Vertices], Graph, Time0, Disc0, Low0, Points0, Bridges0, Disc, Low, Points) :-
		( dfs_lookup(Vertex, _, Disc0) ->
			ap_all(Vertices, Graph, Time0, Disc0, Low0, Points0, Bridges0, Disc, Low, Points)
		;	dfs_visit(Vertex, none, Graph, Time0, Disc0, Low0, Points0, Bridges0, Time1, Disc1, Low1, Points1, _),
			ap_all(Vertices, Graph, Time1, Disc1, Low1, Points1, Bridges0, Disc, Low, Points)
		).

	bridges_all([], _, _, Disc, Low, _, Bridges, Disc, Low, Bridges).
	bridges_all([Vertex| Vertices], Graph, Time0, Disc0, Low0, Points0, Bridges0, Disc, Low, Bridges) :-
		( dfs_lookup(Vertex, _, Disc0) ->
			bridges_all(Vertices, Graph, Time0, Disc0, Low0, Points0, Bridges0, Disc, Low, Bridges)
		;	dfs_visit(Vertex, none, Graph, Time0, Disc0, Low0, Points0, Bridges0, Time1, Disc1, Low1, _, Bridges1),
			bridges_all(Vertices, Graph, Time1, Disc1, Low1, Points0, Bridges1, Disc, Low, Bridges)
		).

	dfs_visit(Vertex, Parent, Graph, Time0, Disc0, Low0, Points0, Bridges0, Time, Disc, Low, Points, Bridges) :-
		dfs_insert(Disc0, Vertex, Time0, Disc1),
		dfs_insert(Low0, Vertex, Time0, Low1),
		Time1 is Time0 + 1,
		::neighbors(Vertex, Graph, Neighbors),
		dfs_neighbors(Neighbors, Vertex, Parent, Graph, Time1, Disc1, Low1, Time2, Disc2, Low2, Time0, 0, ChildCount, Points0, Points1, Bridges0, Bridges1),
		( Parent == none,
		  ChildCount > 1 ->
			set_insert(Points1, Vertex, Points)
		;	Points = Points1
		),
		Time = Time2,
		Disc = Disc2,
		Low = Low2,
		Bridges = Bridges1.

	dfs_neighbors([], _, _, _, Time, Disc, Low, Time, Disc, Low, _, ChildCount, ChildCount, Points, Points, Bridges, Bridges).
	dfs_neighbors([Neighbor| Neighbors], Vertex, Parent, Graph, Time0, Disc0, Low0, Time, Disc, Low, VertexDisc, ChildCount0, ChildCount, Points0, Points, Bridges0, Bridges) :-
		( Neighbor == Parent ->
			dfs_neighbors(Neighbors, Vertex, Parent, Graph, Time0, Disc0, Low0, Time, Disc, Low, VertexDisc, ChildCount0, ChildCount, Points0, Points, Bridges0, Bridges)
		; dfs_lookup(Neighbor, NeighborDisc, Disc0) ->
			dfs_lookup(Vertex, VertexLow0, Low0),
			NewVertexLow is min(VertexLow0, NeighborDisc),
			dfs_insert(Low0, Vertex, NewVertexLow, Low1),
			dfs_neighbors(Neighbors, Vertex, Parent, Graph, Time0, Disc0, Low1, Time, Disc, Low, VertexDisc, ChildCount0, ChildCount, Points0, Points, Bridges0, Bridges)
		;	dfs_visit(Neighbor, Vertex, Graph, Time0, Disc0, Low0, Points0, Bridges0, Time1, Disc1, Low1, Points1, Bridges1),
			dfs_lookup(Vertex, VertexLow0, Low1),
			dfs_lookup(Neighbor, NeighborLow, Low1),
			NewVertexLow is min(VertexLow0, NeighborLow),
			dfs_insert(Low1, Vertex, NewVertexLow, Low2),
			( Parent \== none,
			  NeighborLow >= VertexDisc ->
				set_insert(Points1, Vertex, Points2)
			;	Points2 = Points1
			),
			( NeighborLow > VertexDisc ->
				canonical_edge(Vertex, Neighbor, Edge),
				set_insert(Bridges1, Edge, Bridges2)
			;	Bridges2 = Bridges1
			),
			ChildCount1 is ChildCount0 + 1,
			dfs_neighbors(Neighbors, Vertex, Parent, Graph, Time1, Disc1, Low2, Time, Disc, Low, VertexDisc, ChildCount1, ChildCount, Points2, Points, Bridges2, Bridges)
		).

	canonical_edge(Vertex1, Vertex2, Vertex1-Vertex2) :-
		Vertex1 @< Vertex2,
		!.
	canonical_edge(Vertex1, Vertex2, Vertex2-Vertex1).

	color_vertices([], _, Coloring, Coloring, NumberOfColors, NumberOfColors).
	color_vertices([Vertex| Vertices], Graph, Coloring0, Coloring, NumberOfColors0, NumberOfColors) :-
		::neighbors(Vertex, Graph, Neighbors),
		neighbor_colors(Neighbors, Coloring0, UsedColors),
		smallest_color(1, UsedColors, Color),
		(	Color > NumberOfColors0 ->
			NumberOfColors1 = Color
		;	NumberOfColors1 = NumberOfColors0
		),
		color_vertices(Vertices, Graph, [Vertex-Color| Coloring0], Coloring, NumberOfColors1, NumberOfColors).

	neighbor_colors([], _, []).
	neighbor_colors([Neighbor| Neighbors], Coloring, Colors) :-
		(	color_lookup(Neighbor, Coloring, NeighborColor) ->
			Colors = [NeighborColor| RestColors]
		;	Colors = RestColors
		),
		neighbor_colors(Neighbors, Coloring, RestColors).

	color_lookup(Neighbor, [Vertex-Color| _], Color) :-
		Neighbor == Vertex,
		!.
	color_lookup(Neighbor, [_| Coloring], Color) :-
		color_lookup(Neighbor, Coloring, Color).

	smallest_color(Color0, UsedColors, Color) :-
		(	member(Color0, UsedColors) ->
			Color1 is Color0 + 1,
			smallest_color(Color1, UsedColors, Color)
		;	Color = Color0
		).

	% === Maximal cliques (Bron-Kerbosch with pivoting) ===

	maximal_cliques(Graph, Cliques) :-
		::vertices(Graph, Vertices),
		(	set_empty(Vertices) ->
			Cliques = []
		;	bron_kerbosch([], Vertices, [], Graph, [], Cliques0),
			msort(Cliques0, Cliques)
		).

	% === Maximum cliques ===

	maximum_cliques(Graph, MaxCliques) :-
		maximal_cliques(Graph, Cliques),
		filter_maximum(Cliques, MaxCliques).

	% --- Bron-Kerbosch with pivoting ---

	bron_kerbosch(R, P, X, Graph, Cliques0, Cliques) :-
		(	set_empty(P), set_empty(X) ->
			msort(R, Clique),
			Cliques = [Clique| Cliques0]
		;	\+ set_empty(P) ->
			set_union(P, X, PX),
			choose_pivot(PX, P, Graph, Pivot),
			::neighbors(Pivot, Graph, PivotNeighbors),
			set_subtract(P, PivotNeighbors, Candidates),
			bk_loop(Candidates, R, P, X, Graph, Cliques0, Cliques)
		;	Cliques = Cliques0
		).

	bk_loop(Candidates, R, P, X, Graph, Cliques0, Cliques) :-
		(	set_empty(Candidates) ->
			Cliques = Cliques0
		;	set_select(V, Candidates, Rest),
			::neighbors(V, Graph, VNeighbors),
			set_intersection(P, VNeighbors, NewP),
			set_intersection(X, VNeighbors, NewX),
			bron_kerbosch([V| R], NewP, NewX, Graph, Cliques0, Cliques1),
			set_delete(P, V, P1),
			set_insert(X, V, X1),
			bk_loop(Rest, R, P1, X1, Graph, Cliques1, Cliques)
		).

	% --- Pivot selection: choose vertex from P\/X maximizing |P /\ N(u)| ---

	choose_pivot(PX, P, Graph, Pivot) :-
		set_select(V, PX, Rest),
		::neighbors(V, Graph, VNeighbors),
		set_intersection(P, VNeighbors, Common),
		set_size(Common, Count),
		choose_pivot_loop(Rest, P, Graph, V, Count, Pivot).

	choose_pivot_loop(Candidates, P, Graph, BestPivot, BestCount, Pivot) :-
		(	set_empty(Candidates) ->
			Pivot = BestPivot
		;	set_select(V, Candidates, Rest),
			::neighbors(V, Graph, VNeighbors),
			set_intersection(P, VNeighbors, Common),
			set_size(Common, Count),
			(	Count > BestCount ->
				choose_pivot_loop(Rest, P, Graph, V, Count, Pivot)
			;	choose_pivot_loop(Rest, P, Graph, BestPivot, BestCount, Pivot)
			)
		).

	% --- Filter maximum cliques (keep only those with the largest size) ---

	filter_maximum([], []).
	filter_maximum([Clique| Cliques], MaxCliques) :-
		length(Clique, Size),
		filter_maximum_loop(Cliques, Size, [Clique], MaxCliques).

	filter_maximum_loop([], _, MaxCliques0, MaxCliques) :-
		msort(MaxCliques0, MaxCliques).
	filter_maximum_loop([Clique| Cliques], MaxSize, MaxCliques0, MaxCliques) :-
		length(Clique, Size),
		(	Size > MaxSize ->
			filter_maximum_loop(Cliques, Size, [Clique], MaxCliques)
		;	Size =:= MaxSize ->
			filter_maximum_loop(Cliques, MaxSize, [Clique| MaxCliques0], MaxCliques)
		;	filter_maximum_loop(Cliques, MaxSize, MaxCliques0, MaxCliques)
		).

:- end_category.
