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
		date is 2026-02-23,
		comment is 'Common predicates shared by undirected graph objects. Uses self-dispatch to call object-specific predicates such as ``is_connected/1``, ``vertices/2``, ``edges/2``, and ``neighbors/3``.'
	]).

	:- uses(list, [
		length/2, member/2, msort/2
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

	:- public(graph_coloring/3).
	:- mode(graph_coloring(+graph, -list(pair), -integer), one).
	:- info(graph_coloring/3, [
		comment is 'Computes a greedy vertex coloring of the graph. ``Coloring`` is a list of ``Vertex-Color`` pairs where colors are integers starting from 1. ``NumberOfColors`` is the total number of colors used.',
		argnames is ['Graph', 'Coloring', 'NumberOfColors']
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

	% === Graph coloring (greedy) ===

	graph_coloring(Graph, Coloring, NumberOfColors) :-
		::vertices(Graph, Vertices),
		color_vertices(Vertices, Graph, [], Coloring, 0, NumberOfColors).

	% --- Graph coloring helpers ---

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
