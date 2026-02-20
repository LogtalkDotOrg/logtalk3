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


:- category(undirected_graph_common).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-20,
		comment is 'Common predicates shared by undirected graph objects. Uses self-dispatch to call object-specific predicates such as ``is_connected/1``, ``vertices/2``, ``edges/2``, and ``neighbors/3``.'
	]).

	:- uses(list, [
		length/2, member/2
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
	color_vertices([Vertex| Vertices], Graph, Acc, Coloring, NumberOfColors0, NumberOfColors) :-
		::neighbors(Vertex, Graph, Neighbors),
		neighbor_colors(Neighbors, Acc, UsedColors),
		smallest_color(1, UsedColors, Color),
		(	Color > NumberOfColors0 ->
			NumberOfColors1 = Color
		;	NumberOfColors1 = NumberOfColors0
		),
		color_vertices(Vertices, Graph, [Vertex-Color| Acc], Coloring, NumberOfColors1, NumberOfColors).

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

	smallest_color(C, UsedColors, Color) :-
		(	member(C, UsedColors) ->
			C1 is C + 1,
			smallest_color(C1, UsedColors, Color)
		;	Color = C
		).

:- end_category.
