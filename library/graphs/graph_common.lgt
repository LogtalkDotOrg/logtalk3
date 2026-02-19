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


:- category(graph_common,
	implements(graph_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'Common graph predicates shared by all graph objects. Uses self-dispatch to call object-specific predicates such as ``neighbors/3``, ``vertices/2``, and ``edges/2``.'
	]).

	:- uses(set, [
		union/4 as set_union/4,
		memberchk/2 as set_memberchk/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2, reverse/2
	]).

	% === Graph creation ===

	new(Edges, Graph) :-
		new([], Edges, Graph).

	new(Vertices, Edges, Graph) :-
		::new(Empty),
		add_vertices(Empty, Vertices, G1),
		::add_edges(G1, Edges, Graph).

	% === Vertex list operations ===

	add_vertices(Graph, [], Graph).
	add_vertices(Graph, [V|Vs], NewGraph) :-
		::add_vertex(Graph, V, G1),
		add_vertices(G1, Vs, NewGraph).

	delete_vertices(Graph, [], Graph).
	delete_vertices(Graph, [V|Vs], NewGraph) :-
		::delete_vertex(Graph, V, G1),
		delete_vertices(G1, Vs, NewGraph).

	% === Graph metrics ===

	number_of_vertices(Graph, N) :-
		::vertices(Graph, Vs),
		length(Vs, N).

	number_of_edges(Graph, N) :-
		::edges(Graph, Es),
		length(Es, N).

	% === Reachability ===

	reachable(Vertex, Graph, Vertices) :-
		::neighbors(Vertex, Graph, _),
		reachable_aux([Vertex], Graph, [Vertex], Vertices).

	reachable_aux([], _, Visited, Visited).
	reachable_aux([V|Vs], Graph, Visited, Reachable) :-
		(	::neighbors(V, Graph, Neighbors) ->
			set_union(Visited, Neighbors, NewVisited, NewVertices),
			append(Vs, NewVertices, NextToVisit)
		;	NewVisited = Visited,
			NextToVisit = Vs
		),
		reachable_aux(NextToVisit, Graph, NewVisited, Reachable).

	% === Path existence ===

	has_path(V1, V2, Graph) :-
		reachable(V1, Graph, Reachable),
		set_memberchk(V2, Reachable).

	% === Nondeterministic maximal paths ===

	path(Vertex, Graph, Path) :-
		::neighbors(Vertex, Graph, _),
		path_extend(Vertex, Graph, [Vertex], RevPath),
		reverse(RevPath, Path).

	path_extend(Current, Graph, Visited, Path) :-
		::neighbors(Current, Graph, Ns),
		unvisited(Ns, Visited, Unvisited),
		(	Unvisited == [] ->
			Path = Visited
		;	member(Next, Unvisited),
			path_extend(Next, Graph, [Next|Visited], Path)
		).

	unvisited([], _, []).
	unvisited([N|Ns], Visited, Result) :-
		(	memberchk(N, Visited) ->
			unvisited(Ns, Visited, Result)
		;	Result = [N|Rest],
			unvisited(Ns, Visited, Rest)
		).

:- end_category.
