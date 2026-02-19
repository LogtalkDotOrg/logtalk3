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


:- protocol(graph_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'Common protocol for all types of graphs. Graphs are represented using a dictionary where keys are vertices and values are sorted lists of neighbors (implicitly defining edges).'
	]).

	:- public(new/1).
	:- mode(new(-graph), one).
	:- info(new/1, [
		comment is 'Creates a new empty graph.',
		argnames is ['Graph']
	]).

	:- public(new/2).
	:- mode(new(+list(edge), -graph), one).
	:- info(new/2, [
		comment is 'Creates a new graph from a list of edges. Vertices are defined implicitly by edges.',
		argnames is ['Edges', 'Graph']
	]).

	:- public(new/3).
	:- mode(new(+list(vertex), +list(edge), -graph), one).
	:- info(new/3, [
		comment is 'Creates a new graph from a list of vertices and a list of edges. Vertices may also be defined implicitly by edges.',
		argnames is ['Vertices', 'Edges', 'Graph']
	]).

	:- public(empty/1).
	:- mode(empty(@graph), zero_or_one).
	:- info(empty/1, [
		comment is 'True iff the given graph is empty.',
		argnames is ['Graph']
	]).

	:- public(vertices/2).
	:- mode(vertices(+graph, -list(vertex)), one).
	:- info(vertices/2, [
		comment is 'Unifies ``Vertices`` with a sorted list of all vertices in the graph.',
		argnames is ['Graph', 'Vertices']
	]).

	:- public(edges/2).
	:- mode(edges(+graph, -list(edge)), one).
	:- info(edges/2, [
		comment is 'Unifies ``Edges`` with a list of all edges in the graph.',
		argnames is ['Graph', 'Edges']
	]).

	:- public(add_vertex/3).
	:- mode(add_vertex(+graph, +vertex, -graph), one).
	:- info(add_vertex/3, [
		comment is 'Adds a vertex to the graph. If the vertex already exists, the graph is unchanged.',
		argnames is ['Graph', 'Vertex', 'NewGraph']
	]).

	:- public(add_vertices/3).
	:- mode(add_vertices(+graph, +list(vertex), -graph), one).
	:- info(add_vertices/3, [
		comment is 'Adds a list of vertices to the graph.',
		argnames is ['Graph', 'Vertices', 'NewGraph']
	]).

	:- public(delete_vertex/3).
	:- mode(delete_vertex(+graph, +vertex, -graph), one).
	:- info(delete_vertex/3, [
		comment is 'Deletes a vertex and all edges incident to it from the graph. If the vertex does not exist, the graph is unchanged.',
		argnames is ['Graph', 'Vertex', 'NewGraph']
	]).

	:- public(delete_vertices/3).
	:- mode(delete_vertices(+graph, +list(vertex), -graph), one).
	:- info(delete_vertices/3, [
		comment is 'Deletes a list of vertices and all edges incident to them from the graph.',
		argnames is ['Graph', 'Vertices', 'NewGraph']
	]).

	:- public(add_edges/3).
	:- mode(add_edges(+graph, +list(edge), -graph), one).
	:- info(add_edges/3, [
		comment is 'Adds a list of edges to the graph. Vertices referenced by edges are added implicitly.',
		argnames is ['Graph', 'Edges', 'NewGraph']
	]).

	:- public(delete_edges/3).
	:- mode(delete_edges(+graph, +list(edge), -graph), one).
	:- info(delete_edges/3, [
		comment is 'Deletes a list of edges from the graph. Vertices are not deleted. Non-existing edges are silently ignored.',
		argnames is ['Graph', 'Edges', 'NewGraph']
	]).

	:- public(neighbors/3).
	:- mode(neighbors(+vertex, +graph, -list(vertex)), zero_or_one).
	:- info(neighbors/3, [
		comment is 'Unifies ``Neighbors`` with a sorted list of the neighbors of ``Vertex`` in the graph. Fails if ``Vertex`` is not in the graph.',
		argnames is ['Vertex', 'Graph', 'Neighbors']
	]).

	:- public(reachable/3).
	:- mode(reachable(+vertex, +graph, -list(vertex)), zero_or_one).
	:- info(reachable/3, [
		comment is 'Unifies ``Vertices`` with a sorted list of vertices reachable from ``Vertex`` (including ``Vertex`` itself). Fails if ``Vertex`` is not in the graph.',
		argnames is ['Vertex', 'Graph', 'Vertices']
	]).

	:- public(number_of_vertices/2).
	:- mode(number_of_vertices(+graph, -integer), one).
	:- info(number_of_vertices/2, [
		comment is 'Unifies ``N`` with the number of vertices in the graph.',
		argnames is ['Graph', 'N']
	]).

	:- public(number_of_edges/2).
	:- mode(number_of_edges(+graph, -integer), one).
	:- info(number_of_edges/2, [
		comment is 'Unifies ``N`` with the number of edges in the graph.',
		argnames is ['Graph', 'N']
	]).

	:- public(path/3).
	:- mode(path(+vertex, +graph, -list(vertex)), zero_or_more).
	:- info(path/3, [
		comment is 'Returns a maximal path (list of vertices) rooted at ``Vertex``, enumerating different paths on backtracking. Fails if ``Vertex`` is not in the graph.',
		argnames is ['Vertex', 'Graph', 'Path']
	]).

	:- public(has_path/3).
	:- mode(has_path(+vertex, +vertex, +graph), zero_or_one).
	:- info(has_path/3, [
		comment is 'True iff there is a path from ``Vertex1`` to ``Vertex2`` in the graph.',
		argnames is ['Vertex1', 'Vertex2', 'Graph']
	]).

	:- public(min_path/5).
	:- mode(min_path(+vertex, +vertex, +graph, -list(vertex), -number), zero_or_one).
	:- info(min_path/5, [
		comment is 'Finds the minimum cost path from ``Vertex1`` to ``Vertex2``. For unweighted graphs, cost is the number of edges. Fails if no path exists.',
		argnames is ['Vertex1', 'Vertex2', 'Graph', 'Path', 'Cost']
	]).

	:- public(max_path/5).
	:- mode(max_path(+vertex, +vertex, +graph, -list(vertex), -number), zero_or_one).
	:- info(max_path/5, [
		comment is 'Finds the maximum cost acyclic path from ``Vertex1`` to ``Vertex2``. For unweighted graphs, cost is the number of edges. Fails if no path exists.',
		argnames is ['Vertex1', 'Vertex2', 'Graph', 'Path', 'Cost']
	]).

	:- public(is_complete/1).
	:- mode(is_complete(+graph), zero_or_one).
	:- info(is_complete/1, [
		comment is 'True iff every pair of distinct vertices in the graph is connected by an edge.',
		argnames is ['Graph']
	]).

	:- public(is_bipartite/1).
	:- mode(is_bipartite(+graph), zero_or_one).
	:- info(is_bipartite/1, [
		comment is 'True iff the graph is bipartite, i.e. its vertices can be partitioned into two sets such that every edge connects a vertex in one set to a vertex in the other.',
		argnames is ['Graph']
	]).

	:- public(is_sparse/1).
	:- mode(is_sparse(+graph), zero_or_one).
	:- info(is_sparse/1, [
		comment is 'True iff the graph is sparse, i.e. the number of edges is at most ``|V| * log2(|V|)``. The cutoff ``|E| = |V| * log2(|V|)`` separates sparse graphs (where adjacency lists are efficient) from dense graphs (where adjacency matrix representations may be preferable).',
		argnames is ['Graph']
	]).

:- end_protocol.
