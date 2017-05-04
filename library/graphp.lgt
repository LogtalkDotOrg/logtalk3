%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(graphp).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2017/05/04,
		comment is 'Graph protocol.'
	]).

	:- public(new/3).
	:- mode(new(+list(vertex), +list(edge), -graph), one).
	:- info(new/3, [
		comment is 'Adds all vertices in a list to a graph returning the resulting graph.',
		argnames is ['Vertices', 'Edges', 'Graph']
	]).

	:- public(vertex/2).
	:- mode(vertex(+graph, ?vertex), zero_or_more).
	:- info(vertex/2, [
		comment is 'Enumerates, by backtracking, all the vertices of the given graph.',
		argnames is ['Graph', 'Vertex']
	]).

	:- public(vertices/2).
	:- mode(vertices(+graph, -list), one).
	:- info(vertices/2, [
		comment is 'Returns all the vertices of the given graph.',
		argnames is ['Graph', 'Vertices']
	]).

	:- public(add_vertices/3).
	:- mode(add_vertices(+graph, +list(vertex), -graph), one).
	:- info(add_vertices/3, [
		comment is 'Adds all vertices in a list to a graph returning the resulting graph.',
		argnames is ['Graph', 'Vertices', 'NewGraph']
	]).

	:- public(delete_vertices/3).
	:- mode(delete_vertices(+graph, +list(vertex), -graph), one).
	:- info(delete_vertices/3, [
		comment is 'Deletes all vertices in a list from a graph returning the resulting graph.',
		argnames is ['Graph', 'Vertices', 'NewGraph']
	]).

	:- public(edge/2).
	:- mode(edge(+graph, ?edge), zero_or_more).
	:- info(edge/2, [
		comment is 'Returns all the edges of the given graph.',
		argnames is ['Graph', 'Edges']
	]).

	:- public(edges/2).
	:- mode(edges(+graph, -list(edge)), one).
	:- info(edges/2, [
		comment is 'Returns all the edges of the given graph.',
		argnames is ['Graph', 'Edges']
	]).

	:- public(add_edges/3).
	:- mode(add_edges(+graph, +list(edge), -graph), one).
	:- info(add_edges/3, [
		comment is 'Adds all edges in a list to a graph returning the resulting graph.',
		argnames is ['Graph', 'Edges', 'NewGraph']
	]).

	:- public(delete_edges/3).
	:- mode(delete_edges(+graph, +list(edge), -graph), one).
	:- info(delete_edges/3, [
		comment is 'Deletes all edges in a list from a graph returning the resulting graph.',
		argnames is ['Graph', 'Edges', 'NewGraph']
	]).

	:- public(adjacent/3).
	:- mode(adjacent(+graph, ?vertex, ?vertex), zero_or_more).
	:- info(adjacent/3, [
		comment is 'Enumerates, by backtracking, all vertices that are adjacent to a vertex.',
		argnames is ['Graph', 'Origin', 'Vertex']
	]).

	:- public(neighbors/3).
	:- mode(neighbors(+graph, +vertex, -list(vertex)), zero_or_one).
	:- info(neighbors/3, [
		comment is 'Returns all the neighbors vertices of a given vertex. Fails if the vertex does not exist.',
		argnames is ['Graph', 'Vertex', 'Neighbors']
	]).

	:- public(reachable/3).
	:- mode(reachable(@term, +queue, -queue), zero_or_one).
	:- info(reachable/3, [
		comment is 'Returns all the vertices that are reachable from a given vertex. Fails if the vertex does not exist.',
		argnames is ['Graph', 'Vertex', 'Vertices']
	]).

	:- public(complement/2).
	:- mode(complement(+graph, -graph), one).
	:- info(complement/2, [
		comment is 'Returns the complement of the given graph.',
		argnames is ['Graph', 'Complement']
	]).

	:- public(transpose/2).
	:- mode(transpose(+graph, -graph), one).
	:- info(transpose/2, [
		comment is 'Transposes a graph by reversing all edges.',
		argnames is ['Graph', 'Transpose']
	]).

	:- public(union/3).
	:- mode(union(+graph, +graph, -graph), one).
	:- info(union/3, [
		comment is 'Returns the graph resulting from the union of two graphs.',
		argnames is ['Graph1', 'Graph2', 'NewGraph']
	]).

	:- public(topological_sort/2).
	:- mode(topological_sort(+graph, -graph), zero_or_one).
	:- info(topological_sort/2, [
		comment is 'Returns a set of vertices resulting from the topological sort of the given graph. Fails if the graph is not connected or acyclic.',
		argnames is ['Graph', 'Vertices']
	]).

	:- public(topological_sort/3).
	:- mode(topological_sort(+graph, +graph, -graph), one).
	:- info(topological_sort/3, [
		comment is 'Difference list version of the topological_sort/2 predicate.',
		argnames is ['Graph1', 'Vertices', 'Vertices0']
	]).

:- end_protocol.
