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


:- protocol(directed_graph_protocol,
	extends(graph_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'Protocol for directed graph predicates such as transpose, closures, topological sorting, degree queries, and strongly connected components.'
	]).

	:- public(transpose/2).
	:- mode(transpose(+graph, -graph), one).
	:- info(transpose/2, [
		comment is 'Unifies ``NewGraph`` with a graph obtained by reversing all edges.',
		argnames is ['Graph', 'NewGraph']
	]).

	:- public(transitive_closure/2).
	:- mode(transitive_closure(+graph, -graph), one).
	:- info(transitive_closure/2, [
		comment is 'Generates the transitive closure of the graph.',
		argnames is ['Graph', 'Closure']
	]).

	:- public(symmetric_closure/2).
	:- mode(symmetric_closure(+graph, -graph), one).
	:- info(symmetric_closure/2, [
		comment is 'Generates the symmetric closure of the graph.',
		argnames is ['Graph', 'Closure']
	]).

	:- public(topological_sort/2).
	:- mode(topological_sort(+graph, -list(vertex)), one).
	:- info(topological_sort/2, [
		comment is 'Unifies ``Sorted`` with a topological sort of the vertices in the graph. Assumes the graph is a DAG.',
		argnames is ['Graph', 'Sorted']
	]).

	:- public(in_degree/3).
	:- mode(in_degree(+vertex, +graph, -integer), zero_or_one).
	:- info(in_degree/3, [
		comment is 'Unifies ``Degree`` with the number of incoming edges to ``Vertex``. Fails if ``Vertex`` is not in the graph.',
		argnames is ['Vertex', 'Graph', 'Degree']
	]).

	:- public(out_degree/3).
	:- mode(out_degree(+vertex, +graph, -integer), zero_or_one).
	:- info(out_degree/3, [
		comment is 'Unifies ``Degree`` with the number of outgoing edges from ``Vertex``. Fails if ``Vertex`` is not in the graph.',
		argnames is ['Vertex', 'Graph', 'Degree']
	]).

	:- public(is_dag/1).
	:- mode(is_dag(+graph), zero_or_one).
	:- info(is_dag/1, [
		comment is 'True iff the graph is a directed acyclic graph (DAG).',
		argnames is ['Graph']
	]).

	:- public(strongly_connected_components/2).
	:- mode(strongly_connected_components(+graph, -list(list(vertex))), one).
	:- info(strongly_connected_components/2, [
		comment is 'Computes the strongly connected components of the graph using Tarjan''s algorithm. Each component is a sorted list of vertices. Components are returned in topological order.',
		argnames is ['Graph', 'SCCs']
	]).

:- end_protocol.
