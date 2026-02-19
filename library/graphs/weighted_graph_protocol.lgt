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


:- protocol(weighted_graph_protocol,
	extends(graph_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'Protocol for weighted graph predicates, extending the common graph protocol with weighted edge operations.'
	]).

	:- public(edge/4).
	:- mode(edge(+vertex, +vertex, ?number, +graph), zero_or_one).
	:- info(edge/4, [
		comment is 'True iff there is an edge between ``Vertex1`` and ``Vertex2`` with weight ``Weight`` in ``Graph``.',
		argnames is ['Vertex1', 'Vertex2', 'Weight', 'Graph']
	]).

	:- public(add_edge/5).
	:- mode(add_edge(+graph, +vertex, +vertex, +number, -graph), one).
	:- info(add_edge/5, [
		comment is 'Adds a weighted edge between ``Vertex1`` and ``Vertex2`` with weight ``Weight``.',
		argnames is ['Graph', 'Vertex1', 'Vertex2', 'Weight', 'NewGraph']
	]).

	:- public(delete_edge/5).
	:- mode(delete_edge(+graph, +vertex, +vertex, ?number, -graph), one).
	:- info(delete_edge/5, [
		comment is 'Deletes the weighted edge between ``Vertex1`` and ``Vertex2``. Unifies ``Weight`` with the weight of the deleted edge. The graph is unchanged if the edge does not exist.',
		argnames is ['Graph', 'Vertex1', 'Vertex2', 'Weight', 'NewGraph']
	]).

	:- public(wneighbors/3).
	:- mode(wneighbors(+vertex, +graph, -list(pair)), zero_or_one).
	:- info(wneighbors/3, [
		comment is 'Unifies ``WNeighbors`` with a list of ``NeighborVertex-Weight`` pairs for the neighbors of ``Vertex``. Fails if ``Vertex`` is not in the graph.',
		argnames is ['Vertex', 'Graph', 'WNeighbors']
	]).

:- end_protocol.
