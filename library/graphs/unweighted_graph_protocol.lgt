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


:- protocol(unweighted_graph_protocol,
	extends(graph_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'Protocol for unweighted graph predicates, extending the common graph protocol with unweighted edge operations.'
	]).

	:- public(edge/3).
	:- mode(edge(+vertex, +vertex, +graph), zero_or_one).
	:- info(edge/3, [
		comment is 'True iff there is an edge between ``Vertex1`` and ``Vertex2`` in ``Graph``.',
		argnames is ['Vertex1', 'Vertex2', 'Graph']
	]).

	:- public(add_edge/4).
	:- mode(add_edge(+graph, +vertex, +vertex, -graph), one).
	:- info(add_edge/4, [
		comment is 'Adds an edge between ``Vertex1`` and ``Vertex2`` to the graph.',
		argnames is ['Graph', 'Vertex1', 'Vertex2', 'NewGraph']
	]).

	:- public(delete_edge/4).
	:- mode(delete_edge(+graph, +vertex, +vertex, -graph), one).
	:- info(delete_edge/4, [
		comment is 'Deletes the edge between ``Vertex1`` and ``Vertex2`` from the graph. The graph is unchanged if the edge does not exist.',
		argnames is ['Graph', 'Vertex1', 'Vertex2', 'NewGraph']
	]).

	:- public(complement/2).
	:- mode(complement(+graph, -graph), one).
	:- info(complement/2, [
		comment is 'Unifies ``NewGraph`` with the complement graph where there is an edge between all pairs of vertices not connected in the original graph.',
		argnames is ['Graph', 'NewGraph']
	]).

:- end_protocol.
