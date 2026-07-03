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


:- protocol(json_graph_data_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-03,
		comment is 'Canonical JSON graph data protocol for object-backed graph sources and sinks using atom identifiers.'
	]).

	:- public(graph/2).
	:- dynamic(graph/2).
	:- mode(graph(?atom, ?list), zero_or_more).
	:- info(graph/2, [
		comment is 'Enumerates or stores graph properties for an atom graph identifier.',
		argnames is ['GraphId', 'Properties']
	]).

	:- public(node/3).
	:- dynamic(node/3).
	:- mode(node(?atom, ?atom, ?list), zero_or_more).
	:- info(node/3, [
		comment is 'Enumerates or stores node properties for atom graph and node identifiers.',
		argnames is ['GraphId', 'NodeId', 'Properties']
	]).

	:- public(edge/5).
	:- dynamic(edge/5).
	:- mode(edge(?atom, ?atom, ?atom, ?atom, ?list), zero_or_more).
	:- info(edge/5, [
		comment is 'Enumerates or stores edge properties for atom graph, edge, source node, and target node identifiers.',
		argnames is ['GraphId', 'EdgeId', 'Source', 'Target', 'Properties']
	]).

	:- public(hyperedge/4).
	:- dynamic(hyperedge/4).
	:- mode(hyperedge(?atom, ?atom, ?list(atom), ?list), zero_or_more).
	:- info(hyperedge/4, [
		comment is 'Enumerates or stores undirected hyperedge properties for atom graph and hyperedge identifiers and a list of atom node identifiers.',
		argnames is ['GraphId', 'HyperedgeId', 'Nodes', 'Properties']
	]).

	:- public(hyperedge/5).
	:- dynamic(hyperedge/5).
	:- mode(hyperedge(?atom, ?atom, ?list(atom), ?list(atom), ?list), zero_or_more).
	:- info(hyperedge/5, [
		comment is 'Enumerates or stores directed hyperedge properties for atom graph and hyperedge identifiers and lists of atom source and target node identifiers.',
		argnames is ['GraphId', 'HyperedgeId', 'Sources', 'Targets', 'Properties']
	]).

:- end_protocol.
