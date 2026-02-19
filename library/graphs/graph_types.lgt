%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2024 Paulo Moura <pmoura@logtalk.org>
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


:- category(graph_types).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-02-19,
		comment is 'A set of graph related types and generators.',
		remarks is [
			'Provided types' - 'This category adds ``vertex``, ``edge``, and ``weighted_edge`` types for type-checking when using the ``type`` library object.',
			'Type ``vertex``' - 'Any non-variable term.',
			'Type ``edge``' - 'An unweighted edge represented as a ``V1-V2`` pair of vertices.',
			'Type ``weighted_edge``' - 'A weighted edge represented as ``(V1-V2)-Weight`` where ``V1`` and ``V2`` are vertices and ``Weight`` is a number.',
			'Generating edges' - 'Use the ``edges(N,V0,V)`` generator for unweighted edges or ``weighted_edges(N,V0,V,W)`` for weighted edges. ``N`` is the upper limit to the number of edges. ``V0`` and ``V`` must be positive integers and will be used for the range of vertices. ``W`` is the upper limit for edge weights (positive integers).'
		]
	]).

	:- uses(integer, [
		between/3
	]).

	:- multifile(type::type/1).
	type::type(vertex).
	type::type(edge).
	type::type(weighted_edge).

	:- multifile(type::check/2).
	type::check(vertex, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	true
		).
	type::check(edge, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	Term = Vertex1-Vertex2,
			nonvar(Vertex1),
			nonvar(Vertex2) ->
			true
		;	throw(type_error(edge, Term))
		).
	type::check(weighted_edge, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	Term = (Vertex1-Vertex2)-Weight,
			nonvar(Vertex1),
			nonvar(Vertex2),
			nonvar(Weight),
			number(Weight) ->
			true
		;	throw(type_error(weighted_edge, Term))
		).


	:- multifile(arbitrary::arbitrary/1).
	arbitrary::arbitrary(vertex).
	arbitrary::arbitrary(edge).
	arbitrary::arbitrary(weighted_edge).
	arbitrary::arbitrary(edges(_,_,_)).
	arbitrary::arbitrary(weighted_edges(_,_,_,_)).

	:- multifile(arbitrary::arbitrary/2).
	arbitrary::arbitrary(vertex, Arbitrary) :-
		type::arbitrary(positive_integer, Arbitrary).
	arbitrary::arbitrary(edge, Vertex1-Vertex2) :-
		arbitrary_vertices(Vertex1, Vertex2).
	arbitrary::arbitrary(weighted_edge, (Vertex1-Vertex2)-Weight) :-
		arbitrary_vertices(Vertex1, Vertex2),
		type::arbitrary(positive_integer, Weight).
	arbitrary::arbitrary(edges(N,V0,V), Edges) :-
		findall(
			Vertex1-Vertex2,
			(	between(1, N, _),
				arbitrary_vertices(V0, V, Vertex1, Vertex2)
			),
			Edges0
		),
		sort(Edges0, Edges).
	arbitrary::arbitrary(weighted_edges(N,V0,V,W), Edges) :-
		findall(
			(Vertex1-Vertex2)-Weight,
			(	between(1, N, _),
				arbitrary_vertices(V0, V, Vertex1, Vertex2),
				type::arbitrary(between(positive_integer, 1, W), Weight)
			),
			Edges0
		),
		sort(Edges0, Edges).

	arbitrary_vertices(Vertex1, Vertex2) :-
		repeat,
			type::arbitrary(positive_integer, Vertex1),
			type::arbitrary(positive_integer, Vertex2),
		Vertex1 < Vertex2,
		!.

	arbitrary_vertices(V0, V, Vertex1, Vertex2) :-
		repeat,
			type::arbitrary(between(positive_integer, V0, V), Vertex1),
			type::arbitrary(between(positive_integer, V0, V), Vertex2),
			Vertex1 < Vertex2,
		!.

:- end_category.
