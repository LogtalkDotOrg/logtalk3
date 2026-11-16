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


:- object(image_segmentation).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-16,
		comment is 'Binary synthetic-image segmentation by minimum cut.',
		see_also is [flow_networks(loader), project_selection, baseball_elimination]
	]).

	:- uses(list, [
		memberchk/2, append/3
	]).

	:- public(solve/5).
	:- mode(solve(+list(ground), +list(compound), +list(compound), -list(pair), -integer), one_or_error).
	:- info(solve/5, [
		comment is 'Minimizes a binary grid-label energy. Unary records are ``unary(Pixel, BackgroundPenalty, ForegroundPenalty)`` and pairwise penalties are ``neighbor(Pixel1, Pixel2, DisagreementPenalty)``. A source-side pixel is labeled ``foreground``; all other pixels are ``background``.',
		argnames is ['Pixels', 'UnaryPenalties', 'Neighbors', 'Labels', 'Energy'],
		exceptions is [
			'An input list or record is partial, or a pixel is not ground' - instantiation_error,
			'An input is not a list' - type_error(list, 'Input'),
			'A pixel or penalty record has an invalid shape' - domain_error(image_segmentation_input, 'Term'),
			'A penalty is not an integer' - type_error(integer, 'Penalty'),
			'A penalty is negative' - domain_error(non_negative_integer, 'Penalty'),
			'A pixel identifier is repeated' - domain_error(unique_pixels, 'Pixel'),
			'A pixel has more than one unary record' - domain_error(unique_unary_penalties, 'Pixel'),
			'A unary or pairwise penalty names an undeclared pixel' - existence_error(pixel, 'Pixel'),
			'A pixel has no unary penalty' - existence_error(unary_penalty, 'Pixel')
		]
	]).

	solve(Pixels, Unary, Neighbors, Labels, Energy) :-
		validate_pixels(Pixels, []),
		validate_unaries(Unary, Pixels, []),
		validate_unary_coverage(Pixels, Unary),
		validate_neighbors(Neighbors, Pixels),
		pixel_vertices(Pixels, PixelVertices),
		vertices_with_terminals(PixelVertices, Vertices),
		segmentation_arcs(Unary, Neighbors, Arcs),
		flow_network(avltree)::new(Vertices, Arcs, Network),
		dinic(avltree)::min_cut(Network, flow_example_source(image), flow_example_sink(image), Cut, cut(SourceSide, _, _)),
		decode_labels(Pixels, SourceSide, Labels),
		energy(Unary, Neighbors, Labels, Energy),
		Energy =:= Cut,
		!.

	validate_pixels(Pixels, Seen) :-
		(	var(Pixels) ->
			throw(error(instantiation_error, context(image_segmentation::solve/5, _)))
		;	Pixels == [] ->
			true
		;	Pixels = [Pixel| Rest] ->
			(	ground(Pixel) ->
				(	memberchk(Pixel, Seen) ->
					throw(error(domain_error(unique_pixels, Pixel), context(image_segmentation::solve/5, _)))
				;	validate_pixels(Rest, [Pixel| Seen])
				)
			;	throw(error(instantiation_error, context(image_segmentation::solve/5, _)))
			)
		;	throw(error(type_error(list, Pixels), context(image_segmentation::solve/5, _)))
		).

	validate_unaries(Unary, Pixels, Seen) :-
		(	var(Unary) ->
			throw(error(instantiation_error, context(image_segmentation::solve/5, _)))
		;	Unary == [] ->
			true
		;	Unary = [Record| Rest] ->
			(	ground(Record) ->
				(	Record = unary(Pixel, Background, Foreground) ->
					validate_penalty(Background),
					validate_penalty(Foreground),
					(	memberchk(Pixel, Pixels) ->
						(	memberchk(Pixel, Seen) ->
							throw(error(domain_error(unique_unary_penalties, Pixel), context(image_segmentation::solve/5, _)))
						;	validate_unaries(Rest, Pixels, [Pixel| Seen])
						)
					;	throw(error(existence_error(pixel, Pixel), context(image_segmentation::solve/5, _)))
					)
				;	throw(error(domain_error(image_segmentation_input, Record), context(image_segmentation::solve/5, _)))
				)
			;	throw(error(instantiation_error, context(image_segmentation::solve/5, _)))
			)
		;	throw(error(type_error(list, Unary), context(image_segmentation::solve/5, _)))
		).

	validate_unary_coverage([], _).
	validate_unary_coverage([Pixel| Pixels], Unary) :-
		(	unary_record(Pixel, Unary, _, _) ->
			validate_unary_coverage(Pixels, Unary)
		;	throw(error(existence_error(unary_penalty, Pixel), context(image_segmentation::solve/5, _)))
		).

	validate_neighbors(Neighbors, Pixels) :-
		(	var(Neighbors) ->
			throw(error(instantiation_error, context(image_segmentation::solve/5, _)))
		;	Neighbors == [] ->
			true
		;	Neighbors = [Record| Rest] ->
			(	ground(Record) ->
				(	Record = neighbor(First, Second, Penalty) ->
					validate_penalty(Penalty),
					(	memberchk(First, Pixels) ->
						(	memberchk(Second, Pixels) ->
							validate_neighbors(Rest, Pixels)
						;	throw(error(existence_error(pixel, Second), context(image_segmentation::solve/5, _)))
						)
					;	throw(error(existence_error(pixel, First), context(image_segmentation::solve/5, _)))
					)
				;	throw(error(domain_error(image_segmentation_input, Record), context(image_segmentation::solve/5, _)))
				)
			;	throw(error(instantiation_error, context(image_segmentation::solve/5, _)))
			)
		;	throw(error(type_error(list, Neighbors), context(image_segmentation::solve/5, _)))
		).

	validate_penalty(Penalty) :-
		(	integer(Penalty) ->
			(	Penalty >= 0 ->
				true
			;	throw(error(domain_error(non_negative_integer, Penalty), context(image_segmentation::solve/5, _)))
			)
		;	throw(error(type_error(integer, Penalty), context(image_segmentation::solve/5, _)))
		).

	pixel_vertices([], []).
	pixel_vertices([Pixel| Pixels], [flow_example_pixel(Pixel)| Vertices]) :-
		pixel_vertices(Pixels, Vertices).

	vertices_with_terminals(PixelVertices, [flow_example_source(image), flow_example_sink(image)| PixelVertices]).

	segmentation_arcs(Unary, Neighbors, Arcs) :-
		unary_arcs(Unary, UnaryArcs),
		neighbor_arcs(Neighbors, PairArcs),
		append(UnaryArcs, PairArcs, Arcs).

	unary_arcs([], []).
	unary_arcs([unary(Pixel, Background, Foreground)| Unary], Arcs) :-
		unary_arcs(Unary, Rest),
		Node = flow_example_pixel(Pixel),
		Arcs = [arc(flow_example_source(image), Node, Background), arc(Node, flow_example_sink(image), Foreground)| Rest].

	neighbor_arcs([], []).
	neighbor_arcs([neighbor(First, Second, Penalty)| Neighbors], [arc(flow_example_pixel(First), flow_example_pixel(Second), Penalty), arc(flow_example_pixel(Second), flow_example_pixel(First), Penalty)| Arcs]) :-
		neighbor_arcs(Neighbors, Arcs).

	decode_labels([], _, []).
	decode_labels([Pixel| Pixels], SourceSide, [Pixel-Label| Labels]) :-
		decode_labels(Pixels, SourceSide, Labels),
		(	memberchk(flow_example_pixel(Pixel), SourceSide) ->
			Label = foreground
		;	Label = background
		).

	energy(Unary, Neighbors, Labels, Energy) :-
		unary_energy(Unary, Labels, 0, UnaryEnergy),
		pairwise_energy(Neighbors, Labels, UnaryEnergy, Energy).

	unary_energy([], _, Total, Total).
	unary_energy([unary(Pixel, Background, Foreground)| Unary], Labels, Total0, Total) :-
		label_for(Pixel, Labels, Label),
		(	Label == foreground ->
			Total1 is Total0 + Foreground
		;	Total1 is Total0 + Background
		),
		unary_energy(Unary, Labels, Total1, Total).

	pairwise_energy([], _, Total, Total).
	pairwise_energy([neighbor(First, Second, Penalty)| Neighbors], Labels, Total0, Total) :-
		label_for(First, Labels, FirstLabel),
		label_for(Second, Labels, SecondLabel),
		(	FirstLabel == SecondLabel ->
			Total1 = Total0
		;	Total1 is Total0 + Penalty
		),
		pairwise_energy(Neighbors, Labels, Total1, Total).

	label_for(Pixel, [Pixel0-Label| _], Label) :-
		Pixel == Pixel0,
		!.
	label_for(Pixel, [_| Labels], Label) :-
		label_for(Pixel, Labels, Label).

	unary_record(Pixel, [unary(Pixel0, Background, Foreground)| _], Background, Foreground) :-
		Pixel == Pixel0,
		!.
	unary_record(Pixel, [_| Unary], Background, Foreground) :-
		unary_record(Pixel, Unary, Background, Foreground).

:- end_object.
