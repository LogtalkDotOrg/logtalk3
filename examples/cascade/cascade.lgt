%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- object(cascade).

	:- info([
		version is 1:0:2,
		author is 'Paulo Moura',
		date is 2019-11-21,
		comment is 'Sample example of using expected terms to call a conjunction of goals where any of them may cause an exception condition without using the traditional catch/throw mechanism.'
	]).

	:- public(process_image/2).
	:- mode(process_image(+atom, -compound), one_or_error).
	:- info(process_image/2, [
		comment is 'Processes an "image" by applying a sequence of filters.',
		argnames is ['Image', 'Final'],
		exceptions is [
			'No cat in the image; who let the cat out' - missing_cat,
			'Cat trashed the bow tie; no funny business' - bow_tie_failure,
			'Cat with eyes closed; no sparkling eyes' - eyes_closed,
			'Cat enjoys lasagna; not the path to get smaller' - wants_to_grow,
			'It is a sunny day; no rain for making rainbows' - sunny_day
		]
	]).

	:- uses(random, [maybe/1]).

	process_image(Image, Final) :-
		% encapsulate the "image" in an expected term
		expected::of_expected(Image, Final0),
		% apply a sequence of image filters
		crop_to_cat(Final0, Final1),
		add_bow_tie(Final1, Final2),
		make_eyes_sparkle(Final2, Final3),
		make_smaller(Final3, Final4),
		add_rainbow(Final4, Final5),
		% either return the final image or throw any exception
		% that happened while applying one of the filters
		expected(Final5)::or_else_throw(Final).

	% calling the individual image filter predicates either results in an error
	% (missing_cat, bow_tie_failure, ...) or in the application of the filter;

	crop_to_cat(In, Out) :-
		apply_filter(cropped, missing_cat, In, Out).

	add_bow_tie(In, Out) :-
		apply_filter(with_bow_tie, bow_tie_failure, In, Out).

	make_eyes_sparkle(In, Out) :-
		apply_filter(sparkling_eyes, eyes_closed, In, Out).

	make_smaller(In, Out) :-
		apply_filter(smaller, wants_to_grow, In, Out).

	add_rainbow(In, Out) :-
		apply_filter(with_rainbow, sunny_day, In, Out).

	% we get the random behavior by calling the random::maybe(0.9) predicate,
	% which succeeds with probability 0.9;
	%
	% the application of a filter is represented by a compound term whose functor
	% is the filter and the argument is the input image (which is wrapped by an
	% expected term)

	apply_filter(Filter, Error, In, Out) :-
		Filtered =.. [Filter, Value],
		% the flat_map/2 predicate either simply returns the expected term if
		% it contains an unexpected value (thus passing it along the sequence
		% of calls) or applies a closure to the expected value and the resulting
		% expected term; we use a lambda expression to call the from_goal/4
		% expected term constructor, which takes as arguments a goal whose
		% success or failure/error dictates if we wrap an expected value
		% (Filtered) or an unexpected error (Error)
		expected(In)::flat_map(
			{Filtered,Error}/[Value,Expected]>>(expected::from_goal(maybe(0.9), Filtered, Error, Expected)),
			Out
		).

:- end_object.
