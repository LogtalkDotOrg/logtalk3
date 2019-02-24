%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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


:- object(cascade_dcgs).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2019/01/31,
		comment is 'Sample example of using expected terms to call a conjunction of goals where any of them may cause an exception condition without using the traditional catch/throw mechanism. An alternative implementation of the "cascade" object using DCGs.'
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
		% encapsulate the "image" in an expected term reference
		expected::of_expected(Image, Final0),
		% apply a sequence of image filters
		phrase(process, Final0, Final1),
		% either return the final image or throw any exception
		% that happened while applying one of the filters
		expected(Final1)::or_else_throw(Final).

	process -->
		crop_to_cat,
		add_bow_tie,
		make_eyes_sparkle,
		make_smaller,
		add_rainbow.

	% calling the individual image filter predicates either results in an error
	% (missing_cat, bow_tie_failure, ...) or in the application of the filter;
	%
	% we use call//1 to avoid hard-coding assumptions on how grammar rules are
	% compiled into clauses

	crop_to_cat -->
		call(apply_filter(cropped, missing_cat)).

	add_bow_tie -->
		call(apply_filter(with_bow_tie, bow_tie_failure)).

	make_eyes_sparkle -->
		call(apply_filter(sparkling_eyes, eyes_closed)).

	make_smaller -->
		call(apply_filter(smaller, wants_to_grow)).

	add_rainbow -->
		call(apply_filter(with_rainbow, sunny_day)).

	% we get the random behavior by calling the random::maybe(0.9) predicate,
	% which succeeds with probability 0.9;
	%
	% the application of a filter is represented by a compound term whose functor
	% is the filter and the argument is the input image (which is wrapped by an
	% expected term reference)

	apply_filter(Filter, Error, In, Out) :-
		Filtered =.. [Filter, Value],
		% the flat_map/2 predicate either simply returns the reference if it
		% contains an unexpected term (thus passing it along the sequence of
		% calls) or applies a closure to the expected term and the resulting
		% expected term reference; we use a lambda expression to call the
		% from_goal/4 expected term constructor, which takes as arguments a
		% goal whose success or failure/error dictates if we construct wrap
		% an expected value (Filtered) or an unexpected error (Error)
		expected(In)::flat_map(
			{Filtered}/[Value,Ref]>>(expected::from_goal(maybe(0.9), Filtered, Error, Ref)),
			Out
		).

:- end_object.
