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


:- category(interpolator,
	implements(interpolator_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Common point validation and domain handling for one-dimensional interpolators.'
	]).

	:- protected(prepare_points/3).
	:- mode(prepare_points(+list(pair), -list(number), -list(number)), one_or_error).
	:- info(prepare_points/3, [
		comment is 'Validates and sorts interpolation points and returns separate abscissa and ordinate lists.',
		argnames is ['Points', 'Abscissas', 'Ordinates'],
		exceptions is [
			'``Points`` is a variable' - instantiation_error,
			'``Points`` is not a valid list of at least two numeric points' - domain_error(interpolation_points, 'Points'),
			'``Points`` contains a duplicate abscissa' - domain_error(duplicate_abscissa, 'Abscissa')
		]
	]).

	:- protected(check_argument/3).
	:- mode(check_argument(+number, +number, +number), one_or_error).
	:- info(check_argument/3, [
		comment is 'Checks that an interpolation argument lies in the closed fitted domain.',
		argnames is ['Argument', 'Lower', 'Upper'],
		exceptions is [
			'``Argument`` is a variable' - instantiation_error,
			'``Argument`` is not a number' - type_error(number, 'Argument'),
			'``Argument`` lies outside the fitted domain' - domain_error(interpolation_domain, 'Argument')
		]
	]).

	:- protected(select_interval/3).
	:- mode(select_interval(+number, +list(compound), -compound), one).
	:- info(select_interval/3, [
		comment is 'Selects the fitted interval containing an already validated argument.',
		argnames is ['Argument', 'Intervals', 'Interval']
	]).

	:- uses(list, [
		length/2
	]).

	fit(Points, Model) :-
		::fit(Points, Model, []).

	prepare_points(Points, Abscissas, Ordinates) :-
		(	var(Points) ->
			instantiation_error
		;	valid_points(Points) ->
			true
		;	domain_error(interpolation_points, Points)
		),
		keysort(Points, Sorted),
		strict_points(Sorted),
		pairs_lists(Sorted, Abscissas, Ordinates).

	valid_points(Points) :-
		length(Points, Length),
		Length >= 2,
		valid_point_values(Points).

	valid_point_values([]).
	valid_point_values([X-Y| Points]) :-
		number(X),
		number(Y),
		valid_point_values(Points).

	strict_points([_]) :-
		!.
	strict_points([X-_, NextX-_| Points]) :-
		(	X < NextX ->
			true
		;	domain_error(duplicate_abscissa, X)
		),
		strict_points([NextX-_| Points]).

	pairs_lists([], [], []).
	pairs_lists([X-Y| Points], [X| Abscissas], [Y| Ordinates]) :-
		pairs_lists(Points, Abscissas, Ordinates).

	check_argument(Argument, _, _) :-
		var(Argument),
		instantiation_error.
	check_argument(Argument, Lower, Upper) :-
		(	number(Argument) ->
			true
		;	type_error(number, Argument)
		),
		(	Argument >= Lower, Argument =< Upper ->
			true
		;	domain_error(interpolation_domain, Argument)
		).

	select_interval(_Argument, [Interval], Interval) :-
		!.
	select_interval(Argument, [Candidate| _], Candidate) :-
		arg(2, Candidate, Upper),
		Argument =< Upper,
		!.
	select_interval(Argument, [_| Intervals], Interval) :-
		select_interval(Argument, Intervals, Interval).

:- end_category.
