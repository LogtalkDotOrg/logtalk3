%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


:- object(number,
	extends(atomic)).

	:- info([
		version is 1:13:0,
		author is 'Paulo Moura',
		date is 2023-10-02,
		comment is 'Number data type predicates.'
	]).

	:- public(approximately_equal/2).
	:- mode(approximately_equal(+number, +number), zero_or_one).
	:- info(approximately_equal/2, [
		comment is 'Compares two numbers for approximate equality given the ``epsilon`` arithmetic constant value using the de facto standard formula ``abs(Number1 - Number2) =< max(abs(Number1), abs(Number2)) * epsilon``. No type-checking.',
		argnames is ['Number1', 'Number2']
	]).

	:- public(approximately_equal/3).
	:- mode(approximately_equal(+number, +number, +number), zero_or_one).
	:- info(approximately_equal/3, [
		comment is 'Compares two numbers for approximate equality given a user-defined epsilon value using the de facto standard formula ``abs(Number1 - Number2) =< max(abs(Number1), abs(Number2)) * Epsilon``. No type-checking.',
		argnames is ['Number1', 'Number2', 'Epsilon'],
		remarks is [
			'Epsilon range' - 'Epsilon should be the ``epsilon`` arithmetic constant value or a small multiple of it. Only use a larger value if a greater error is expected.',
			'Comparison with essential equality' - 'For the same epsilon value, approximate equality is weaker requirement than essential equality.'
		]
	]).

	:- public(essentially_equal/3).
	:- mode(essentially_equal(+number, +number, +number), zero_or_one).
	:- info(essentially_equal/3, [
		comment is 'Compares two numbers for essential equality given an epsilon value using the de facto standard formula abs(Number1 - Number2) =< min(abs(Number1), abs(Number2)) * Epsilon. No type-checking.',
		argnames is ['Number1', 'Number2', 'Epsilon'],
		remarks is [
			'Comparison with approximate equality' - 'For the same epsilon value, essential equality is a stronger requirement than approximate equality.'
		]
	]).

	:- public(tolerance_equal/4).
	:- mode(tolerance_equal(+number, +number, +number, +number), zero_or_one).
	:- info(tolerance_equal/4, [
		comment is 'Compares two numbers for close equality given relative and absolute tolerances using the de facto standard formula abs(Number1 - Number2) =< max(RelativeTolerance * max(abs(Number1), abs(Number2)), AbsoluteTolerance). No type-checking.',
		argnames is ['Number1', 'Number2', 'RelativeTolerance', 'AbsoluteTolerance']
	]).

	:- public(op(700, xfx, ('=~='))).
	:- public(('=~=')/2).
	:- mode('=~='(+number, +number), zero_or_one).
	:- mode('=~='(+list(number), +list(number)), zero_or_one).
	:- info(('=~=')/2, [
		comment is 'Compares two floats (or lists of floats) for approximate equality using 100*epsilon for the absolute error and, if that fails, 99.999% accuracy for the relative error. Note that these precision values may not be adequate for all cases. No type-checking.',
		argnames is ['Float1', 'Float2']
	]).

	approximately_equal(Number1, Number2) :-
		epsilon(Epsilon),
		abs(Number1 - Number2) =< max(abs(Number1), abs(Number2)) * Epsilon.

	approximately_equal(Number1, Number2, Epsilon) :-
		{abs(Number1 - Number2) =< max(abs(Number1), abs(Number2)) * Epsilon}.

	essentially_equal(Number1, Number2, Epsilon) :-
		{abs(Number1 - Number2) =< min(abs(Number1), abs(Number2)) * Epsilon}.

	tolerance_equal(Number1, Number2, RelativeTolerance, AbsoluteTolerance) :-
		{abs(Number1 - Number2) =< max(RelativeTolerance * max(abs(Number1), abs(Number2)), AbsoluteTolerance)}.

	'=~='([], []) :-
		!.
	'=~='([Float1| Floats1], [Float2| Floats2]) :-
		!,
		'=~='(Float1, Float2),
		'=~='(Floats1, Floats2).
	'=~='(Float1, Float2) :-
		(	% first test the absolute error, for meaningful results with numbers very close to zero:
			epsilon(Epsilon), {abs(Float1 - Float2) < 100*Epsilon} ->
			true
		;	% if that fails, test the relative error (99.999% accuracy):
			{abs(Float1 - Float2) < 0.00001 * max(abs(Float1), abs(Float2))}
		).

	:- if((
		current_logtalk_flag(prolog_dialect, Dialect),
		(	Dialect == swi; Dialect == yap; Dialect == gnu; Dialect == b;
			Dialect == cx; Dialect == tau; Dialect == lvm; Dialect == trealla
		)
	)).
		epsilon(Epsilon) :-
			Epsilon is epsilon.
	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).
		epsilon(Epsilon) :-
			Epsilon is nexttoward(1.0, 2.0) - 1.0.
	:- else.
		epsilon(0.000000000001).
	:- endif.

	valid(Number) :-
		number(Number).

	check(Term) :-
		(	number(Term) ->
			true
		;	var(Term) ->
			instantiation_error
		;	type_error(number, Term)
		).

	% redefinition of the implementation of "comparingp" protocol
	% predicates that are inherited from "term" object

	Term1 < Term2 :-
		{Term1 < Term2}.

	Term1 =< Term2 :-
		{Term1 =< Term2}.

	Term1 > Term2 :-
		{Term1 > Term2}.

	Term1 >= Term2 :-
		{Term1 >= Term2}.

	Term1 =:= Term2 :-
		{Term1 =:= Term2}.

	Term1 =\= Term2 :-
		{Term1 =\= Term2}.

:- end_object.
