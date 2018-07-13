%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.4,
		author is 'Paulo Moura',
		date is 2018/07/13,
		comment is 'Number data type predicates.'
	]).

	:- public(approximately_equal/3).
	:- mode(approximately_equal(+number, +number, +number), zero_or_one).
	:- info(approximately_equal/3, [
		comment is 'Compares two numbers for approximate equality given an epsilon value using the de facto standard formula abs(Number1 - Number2) =< max(abs(Number1), abs(Number2)) * Epsilon. No type-checking.',
		argnames is ['Number1', 'Number2', 'Epsilon']
	]).

	:- public(essentially_equal/3).
	:- mode(essentially_equal(+number, +number, +number), zero_or_one).
	:- info(essentially_equal/3, [
		comment is 'Compares two numbers for essential equality given an epsilon value using the de facto standard formula abs(Number1 - Number2) =< min(abs(Number1), abs(Number2)) * Epsilon. No type-checking.',
		argnames is ['Number1', 'Number2', 'Epsilon']
	]).

	:- public(tolerance_equal/4).
	:- mode(tolerance_equal(+number, +number, +number, +number), zero_or_one).
	:- info(tolerance_equal/4, [
		comment is 'Compares two numbers for close equality given relative and absolute tolerances using the de facto standard formula abs(Number1 - Number2) =< max(RelativeTolerance * max(abs(Number1), abs(Number2)), AbsoluteTolerance). No type-checking.',
		argnames is ['Number1', 'Number2', 'RelativeTolerance', 'AbsoluteTolerance']
	]).

	approximately_equal(Number1, Number2, Epsilon) :-
		abs(Number1 - Number2) =< max(abs(Number1), abs(Number2)) * Epsilon.

	essentially_equal(Number1, Number2, Epsilon) :-
		abs(Number1 - Number2) =< min(abs(Number1), abs(Number2)) * Epsilon.

	tolerance_equal(Number1, Number2, RelativeTolerance, AbsoluteTolerance) :-
		abs(Number1 - Number2) =< max(RelativeTolerance * max(abs(Number1), abs(Number2)), AbsoluteTolerance).

	valid(Number) :-
		number(Number).

	check(Term) :-
		(	number(Term) ->
			true
		;	var(Term) ->
			instantiation_error
		;	type_error(number, Term)
		).

:- end_object.
