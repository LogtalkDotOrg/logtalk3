%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


:- object(boolean,
	extends(term)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2018-07-11,
		comment is 'Boolean data type predicates.'
	]).

	:- public(eval/2).
	:- mode(eval(+nonvar, -boolean), zero_or_one).
	:- info(eval/2, [
		comment is 'Evaluates a boolean expression, returning either true or false. Expressions use the (,)/2, (;)/2, and \+/1 standard operators, plus the atoms true and false.',
		argnames is ['Expression', 'Value']
	]).

	eval(Expression, Value) :-
		(	var(Expression) ->
			instantiation_error
		;	call(Expression) ->
			Value = true
		;	Value = false
		).

	valid((-)) :-		% catch variables
		!,
		fail.
	valid(true).
	valid(false).
	valid((BE1, BE2)) :-
		valid(BE1),
		valid(BE2).
	valid((BE1; BE2)) :-
		valid(BE1),
		valid(BE2).
	valid(\+ BE) :-
		valid(BE).

	check(Term) :-
		(	valid(Term) ->
			true
		;	var(Term) ->
			instantiation_error
		;	type_error(boolean, Term)
		).

:- end_object.
