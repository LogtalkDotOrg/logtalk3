%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- object(boolean,
	extends(term)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2010/05/31,
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
			throw(instantiation_error)
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
		this(This),
		sender(Sender),
		(	valid(Term) ->
			true
		;	var(Term) ->
			throw(error(instantiation_error, This::check(Term), Sender))
		;	throw(error(type_error(This, Term), This::check(Term), Sender))
		).

:- end_object.
