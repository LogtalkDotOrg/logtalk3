%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>
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



:- object(genint).

	:- info([
		version is 1.01,
		author is 'Arun Majumdar. Adapted to Logtalk by Paulo Moura.',
		date is 2012/04/07,
		comment is 'Predicates for generating unique atoms.']).

	:- public(reset_genint/0).
	:- synchronized(reset_genint/0).
	:- mode(reset_genint, one).
	:- info(reset_genint/0, [
		comment is 'Resets the all the integer counters.']).

	:- public(reset_genint/1).
	:- synchronized(reset_genint/1).
	:- mode(reset_genint(+atom), one).
	:- info(reset_genint/1, [
		comment is 'Resets the named integer counter.',
		argnames is ['Counter']]).

	:- public(genint/2).
	:- synchronized(genint/2).
	:- mode(genint(+atom, -integer), one).
	:- info(genint/2, [
		comment is 'Returns the next integer for the given counter.',
		argnames is ['Counter', 'Value']]).

	:- private(base_/2).
	:- dynamic(base_/2).
	:- mode(base_(?atom, ?integer), zero_or_more).
	:- info(base_/2, [
		comment is 'Table of counters and respective values.',
		argnames is ['Counter', 'Value']]).

	reset_genint :-
		retract(base_(Counter, _)),
		asserta(base_(Counter, 0)),
		fail.
	reset_genint.

	reset_genint(Counter) :-
		retractall(base_(Counter, _)),
		asserta(base_(Counter, 0)).

	genint(Counter, Unique) :-
		(	retract(base_(Counter, OldInt)) ->
			NewInt is OldInt + 1
		;	NewInt is 1
		),
		asserta(base_(Counter, NewInt)),
		number_codes(NewInt, NewCodes),
		atom_codes(NewAtom, NewCodes),
		atom_concat(Counter, NewAtom, Unique).

:- end_object.
