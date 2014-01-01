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



:- category(counters).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2011/01/03,
		comment is 'Named integer counters. Counter names can be any nonvar term.'
	]).

	:- public(counter/2).
	:- mode(counter(?nonvar, ?integer), zero_or_more).
	:- info(counter/2, [
		comment is 'True if Counter is a counter with value Value.',
		argnames is ['Counter', 'Value']
	]).

	:- public(increment_counter/1).
	:- mode(increment_counter(+nonvar), one).
	:- info(increment_counter/1, [
		comment is 'Increments the named counter.',
		argnames is ['Counter']
	]).

	:- public(decrement_counter/1).
	:- mode(decrement_counter(+nonvar), one).
	:- info(decrement_counter/1, [
		comment is 'Decrements the named counter.',
		argnames is ['Counter']
	]).

	:- public(reset_counter/1).
	:- mode(reset_counter(+nonvar), one).
	:- info(reset_counter/1, [
		comment is 'Resets the named counter to zero. Creates the counter if it does not exist.',
		argnames is ['Counter']
	]).

	:- public(reset_counters/0).
	:- mode(reset_counters, one).
	:- info(reset_counter/0, [
		comment is 'Resets all existing named counters to zero.'
	]).

	:- private(counter_/2).
	:- dynamic(counter_/2).
	:- mode(counter_(?nonvar, ?integer), zero_or_more).
	:- info(counter_/2, [
		comment is 'Table of named counters.',
		argnames is ['Counter', 'Value']
	]).

	counter(Counter, Value) :-
		::counter_(Counter, Value).

	increment_counter(Counter) :-
		::retract(counter_(Counter, OldValue)),
		NewValue is OldValue + 1,
		::assertz(counter_(Counter, NewValue)).

	decrement_counter(Counter) :-
		::retract(counter_(Counter, OldValue)),
		NewValue is OldValue - 1,
		::assertz(counter_(Counter, NewValue)).

	reset_counter(Counter) :-
		::retractall(counter_(Counter, _)),
		::assertz(counter_(Counter, 0)).

	reset_counters :-
		::retract(counter_(Counter, _)),
		::assertz(counter_(Counter, 0)),
		fail.
	reset_counters.

:- end_category.
