%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
