%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


:- object(state_space,
	instantiates(class),
	specializes(object)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2008/6/9,
		comment is 'State space description predicates.'
	]).

	:- public(initial_state/1).
	:- mode(initial_state(?nonvar), one_or_more).
	:- info(initial_state/1, [
		comment is 'Initial state.',
		argnames is ['State']
	]).

	:- public(initial_state/2).
	:- mode(initial_state(?atom, ?nonvar), zero_or_more).
	:- info(initial_state/2, [
		comment is 'Named initial state.',
		argnames is ['Name', 'State']
	]).

	:- public(next_state/2).
	:- mode(next_state(+nonvar, -nonvar), zero_or_more).
	:- info(next_state/2, [
		comment is 'Generates a state sucessor.',
		argnames is ['State', 'Next']
	]).

	:- public(goal_state/1).
	:- mode(goal_state(?nonvar), one_or_more).
	:- info(goal_state/1, [
		comment is 'Goal state.',
		argnames is ['State']
	]).

	:- public(goal_state/2).
	:- mode(goal_state(?atom, ?nonvar), zero_or_more).
	:- info(goal_state/2, [
		comment is 'Named goal state.',
		argnames is ['Name', 'State']
	]).

	:- public(member_path/2).
	:- mode(member_path(+nonvar, +list), zero_or_one).
	:- info(member_path/2, [
		comment is 'True if a state is member of a list of states.',
		argnames is ['State', 'Path']
	]).

	:- public(print_state/1).
	:- mode(print_state(+nonvar), one).
	:- info(print_state/1, [
		comment is 'Pretty print state.',
		argnames is ['State']
	]).

	:- public(print_path/1).
	:- mode(print_path(+list), one).
	:- info(print_path/1, [
		comment is 'Pretty print a path (list of states).',
		argnames is ['Path']
	]).

	initial_state(State) :-
		::initial_state(_, State).

	goal_state(State) :-
		::goal_state(_, State).

	print_state(State) :-
		writeq(State), nl.

	member_path(State, [State| _]) :-
		!.
	member_path(State, [_| Path]) :-
		member_path(State, Path).

	print_path([]).
	print_path([State| States]) :-
		::print_state(State),
		print_path(States).

:- end_object.
