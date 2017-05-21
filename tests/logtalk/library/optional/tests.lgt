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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2017/05/21,
		comment is 'Unit tests for the "optional" library.'
	]).

	:- discontiguous([
		succeeds/1, throws/2
	]).

	% is_empty/0 tests

	succeeds(optionals_is_empty_1_01) :-
		optional::empty(Ref), optional(Ref)::is_empty.

	succeeds(optionals_is_empty_1_02) :-
		optional::of(0, Ref), \+ optional(Ref)::is_empty.

	% is_present/0 tests

	succeeds(optionals_is_present_1_01) :-
		optional::empty(Ref), \+ optional(Ref)::is_present.

	succeeds(optionals_is_present_1_02) :-
		optional::of(0, Ref), optional(Ref)::is_present.

	% if_empty/1 tests

	succeeds(optionals_if_empty_1_01) :-
		optional::empty(Ref), optional(Ref)::if_empty(X = 1),
		X == 1.

	succeeds(optionals_if_empty_1_02) :-
		optional::of(0, Ref), optional(Ref)::if_empty(X = 1),
		var(X).

	% if_present/1 tests

	succeeds(optionals_if_present_1_01) :-
		optional::empty(Ref), optional(Ref)::if_present({Y}/[X]>>(Y is X + 1)),
		var(Y).

	succeeds(optionals_if_present_1_02) :-
		optional::of(0, Ref), optional(Ref)::if_present({Y}/[X]>>(Y is X + 1)),
		Y == 1.

	% filter/2 tests

	succeeds(optionals_filter_2_01) :-
		optional::of(1, Ref), optional(Ref)::filter(integer, NewRef),
		optional(NewRef)::is_present.

	succeeds(optionals_filter_2_02) :-
		optional::of(a, Ref), optional(Ref)::filter(integer, NewRef),
		optional(NewRef)::is_empty.

	% map/2 tests

	succeeds(optionals_map_2_01) :-
		optional::empty(Ref), optional(Ref)::map(char_code, NewRef),
		optional(NewRef)::is_empty.

	succeeds(optionals_map_2_02) :-
		optional::of(a, Ref), optional(Ref)::map(char_code, NewRef),
		optional(NewRef)::get(Term), Term == 97.

	% get/1 tests

	throws(optionals_get_1_01, error(existence_error(optional_term,_), _)) :-
		optional::empty(Ref), optional(Ref)::get(_).

	succeeds(optionals_get_1_02) :-
		optional::of(0, Ref), optional(Ref)::get(Term),
		Term == 0.

	% or_else/2 tests

	succeeds(optionals_or_else_2_01) :-
		optional::empty(Ref), optional(Ref)::or_else(Term, 0),
		Term == 0.

	succeeds(optionals_or_else_2_02) :-
		optional::of(1, Ref), optional(Ref)::or_else(Term, 0),
		Term == 1.

:- end_object.
