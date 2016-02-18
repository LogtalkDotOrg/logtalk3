%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


:- object(prototype).

	:- public(object_assert/0).
	:- public(self_assert/0).
	:- public(this_assert/0).

	:- public(dynamic_predicates/0).

	:- public(public_predicate/0).
	:- dynamic(public_predicate/0).

	object_assert :-
		self(Self),
		Self::assertz(public_predicate).

	:- protected(protected_predicate/0).
	:- dynamic(protected_predicate/0).

	self_assert :-
		::assertz(protected_predicate).

	:- private(private_predicate/0).
	:- dynamic(private_predicate/0).

	this_assert :-
		assertz(private_predicate).

	dynamic_predicates :-
		current_predicate(Functor/Arity),
		functor(Predicate, Functor, Arity),
		predicate_property(Predicate, (dynamic)),
		predicate_property(Predicate, Scope),
		scope(Scope),
		writeq(Functor/Arity), write(' - '), writeq(Scope), nl,
		fail.

	dynamic_predicates.

	scope(private).
	scope(protected).
	scope((public)).

:- end_object.
