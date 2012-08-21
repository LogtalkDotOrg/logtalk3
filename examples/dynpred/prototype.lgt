%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
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
