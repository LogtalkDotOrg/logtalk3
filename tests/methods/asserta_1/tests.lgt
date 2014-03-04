%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(asserta_1_test_object).

	:- set_logtalk_flag(dynamic_declarations, deny).

	:- public(p/1).
	:- dynamic(p/1).

	:- protected(q/2).
	:- dynamic(q/2).

	:- private(r/3).
	:- dynamic(r/3).

	:- public(s/4).

:- end_object.



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/06/07,
		comment is 'Unit tests for the asserta/1 built-in method.'
	]).

	throws(asserta_1_1, error(instantiation_error, logtalk(asserta_1_test_object::asserta(_),user))) :-
		{asserta_1_test_object::asserta(_)}.

	throws(asserta_1_2, error(instantiation_error, logtalk(asserta_1_test_object::asserta((_:-_)),user))) :-
		{asserta_1_test_object::asserta((_ :- _))}.

	throws(asserta_1_3, error(type_error(callable, 1), logtalk(asserta_1_test_object::asserta(1),user))) :-
		{asserta_1_test_object::asserta(1)}.

	throws(asserta_1_4, error(type_error(callable, 1), logtalk(asserta_1_test_object::asserta((1:-_)),user))) :-
		{asserta_1_test_object::asserta((1 :- _))}.

	throws(asserta_1_5, error(type_error(callable, 1), logtalk(asserta_1_test_object::asserta((p:-1)),user))) :-
		{asserta_1_test_object::asserta((p :- 1))}.

	throws(asserta_1_6, error(permission_error(modify, protected_predicate, q/2), logtalk(asserta_1_test_object::asserta(q(_,_)),user))) :-
		{asserta_1_test_object::asserta(q(_,_))}.

	throws(asserta_1_7, error(permission_error(modify, protected_predicate, q/2), logtalk(asserta_1_test_object::asserta((q(_,_) :-nl)),user))) :-
		{asserta_1_test_object::asserta((q(_,_) :- nl))}.

	throws(asserta_1_8, error(permission_error(modify, private_predicate, r/3), logtalk(asserta_1_test_object::asserta(r(_,_,_)),user))) :-
		{asserta_1_test_object::asserta(r(_,_,_))}.

	throws(asserta_1_9, error(permission_error(modify, private_predicate, r/3), logtalk(asserta_1_test_object::asserta((r(_,_,_) :-nl)),user))) :-
		{asserta_1_test_object::asserta((r(_,_,_) :- nl))}.

	throws(asserta_1_10, error(permission_error(modify, static_predicate, s/4), logtalk(asserta_1_test_object::asserta(s(_,_,_,_)),user))) :-
		{asserta_1_test_object::asserta(s(_,_,_,_))}.

	throws(asserta_1_11, error(permission_error(modify, static_predicate, s/4), logtalk(asserta_1_test_object::asserta((s(_,_,_,_) :-nl)),user))) :-
		{asserta_1_test_object::asserta((s(_,_,_,_) :- nl))}.

	throws(asserta_1_12, error(permission_error(create, predicate_declaration, new/0), logtalk(asserta_1_test_object::asserta(new),user))) :-
		{asserta_1_test_object::asserta(new)}.

	succeeds(asserta_1_13) :-
		create_object(Object, [], [public(a/1), public(p/1)], []),
		Object::asserta(a(1)),
		Object::asserta(a(2)),
		Object::asserta(a(3)),
		Object::asserta((p(X) :- a(X))),
		findall(X, Object::a(X), Xs),
		Xs == [3,2,1],
		findall(Y, Object::p(Y), Ys),
		Ys == [3,2,1],
		abolish_object(Object).

	succeeds(asserta_1_14) :-
		create_object(Object, [], [set_logtalk_flag(dynamic_declarations, allow)], []),
		Object::asserta(a(1)),
		Object::asserta(a(2)),
		Object::asserta(a(3)),
		Object::asserta((p(X) :- a(X))),
		findall(X, Object::a(X), Xs),
		Xs == [3,2,1],
		findall(Y, Object::p(Y), Ys),
		Ys == [3,2,1],
		abolish_object(Object).

:- end_object.
