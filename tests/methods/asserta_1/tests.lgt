
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
		date is 2012/12/03,
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

	throws(asserta_1_6, error(permission_error(modify, protected_predicate, q(_,_)), logtalk(asserta_1_test_object::asserta(q(_,_)),user))) :-
		{asserta_1_test_object::asserta(q(_,_))}.

	throws(asserta_1_7, error(permission_error(modify, protected_predicate, q(_,_)), logtalk(asserta_1_test_object::asserta((q(_,_):-nl)),user))) :-
		{asserta_1_test_object::asserta((q(_,_) :- nl))}.

	throws(asserta_1_8, error(permission_error(modify, private_predicate, r(_,_,_)), logtalk(asserta_1_test_object::asserta(r(_,_,_)),user))) :-
		{asserta_1_test_object::asserta(r(_,_,_))}.

	throws(asserta_1_9, error(permission_error(modify, private_predicate, r(_,_,_)), logtalk(asserta_1_test_object::asserta((r(_,_,_):-nl)),user))) :-
		{asserta_1_test_object::asserta((r(_,_,_) :- nl))}.

	throws(asserta_1_10, error(permission_error(modify, static_predicate, s(_,_,_,_)), logtalk(asserta_1_test_object::asserta(s(_,_,_,_)),user))) :-
		{asserta_1_test_object::asserta(s(_,_,_,_))}.

	throws(asserta_1_11, error(permission_error(modify, static_predicate, s(_,_,_,_)), logtalk(asserta_1_test_object::asserta((s(_,_,_,_):-nl)),user))) :-
		{asserta_1_test_object::asserta((s(_,_,_,_) :- nl))}.

	throws(asserta_1_12, error(permission_error(create, predicate_declaration, new), logtalk(asserta_1_test_object::asserta(new),user))) :-
		{asserta_1_test_object::asserta(new)}.

:- end_object.
