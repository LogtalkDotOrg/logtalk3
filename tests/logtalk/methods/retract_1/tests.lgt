%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/04/16,
		comment is 'Unit tests for the retract/1 built-in method.'
	]).

	throws(retract_1_1, error(instantiation_error, logtalk(test_object::retract(_),user))) :-
		{test_object::retract(_)}.

	throws(retract_1_2, error(instantiation_error, logtalk(test_object::retract((_:-_)),user))) :-
		{test_object::retract((_ :- _))}.

	throws(retract_1_3, error(type_error(callable, 1), logtalk(test_object::retract(1),user))) :-
		{test_object::retract(1)}.

	throws(retract_1_4, error(type_error(callable, 1), logtalk(test_object::retract((1:-_)),user))) :-
		{test_object::retract((1 :- _))}.

	throws(retract_1_5, error(permission_error(modify, protected_predicate, q/2), logtalk(test_object::retract(q(_,_)),user))) :-
		{test_object::retract(q(_,_))}.

	throws(retract_1_6, error(permission_error(modify, private_predicate, r/3), logtalk(test_object::retract(r(_,_,_)),user))) :-
		{test_object::retract(r(_,_,_))}.

	throws(retract_1_7, error(permission_error(modify, static_predicate, s/4), logtalk(test_object::retract(s(_,_,_,_)),user))) :-
		{test_object::retract(s(_,_,_,_))}.

	throws(retract_1_8, error(existence_error(predicate_declaration, unknown/1), logtalk(test_object::retract(unknown(_)),user))) :-
		{test_object::retract(unknown(_))}.

	throws(retract_1_9, error(instantiation_error, logtalk(_::retract(foo),test_object))) :-
		{test_object::ie(_)}.

	throws(retract_1_10, error(type_error(object_identifier, 1), logtalk(1::retract(foo),test_object))) :-
		{test_object::te}.

	succeeds(retract_1_11) :-
		test_object::retract((t(X) :-true)),
		X == 1,
		test_object::retract((t(2) :-Body1)),
		Body1 == t(1),
		test_object::retract((t(3) :-Body2)),
		Body2 == (t(1), t(2)).

	succeeds(retract_1_12) :-
		create_object(Object, [], [public(t/1), dynamic(t/1)], [t(1), (t(2) :-t(1)), (t(3) :-t(1),t(2))]),
		Object::retract((t(X) :-true)),
		X == 1,
		Object::retract((t(2) :-Body1)),
		Body1 == t(1),
		Object::retract((t(3) :-Body2)),
		Body2 == (t(1), t(2)),
		abolish_object(Object).

:- end_object.
