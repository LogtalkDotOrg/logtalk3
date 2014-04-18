%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(current_predicate_1_test_object).

	:- public(ie/1).
	ie(Object) :-
		Object::current_predicate(foo/1).

	:- public(te/0).
	te :-
		Object = 1,
		Object::current_predicate(foo/1).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/04/18,
		comment is 'Unit tests for the current_predicate/1 built-in method.'
	]).

	throws(current_predicate_1_1, error(type_error(predicate_indicator, 1), logtalk(This::current_predicate(1),user))) :-
		this(This),
		{This::current_predicate(1)}.

	throws(current_predicate_1_2, error(type_error(atom,1), logtalk(This::current_predicate(1/b),user))) :-
		this(This),
		{This::current_predicate(1/b)}.

	throws(current_predicate_1_3, error(type_error(integer,b), logtalk(This::current_predicate(a/b),user))) :-
		this(This),
		{This::current_predicate(a/b)}.

	throws(current_predicate_1_4, error(domain_error(not_less_than_zero, -1), logtalk(This::current_predicate(a/(-1)),user))) :-
		this(This),
		{This::current_predicate(a/(-1))}.

	throws(current_predicate_1_5, error(instantiation_error, logtalk(_::current_predicate(foo/1),current_predicate_1_test_object))) :-
		{current_predicate_1_test_object::ie(_)}.

	throws(current_predicate_1_6, error(type_error(object_identifier, 1), logtalk(1::current_predicate(foo/1),current_predicate_1_test_object))) :-
		{current_predicate_1_test_object::te}.

:- end_object.
