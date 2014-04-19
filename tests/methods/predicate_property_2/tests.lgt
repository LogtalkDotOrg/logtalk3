%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(predicate_property_2_test_object).

	:- public(ie/1).
	ie(Object) :-
		Object::predicate_property(foo, _).

	:- public(te/0).
	te :-
		Object = 1,
		Object::predicate_property(foo, _).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/04/18,
		comment is 'Unit tests for the predicate_property/2 built-in method.'
	]).

	throws(predicate_property_2_1, error(instantiation_error,logtalk(This::predicate_property(_,_),user))) :-
		this(This),
		{This::predicate_property(_, _)}.

	throws(predicate_property_2_2, error(type_error(callable, 1),logtalk(This::predicate_property(1, _),user))) :-
		this(This),
		{This::predicate_property(1, _)}.

	throws(predicate_property_2_3, error(domain_error(predicate_property, bar),logtalk(This::predicate_property(foo, bar),user))) :-
		this(This),
		{This::predicate_property(foo, bar)}.

	throws(predicate_property_2_4, error(instantiation_error, logtalk(_::predicate_property(foo,_),predicate_property_2_test_object))) :-
		{predicate_property_2_test_object::ie(_)}.

	throws(predicate_property_2_5, error(type_error(object_identifier, 1), logtalk(1::predicate_property(foo,_),predicate_property_2_test_object))) :-
		{predicate_property_2_test_object::te}.

:- end_object.
