%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(context_switch_test_object).

	:- set_logtalk_flag(context_switching_calls, deny).

	p.

:- end_object.



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/12/05,
		comment is 'Unit tests for the (<<)/2 built-in control construct.'
	]).

	throws(context_switch_2_1, error(instantiation_error, logtalk(_<<goal,user))) :-
		{_ << goal}.

	throws(context_switch_2_2, error(instantiation_error, logtalk(logtalk<<_,user))) :-
		{logtalk << _}.

	throws(context_switch_2_3, error(type_error(object_identifier, 3), logtalk(3<<goal,user))) :- 
		{3 << goal}.

	throws(context_switch_2_4, error(type_error(callable, 3), logtalk(object<<3,user))) :-
		{object << 3}.

	throws(context_switch_2_5, error(existence_error(procedure, goal/0), logtalk(This<<goal,user))) :-
		this(This),
		{This << goal}.

	throws(context_switch_2_6, error(existence_error(object, foo), logtalk(foo<<goal,user))) :-
		{foo << goal}.

	throws(context_switch_2_7, error(permission_error(access, database, p), logtalk(context_switch_test_object<<p,user))) :-
		{context_switch_test_object << p}.

	succeeds(context_switch_2_8) :-
		{user << true}.

	succeeds(context_switch_2_9) :-
		this(This),
		findall(X, {This << a(X)}, Xs),
		Xs == [1,2,3].

	succeeds(context_switch_2_10) :-
		create_object(Object, [], [], [a(1),a(2),a(3)]),
		findall(X, {Object << a(X)}, Xs),
		Xs == [1,2,3],
		abolish_object(Object).

	fails(context_switch_2_11) :-
		{user << fail}.

	fails(context_switch_2_12) :-
		logtalk::assertz(foo(_)),
		logtalk::retractall(foo(_)),
		logtalk << foo(_).

	fails(context_switch_2_13) :-
		this(This),
		{This << a(4)}.

	a(1). a(2). a(3).

:- end_object.
