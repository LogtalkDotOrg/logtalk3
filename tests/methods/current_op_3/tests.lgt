%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(current_op_3_test_object_1).

	:- set_logtalk_flag(context_switching_calls, allow).

	% scoped operarors; seen by the reflection built-in methods
	:- public(op(501, xfx, abc)).
	:- protected(op(501, xfx, def)).
	:- private(op(501, xfx, ghi)).

	% local operator; invisible to the reflection built-in methods
	:- op(501, xfx, jkl).

	% test operator overriding
	:- public(op(600, xfx, (:))).

	% test for call in "self"
	:- public(operators/1).
	operators(Operators) :-
		setof(Operator, ::current_op(501, xfx, Operator), Operators).

:- end_object.



:- object(current_op_3_test_object_2,
	extends(current_op_3_test_object_1)).

	:- public(op(501, xfx, opq)).
	:- protected(op(501, xfx, rst)).
	:- private(op(501, xfx, uvw)).

	% local operator; invisible to the reflection built-in methods
	:- op(501, xfx, xyz).

:- end_object.



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/02/08,
		comment is 'Unit tests for the current_op/3 built-in directive.'
	]).

	throws(current_op_3_1, error(type_error(integer,a), logtalk(This::current_op(a,_,_),user))) :-
		this(This),
		{This::current_op(a, _, _)}.

	throws(current_op_3_2, error(domain_error(operator_priority,3000), logtalk(This::current_op(3000,_,_),user))) :-
		this(This),
		{This::current_op(3000, _, _)}.

	throws(current_op_3_3, error(type_error(atom,1), logtalk(This::current_op(_,1,_),user))) :-
		this(This),
		{This::current_op(_, 1, _)}.

	throws(current_op_3_4, error(domain_error(operator_specifier,a), logtalk(This::current_op(_,a,_),user))) :-
		this(This),
		{This::current_op(_, a, _)}.

	throws(current_op_3_5, error(type_error(atom,1), logtalk(This::current_op(_,_,1),user))) :-
		this(This),
		{This::current_op(_, _, 1)}.

	succeeds(current_op_3_6) :-
		setof(Operator, current_op_3_test_object_1<<current_op(501, xfx, Operator), Operators),
		Operators == [abc, def, ghi].

	succeeds(current_op_3_7) :-
		setof(Operator, current_op_3_test_object_1::current_op(501, xfx, Operator), Operators),
		Operators == [abc].

	succeeds(current_op_3_8) :-
		current_op_3_test_object_1::current_op(600, xfx, (:)),
		\+ current_op_3_test_object_1::current_op(600, xfy, (:)).

	succeeds(current_op_3_9) :-
		\+ current_op_3_test_object_2::current_op(600, xfx, (:)).

	succeeds(current_op_3_10) :-
		current_op_3_test_object_1::operators(Operators),
		Operators == [abc, def, ghi].

	succeeds(current_op_3_11) :-
		current_op_3_test_object_2::operators(Operators),
		Operators == [opq, rst].

:- end_object.
