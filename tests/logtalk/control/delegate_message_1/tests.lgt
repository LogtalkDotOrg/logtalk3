%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(library).

	:- public(my_call/2).
	:- meta_predicate(my_call(1, *)).
	my_call(Closure, Arg) :-
		call(Closure, Arg).

:- end_object.



:- object(delegate_message_test_object_1).

	:- public(s/1).
	s(X) :-
		sender(X).

:- end_object.



:- object(delegate_message_test_object_2).

	:- public(p/1).
	p(X) :-
		delegate_message_test_object_1::s(X).

	:- public(q/1).
	q(X) :-
		[delegate_message_test_object_1::s(X)].

	:- public(r/1).
	r(X) :-
		call([delegate_message_test_object_1::s], X).

	:- public(s/1).
	s(X) :-
		Closure = [delegate_message_test_object_1::s],
		call(Closure, X).

	:- public(t/1).
	t(X) :-
		library::my_call([delegate_message_test_object_1::s], X).

	:- public(u/0).
	u :-
		[user::foo].

:- end_object.



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/08/28,
		comment is 'Unit tests for the []/1 built-in control construct.'
	]).

	succeeds(delegate_message_1_1) :-
		{delegate_message_test_object_2::p(X)},
		X == delegate_message_test_object_2.

	succeeds(delegate_message_1_2) :-
		{delegate_message_test_object_2::q(X)},
		X == user.

	succeeds(delegate_message_1_3) :-
		{delegate_message_test_object_2::r(X)},
		X == user.

	succeeds(delegate_message_1_4) :-
		{delegate_message_test_object_2::s(X)},
		X == user.

	succeeds(delegate_message_1_5) :-
		{delegate_message_test_object_2::t(X)},
		X == user.

	throws(delegate_message_1_6, error(instantiation_error, logtalk(logtalk<<[_], user))) :-
		{logtalk << [_]}.

	throws(delegate_message_1_7, error(type_error(callable, 1), logtalk(logtalk<<[1], user))) :-
		{logtalk << [1]}.

	throws(delegate_message_1_8, error(permission_error(access, object, user), logtalk([user::foo], delegate_message_test_object_2))) :-
		{delegate_message_test_object_2::u}.

:- end_object.
