%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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

	:- public(r/0).
	r :-
		[user::foo].

:- end_object.



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/02/02,
		comment is 'Unit tests for the []/1 built-in control construct.'
	]).

	succeeds(delegate_message_1_1) :-
		{delegate_message_test_object_2::p(X)},
		X == delegate_message_test_object_2.

	succeeds(delegate_message_1_2) :-
		{delegate_message_test_object_2::q(X)},
		X == user.

	throws(delegate_message_1_3, error(instantiation_error, logtalk(logtalk<<[_], user))) :-
		{logtalk << [_]}.

	throws(delegate_message_1_4, error(type_error(callable, 1), logtalk(logtalk<<[1], user))) :-
		{logtalk << [1]}.

	throws(delegate_message_1_5, error(permission_error(access, object, user), logtalk([user::foo], delegate_message_test_object_2))) :-
		{delegate_message_test_object_2::r}.

:- end_object.
