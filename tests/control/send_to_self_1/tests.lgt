%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(send_to_self_test_object_1).

	:- public(p/1).
	p(Goal) :-
		::Goal.

	:- protected(q/1).
	:- protected(r/1).

	:- private(s/1).

:- end_object.


:- object(send_to_self_test_object_2,
	extends(send_to_self_test_object_1)).

	:- private(t/1).
	t(1).

	s(2).

	q(3).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/12/05,
		comment is 'Unit tests for the (::)/1 built-in control construct.'
	]).

	throws(send_to_self_1_1, error(instantiation_error,logtalk(::_,send_to_self_test_object_1))) :-
		send_to_self_test_object_2::p(_).

	throws(send_to_self_1_2, error(type_error(callable,1),logtalk(::1,send_to_self_test_object_1))) :-
		send_to_self_test_object_2::p(1).

	throws(send_to_self_1_3, error(permission_error(access,private_predicate,t/1),logtalk(::t(_),send_to_self_test_object_1))) :-
		send_to_self_test_object_2::p(t(_)).

	throws(send_to_self_1_4, error(existence_error(predicate_declaration,u/1),logtalk(::u(_),send_to_self_test_object_1))) :-
		send_to_self_test_object_2::p(u(_)).

	succeeds(send_to_self_1_5) :-
		send_to_self_test_object_2::p(s(X)),
		X == 2.

	succeeds(send_to_self_1_6) :-
		send_to_self_test_object_2::p(q(X)),
		X == 3.

	fails(send_to_self_1_7) :-
		send_to_self_test_object_2::p(r(_)).

:- end_object.
