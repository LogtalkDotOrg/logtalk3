%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(call_super_test_object_1).

	:- public(p/1).

	:- protected(q/1).
	q(1).

	:- protected(r/1).

	:- private(s/1).
	s(2).

	:- public(t1/1).

	:- public(t2/1).

:- end_object.


:- object(call_super_test_object_2,
	extends(call_super_test_object_1)).

	p(Goal) :-
		^^Goal.

	t1(X) :-
		^^q(X).

	t2(X) :-
		Closure = ^^q,
		call(Closure, X).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/02/26,
		comment is 'Unit tests for the (^^)/1 built-in control construct.'
	]).

	throws(call_super_1_1, error(instantiation_error,logtalk(^^_,call_super_test_object_2))) :-
		call_super_test_object_2::p(_).

	throws(call_super_1_2, error(type_error(callable,1),logtalk(^^1,call_super_test_object_2))) :-
		call_super_test_object_2::p(1).

	throws(call_super_1_3, error(permission_error(access,private_predicate,s/1),logtalk(^^s(_),call_super_test_object_2))) :-
		call_super_test_object_2::p(s(_)).

	throws(call_super_1_4, error(existence_error(predicate_declaration,t/1),logtalk(^^t(_),call_super_test_object_2))) :-
		call_super_test_object_2::p(t(_)).

	succeeds(call_super_1_5) :-
		call_super_test_object_2::p(q(X)),
		X == 1.

	succeeds(call_super_1_6) :-
		call_super_test_object_2::t1(X),
		X == 1.
	succeeds(call_super_1_7) :-
		call_super_test_object_2::t2(X),
		X == 1.

	fails(call_super_1_8) :-
		call_super_test_object_2::p(r(_)).

:- end_object.
