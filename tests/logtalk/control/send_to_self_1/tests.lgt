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

	% predicates for testing of runtime bound messages

	:- public(rt/1).
	rt(Goal) :-
		::Goal.

	% predicates for testing of compile-time bound messages

	:- public(ct_p/1).
	ct_p(X) :-
		::p(X).

	:- public(ct_q/1).
	ct_q(X) :-
		::q(X).

	:- public(ct_r/1).
	ct_r(X) :-
		::r(X).

	:- public(ct_s/1).
	ct_s(X) :-
		::s(X).

	:- public(ct_t/1).
	ct_t(X) :-
		::t(X).

	:- public(ct_b1/0).
	ct_b1 :-
		::({atom(a)}).

	:- public(ct_b2/0).
	ct_b2 :-
		::(({atom(a)}, {number(1)})).

	:- public(ct_b3/0).
	ct_b3 :-
		::atom(a).

	:- protected(p/1).
	:- protected(q/1).

	:- private(r/1).

:- end_object.


:- object(send_to_self_test_object_2,
	extends(send_to_self_test_object_1)).

	:- private(s/1).
	s(1).

	p(2).

	r(3).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/03/31,
		comment is 'Unit tests for the (::)/1 built-in control construct.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests for runtime bound messages

	throws(send_to_self_1_1, error(instantiation_error,logtalk(::_,send_to_self_test_object_1))) :-
		send_to_self_test_object_2::rt(_).

	throws(send_to_self_1_2, error(type_error(callable,1),logtalk(::1,send_to_self_test_object_1))) :-
		send_to_self_test_object_2::rt(1).

	throws(send_to_self_1_3, error(permission_error(access,private_predicate,s/1),logtalk(::s(_),send_to_self_test_object_1))) :-
		send_to_self_test_object_2::rt(s(_)).

	throws(send_to_self_1_4, error(existence_error(predicate_declaration,t/1),logtalk(::t(_),send_to_self_test_object_1))) :-
		send_to_self_test_object_2::rt(t(_)).

	throws(send_to_self_1_5, error(existence_error(predicate_declaration,atom/1),logtalk(::atom(a),send_to_self_test_object_1))) :-
		send_to_self_test_object_2::rt(atom(a)).

	succeeds(send_to_self_1_6) :-
		send_to_self_test_object_2::rt(p(X)),
		X == 2.

	succeeds(send_to_self_1_7) :-
		send_to_self_test_object_2::rt(r(X)),
		X == 3.

	succeeds(send_to_self_1_8) :-
		send_to_self_test_object_2::rt({atom(a)}).

	succeeds(send_to_self_1_9) :-
		send_to_self_test_object_2::rt(({atom(a)}, {number(1)})).

	fails(send_to_self_1_10) :-
		send_to_self_test_object_2::rt(q(_)).

	% tests for compile-time bound messages

	throws(send_to_self_1_11, error(permission_error(access,private_predicate,s/1),logtalk(::s(_),send_to_self_test_object_1))) :-
		send_to_self_test_object_2::ct_s(_).

	throws(send_to_self_1_12, error(existence_error(predicate_declaration,t/1),logtalk(::t(_),send_to_self_test_object_1))) :-
		send_to_self_test_object_2::ct_t(_).

	throws(send_to_self_1_13, error(existence_error(predicate_declaration,atom/1),logtalk(::atom(a),send_to_self_test_object_1))) :-
		send_to_self_test_object_2::ct_b3.

	succeeds(send_to_self_1_14) :-
		send_to_self_test_object_2::ct_p(X),
		X == 2.

	succeeds(send_to_self_1_15) :-
		send_to_self_test_object_2::ct_r(X),
		X == 3.

	succeeds(send_to_self_1_16) :-
		send_to_self_test_object_2::ct_b1.

	succeeds(send_to_self_1_17) :-
		send_to_self_test_object_2::ct_b2.

	fails(send_to_self_1_18) :-
		send_to_self_test_object_2::ct_q(_).

:- end_object.
