%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(send_to_object_test_object(_)).

	% predicates for testing of runtime bound messages

	:- public(rt/1).
	rt(Goal) :-
		this(This),
		{This::Goal}.

	:- public(p/1).
	p(X) :-
		parameter(1, X).

	:- public(q/2).
	:- protected(r/3).
	:- private(s/4).

:- end_object.


% for testing proxies:
send_to_object_test_object(2).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/03/31,
		comment is 'Unit tests for the (::)/2 built-in control construct.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests for runtime bound messages

	throws(send_to_object_2_1, error(instantiation_error,logtalk(send_to_object_test_object(_)::_,user))) :-
		send_to_object_test_object(_)::rt(_).

	throws(send_to_object_2_2, error(type_error(callable,1),logtalk(send_to_object_test_object(_)::1,user))) :-
		send_to_object_test_object(_)::rt(1).

	throws(send_to_object_2_3, error(permission_error(access,private_predicate,s/4),logtalk(send_to_object_test_object(_)::s(_,_,_,_),user))) :-
		send_to_object_test_object(_)::rt(s(_,_,_,_)).

	throws(send_to_object_2_4, error(existence_error(predicate_declaration,t/1),logtalk(send_to_object_test_object(_)::t(_),user))) :-
		send_to_object_test_object(_)::rt(t(_)).

	throws(send_to_object_2_5, error(existence_error(predicate_declaration,atom/1),logtalk(send_to_object_test_object(_)::atom(a),user))) :-
		send_to_object_test_object(_)::rt(atom(a)).

	succeeds(send_to_object_2_6) :-
		send_to_object_test_object(1)::rt(p(X)),
		X == 1.

	succeeds(send_to_object_2_7) :-
		{send_to_object_test_object(_)}::rt(p(X)),
		X == 2.

	succeeds(send_to_object_2_8) :-
		send_to_object_test_object(_)::rt({atom(a)}).

	succeeds(send_to_object_2_9) :-
		send_to_object_test_object(_)::rt(({atom(a)}, {number(1)})).

	fails(send_to_object_2_10) :-
		send_to_object_test_object(_)::rt(q(_,_)).

	% tests for compile-time bound messages

	throws(send_to_object_2_11, error(instantiation_error, logtalk(_::true,user))) :-
		{_::true}.

	throws(send_to_object_2_12, error(type_error(object_identifier,1), logtalk(1::true,user))) :-
		{1::true}.

	throws(send_to_object_2_13, error(instantiation_error, logtalk(logtalk::_,user))) :-
		{logtalk::_}.

	throws(send_to_object_2_14, error(type_error(callable,1), logtalk(logtalk::1,user))) :-
		{logtalk::1}.

	throws(send_to_object_2_15, error(existence_error(object,foo), logtalk(foo::true,user))) :-
		{foo::true}.

	throws(send_to_object_2_16, error(existence_error(predicate_declaration,foo/0), logtalk(logtalk::foo,user))) :-
		{logtalk::foo}.

	throws(send_to_object_2_17, error(existence_error(predicate_declaration,atom/1), logtalk(logtalk::atom(a),user))) :-
		{logtalk::atom(a)}.

	throws(send_to_object_2_18, error(permission_error(access,protected_predicate,r/3), logtalk(send_to_object_test_object(_)::r(_,_,_),user))) :-
		{send_to_object_test_object(_)::r(_,_,_)}.

	throws(send_to_object_2_19, error(permission_error(access, private_predicate,s/4), logtalk(send_to_object_test_object(_)::s(_,_,_,_),user))) :-
		{send_to_object_test_object(_)::s(_,_,_,_)}.

	throws(send_to_object_2_20, error(instantiation_error, logtalk({_}::true,user))) :-
		{{_}::true}.

	throws(send_to_object_2_21, error(type_error(object_identifier,1), logtalk({1}::true,user))) :-
		{{1}::true}.

	throws(send_to_object_2_22, error(existence_error(procedure,foo/0), logtalk({foo}::true,user))) :-
		{{foo}::true}.

	succeeds(send_to_object_2_23) :-
		send_to_object_test_object(1)::p(X),
		X == 1.

	succeeds(send_to_object_2_24) :-
		{send_to_object_test_object(_)}::p(X),
		X == 2.

	succeeds(send_to_object_2_25) :-
		send_to_object_test_object(_)::{atom(a)}.

	succeeds(send_to_object_2_26) :-
		send_to_object_test_object(_)::({atom(a)}, {number(1)}).

	fails(send_to_object_2_27) :-
		send_to_object_test_object(_)::q(_, _).

:- end_object.
