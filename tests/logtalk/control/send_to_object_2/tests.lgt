%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(test_object(_)).

	:- public(p/1).
	p(X) :-
		parameter(1, X).

	:- public(q/2).
	:- protected(r/3).
	:- private(s/4).

:- end_object.


% for testing proxies:
test_object(2).


:- object(send_to_object_test_object(_)).

	% predicates for testing of runtime bound messages

	:- public(rt/1).
	rt(Message) :-
		parameter(1, X),
		test_object(X)::Message.

	:- public(rtmc/1).
	rtmc(Message) :-
		parameter(1, X),
		Closure = (::),
		Object = test_object(X),
		call(Closure, Object, Message).

:- end_object.


% for testing proxies:
send_to_object_test_object(2).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.3,
		author is 'Paulo Moura',
		date is 2020/01/28,
		comment is 'Unit tests for the (::)/2 built-in control construct.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests for runtime bound messages

	throws(send_to_object_2_01, error(instantiation_error,logtalk(test_object(_)::_,_))) :-
		send_to_object_test_object(_)::rt(_).

	throws(send_to_object_2_02, error(type_error(callable,1),logtalk(test_object(_)::1,_))) :-
		send_to_object_test_object(_)::rt(1).

	throws(send_to_object_2_03, error(permission_error(access,private_predicate,s/4),logtalk(test_object(_)::s(_,_,_,_),_))) :-
		send_to_object_test_object(_)::rt(s(_,_,_,_)).

	throws(send_to_object_2_04, error(existence_error(predicate_declaration,t/1),logtalk(test_object(_)::t(_),_))) :-
		send_to_object_test_object(_)::rt(t(_)).

	throws(send_to_object_2_05, error(existence_error(predicate_declaration,atom/1),logtalk(test_object(_)::atom(a),_))) :-
		send_to_object_test_object(_)::rt(atom(a)).

	succeeds(send_to_object_2_06) :-
		send_to_object_test_object(1)::rt(p(X)),
		X == 1.

	succeeds(send_to_object_2_07) :-
		{send_to_object_test_object(_)}::rt(p(X)),
		X == 2.

	succeeds(send_to_object_2_08) :-
		send_to_object_test_object(_)::rt({atom(a)}).

	succeeds(send_to_object_2_09) :-
		send_to_object_test_object(_)::rt(({atom(a)}, {number(1)})).

	fails(send_to_object_2_10) :-
		send_to_object_test_object(_)::rt(q(_,_)).

	% tests for runtime bound messages using call/N

	throws(send_to_object_2_11, error(instantiation_error,logtalk(call(test_object(_)::_),_))) :-
		send_to_object_test_object(_)::rtmc(_).

	throws(send_to_object_2_12, error(type_error(callable,1),logtalk(call(test_object(_)::1),_))) :-
		send_to_object_test_object(_)::rtmc(1).

	throws(send_to_object_2_13, error(permission_error(access,private_predicate,s/4),logtalk(call(test_object(_)::s(_,_,_,_)),_))) :-
		send_to_object_test_object(_)::rtmc(s(_,_,_,_)).

	throws(send_to_object_2_14, error(existence_error(predicate_declaration,t/1),logtalk(call(test_object(_)::t(_)),_))) :-
		send_to_object_test_object(_)::rtmc(t(_)).

	throws(send_to_object_2_15, error(existence_error(predicate_declaration,atom/1),logtalk(call(test_object(_)::atom(a)),_))) :-
		send_to_object_test_object(_)::rtmc(atom(a)).

	succeeds(send_to_object_2_16) :-
		send_to_object_test_object(1)::rtmc(p(X)),
		X == 1.

	succeeds(send_to_object_2_17) :-
		{send_to_object_test_object(_)}::rtmc(p(X)),
		X == 2.

	succeeds(send_to_object_2_18) :-
		send_to_object_test_object(_)::rtmc({atom(a)}).

	succeeds(send_to_object_2_19) :-
		send_to_object_test_object(_)::rtmc(({atom(a)}, {number(1)})).

	fails(send_to_object_2_20) :-
		send_to_object_test_object(_)::rtmc(q(_,_)).

	% tests for compile-time bound messages

	throws(send_to_object_2_21, error(instantiation_error, logtalk(_::true,_))) :-
		% delay the error to runtime
		{_::true}.

	throws(send_to_object_2_22, error(type_error(object_identifier,1), logtalk(1::true,_))) :-
		% delay the error to runtime
		{1::true}.

	throws(send_to_object_2_23, error(instantiation_error, logtalk(logtalk::_,_))) :-
		% delay the error to runtime
		{logtalk::_}.

	throws(send_to_object_2_24, error(type_error(callable,1), logtalk(logtalk::1,_))) :-
		% delay the error to runtime
		{logtalk::1}.

	throws(send_to_object_2_25, error(existence_error(object,foo), logtalk(foo::true,_))) :-
		% delay the error to runtime
		{foo::true}.

	throws(send_to_object_2_26, error(existence_error(predicate_declaration,foo/0), logtalk(logtalk::foo,_))) :-
		% delay the error to runtime
		{logtalk::foo}.

	throws(send_to_object_2_27, error(existence_error(predicate_declaration,atom/1), logtalk(logtalk::atom(a),_))) :-
		% delay the error to runtime
		{logtalk::atom(a)}.

	throws(send_to_object_2_28, error(permission_error(access,protected_predicate,r/3), logtalk(test_object(_)::r(_,_,_),_))) :-
		% delay the error to runtime
		{test_object(_)::r(_,_,_)}.

	throws(send_to_object_2_29, error(permission_error(access, private_predicate,s/4), logtalk(test_object(_)::s(_,_,_,_),_))) :-
		% delay the error to runtime
		{test_object(_)::s(_,_,_,_)}.

	throws(send_to_object_2_30, error(instantiation_error, _)) :-
		% delay the error to runtime
		{{_}::true}.

	throws(send_to_object_2_31, error(type_error(callable,1), _)) :-
		% delay the error to runtime
		{{1}::true}.

	throws(send_to_object_2_32, error(existence_error(procedure,foo/0), _)) :-
		% delay the error to runtime
		{{foo}::true}.

	succeeds(send_to_object_2_33) :-
		test_object(1)::p(X),
		X == 1.

	succeeds(send_to_object_2_34) :-
		{test_object(_)}::p(X),
		X == 2.

	succeeds(send_to_object_2_35) :-
		test_object(_)::{atom(a)}.

	succeeds(send_to_object_2_36) :-
		test_object(_)::({atom(a)}, {number(1)}).

	fails(send_to_object_2_37) :-
		test_object(_)::q(_, _).

:- end_object.
