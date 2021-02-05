%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- set_logtalk_flag(undefined_predicates, silent).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2018-02-08,
		comment is 'Unit tests for the call/1-N built-in method.'
	]).

	throws(call_N_1, error(instantiation_error,logtalk(call(_),_))) :-
		call(_).

	throws(call_N_2, error(instantiation_error,logtalk(call(_,_),_))) :-
		call(_, _).

	throws(call_N_3, error(instantiation_error,logtalk(call(_,_,_),_))) :-
		call(_, _, _).

	throws(call_N_4, error(type_error(callable,1),logtalk(call(1),_))) :-
		Goal = 1,
		call(Goal).

	throws(call_N_5, error(type_error(callable,1),logtalk(call(1,_),_))) :-
		Closure = 1,
		call(Closure, _).

	throws(call_N_6, error(type_error(callable,1),logtalk(call(1,_,_),_))) :-
		Closure = 1,
		call(Closure, _, _).

	% it's not always possible to decompile the actual call

	throws(call_N_7, error(existence_error(procedure,_),logtalk(call(foo(_,_)),_))) :-
		Goal = foo(_,_),
		call(Goal).

	throws(call_N_8, error(existence_error(procedure,_),_)) :-
		Closure = foo(_),
		call(Closure, _).

	throws(call_N_9, error(existence_error(procedure,_),_)) :-
		Closure = foo,
		call(Closure, _, _).

	succeeds(call_N_10) :-
		call(a(X)),
		X == 1.

	succeeds(call_N_11) :-
		call(a, X),
		X == 1.

	succeeds(call_N_12) :-
		call(b(X, Y)),
		X == 1, Y == one.

	succeeds(call_N_13) :-
		call(b(X), Y),
		X == 1, Y == one.

	succeeds(call_N_14) :-
		call(b, X, Y),
		X == 1, Y == one.

	succeeds(call_N_15) :-
		call(c(X, Y, Z)),
		X == 1, Y == one, Z == 'ONE'.

	succeeds(call_N_16) :-
		call(c(X,Y), Z),
		X == 1, Y == one, Z == 'ONE'.

	succeeds(call_N_17) :-
		call(c(X), Y, Z),
		X == 1, Y == one, Z == 'ONE'.

	succeeds(call_N_18) :-
		call(c, X, Y, Z),
		X == 1, Y == one, Z == 'ONE'.

	fails(call_N_19) :-
		Goal = d(_,_,_,_),
		call(Goal).

	% calls to declared but undefined predicates must
	% fail instead of throwing an existence error

	fails(call_N_20) :-
		call(d(_,_,_),_).

	fails(call_N_21) :-
		call(d(_,_),_,_).

	fails(call_N_22) :-
		call(d(_),_,_,_).

	fails(call_N_23) :-
		Goal = e(_,_,_,_,_),
		call(Goal).

	fails(call_N_24) :-
		call(e(_,_,_,_),_).

	fails(call_N_25) :-
		call(e(_,_,_),_,_).

	fails(call_N_26) :-
		call(e(_,_),_,_,_).

	fails(call_N_27) :-
		call(e(_),_,_,_,_).

	% call/1-N is opaque to cuts

	succeeds(call_N_28) :-
		findall(X, ((X = 1; X =2; X = 3), call(!)), L),
		L == [1, 2, 3].

	succeeds(call_N_29) :-
		findall(X, ((X = 1; X =2; X = 3), call((true,!))), L),
		L == [1, 2, 3].

	succeeds(call_N_30) :-
		findall(X, ((X = 1; X =2; X = 3), call((true;!))), L),
		L == [1, 1, 2, 2, 3, 3].

	succeeds(call_N_31) :-
		findall(X, ((X = 1; X =2; X = 3), call(','(true), !)), L),
		L == [1, 2, 3].

	succeeds(call_N_32) :-
		findall(X, ((X = 1; X =2; X = 3), call(';'(true), !)), L),
		L == [1, 1, 2, 2, 3, 3].

	% some data for the tests

	:- private(d/4).

	:- private(e/5).
	:- dynamic(e/5).

	a(1).
	b(1, one).
	c(1, one, 'ONE').

:- end_object.
