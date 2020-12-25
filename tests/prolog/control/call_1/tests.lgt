%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


% database for tests from the ISO/IEC 13211-1:1995(E) standard, section 7.8.3.4

b(X) :-
	Y = (write(X), X),
	call(Y).

a(1).
a(2).


% calls to declared predicates with no clauses must fail

:- dynamic(unicorn/0).

:- multifile(fenix/1).

:- discontiguous(scattered/2).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2020-11-18,
		comment is 'Unit tests for the ISO Prolog standard call/1 control construct.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 7.8.3.4

	succeeds(iso_call_1_01) :-
		{call(!)}.

	fails(iso_call_1_02) :-
		{call(fail)}.

	fails(iso_call_1_03) :-
		{call((fail, _X))}.

	fails(iso_call_1_04) :-
		X = 1,
		{call((fail, call(X)))}.

	throws(iso_call_1_05, error(instantiation_error,_)) :-
		^^suppress_text_output,
		{b(_)}.

	throws(iso_call_1_06, [error(type_error(callable,(write(3),3)),_), error(type_error(callable,3),_)]) :-
		% the second exception term is a common but not strictly conforming alternative
		^^suppress_text_output,
		{b(3)}.

	succeeds(iso_call_1_07) :-
		{(Z = !, call((Z=!, a(X), Z)))},
		Z == !, X == 1.

	succeeds(iso_call_1_08) :-
		findall(Z-X, {call((Z=!, a(X), Z))}, L),
		L == [ !-1, !-2 ].

	throws(iso_call_1_09, error(instantiation_error,_)) :-
		^^suppress_text_output,
		{call((write(3), _X))}.

	throws(iso_call_1_10, error(type_error(callable,1),_)) :-
		^^suppress_text_output,
		X = 1,
		{call((write(3), call(X)))}.

	throws(iso_call_1_11, error(instantiation_error,_)) :-
		{call(_X)}.

	throws(iso_call_1_12, error(type_error(callable,1),_)) :-
		X = 1,
		{call(X)}.

	throws(iso_call_1_13, [error(type_error(callable,(fail,1)),_), error(type_error(callable,1),_)]) :-
		% the second exception term is a common but not strictly conforming alternative
		X = 1,
		{call((fail, X))}.

	throws(iso_call_1_14, [error(type_error(callable,(write(3),1)),_), error(type_error(callable,1),_)]) :-
		% the second exception term is a common but not strictly conforming alternative
		^^suppress_text_output,
		X = 1,
		{call((write(3), X))}.

	throws(iso_call_1_15, [error(type_error(callable,(1;true)),_), error(type_error(callable,1),_)]) :-
		% the second exception term is a common but not strictly conforming alternative
		X = 1,
		{call((X; true))}.

	% tests from the Logtalk portability work

	fails(lgt_call_1_16) :-
		{call(unicorn)}.

	fails(lgt_call_1_17) :-
		{call(fenix(_))}.

	fails(lgt_call_1_18) :-
		{call(scattered(_, _))}.

:- end_object.
