%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


% database for tests from the ISO/IEC 13211-1:1995(E) standard, section 8.9.1.4

:- dynamic(legs/2).
:- dynamic(foo/1).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:6:0,
		author is 'Paulo Moura',
		date is 2021-07-15,
		comment is 'Unit tests for the ISO Prolog standard asserta/1 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.9.1.4

	test(iso_asserta_1_01, true) :-
		{asserta(legs(octopus, 8))}.

	test(iso_asserta_1_02, true) :-
		{asserta((legs(A,4):-animal(A)))}.

	test(iso_asserta_1_03, true) :-
		{asserta((foo(X) :- X,call(X)))}.

	test(iso_asserta_1_04, error(instantiation_error)) :-
		{asserta(_)}.

	test(iso_asserta_1_05, error(type_error(callable,4))) :-
		% try to delay the error to runtime
		four(Four),
		{asserta(Four)}.

	test(iso_asserta_1_06, error(type_error(callable,4))) :-
		% try to delay the error to runtime
		four(Four),
		{asserta((foo :- Four))}.

	test(iso_asserta_1_07, errors([permission_error(modify,static_procedure,atom/1), permission_error(modify,static_procedure,':'(user,atom/1))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{asserta((atom(_) :- true))}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(eddbali_asserta_1_08, true(L == [bee-ant, bee-bee])) :-
		findall(X-Y, {asserta(insct(bee)),insct(X),asserta(insct(ant)),insct(Y)}, L).

	% tests from the Logtalk portability work

	test(lgt_asserta_1_09, error(instantiation_error)) :-
		{asserta((_ :- foo))}.

	test(lgt_asserta_1_10, error(instantiation_error)) :-
		{asserta((_ :- _))}.

	test(lgt_asserta_1_11, error(type_error(callable,4))) :-
		% try to delay the error to runtime
		four(Four),
		{asserta((Four :- foo))}.

	test(lgt_asserta_1_12, errors([type_error(callable,(fail,4)), type_error(callable,4)])) :-
		% the second exception term is throw by some of the Prolog compilers
		% try to delay the error to runtime
		four(Four),
		{asserta((foo :- fail,Four))}.

	% tests from the WG17 standardization work

	test(wg17_asserta_1_13, errors([permission_error(modify,static_procedure,true/0), permission_error(modify,static_procedure,':'(user,true/0))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{asserta((true :- true))}.

	test(wg17_asserta_1_14, errors([permission_error(modify,static_procedure,fail/0), permission_error(modify,static_procedure,':'(user,fail/0))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{asserta((fail :- true))}.

	test(wg17_asserta_1_15, errors([permission_error(modify,static_procedure,false/0), permission_error(modify,static_procedure,':'(user,false/0))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{asserta((false :- true))}.

	test(wg17_asserta_1_16, errors([permission_error(modify,static_procedure,call/1), permission_error(modify,static_procedure,':'(user,call/1))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{asserta((call(_) :- true))}.

	test(wg17_asserta_1_17, errors([permission_error(modify,static_procedure,!/0), permission_error(modify,static_procedure,':'(user,!/0))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{asserta((! :- true))}.

	test(wg17_asserta_1_18, errors([permission_error(modify,static_procedure,(',')/2), permission_error(modify,static_procedure,':'(user,(',')/2))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{asserta(((a,b) :- true))}.

	test(wg17_asserta_1_19, errors([permission_error(modify,static_procedure,(;)/2), permission_error(modify,static_procedure,':'(user,(;)/2))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{asserta(((a;b) :- true))}.

	test(wg17_asserta_1_20, errors([permission_error(modify,static_procedure,(->)/2), permission_error(modify,static_procedure,':'(user,(->)/2))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{asserta(((a->b) :- true))}.

	test(wg17_asserta_1_21, errors([permission_error(modify,static_procedure,catch/3), permission_error(modify,static_procedure,':'(user,catch/3))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{asserta((catch(_,_,_) :- true))}.

	test(wg17_asserta_1_22, errors([permission_error(modify,static_procedure,throw/1), permission_error(modify,static_procedure,':'(user,throw/1))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{asserta((throw(_) :- true))}.

	test(wg17_asserta_1_23, errors([permission_error(modify,static_procedure,(:-)/2), permission_error(modify,static_procedure,':'(user,(:-)/2))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{asserta(((a :- b) :- true))}.

	test(wg17_asserta_1_24, errors([permission_error(modify,static_procedure,(:-)/1), permission_error(modify,static_procedure,':'(user,(:-)/1))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{asserta(((:- b) :- true))}.

	% auxiliary predicates

	four(4).

:- end_object.
