%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


% database for tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.15.4.4

call_n_maplist(_Cont, []).
call_n_maplist(Cont, [E|Es]) :-
    call(Cont, E),
    call_n_maplist(Cont, Es).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:6:0,
		author is 'Paulo Moura',
		date is 2021-09-23,
		comment is 'Unit tests for the ISO Prolog standard call/N built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.15.4.4

	test(iso_call_N_01, true) :-
		{call(integer, 3)}.

	test(iso_call_N_02, true(F == c)) :-
		{call(functor(F,c), 0)}.

	test(iso_call_N_03, true(Atom == prolog)) :-
		{call(call(call(atom_concat, pro), log), Atom)}.

	test(iso_call_N_04, variant(L, [1-_, _-2])) :-
		findall(X-Y, {call(;, X=1, Y=2)}, L).

	test(iso_call_N_05, false) :-
		{call(;, (true->fail), _X=1)}.

	test(iso_call_N_06, true) :-
		{call_n_maplist(>(3), [1, 2])}.

	test(iso_call_N_07, false) :-
		{call_n_maplist(>(3), [1, 2, 3])}.

	test(iso_call_N_08, true(Xs == [])) :-
		{call_n_maplist(=(_X), Xs)}.

	% tests from the Logtalk portability work

	test(lgt_call_N_09, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(X),
		{call(X, _)}.

	test(lgt_call_N_10, error(type_error(callable,3))) :-
		% try to delay the error to runtime
		three(Three),
		{call(Three, _)}.

	test(lgt_call_N_11, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(X),
		{call(X, _, _)}.

	test(lgt_call_N_12, error(type_error(callable,3))) :-
		% try to delay the error to runtime
		three(Three),
		{call(Three, _, _)}.

	test(lgt_call_N_13, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(X),
		{call(X, _, _, _)}.

	test(lgt_call_N_14, error(type_error(callable,3))) :-
		% try to delay the error to runtime
		three(Three),
		{call(Three, _, _, _)}.

	test(lgt_call_N_15, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(X),
		{call(X, _, _, _, _)}.

	test(lgt_call_N_16, error(type_error(callable,3))) :-
		% try to delay the error to runtime
		three(Three),
		{call(Three, _, _, _, _)}.

	test(lgt_call_N_17, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(X),
		{call(X, _, _, _, _, _)}.

	test(lgt_call_N_18, error(type_error(callable,3))) :-
		% try to delay the error to runtime
		three(Three),
		{call(Three, _, _, _, _, _)}.

	test(lgt_call_N_19, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(X),
		{call(X, _, _, _, _, _, _)}.

	test(lgt_call_N_20, error(type_error(callable,3))) :-
		% try to delay the error to runtime
		three(Three),
		{call(Three, _, _, _, _, _, _)}.

	test(lgt_call_N_21, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(X),
		{call(X, _, _, _, _, _, _, _)}.

	test(lgt_call_N_22, error(type_error(callable,3))) :-
		% try to delay the error to runtime
		three(Three),
		{call(Three, _, _, _, _, _, _, _)}.

	% tests from the ECLiPSe test suite

	test(eclipse_call_N_23, errors([
			type_error(callable,(fail,3)), type_error(callable,3),
			type_error(callable,(':'(user,fail),':'(user,3))), type_error(callable,':'(user,3))
	])) :-
		% the first exception term is the strictly conforming one
		% try to avoid a compile time error with some backends
		three(Three),
		{call(',', fail, Three)}.

	test(eclipse_call_N_24, errors([
			type_error(callable,(!;3)), type_error(callable,3),
			type_error(callable,(':'(user,!);':'(user,3))), type_error(callable,':'(user,3))
	])) :-
		% the first exception term is the strictly conforming one
		% try to avoid a compile time error with some backends
		three(Three),
		{call(';', !, Three)}.

	test(eclipse_call_N_25, errors([
			type_error(callable,(fail->3)), type_error(callable,3),
			type_error(callable,(':'(user,fail)->':'(user,3))), type_error(callable,':'(user,3))
	])) :-
		% the first exception term is the strictly conforming one
		% try to avoid a compile time error with some backends
		three(Three),
		{call('->', fail, Three)}.

	test(eclipse_call_N_26, true(L == [1, 2])) :-
		findall(X, {call(',', C=!, (X=1,C;X=2))}, L).

	test(eclipse_call_N_27, errors([
			type_error(callable,(fail,3)), type_error(callable,3),
			type_error(callable,(':'(user,fail),':'(user,3))), type_error(callable,':'(user,3))
	])) :-
		% the first exception term is the strictly conforming one
		% try to avoid a compile time error with some backends
		three(Three),
		{call(','(fail), Three)}.

	test(eclipse_call_N_28, errors([
			type_error(callable,(!;3)), type_error(callable,3),
			type_error(callable,(':'(user,!);':'(user,3))), type_error(callable,':'(user,3))
	])) :-
		% the first exception term is the strictly conforming one
		% try to avoid a compile time error with some backends
		three(Three),
		{call(';'(!), Three)}.

	test(eclipse_call_N_29, errors([
			type_error(callable,(fail->3)), type_error(callable,3),
			type_error(callable,(':'(user,fail)->':'(user,3))), type_error(callable,':'(user,3))
	])) :-
		% the first exception term is the strictly conforming one
		% try to avoid a compile time error with some backends
		three(Three),
		{call('->'(fail), Three)}.

	test(eclipse_call_N_30, true(L == [1, 2])) :-
		findall(X, {call(','(C=!), (X=1,C;X=2))}, L).

	% auxiliary predicates used to delay errors to runtime

	three(3).

	variable(_).

:- end_object.
