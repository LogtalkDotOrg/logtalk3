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


% database for the tests

:- dynamic(v/1).

ndet(a).
ndet(b).
ndet(_) :- 1 =:= 0.

% this should undo the bindings of G and B before calling the
% cleanup handler.  I.e., S must be 1 and G and B must be var.

test_error_choice :-
	setup_call_cleanup(
		S=1,
		(G=2 ; G=3),
		asserta(v(x(S,G,B)))
	),
	B = 4,
	throw(x).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:2:1,
		author is 'Jan Wielemaker and Paulo Moura.',
		date is 2021-09-10,
		comment is 'Unit tests for the setup_call_cleanup/3 built-in predicate that is becoming a de facto standard.',
		source is 'Several tests adapted with permission from the SWI-Prolog distribution.'
	]).

	setup :-
		{retractall(v(_))}.

	test(swi_setup_call_cleanup_3_01, true(X == 42)) :-
		{setup_call_cleanup(A=42, true, asserta(v(A))), retract(v(X))}.

	test(swi_setup_call_cleanup_3_02, true(X == 42)) :-
		{setup_call_cleanup(A=42, (true;true), asserta(v(A))), !, retract(v(X))}.

	test(swi_setup_call_cleanup_3_03, true(X == 42)) :-
		{\+ setup_call_cleanup(A=42, fail, asserta(v(A))), retract(v(X))}.

	test(swi_setup_call_cleanup_3_04, variant(X, [42,_])) :-
		{\+ setup_call_cleanup(A=42, (B=2,fail), assertz(v([A,B]))), retract(v(X))}.

	test(swi_setup_call_cleanup_3_05, true([X,E] == [42,error(x)])) :-
		{catch(setup_call_cleanup(A=42, throw(error(x)), assertz(v(A))), E, true), retract(v(X))}.

	test(swi_setup_call_cleanup_3_06, true(Vs == [a,b,done])) :-
		{	setup_call_cleanup(true, (ndet(X), assertz(v(X))), assertz(v(done))),
			fail
		;	findall(V, retract(v(V)), Vs)
		},
		{retractall(v(_))}.

	test(swi_setup_call_cleanup_3_07, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(X),
		{setup_call_cleanup(true, true, X)}.

	test(swi_setup_call_cleanup_3_08, true) :-
		{setup_call_cleanup(X=true, true, X)}.

	test(swi_setup_call_cleanup_3_09, ball(first)) :-
		{setup_call_cleanup(true, (G=1;G=2), throw(second)), throw(first)}.

	test(swi_setup_call_cleanup_3_10, ball(a(first))) :-
		{setup_call_cleanup(true, (G=1;G=2), throw(a(second))), throw(a(first))}.

	test(swi_setup_call_cleanup_3_11, variant(E+Xs, x+[x(1,_,_)])) :-
		{catch(test_error_choice, E, true), findall(X, retract(v(X)), Xs)}.

	% tests from the Logtalk portability work

	test(lgt_setup_call_cleanup_3_12, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(X),
		{setup_call_cleanup(X, true, true)}.

	test(lgt_setup_call_cleanup_3_13, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(X),
		{setup_call_cleanup(true, X, true)}.

	test(lgt_setup_call_cleanup_3_14, error(type_error(callable,1))) :-
		% try to delay the error to runtime
		one(One),
		{setup_call_cleanup(One, true, true)}.

	test(lgt_setup_call_cleanup_3_15, error(type_error(callable,1))) :-
		% try to delay the error to runtime
		one(One),
		{setup_call_cleanup(true, One, true)}.

	test(lgt_setup_call_cleanup_3_16, error(type_error(callable,1))) :-
		% try to delay the error to runtime
		one(One),
		{setup_call_cleanup(true, true, One)}.

	test(lgt_setup_call_cleanup_3_17, false) :-
		{setup_call_cleanup(fail, true, true)}.

	test(lgt_setup_call_cleanup_3_18, true(Assertion)) :-
		^^set_text_output(''),
		({setup_call_cleanup(fail, write(hello), write(there))} -> true; true),
		^^text_output_assertion('', Assertion).

	test(lgt_setup_call_cleanup_3_19, true) :-
		{setup_call_cleanup(true, true, fail)}.

	test(lgt_setup_call_cleanup_3_20, true(X-Y == 1-3)) :-
		{setup_call_cleanup(true, setup_call_cleanup(true, (X=1;X=2), true), Y=3), !}.

	% auxiliary predicate used to delay errors to runtime

	variable(_).

	one(1).

:- end_object.
