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


% database for tests from the ISO/IEC 13211-1:1995(E) standard, section 7.8.4.4

twice(!) :- write('C ').
twice(true) :- write('Moss ').

goal((twice(_), !)).
goal(write('Three ')).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2020-08-03,
		comment is 'Unit tests for the ISO Prolog standard !/0 control construct.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 7.8.4.4

	test(iso_cut_0_01, true) :-
		{!}.

	test(iso_cut_0_02, fail) :-
		{(!,fail;true)}.

	test(iso_cut_0_03, true) :-
		{(call(!),fail;true)}.

	test(iso_cut_0_04, true(Assertion)) :-
		^^set_text_output(''),
		\+ {(twice(_), !, write('Forwards '), fail)},
		^^text_output_assertion('C Forwards ', Assertion).

	test(iso_cut_0_05, true(Assertion)) :-
		^^set_text_output(''),
		\+ {((!; write('No ')), write('Cut disjunction '), fail)},
		^^text_output_assertion('Cut disjunction ', Assertion).

	test(iso_cut_0_06, true(Assertion)) :-
		^^set_text_output(''),
		\+ {(twice(_), (write('No '); !), write('Cut '), fail)},
		^^text_output_assertion('C No Cut Cut ', Assertion).

	test(iso_cut_0_07, true(Assertion)) :-
		^^set_text_output(''),
		\+ {(twice(_), (!, fail; write('No ')))},
		^^text_output_assertion('C ', Assertion).

	test(iso_cut_0_08, true(Assertion)) :-
		^^set_text_output(''),
		\+ {(twice(X), call(X), write('Forwards '), fail)},
		^^text_output_assertion('C Forwards Moss Forwards ', Assertion).

	test(iso_cut_0_09, true(Assertion)) :-
		^^set_text_output(''),
		\+ {(goal(X), call(X), write('Forwards '), fail)},
		^^text_output_assertion('C Forwards Three Forwards ', Assertion).

	test(iso_cut_0_10, true(Assertion)) :-
		^^set_text_output(''),
		\+ {(twice(_), \+(\+(!)), write('Forwards '), fail)},
		^^text_output_assertion('C Forwards Moss Forwards ', Assertion).

	test(iso_cut_0_11, true(Assertion)) :-
		^^set_text_output(''),
		\+ {(twice(_), once(!), write('Forwards '), fail)},
		^^text_output_assertion('C Forwards Moss Forwards ', Assertion).

	test(iso_cut_0_12, true(Assertion)) :-
		^^set_text_output(''),
		\+ {(twice(_), call(!), write('Forwards '), fail)},
		^^text_output_assertion('C Forwards Moss Forwards ', Assertion).

	% tests from the ECLiPSe test suite

	test(eclipse_cut_0_13, true(L == [1])) :-
		findall(X, {(X=1;X=2), !}, L).

	test(eclipse_cut_0_14, true(L == [1])) :-
		findall(X, {(!,X=1;X=2)}, L).

	test(eclipse_cut_0_15, true(L == [1, 1])) :-
		findall(X, {(X=1;X=2), (true;!)}, L).

	test(eclipse_cut_0_16, fail) :-
		{(X=1;X=2), (!,fail;true)}.

	test(eclipse_cut_0_17, true(L == [!, true])) :-
		findall(X, {(X=!;X=true), call(X)}, L).

	test(eclipse_cut_0_18, true(L == [1, 3])) :-
		findall(X, {(G = ((X=1; X=2), !); G = (X=3)), call(G)}, L).

	test(eclipse_cut_0_19, true(L == [1, 2])) :-
		findall(X, {(X=1;X=2), \+(\+(!))}, L).

	test(eclipse_cut_0_20, true(L == [1, 2])) :-
		findall(X, {(X=1;X=2), once(!)}, L).

	test(eclipse_cut_0_21, true(L == [1, 2])) :-
		findall(X, {(X=1;X=2), call(!)}, L).

:- end_object.
