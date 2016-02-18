%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/05/05,
		comment is 'Unit tests for the ISO Prolog standard !/0 control construct.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 7.8.4.4

	succeeds(iso_cut_0_01) :-
		{!}.

	fails(iso_cut_0_02) :-
		{(!,fail;true)}.

	succeeds(iso_cut_0_03) :-
		{(call(!),fail;true)}.

	fails(iso_cut_0_04) :-
		{(twice(_), !, write('Forwards '), fail)}.

	fails(iso_cut_0_05) :-
		{((!; write('No ')), write('Cut disjunction '), fail)}.

	fails(iso_cut_0_06) :-
		{(twice(_), (write('No '); !), write('Cut '), fail)}.

	fails(iso_cut_0_07) :-
		{(twice(_), (!, fail; write('No ')))}.

	fails(iso_cut_0_08) :-
		{(twice(X), call(X), write('Forwards '), fail)}.

	fails(iso_cut_0_09) :-
		{(goal(X), call(X), write('Forwards '), fail)}.

	fails(iso_cut_0_10) :-
		{(twice(_), \+(\+(!)), write('Forwards '), fail)}.

	fails(iso_cut_0_11) :-
		{(twice(_), once(!), write('Forwards '), fail)}.

	fails(iso_cut_0_12) :-
		{(twice(_), call(!), write('Forwards '), fail)}.

	% tests from the ECLiPSe test suite

	succeeds(eclipse_cut_0_13) :-
		findall(X, {(X=1;X=2), !}, L),
		L == [1].

	succeeds(eclipse_cut_0_14) :-
		findall(X, {(!,X=1;X=2)}, L),
		L == [1].

	succeeds(eclipse_cut_0_15) :-
		findall(X, {(X=1;X=2), (true;!)}, L),
		L == [1, 1].

	fails(eclipse_cut_0_16) :-
		{(X=1;X=2), (!,fail;true)}.

	succeeds(eclipse_cut_0_17) :-
		findall(X, {(X=!;X=true), call(X)}, L),
		L == [!, true].

	succeeds(eclipse_cut_0_18) :-
		findall(X, {(G=((X=1;X=2),!);G=(X=3)), call(G)}, L),
		L == [1, 3].

	succeeds(eclipse_cut_0_19) :-
		findall(X, {(X=1;X=2), \+(\+(!))}, L),
		L == [1, 2].

	succeeds(eclipse_cut_0_20) :-
		findall(X, {(X=1;X=2), once(!)}, L),
		L == [1, 2].

	succeeds(eclipse_cut_0_21) :-
		findall(X, {(X=1;X=2), call(!)}, L),
		L == [1, 2].

:- end_object.
