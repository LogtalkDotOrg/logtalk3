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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2015/06/01,
		comment is 'Unit tests for the ISO Prolog standard sort/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.4.3.4

	succeeds(iso_sort_2_01) :-
		{sort([1, 1], Sorted)},
		Sorted == [1].

	succeeds(iso_sort_2_02) :-
		{sort([1+Y, z, a, V, 1, 2, V, 1, 7.0, 8.0, 1+Y, 1+2, 8.0, -a, -X, a], Sorted)},
		Sorted == [V, 7.0, 8.0, 1, 2, a, z, -X, -a, 1+Y, 1+2].

	fails(iso_sort_2_03) :-
		{sort([1, 1], [1, 1])}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).
		succeeds(iso_sort_2_04) :-
			{sort([V], V)}.
	:- else.
		- succeeds(iso_sort_2_04) :-
			% STO; Undefined.
			{sort([V], V)}.
	:- endif.

	succeeds(iso_sort_2_05) :-
		{sort([f(U),U,U,f(V),f(U),V],L)},
		(	L == [U,V,f(U),f(V)] ->
			true
		;	L == [V,U,f(V),f(U)]
		).

	% tests from the ECLiPSe test suite

	throws(eclipse_sort_2_06, error(instantiation_error,_)) :-
		{sort(_, _)}.

	throws(eclipse_sort_2_07, error(instantiation_error,_)) :-
		{sort([a|_],_)}.

	throws(eclipse_sort_2_08, error(type_error(list,3),_)) :-
		{sort(3, _)}.

	throws(eclipse_sort_2_09, error(type_error(list,[a|b]),_)) :-
		{sort([a|b],_)}.

	throws(eclipse_sort_2_10, error(type_error(list,3),_)) :-
		{sort([], 3)}.

	throws(eclipse_sort_2_11, error(type_error(list,[a|b]),_)) :-
		{sort([], [a|b])}.

:- end_object.
