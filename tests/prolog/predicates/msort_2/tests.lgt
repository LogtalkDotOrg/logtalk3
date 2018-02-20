%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2018/02/20,
		comment is 'Unit tests for the de facto Prolog standard msort/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the Logtalk portability work

	succeeds(lgt_msort_2_01) :-
		{msort([1, 1], Sorted)},
		Sorted == [1, 1].

	succeeds(lgt_msort_2_02) :-
		{msort([3, 2, 1, 1, 2, 3], Sorted)},
		Sorted == [1, 1, 2, 2, 3, 3].

	succeeds(lgt_msort_2_03) :-
		{msort([1+Y, z, a, V, 1, 2, V, 1, 7.0, 8.0, 1+Y, 1+2, 8.0, -a, -X, a], Sorted)},
		Sorted == [V, V, 7.0, 8.0, 8.0, 1, 1, 2, a, a, z, -X, -a, 1+Y, 1+Y, 1+2].

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).
		succeeds(lgt_msort_2_04) :-
			{msort([V], V)}.
	:- else.
		- succeeds(lgt_msort_2_04) :-
			% STO; Undefined.
			{msort([V], V)}.
	:- endif.

	succeeds(lgt_msort_2_05) :-
		{msort([f(U),U,U,f(V),f(U),V],L)},
		(	L == [U,U,V,f(U),f(U),f(V)] ->
			true
		;	L == [V,U,U,f(V),f(U),f(U)]
		).

	succeeds(lgt_msort_2_06) :-
		{msort([[e],[c,d],[a,b]], Sorted)},
		Sorted == [[a,b],[c,d],[e]].

	throws(lgt_msort_2_07, error(instantiation_error,_)) :-
		{msort(_, _)}.

	throws(lgt_msort_2_08, error(instantiation_error,_)) :-
		{msort([a|_],_)}.

	throws(lgt_msort_2_09, error(type_error(list,3),_)) :-
		{msort(3, _)}.

	throws(lgt_msort_2_10, error(type_error(list,[a|b]),_)) :-
		{msort([a|b],_)}.

	throws(lgt_msort_2_11, error(type_error(list,3),_)) :-
		{msort([], 3)}.

:- end_object.
