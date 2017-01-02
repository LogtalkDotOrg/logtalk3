%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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
		date is 2014/11/21,
		comment is 'Unit tests for the ISO Prolog standard (=..)/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.5.3.4

	succeeds(iso_univ_2_01) :-
		{'=..'(foo(a,b), [foo,a,b])}.

	succeeds(iso_univ_2_02) :-
		{'=..'(X, [foo,a,b])},
		X == foo(a,b).

	succeeds(iso_univ_2_03) :-
		{'=..'(foo(a,b), L)},
		L == [foo,a,b].

	succeeds(iso_univ_2_04) :-
		{'=..'(foo(X,b), [foo,a,Y])},
		X == a,Y == b.

	succeeds(iso_univ_2_05) :-
		{'=..'(1, [1])}.

	fails(iso_univ_2_06) :-
		{'=..'(foo(a,b), [foo,b,a])}.

	throws(iso_univ_2_07, error(instantiation_error,_)) :-
		{'=..'(_X, _Y)}.

	throws(iso_univ_2_08, error(instantiation_error,_)) :-
		{'=..'(_X, [foo,a|_Y])}.

	throws(iso_univ_2_09, error(type_error(list,[foo|bar]),_)) :-
		{'=..'(_X, [foo|bar])}.

	throws(iso_univ_2_10, error(instantiation_error,_)) :-
		{'=..'(_X, [_Foo,bar])}.

	throws(iso_univ_2_11, error(type_error(atom,3),_)) :-
		{'=..'(_X, [3,1])}.

	throws(iso_univ_2_12, error(type_error(atom,1.1),_)) :-
		{'=..'(_X, [1.1,foo])}.

	throws(iso_univ_2_13, error(type_error(atom,a(b)),_)) :-
		{'=..'(_X, [a(b),1])}.

	throws(iso_univ_2_14, error(type_error(list,4),_)) :-
		{'=..'(_X, 4)}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).
		succeeds(iso_univ_2_15) :-
			{'=..'(f(X), [f,u(X)])}.
	:- else.
		- succeeds(iso_univ_2_15) :-
			% STO; Undefined
			{'=..'(f(X), [f,u(X)])}.
	:- endif.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_univ_2_16, error(type_error(atomic,f(a)),_)) :-
		{'=..'(_X, [f(a)])}.

	throws(sics_univ_2_17, error(domain_error(non_empty_list,[]),_)) :-
		{'=..'(_X, [])}.

	:- if(current_prolog_flag(max_arity, unbounded)).
		succeeds(sics_univ_2_18) :-
			true.
	:- else.
		throws(sics_univ_2_18, error(representation_error(max_arity),_)) :-
			{current_prolog_flag(max_arity, Max)},
			N is Max+1, list_of(N, 1, L),
			{'=..'(_X, [f|L])}.

		list_of(0, _, []).
		list_of(N, A, [A|L]) :-
			N > 0, N1 is N-1,
			list_of(N1, A, L).
	:- endif.

:- end_object.
