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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/04/26,
		comment is 'Unit tests for the ISO Prolog standard functor/3 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.5.1.4

	succeeds(iso_functor_3_01) :-
		{functor(foo(a,b,c), foo, 3)}.

	succeeds(iso_functor_3_02) :-
		{functor(foo(a,b,c), X, Y)},
		X == foo, Y == 3.

	succeeds(iso_functor_3_03) :-
		{functor(X, foo, 3)},
		X = foo(_A,_B,_C).

	succeeds(iso_functor_3_04) :-
		{functor(X, foo, 0)},
		X == foo.

	succeeds(iso_functor_3_05) :-
		{functor(mats(A,B), A, B)},
		A == mats, B == 2.

	fails(iso_functor_3_06) :-
		{functor(foo(a), foo, 2)}.

	fails(iso_functor_3_07) :-
		{functor(foo(a), fo, 1)}.

	succeeds(iso_functor_3_08) :-
		{functor(1, X, Y)},
		X == 1,Y == 0.

	succeeds(iso_functor_3_09) :-
		{functor(X, 1.1, 0)},
		X == 1.1.

	succeeds(iso_functor_3_10) :-
		{functor([_|_], '.', 2)}.

	succeeds(iso_functor_3_11) :-
		{functor([], [], 0)}.

	% in the tests that follow, try to delay the expected error to runtime

	throws(iso_functor_3_12, error(instantiation_error,_)) :-
		{G = functor(_X, _Y, 3), call(G)}.

	throws(iso_functor_3_13, error(instantiation_error,_)) :-
		{G = functor(_X, foo, _N), call(G)}.

	throws(iso_functor_3_14, error(type_error(integer,a),_)) :-
		{G = functor(_X, foo, a), call(G)}.

	throws(iso_functor_3_15, error(type_error(atom,1.5),_)) :-
		{G = functor(_F, 1.5, 1), call(G)}.

	throws(iso_functor_3_16, error(type_error(atomic,foo(a)),_)) :-
		{G = functor(_F, foo(a), 1), call(G)}.

	:- if(current_prolog_flag(max_arity, unbounded)).
		succeeds(iso_functor_3_17) :-
			true.
	:- else.
		throws(iso_functor_3_17, error(representation_error(max_arity),_)) :-
			{current_prolog_flag(max_arity, A), X is A+1, functor(_T, foo, X)}.
	:- endif.

	throws(iso_functor_3_18, error(domain_error(not_less_than_zero,-1),_)) :-
		{G = functor(_T, foo, -1), call(G)}.

	% tests from the Logtalk portability work

	succeeds(lgt_functor_3_19) :-
		{functor(T, [], 0)},
		T == [].

:- end_object.
