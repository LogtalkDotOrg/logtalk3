%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
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
		date is 2014/10/14,
		comment is 'Unit tests for the ISO Prolog standard unify_with_occurs_check/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.2.2.4

	succeeds(iso_unify_with_occurs_check_2_01) :-
		{unify_with_occurs_check(1, 1)}.

	succeeds(iso_unify_with_occurs_check_2_02) :-
		{unify_with_occurs_check(X, 1)},
		X == 1.

	succeeds(iso_unify_with_occurs_check_2_03) :-
		{unify_with_occurs_check(X, Y)},
		X == Y.

	succeeds(iso_unify_with_occurs_check_2_04) :-
		{unify_with_occurs_check(_, _)}.

	succeeds(iso_unify_with_occurs_check_2_05) :-
		{unify_with_occurs_check(X, Y), unify_with_occurs_check(X, abc)},
		X == abc, Y == abc.

	succeeds(iso_unify_with_occurs_check_2_06) :-
		{unify_with_occurs_check(f(X,def), f(def,Y))},
		X == def, Y == def.

	fails(iso_unify_with_occurs_check_2_07) :-
		{unify_with_occurs_check(1, 2)}.

	fails(iso_unify_with_occurs_check_2_08) :-
		{unify_with_occurs_check(1, 1.0)}.

	fails(iso_unify_with_occurs_check_2_09) :-
		{unify_with_occurs_check(g(X), f(f(X)))}.

	fails(iso_unify_with_occurs_check_2_10) :-
		{unify_with_occurs_check(f(X,1), f(a(X)))}.

	fails(iso_unify_with_occurs_check_2_11) :-
		{unify_with_occurs_check(f(X,Y,X), f(a(X),a(Y),Y,2))}.

	fails(iso_unify_with_occurs_check_2_12) :-
		{unify_with_occurs_check(X, a(X))}.

	fails(iso_unify_with_occurs_check_2_13) :-
		{unify_with_occurs_check(f(X,1), f(a(X),2))}.

	fails(iso_unify_with_occurs_check_2_14) :-
		{unify_with_occurs_check(f(1,X,1), f(2,a(X),2))}.

	fails(iso_unify_with_occurs_check_2_15) :-
		{unify_with_occurs_check(f(1,X), f(2,a(X)))}.

	fails(iso_unify_with_occurs_check_2_16) :-
		{unify_with_occurs_check(f(X,Y,X,1), f(a(X),a(Y),Y,2))}.

:- end_object.
