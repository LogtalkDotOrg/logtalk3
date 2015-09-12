%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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


% database for tests from the ISO/IEC 13211-1:1995(E) standard, section 8.9.2.4

:- dynamic(legs/2).
:- dynamic(foo/1).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.3,
		author is 'Paulo Moura',
		date is 2015/05/05,
		comment is 'Unit tests for the ISO Prolog standard assertz/1 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.9.2.4

	succeeds(iso_assertz_1_01) :-
		{assertz(legs(spider, 8))}.

	succeeds(iso_assertz_1_02) :-
		{assertz((legs(B, 2):-bird(B)))}.

	succeeds(iso_assertz_1_03) :-
		{assertz((foo(X):- X -> call(X)))}.

	throws(iso_assertz_1_04, error(instantiation_error,_)) :-
		{assertz(_)}.

	throws(iso_assertz_1_05, error(type_error(callable,4),_)) :-
		{assertz(4)}.

	throws(iso_assertz_1_06, error(type_error(callable,4),_)) :-
		{assertz((foo :- 4))}.

	throws(iso_assertz_1_07, [error(permission_error(modify,static_procedure,atom/1),_), error(permission_error(modify,static_procedure,':'(user,atom/1)),_)]) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{assertz((atom(_) :- true))}.

	% tests from the Logtalk portability work

	throws(lgt_assertz_1_08, error(instantiation_error,_)) :-
		{assertz((_ :- foo))}.

	throws(lgt_assertz_1_09, error(instantiation_error,_)) :-
		{assertz((_ :- _))}.

	throws(lgt_assertz_1_10, error(type_error(callable,4),_)) :-
		{assertz((4 :- foo))}.

	throws(lgt_assertz_1_11, error(type_error(callable,(fail,4)),_)) :-
		{assertz((foo :- fail,4))}.

:- end_object.
