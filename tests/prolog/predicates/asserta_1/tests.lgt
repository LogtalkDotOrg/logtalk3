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


% database for tests from the ISO/IEC 13211-1:1995(E) standard, section 8.9.1.4

:- dynamic(legs/2).
:- dynamic(foo/1).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.3,
		author is 'Paulo Moura',
		date is 2015/05/05,
		comment is 'Unit tests for the ISO Prolog standard asserta/1 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.9.1.4

	succeeds(iso_asserta_1_01) :-
		{asserta(legs(octopus, 8))}.

	succeeds(iso_asserta_1_02) :-
		{asserta((legs(A,4):-animal(A)))}.

	succeeds(iso_asserta_1_03) :-
		{asserta((foo(X) :- X,call(X)))}.

	throws(iso_asserta_1_04, error(instantiation_error,_)) :-
		{asserta(_)}.

	throws(iso_asserta_1_05, error(type_error(callable,4),_)) :-
		{asserta(4)}.

	throws(iso_asserta_1_06, error(type_error(callable,4),_)) :-
		{asserta((foo :- 4))}.

	throws(iso_asserta_1_07, [error(permission_error(modify,static_procedure,atom/1),_), error(permission_error(modify,static_procedure,':'(user,atom/1)),_)]) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{asserta((atom(_) :- true))}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	succeeds(eddbali_asserta_1_08) :-
		findall(X-Y, {asserta(insct(bee)),insct(X),asserta(insct(ant)),insct(Y)}, L),
		L == [bee-ant, bee-bee].

	% tests from the Logtalk portability work

	throws(lgt_asserta_1_09, error(instantiation_error,_)) :-
		{asserta((_ :- foo))}.

	throws(lgt_asserta_1_10, error(instantiation_error,_)) :-
		{asserta((_ :- _))}.

	throws(lgt_asserta_1_11, error(type_error(callable,4),_)) :-
		{asserta((4 :- foo))}.

	throws(lgt_asserta_1_12, error(type_error(callable,(fail,4)),_)) :-
		{asserta((foo :- fail,4))}.

:- end_object.
