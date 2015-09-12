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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/05/05,
		comment is 'Unit tests for the ISO Prolog standard findall/3 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.10.1.4

	succeeds(iso_findall_3_01) :-
		{findall(X, (X=1;X=2), S)},
		S == [1,2].

	succeeds(iso_findall_3_02) :-
		{findall(X+_Y, (X=1), S)},
		S = [1+_].

	succeeds(iso_findall_3_03) :-
		{findall(_X, fail, L)},
		L == [].

	succeeds(iso_findall_3_04) :-
		{findall(X, (X=1;X=1), S)},
		S == [1,1].

	fails(iso_findall_3_05) :-
		{findall(X, (X=2;X=1), [1,2])}.

	succeeds(iso_findall_3_06) :-
		{findall(X, (X=1;X=2), [X,Y])},
		X == 1, Y == 2.

	throws(iso_findall_3_07, error(instantiation_error,_)) :-
		{findall(_X, _Goal, _S)}.

	throws(iso_findall_3_08, error(type_error(callable,4),_)) :-
		{findall(_X, 4, _S)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_findall_3_09, error(type_error(list,[A|1]),_)) :-
		{findall(X, X=1, [A|1])}.

	% tests from the ECLiPSe test suite

	throws(eclipse_findall_3_10, error(type_error(list,12),_)) :-
		{findall(X, (X=2; X=1), 12)}.

:- end_object.
