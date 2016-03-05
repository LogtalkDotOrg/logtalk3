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
		date is 2015/05/11,
		comment is 'Unit tests for the ISO Prolog standard once/1 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.15.2.4

	succeeds(iso_once_1_01) :-
		{once(!)}.

	succeeds(iso_once_1_02) :-
		findall(X, {once(!), (X=1; X=2)}, L),
		L == [1, 2].

	succeeds(iso_once_1_03) :-
		{once(repeat)}.

	fails(iso_once_1_04) :-
		{once(fail)}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).
		succeeds(iso_once_1_05) :-
			{once((X = f(X)))}.
	:- else.
		- succeeds(iso_once_1_05) :-
			% STO; Undefined
			{once((X = f(X)))}.
	:- endif.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(eddbali_once_1_06, error(type_error(callable,3),_)) :-
		% try to delay the error to runtime
		G = 3, {once(G)}.

	throws(eddbali_once_1_07, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(X),
		{once(X)}.

	% auxiliary predicate used to delay errors to runtime

	variable(_).

:- end_object.
