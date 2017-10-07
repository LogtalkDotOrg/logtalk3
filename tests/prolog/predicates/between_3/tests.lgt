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


:- if(current_logtalk_flag(prolog_dialect, xsb)).
	:- import(from(/(between,3), basics)).
:- endif.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2017/10/07,
		comment is 'Unit tests for the de facto Prolog standard between/3 built-in predicate.'
	]).

	deterministic(commons_between_3_01) :-
		{between(1,5,1)}.

	deterministic(commons_between_3_02) :-
		{between(1,5,3)}.

	deterministic(commons_between_3_03) :-
		{between(1,5,5)}.

	fails(commons_between_3_04) :-
		{between(1,3,0)}.

	fails(commons_between_3_05) :-
		{between(1,3,5)}.

	fails(commons_between_3_06) :-
		{between(1, 0, _)}.

	succeeds(commons_between_3_07) :-
		findall(N, {between(1,3,N)}, L),
		L == [1, 2, 3].

	succeeds(commons_between_3_08) :-
		findall(N, {between(1,1,N)}, L),
		L == [1].

	throws(commons_between_3_09, error(instantiation_error,_)) :-
		{between(_, 3, _)}.

	throws(commons_between_3_10, error(instantiation_error,_)) :-
		{between(1, _, _)}.

	throws(commons_between_3_11, error(type_error(integer,a),_)) :-
		{between(a, 3, _)}.

	throws(commons_between_3_12, error(type_error(integer,a),_)) :-
		{between(1, a, _)}.

	throws(commons_between_3_13, error(type_error(integer,a),_)) :-
		{between(1, 3, a)}.

:- end_object.
