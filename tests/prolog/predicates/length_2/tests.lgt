%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
	:- import(from(/(length,2), basics)).
:- endif.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2019-01-22,
		comment is 'Unit tests for the de facto Prolog standard length/2 built-in predicate.'
	]).

	deterministic(commons_length_2_01) :-
		{length([], N)},
		N == 0.

	deterministic(commons_length_2_02) :-
		{length([], 0)}.

	deterministic(commons_length_2_03) :-
		{length(List, 0)},
		List == [].

	deterministic(commons_length_2_04) :-
		{length([1,2,3], N)},
		N == 3.

	deterministic(commons_length_2_05) :-
		{length([1,2,3], 3)}.

	deterministic(commons_length_2_06) :-
		{length(List, 3)},
		^^variant(List, [_, _, _]).

	fails(commons_length_2_07) :-
		{length([1,2,3], 0)}.

	fails(commons_length_2_08) :-
		{length([], 3)}.

	succeeds(commons_length_2_09) :-
		findall(List, {between(0,3,N), length(List,N)}, Lists),
		^^variant(Lists, [[], [_], [_,_], [_,_,_]]).

	throws(commons_length_2_10, error(type_error(integer,a),_)) :-
		{length(_, a)}.

	throws(commons_length_2_11, error(type_error(list,a),_)) :-
		{length(a, _)}.

	throws(commons_length_2_12, error(domain_error(not_less_than_zero,-1),_)) :-
		{length(_, -1)}.

:- end_object.
