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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/08/25,
		comment is 'Unit tests for the de facto Prolog standard numbervars/3 built-in predicate.'
	]).

	succeeds(commons_numbervars_3_01) :-
		{numbervars(t, 0, N)},
		N == 0.

	succeeds(commons_numbervars_3_02) :-
		{numbervars(T, 0, N)},
		ground(T), N == 1.

	succeeds(commons_numbervars_3_03) :-
		T = a(_X,_Y,_Z),
		{numbervars(T, 0, N)},
		ground(T), N == 3.

	succeeds(commons_numbervars_3_04) :-
		T = a(_X,_Y,_X),
		{numbervars(T, 0, N)},
		ground(T), N == 2.

	succeeds(commons_numbervars_3_05) :-
		T = a(_X, 1, b(_Y, c(_X), 2), 3, _W),
		{numbervars(T, 3, N)},
		ground(T), N == 6.

	succeeds(commons_numbervars_3_06) :-
		T = a(X,Y,Z),
		{numbervars(T, 0, _)},
		X == '$VAR'(0), Y == '$VAR'(1), Z == '$VAR'(2).

	succeeds(commons_numbervars_3_07) :-
		T = a(X,Y,Z),
		{numbervars(T, -7, N)},
		X == '$VAR'(-7), Y == '$VAR'(-6), Z == '$VAR'(-5), N == -4.

:- end_object.
