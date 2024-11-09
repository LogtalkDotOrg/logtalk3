%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- object(symbiosis).

	:- info([
		author is 'Paulo Moura',
		version is 1:4:0,
		date is 2024-11-07,
		comment is 'Examples of using Prolog built-in meta-predicates and module meta-predicates that take closures as arguments.'
	]).

	:- if(current_logtalk_flag(prolog_dialect, gnu)).
		% in GNU Prolog, the maplist/2-3 predicates are built-in predicates;
		% thus, the following uses/2 directive is not necessary but can still
		% be used e.g. for helping document the meta-predicate dependencies
		:- uses(user, [maplist/2, maplist/3]).
	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).
		:- use_module(lists, [checklist/2:maplist/2, maplist/3]).
	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).
		:- use_module(lists, [maplist/2, maplist/3]).
	:- elif(current_logtalk_flag(prolog_dialect, swi)).
		:- use_module(apply, [maplist/2, maplist/3]).
	:- elif(current_logtalk_flag(prolog_dialect, tau)).
		:- use_module(lists, [maplist/2, maplist/3]).
	:- elif(current_logtalk_flag(prolog_dialect, trealla)).
		:- use_module(lists, [maplist/2, maplist/3]).
	:- elif(current_logtalk_flag(prolog_dialect, yap)).
		:- use_module(maplist, [maplist/2, maplist/3]).
	:- endif.

	:- public(p/0).
	p :-
		maplist(integer, [1,2,3]).

	:- public(q/1).
	q(L) :-
		maplist(my_char_code, [a,b,c], L).

	my_char_code(Char, Code) :-
		char_code(Char, Code).

	:- public(r/1).
	r(L) :-
		maplist(list::sort, [[3,1,2]], [L]).

	:- public(s/1).
	s(L) :-
		maplist([X,Y]>>{Y is X+1}, [1,2,3], L).

	:- public(t/1).
	t(L) :-
		maplist([X,Y]>>add1(X, Y), [1,2,3], L).

	add1(X, Y) :-
		Y is X+1.

:- end_object.
