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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2015/05/19,
		comment is 'Unit tests for the ISO Prolog standard (\\+)/1 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.15.1.4

	fails(iso_not_1_01) :-
		{'\\+'(true)}.

	fails(iso_not_1_02) :-
		{'\\+'(!)}.

	succeeds(iso_not_1_03) :-
		{'\\+'((!,fail))}.

	succeeds(iso_not_1_04) :-
		findall(X, {(X=1;X=2), '\\+'((!,fail))}, L),
		L == [1, 2].

	succeeds(iso_not_1_05) :-
		{'\\+'(4 = 5)}.

	throws(iso_not_1_06, [error(type_error(callable,3),_), error(type_error(callable,'\\+'(3)),_)]) :-
		% the second exception term is a common but not strictly conforming alternative
		% try to force runtime goal checking
		G = '\\+'(3), {G}.

	throws(iso_not_1_07, error(instantiation_error,_)) :-
		% try to force runtime goal checking
		G = '\\+'(_X), {G}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).
		fails(iso_not_1_08) :-
			{'\\+'(X=f(X))}.
	:- else.
		- fails(iso_not_1_08) :-
			% STO; Undefined
			{'\\+'(X=f(X))}.
	:- endif.

	% tests from the Logtalk portability work

	succeeds(lgt_not_1_09) :-
		{'\\+'('\\+'(X=1))},
		var(X).

:- end_object.
