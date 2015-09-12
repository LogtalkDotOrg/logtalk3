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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/11/21,
		comment is 'Unit tests for the ISO Prolog standard (=)/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.2.1.4

	succeeds(iso_unify_2_01) :-
		{'='(1, 1)}.

	succeeds(iso_unify_2_02) :-
		{'='(X, 1)},
		X == 1.

	succeeds(iso_unify_2_03) :-
		{'='(X, Y)},
		X == Y.

	succeeds(iso_unify_2_04) :-
		{'='(_, _)}.

	succeeds(iso_unify_2_05) :-
		{('='(X,Y), '='(X,abc))},
		X == abc, Y == abc.

	succeeds(iso_unify_2_06) :-
		{'='(f(X,def), f(def,Y))},
		X == def, Y == def.

	fails(iso_unify_2_07) :-
		{'='(1, 2)}.

	fails(iso_unify_2_08) :-
		{'='(1, 1.0)}.

	fails(iso_unify_2_09) :-
		{'='(g(X), f(f(X)))}.

	fails(iso_unify_2_10) :-
		{'='(f(X,1), f(a(X)))}.

	fails(iso_unify_2_11) :-
		{'='(f(X,Y,X), f(a(X),a(Y),Y,2))}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).
		succeeds(iso_unify_2_12) :-
			{'='(X,a(X))}.

		fails(iso_unify_2_13) :-
			{'='(f(X,1), f(a(X),2))}.

		fails(iso_unify_2_14) :-
			{'='(f(1,X,1), f(2,a(X),2))}.

		fails(iso_unify_2_15) :-
			{'='(f(1,X), f(2,a(X)))}.

		fails(iso_unify_2_16) :-
			{'='(f(X,Y,X,1), f(a(X),a(Y),Y,2))}.
	:- else.
		- succeeds(iso_unify_2_12) :-
			% STO; Undefined
			{'='(X,a(X))}.

		- fails(iso_unify_2_13) :-
			% STO; Undefined
			{'='(f(X,1), f(a(X),2))}.

		- fails(iso_unify_2_14) :-
			% STO; Undefined
			{'='(f(1,X,1), f(2,a(X),2))}.

		- fails(iso_unify_2_15) :-
			% STO; Undefined
			{'='(f(1,X), f(2,a(X)))}.

		- fails(iso_unify_2_16) :-
			% STO; Undefined
			{'='(f(X,Y,X,1), f(a(X),a(Y),Y,2))}.
	:- endif.

:- end_object.
