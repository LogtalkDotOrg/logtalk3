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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/02/18,
		comment is 'Unit tests for the ISO Prolog standard term comparison built-in predicates.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.4.1.4

	succeeds(iso_term_comparison_01) :-
		{'@=<'(1.0, 1)}.

	succeeds(iso_term_comparison_02) :-
		{'@<'(1.0, 1)}.

	fails(iso_term_comparison_03) :-
		{'\\=='(1, 1)}.

	succeeds(iso_term_comparison_04) :-
		{'@=<'(aardvark, zebra)}.

	succeeds(iso_term_comparison_05) :-
		{'@=<'(short, short)}.

	succeeds(iso_term_comparison_06) :-
		{'@=<'(short, shorter)}.

	fails(iso_term_comparison_07) :-
		{'@>='(short, shorter)}.

	fails(iso_term_comparison_08) :-
		{'@<'(foo(a,b), north(a))}.

	succeeds(iso_term_comparison_09) :-
		{'@>'(foo(b), foo(a))}.

	succeeds(iso_term_comparison_10) :-
		{'@<'(foo(a, _X), foo(b, _Y))}.

	succeeds(iso_term_comparison_11) :-
		(	{'@<'(foo(_X, a), foo(_Y, b))} ->
			true
		;	true
		).

	succeeds(iso_term_comparison_12) :-
		{'@=<'(X, X)}.

	succeeds(iso_term_comparison_13) :-
		{'=='(X, X)}.

	succeeds(iso_term_comparison_14) :-
		(	{'@=<'(_X, _Y)} ->
			true
		;	true
		).

	fails(iso_term_comparison_15) :-
		{'=='(_X, _Y)}.

	succeeds(iso_term_comparison_16) :-
		{\==(_, _)}.

	fails(iso_term_comparison_17) :-
		{'=='(_, _)}.

	succeeds(iso_term_comparison_18) :-
		(	{'@=<'(_, _)} ->
			true
		;	true
		).

	succeeds(iso_term_comparison_19) :-
		(	{'@=<'(foo(_X, a), foo(_Y, b))} ->
			true
		;	true
		).

	% standard order tests from the Logtalk portability work

	succeeds(lgt_term_comparison_20) :-
		{'@<'(_X, 1.1)}.

	succeeds(lgt_term_comparison_21) :-
		{'@<'(1.1, 1)}.

	succeeds(lgt_term_comparison_22) :-
		{'@<'(1, a)}.

	succeeds(lgt_term_comparison_23) :-
		{'@<'(a, a(_))}.

	succeeds(lgt_term_comparison_24) :-
		{'@<'(a(_), a(_,_))}.

	succeeds(lgt_term_comparison_25) :-
		{'@<'(b(_), a(_,_))}.

	succeeds(lgt_term_comparison_26) :-
		{'@<'(a(1,2), a(1,3))}.

	succeeds(lgt_term_comparison_27) :-
		{'@<'(a(1,2), b(1,2))}.

	% other tests

	succeeds(lgt_term_comparison_28) :-
		{'@>='((4,1,0), (4,0,1))}.

	fails(lgt_term_comparison_29) :-
		{'@>='((4,0,1), (4,1,0))}.

	fails(lgt_term_comparison_30) :-
		{'@=<'((4,1,0), (4,0,1))}.

	succeeds(lgt_term_comparison_31) :-
		{'@=<'((4,0,1), (4,1,0))}.

	succeeds(lgt_term_comparison_32) :-
		{'@>'((4,1,0), (4,0,1))}.

	fails(lgt_term_comparison_33) :-
		{'@>'((4,0,1), (4,1,0))}.

	fails(lgt_term_comparison_34) :-
		{'@<'((4,1,0), (4,0,1))}.

	succeeds(lgt_term_comparison_35) :-
		{'@<'((4,0,1), (4,1,0))}.

:- end_object.
