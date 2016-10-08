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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/10/14,
		comment is 'Unit tests for the ISO Prolog standard subsumes_term/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.2.4.4

	succeeds(iso_subsumes_term_2_01) :-
		{subsumes_term(a, a)}.

	succeeds(iso_subsumes_term_2_02) :-
		{subsumes_term(f(_X,_Y), f(Z,Z))}.

	fails(iso_subsumes_term_2_03) :-
		{subsumes_term(f(Z,Z), f(_X,_Y))}.

	fails(iso_subsumes_term_2_04) :-
		{subsumes_term(g(X), g(f(X)))}.

	fails(iso_subsumes_term_2_05) :-
		{subsumes_term(X, f(X))}.

	succeeds(iso_subsumes_term_2_06) :-
		{subsumes_term(X, Y), subsumes_term(Y, f(X))}.

:- end_object.
