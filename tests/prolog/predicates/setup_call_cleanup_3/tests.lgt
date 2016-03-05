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


% database for the tests

:- dynamic(v/1).

ndet(a).
ndet(b).
ndet(_) :- 1 =:= 0.

% this should undo the bindings of G and B before calling the
% cleanup handler.  I.e., S must be 1 and G and B must be var.

test_error_choice :-
	setup_call_cleanup(
		S=1,
		(G=2 ; G=3),
		asserta(v(x(S,G,B)))
	),
	B = 4,
	throw(x).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Jan Wielemaker. Adapted to Logtalk by Paulo Moura.',
		date is 2015/05/12,
		comment is 'Unit tests for the setup_call_cleanup/3 built-in predicate that is becoming a de facto standard.',
		source is 'Tests adapted with permission from the SWI-Prolog distribution.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	setup :-
		{retractall(v(_))}.

	succeeds(swi_setup_call_cleanup_3_01) :-
		{setup_call_cleanup(A=42, true, asserta(v(A))), retract(v(X))},
		X == 42.

	succeeds(swi_setup_call_cleanup_3_02) :-
		{setup_call_cleanup(A=42, (true;true), asserta(v(A))), !, retract(v(X))},
		X == 42.

	succeeds(swi_setup_call_cleanup_3_03) :-
		{\+ setup_call_cleanup(A=42, fail, asserta(v(A))), retract(v(X))},
		X == 42.

	succeeds(swi_setup_call_cleanup_3_04) :-
		{\+ setup_call_cleanup(A=42, (B=2,fail), assertz(v([A,B]))), retract(v(X))},
		subsumes_term(X, [42,_]), subsumes_term([42,_], X).

	succeeds(swi_setup_call_cleanup_3_05) :-
		{catch(setup_call_cleanup(A=42, throw(error(x)), assertz(v(A))), E, true), retract(v(X))},
		[X,E] == [42,error(x)].

	succeeds(swi_setup_call_cleanup_3_06) :-
		{	setup_call_cleanup(true, (ndet(X), assertz(v(X))), assertz(v(done))),
	    	fail
		;   findall(V, retract(v(V)), Vs)
		},
		Vs == [a,b,done],
		{retractall(v(_))}.

	throws(swi_setup_call_cleanup_3_07, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(X),
		{setup_call_cleanup(true, true, X)}.

	succeeds(swi_setup_call_cleanup_3_08) :-
		{setup_call_cleanup(X=true, true, X)}.

	throws(swi_setup_call_cleanup_3_09, first) :-
		{setup_call_cleanup(true, (G=1;G=2), throw(second)), throw(first)}.

	throws(swi_setup_call_cleanup_3_10, a(first)) :-
		{setup_call_cleanup(true, (G=1;G=2), throw(a(second))), throw(a(first))}.

	succeeds(swi_setup_call_cleanup_3_11) :-
		{catch(test_error_choice, E, true), findall(X, retract(v(X)), Xs)},
		subsumes_term(E+Xs, x+[x(1,_,_)]), subsumes_term(x+[x(1,_,_)], E+Xs).

	% auxiliary predicate used to delay errors to runtime

	variable(_).

:- end_object.
