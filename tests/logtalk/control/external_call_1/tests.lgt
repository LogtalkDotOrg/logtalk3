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


:- object(external_call_test_object).

	:- public(p/1).
	p(Goal) :-
		{Goal}.

	:- public(q/1).
	q(Xs) :-
		findall(X, (a(X), {!}), Xs).

	:- public(r/1).
	r(Xs) :-
		G = !,
		findall(X, (a(X), {G}), Xs).

	a(1).
	a(2).
	a(3).

:- end_object.



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/03/31,
		comment is 'Unit tests for the {}/1 built-in control construct.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	throws(external_call_1_1, error(instantiation_error,_)) :-
		external_call_test_object::p(_).

	throws(external_call_1_2, error(type_error(callable,1),_)) :-
		external_call_test_object::p(1).

	throws(external_call_1_3, error(existence_error(predicate_declaration,atom_concat/3),_)) :-
		external_call_test_object::atom_concat(a, b, _).

	succeeds(external_call_1_4) :-
		external_call_test_object::p(true).

	fails(external_call_1_5) :-
		external_call_test_object::p(fail).

	succeeds(external_call_1_6) :-
		external_call_test_object::q(Xs),
		Xs == [1,2,3].

	succeeds(external_call_1_7) :-
		external_call_test_object::r(Xs),
		Xs == [1,2,3].

	succeeds(external_call_1_8) :-
		external_call_test_object::{atom_concat(a, b, AB)},
		AB == ab.

:- end_object.
