%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


% predicate uses when testing that the {}/1 control construct is opaque to cuts
nop.


:- object(external_call_test_object).

	:- public(p/1).
	:- meta_predicate(p(::)).
	p(Goal) :-
		{Goal}.

	:- public(q/1).
	q(Xs) :-
		findall(X, (a(X), {!}), Xs).

	:- public(r/1).
	r(Xs) :-
		G = !,
		findall(X, (a(X), {G}), Xs).

	:- public(s/1).
	s(Xs) :-
		G = (nop, !),
		findall(X, (a(X), {G}), Xs).

	:- public(t/1).
	t(Xs) :-
		findall(X, (a(X), {nop, !}), Xs).

	:- public(u/1).
	u(Xs) :-
		findall(X, (a(X), {nop; !}), Xs).

	:- public(v/1).
	v(Xs) :-
		findall(X, (a(X), {nop -> !}), Xs).

	a(1).
	a(2).
	a(3).

:- end_object.



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2017-04-26,
		comment is 'Unit tests for the {}/1 built-in control construct.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	throws(external_call_1_01, error(instantiation_error,_)) :-
		external_call_test_object::p(_).

	throws(external_call_1_02, error(type_error(callable,1),_)) :-
		external_call_test_object::p(1).

	throws(external_call_1_03, error(existence_error(predicate_declaration,atom_concat/3),_)) :-
		external_call_test_object::atom_concat(a, b, _).

	succeeds(external_call_1_04) :-
		external_call_test_object::p(true).

	fails(external_call_1_05) :-
		external_call_test_object::p(fail).

	succeeds(external_call_1_06) :-
		external_call_test_object::q(Xs),
		Xs == [1,2,3].

	succeeds(external_call_1_07) :-
		external_call_test_object::r(Xs),
		Xs == [1,2,3].

	succeeds(external_call_1_08) :-
		external_call_test_object::s(Xs),
		Xs == [1,2,3].

	succeeds(external_call_1_09) :-
		external_call_test_object::t(Xs),
		Xs == [1,2,3].

	succeeds(external_call_1_10) :-
		external_call_test_object::u(Xs),
		Xs == [1, 1, 2, 2, 3, 3].

	succeeds(external_call_1_11) :-
		external_call_test_object::v(Xs),
		Xs == [1,2,3].

	succeeds(external_call_1_12) :-
		external_call_test_object::{atom_concat(a, b, AB)},
		AB == ab.

:- end_object.
