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
		date is 2016/10/04,
		comment is 'Unit tests for the "lgtunit" tool utility predicates.'
	]).

	:- uses(lgtunit, [
		('=~=')/2,
		deterministic/1,
		epsilon/1,
		variant/2
	]).

	% epsilon/1 tests

	test(epsilon_1_01) :-
		epsilon(Epsilon),
		float(Epsilon),
		Epsilon > 0.

	% ('=~=')/2 tests

	test('=~=_2_01') :-
		'=~='(0.0, 0.0).

	test('=~=_2_02') :-
		epsilon(Epsilon),
		EpsilonX10 is Epsilon*10,
		'=~='(Epsilon, EpsilonX10).

	test('=~=_2_03') :-
		epsilon(Epsilon),
		EpsilonX100 is Epsilon*100,
		'=~='(Epsilon, EpsilonX100).

	test('=~=_2_04') :-
		epsilon(Epsilon),
		EpsilonX1000 is Epsilon*1000,
		\+ '=~='(Epsilon, EpsilonX1000).

	test('=~=_2_05') :-
		\+ '=~='(0.0, 0.0001).

	test('=~=_2_06') :-
		\+ '=~='(0.0, 1.0).

	% deterministic/1 tests

	test(deterministic_1_01) :-
		findall(1, deterministic(true), Solutions),
		Solutions == [1].

	test(deterministic_1_02) :-
		findall(1, deterministic(once(repeat)), Solutions),
		Solutions == [1].

	test(deterministic_1_03) :-
		\+ deterministic(fail).

	test(deterministic_1_04) :-
		\+ deterministic(repeat).

	test(deterministic_1_05) :-
		\+ deterministic((N=1; N=2)).

	% variant/2 tests

	test(variant_2_01) :-
		variant(1, 1).

	test(variant_2_02) :-
		variant(X, X).

	test(variant_2_03) :-
		variant(_X, _Y).

	test(variant_2_04) :-
		variant(a(_X,_Y), a(_A,_B)).

	test(variant_2_05) :-
		\+ variant(1, 2).

	test(variant_2_06) :-
		\+ variant(a(1,_Y), a(_A,2)).

:- end_object.
