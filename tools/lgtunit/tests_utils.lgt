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


:- object(tests_utils,
	extends(lgtunit)).

	:- info([
		version is 1.5,
		author is 'Paulo Moura',
		date is 2018/02/17,
		comment is 'Unit tests for the "lgtunit" tool utility predicates.'
	]).

	:- uses(lgtunit, [
		benchmark/2, benchmark_reified/3,
		benchmark/3,
		epsilon/1, ('=~=')/2,
		deterministic/1, deterministic/2,
		variant/2,
		assertion/2,
		quick_check/3, quick_check/2, quick_check/1
	]).

	:- discontiguous([
		succeeds/1, throws/2
	]).

	% benchmark/2 tests

	succeeds(benchmark_2_01) :-
		benchmark(atom_codes('sample test atom',_), Time),
		float(Time).

	succeeds(benchmark_2_02) :-
		benchmark(fail, Time),
		float(Time).

	% benchmark_reified/3 tests

	succeeds(benchmark_reified_3_01) :-
		benchmark_reified(throw(err), Time, Result),
		float(Time), Result == error(err).

	succeeds(benchmark_reified_3_02) :-
		benchmark_reified(true, Time, Result),
		float(Time), Result == success.

	succeeds(benchmark_reified_3_03) :-
		benchmark_reified(fail, Time, Result),
		float(Time), Result == failure.

	% benchmark/3 tests

	succeeds(benchmark_3_01) :-
		benchmark(atom_codes('sample test atom',_), 100, Time),
		float(Time).

	succeeds(benchmark_3_02) :-
		benchmark(fail, 100, Time),
		float(Time).

	% epsilon/1 tests

	succeeds(epsilon_1_01) :-
		epsilon(Epsilon),
		float(Epsilon),
		Epsilon > 0.

	% ('=~=')/2 tests

	succeeds('=~=_2_01') :-
		'=~='(0.0, 0.0).

	succeeds('=~=_2_02') :-
		epsilon(Epsilon),
		EpsilonX10 is Epsilon*10,
		'=~='(Epsilon, EpsilonX10).

	succeeds('=~=_2_03') :-
		epsilon(Epsilon),
		EpsilonX100 is Epsilon*100,
		'=~='(Epsilon, EpsilonX100).

	succeeds('=~=_2_04') :-
		epsilon(Epsilon),
		EpsilonX1000 is Epsilon*1000,
		\+ '=~='(Epsilon, EpsilonX1000).

	succeeds('=~=_2_05') :-
		\+ '=~='(0.0, 0.0001).

	succeeds('=~=_2_06') :-
		\+ '=~='(0.0, 1.0).

	succeeds('=~=_2_07') :-
		'=~='([0.0,1.0,2.0,3.0], [0.0,1.0,2.0,3.0]).

	succeeds('=~=_2_08') :-
		\+ '=~='([0.0,1.0,2.0,3.0], [0.0,1.0,2.0,2.0]).

	succeeds('=~=_2_09') :-
		'=~='([[0.0,1.0],[2.0,3.0]], [[0.0,1.0],[2.0,3.0]]).

	succeeds('=~=_2_10') :-
		\+ '=~='([[0.0,1.0],[2.0,3.0]], [[0.0,1.0],[2.0,2.0]]).

	throws('=~=_2_11', error(instantiation_error,_)) :-
		'=~='(_, _).

	throws('=~=_2_12', error(instantiation_error,_)) :-
		'=~='(1.0, _).

	throws('=~=_2_13', error(instantiation_error,_)) :-
		'=~='(_, 2.0).

	throws('=~=_2_14', error(instantiation_error,_)) :-
		'=~='([0.0,1.0,2.0,_], [0.0,1.0,2.0,_]).

	throws('=~=_2_15', error(instantiation_error,_)) :-
		'=~='([0.0,1.0,2.0,_], [0.0,1.0,2.0,3.0]).

	throws('=~=_2_16', error(instantiation_error,_)) :-
		'=~='([0.0,1.0,2.0,3.0], [0.0,1.0,2.0,_]).

	throws('=~=_2_17', error(instantiation_error,_)) :-
		'=~='([[0.0,1.0],[2.0,_]], [[0.0,1.0],[2.0,_]]).

	throws('=~=_2_18', error(instantiation_error,_)) :-
		'=~='([[0.0,1.0],[2.0,_]], [[0.0,1.0],[2.0,3.0]]).

	throws('=~=_2_19', error(instantiation_error,_)) :-
		'=~='([[0.0,1.0],[2.0,3.0]], [[0.0,1.0],[2.0,_]]).

	% deterministic/1 tests

	succeeds(deterministic_1_01) :-
		findall(1, deterministic(true), Solutions),
		Solutions == [1].

	succeeds(deterministic_1_02) :-
		findall(1, deterministic(once(repeat)), Solutions),
		Solutions == [1].

	succeeds(deterministic_1_03) :-
		\+ deterministic(fail).

	succeeds(deterministic_1_04) :-
		\+ deterministic(repeat).

	succeeds(deterministic_1_05) :-
		\+ deterministic((N=1; N=2)).

	% deterministic/2 tests

	succeeds(deterministic_2_01) :-
		deterministic(true, Deterministic),
		Deterministic == true.

	succeeds(deterministic_2_02) :-
		deterministic(once(repeat), Deterministic),
		Deterministic == true.

	succeeds(deterministic_2_03) :-
		\+ deterministic(fail, _).

	succeeds(deterministic_2_04) :-
		deterministic(repeat, Deterministic),
		Deterministic == false.

	succeeds(deterministic_2_05) :-
		deterministic((N=1; N=2), Deterministic),
		Deterministic == false.

	% variant/2 tests

	succeeds(variant_2_01) :-
		variant(1, 1).

	succeeds(variant_2_02) :-
		variant(X, X).

	succeeds(variant_2_03) :-
		variant(_X, _Y).

	succeeds(variant_2_04) :-
		variant(a(_X,_Y), a(_A,_B)).

	succeeds(variant_2_05) :-
		\+ variant(1, 2).

	succeeds(variant_2_06) :-
		\+ variant(a(1,_Y), a(_A,2)).

	% assertion/2 tests

	succeeds(assertion_2_01) :-
		assertion(1,integer(1)).

	throws(assertion_2_02, assertion_failure(2)) :-
		assertion(2, integer(1.1)).

	throws(assertion_2_03, assertion_error(3, e)) :-
		assertion(3, throw(e)).

	% quick_check/3 tests

	succeeds(quick_check_3_01) :-
		quick_check(atom(+atom), Result, []),
		Result == passed.

	succeeds(quick_check_3_02) :-
		quick_check(atom(+atom), Result, [n(25)]),
		Result == passed.

	succeeds(quick_check_3_03) :-
		quick_check(atom(+integer), Result, []),
		Result = failed(atom(Integer)), integer(Integer).

	succeeds(quick_check_3_04) :-
		quick_check(atom(+integer), Result, [n(25)]),
		Result = failed(atom(Integer)), integer(Integer).

	% quick_check/2 tests

	succeeds(quick_check_2_01) :-
		quick_check(atom(+atom), []),
		quick_check_passed(N), N == 100.

	succeeds(quick_check_2_02) :-
		quick_check(atom(+atom), [n(25)]),
		quick_check_passed(N), N == 25.

	succeeds(quick_check_2_03) :-
		\+ quick_check(atom(+integer), []).

	succeeds(quick_check_2_04) :-
		\+ quick_check(atom(+integer), [n(25)]).

	% quick_check/1 tests

	succeeds(quick_check_1_01) :-
		quick_check(atom(+atom)),
		quick_check_passed(N), N == 100.

	succeeds(quick_check_1_02) :-
		\+ quick_check(atom(+integer)).

	% supress quick_check/1-3 messages and save option values for tests

	:- private(quick_check_passed/1).
	:- dynamic(quick_check_passed/1).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(quick_check_passed(NumberOfTests), _, lgtunit, _) :-
		retractall(quick_check_passed(_)),
		assertz(quick_check_passed(NumberOfTests)).
	logtalk::message_hook(quick_check_failed(_), _, lgtunit, _).

:- end_object.
