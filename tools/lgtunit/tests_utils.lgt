%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


% test data
a(1).


:- object(tests_utils,
	extends(lgtunit)).

	:- info([
		version is 2:3:0,
		author is 'Paulo Moura',
		date is 2021-09-04,
		comment is 'Unit tests for the "lgtunit" tool utility predicates.'
	]).

	:- uses(lgtunit, [
		benchmark/2, benchmark_reified/3,
		benchmark/3,
		epsilon/1, ('=~=')/2,
		approximately_equal/3, essentially_equal/3, tolerance_equal/4,
		deterministic/1, deterministic/2,
		variant/2,
		assertion/1, assertion/2,
		quick_check/3, quick_check/2, quick_check/1
	]).

	:- uses(integer, [
		between/3
	]).

	:- uses(list, [
		msort/2
	]).

	% benchmark/2 tests

	test(benchmark_2_01, true(float(Time))) :-
		benchmark(atom_codes('sample test atom',_), Time).

	test(benchmark_2_02, true(float(Time))) :-
		benchmark(fail, Time).

	% benchmark_reified/3 tests

	test(benchmark_reified_3_01, true) :-
		benchmark_reified(throw(err), Time, Result),
		assertion(time, float(Time)),
		assertion(result, Result == error(err)).

	test(benchmark_reified_3_02, true) :-
		benchmark_reified(true, Time, Result),
		assertion(time, float(Time)),
		assertion(result, Result == success).

	test(benchmark_reified_3_03, true) :-
		benchmark_reified(fail, Time, Result),
		assertion(time, float(Time)),
		assertion(result, Result == failure).

	% benchmark/3 tests

	test(benchmark_3_01, true(float(Time))) :-
		benchmark(atom_codes('sample test atom',_), 100, Time).

	test(benchmark_3_02, true(float(Time))) :-
		benchmark(fail, 100, Time).

	% epsilon/1 tests

	test(epsilon_1_01, true) :-
		epsilon(Epsilon),
		assertion(type, float(Epsilon)),
		assertion(value, Epsilon > 0).

	% approximately_equal/3 tests

	test(approximately_equal_3_01, true) :-
		approximately_equal(95.1, 100.0, 0.05).

	test(approximately_equal_3_02, false) :-
		approximately_equal(94.7, 100.0, 0.05).

	test(approximately_equal_3_03, error(instantiation_error)) :-
		approximately_equal(_, 100.0, 0.05).

	test(approximately_equal_3_04, error(instantiation_error)) :-
		approximately_equal(95.1, _, 0.05).

	test(approximately_equal_3_05, error(instantiation_error)) :-
		approximately_equal(95.1, 100.0, _).

	test(approximately_equal_3_06, error(type_error(number,a))) :-
		approximately_equal(a, 100.0, 0.05).

	test(approximately_equal_3_07, error(type_error(number,a))) :-
		approximately_equal(95.1, a, 0.05).

	test(approximately_equal_3_08, error(type_error(number,a))) :-
		approximately_equal(95.1, 100.0, a).

	% essentially_equal/3 tests

	test(essentially_equal_3_01, true) :-
		essentially_equal(98.7, 100.0, 0.05).

	test(essentially_equal_3_02, false) :-
		essentially_equal(95.1, 100.0, 0.05).

	test(essentially_equal_3_03, error(instantiation_error)) :-
		essentially_equal(_, 100.0, 0.05).

	test(essentially_equal_3_04, error(instantiation_error)) :-
		essentially_equal(95.1, _, 0.05).

	test(essentially_equal_3_05, error(instantiation_error)) :-
		essentially_equal(95.1, 100.0, _).

	test(essentially_equal_3_06, error(type_error(number,a))) :-
		essentially_equal(a, 100.0, 0.05).

	test(essentially_equal_3_07, error(type_error(number,a))) :-
		essentially_equal(95.1, a, 0.05).

	test(essentially_equal_3_08, error(type_error(number,a))) :-
		essentially_equal(95.1, 100.0, a).

	% tolerance_equal/4 tests

	test(tolerance_equal_4_01, true) :-
		tolerance_equal(0.0, 0.0, 1.0e-9, 1.0e-16).

	test(tolerance_equal_4_02, true) :-
		tolerance_equal(100.0, 99.95, 1.0e-3, 1.0e-6).

	test(tolerance_equal_4_03, true) :-
		tolerance_equal(100, 95, 0.05, 1).

	test(tolerance_equal_4_04, false) :-
		tolerance_equal(100, 94, 0.05, 1).

	test(tolerance_equal_4_05, error(instantiation_error)) :-
		tolerance_equal(_, 99.95, 1.0e-3, 1.0e-6).

	test(tolerance_equal_4_06, error(instantiation_error)) :-
		tolerance_equal(100.0, _, 1.0e-3, 1.0e-6).

	test(tolerance_equal_4_07, error(instantiation_error)) :-
		tolerance_equal(100.0, 99.95, _, 1.0e-6).

	test(tolerance_equal_4_08, error(instantiation_error)) :-
		tolerance_equal(100.0, 99.95, 1.0e-3, _).

	test(tolerance_equal_4_09, error(type_error(number,a))) :-
		tolerance_equal(a, 99.95, 1.0e-3, 1.0e-6).

	test(tolerance_equal_4_10, error(type_error(number,a))) :-
		tolerance_equal(100.0, a, 1.0e-3, 1.0e-6).

	test(tolerance_equal_4_11, error(type_error(number,a))) :-
		tolerance_equal(100.0, 99.95, a, 1.0e-6).

	test(tolerance_equal_4_12, error(type_error(number,a))) :-
		tolerance_equal(100.0, 99.95, 1.0e-3, a).

	% ('=~=')/2 tests

	test('=~=_2_01', true) :-
		'=~='(0.0, 0.0).

	test('=~=_2_02', true) :-
		epsilon(Epsilon),
		EpsilonX10 is Epsilon*10,
		'=~='(Epsilon, EpsilonX10).

	test('=~=_2_03', true) :-
		epsilon(Epsilon),
		EpsilonX100 is Epsilon*100,
		'=~='(Epsilon, EpsilonX100).

	test('=~=_2_04', false) :-
		epsilon(Epsilon),
		EpsilonX1000 is Epsilon*1000,
		'=~='(Epsilon, EpsilonX1000).

	test('=~=_2_05', false) :-
		'=~='(0.0, 0.0001).

	test('=~=_2_06', false) :-
		'=~='(0.0, 1.0).

	test('=~=_2_07', true) :-
		'=~='([0.0,1.0,2.0,3.0], [0.0,1.0,2.0,3.0]).

	test('=~=_2_08', true) :-
		\+ '=~='([0.0,1.0,2.0,3.0], [0.0,1.0,2.0,2.0]).

	test('=~=_2_09', true) :-
		'=~='([[0.0,1.0],[2.0,3.0]], [[0.0,1.0],[2.0,3.0]]).

	test('=~=_2_10', true) :-
		\+ '=~='([[0.0,1.0],[2.0,3.0]], [[0.0,1.0],[2.0,2.0]]).

	test('=~=_2_11', error(instantiation_error)) :-
		'=~='(1.0, _).

	test('=~=_2_12', error(instantiation_error)) :-
		'=~='(_, 2.0).

	test('=~=_2_13', error(instantiation_error)) :-
		'=~='([0.0,1.0,2.0,_], [0.0,1.0,2.0,3.0]).

	test('=~=_2_14', error(instantiation_error)) :-
		'=~='([0.0,1.0,2.0,3.0], [0.0,1.0,2.0,_]).

	test('=~=_2_15', error(instantiation_error)) :-
		'=~='([[0.0,1.0],[2.0,_]], [[0.0,1.0],[2.0,3.0]]).

	test('=~=_2_16', error(instantiation_error)) :-
		'=~='([[0.0,1.0],[2.0,3.0]], [[0.0,1.0],[2.0,_]]).

	test('=~=_2_17', error(type_error(number,a))) :-
		'=~='(1.0, a).

	test('=~=_2_18', error(type_error(number,a))) :-
		'=~='(a, 2.0).

	test('=~=_2_19', error(type_error(number,a))) :-
		'=~='([0.0,1.0,2.0,a], [0.0,1.0,2.0,3.0]).

	test('=~=_2_20', error(type_error(number,a))) :-
		'=~='([0.0,1.0,2.0,3.0], [0.0,1.0,2.0,a]).

	test('=~=_2_21', error(type_error(number,a))) :-
		'=~='([[0.0,1.0],[2.0,a]], [[0.0,1.0],[2.0,3.0]]).

	test('=~=_2_22', error(type_error(number,a))) :-
		'=~='([[0.0,1.0],[2.0,3.0]], [[0.0,1.0],[2.0,a]]).

	% deterministic/1 tests

	test(deterministic_1_01, true(Solutions == [1])) :-
		findall(1, deterministic(true), Solutions).

	test(deterministic_1_02, true(Solutions == [1])) :-
		findall(1, deterministic(once(repeat)), Solutions).

	test(deterministic_1_03, false) :-
		deterministic(fail).

	test(deterministic_1_04, false) :-
		deterministic(repeat).

	test(deterministic_1_05, false) :-
		deterministic((N=1; N=2)).

	% deterministic/2 tests

	test(deterministic_2_01, true(Deterministic == true)) :-
		deterministic(true, Deterministic).

	test(deterministic_2_02, true(Deterministic == true)) :-
		deterministic(once(repeat), Deterministic).

	test(deterministic_2_03, false) :-
		deterministic(fail, _).

	test(deterministic_2_04, true(Deterministic == false)) :-
		deterministic(repeat, Deterministic).

	test(deterministic_2_05, true(Deterministic == false)) :-
		deterministic((N=1; N=2), Deterministic).

	% variant/2 tests

	test(variant_2_01, true) :-
		variant(1, 1).

	test(variant_2_02, true) :-
		variant(X, X).

	test(variant_2_03, true) :-
		variant(_X, _Y).

	test(variant_2_04, true) :-
		variant(a(_X,_Y), a(_A,_B)).

	test(variant_2_05, false) :-
		variant(1, 2).

	test(variant_2_06, false) :-
		variant(a(1,_Y), a(_A,2)).

	% assertion/1 tests

	test(assertion_1_01, true) :-
		% delay calling the assertion to runtime
		Assertion = integer(1),
		assertion(Assertion).

	test(assertion_1_02, true) :-
		% delay calling the assertion to runtime
		Assertion = foo(1),
		assertion(Assertion).

	test(assertion_1_03, ball(assertion_failure(integer(1.1)))) :-
		% delay calling the assertion to runtime
		Assertion = integer(1.1),
		assertion(Assertion).

	test(assertion_1_04, ball(assertion_failure(variant(1,2)))) :-
		% delay calling the assertion to runtime
		Assertion = variant(1, 2),
		assertion(Assertion).

	test(assertion_1_05, ball(assertion_failure(foo(2)))) :-
		% delay calling the assertion to runtime
		Assertion = foo(2),
		assertion(Assertion).

	test(assertion_1_06, ball(assertion_error(throw(e), e))) :-
		% delay calling the assertion to runtime
		Assertion = throw(e),
		assertion(Assertion).

	test(assertion_1_07, ball(assertion_error(foobar(1), error(existence_error(procedure,_),_)))) :-
		% delay calling the assertion to runtime
		Assertion = foobar(1),
		assertion(Assertion).

	test(assertion_1_08, true(var(X))) :-
		% delay calling the assertion to runtime
		Assertion = {a(X)},
		assertion(Assertion).

	% assertion/2 tests

	test(assertion_2_01, true) :-
		% delay calling the assertion to runtime
		Assertion = integer(1),
		assertion(1, Assertion).

	test(assertion_2_02, ball(assertion_failure(2))) :-
		% delay calling the assertion to runtime
		Assertion = integer(1.1),
		assertion(2, Assertion).

	test(assertion_2_03, ball(assertion_error(3, e))) :-
		% delay calling the assertion to runtime
		Assertion = throw(e),
		assertion(3, Assertion).

	test(assertion_2_04, ball(assertion_error(4, error(existence_error(procedure,_),_)))) :-
		% delay calling the assertion to runtime
		Assertion = foobar(1),
		assertion(4, Assertion).

	test(assertion_2_05, true(var(X))) :-
		% delay calling the assertion to runtime
		Assertion = {a(X)},
		assertion(a/1, Assertion).

	% quick_check/3 tests

	test(quick_check_3_01, subsumes(passed(_,_,_), Result)) :-
		quick_check(atom(+atom), Result, []).

	test(quick_check_3_02, subsumes(passed(_,_,_), Result)) :-
		quick_check(atom(+atom), Result, [n(25)]).

	test(quick_check_3_03, true) :-
		quick_check(atom(+integer), Result, []),
		assertion(result, subsumes_term(failed(atom(Integer), _Seed), Result)),
		Result = failed(atom(Integer), _Seed),
		assertion(type, integer(Integer)).

	test(quick_check_3_04, true) :-
		quick_check(atom(+integer), Result, [n(25)]),
		assertion(result, subsumes_term(failed(atom(Integer), _Seed), Result)),
		Result = failed(atom(Integer), _Seed),
		assertion(type, integer(Integer)).

	test(quick_check_3_05, subsumes(error(instantiation_error,_), Result)) :-
		quick_check(_, Result, []).

	test(quick_check_3_06, subsumes(error(type_error(callable,1),_), Result)) :-
		quick_check(1, Result, []).

	test(quick_check_3_07, subsumes(error(existence_error(predicate_declaration,foo42/1),_,_), Result)) :-
		quick_check(type::foo42(+integer), Result, []).

	test(quick_check_3_08, subsumes(failed(foo(1),_), Result)) :-
		quick_check(foo(@var), Result, []).

	test(quick_check_3_09, true) :-
		quick_check(integer(+byte), passed(Seed, Discarded, Pairs), [l(label1), n(1000)]),
		assertion(seed, ground(Seed)),
		assertion(discarded, Discarded == 0),
		assertion(pairs,
			(	ground(Pairs), msort(Pairs, Sorted),
				Sorted = [even-Even, odd-Odd],
				integer(Even), integer(Odd), 1000 =:= Even + Odd
			)
		).

	test(quick_check_3_10, true) :-
		quick_check(integer(+byte), passed(Seed, Discarded, Pairs), [l(label2), n(1000)]),
		assertion(seed, ground(Seed)),
		assertion(discarded, Discarded == 0),
		assertion(pairs,
			(	ground(Pairs), msort(Pairs, Sorted),
				Sorted = [all-All, even-Even, odd-Odd],
				integer(All), integer(Even), integer(Odd), All =:= 1000, 1000 =:= Even + Odd
			)
		).

	test(quick_check_3_11, true(Result == error(label_goal_failure, label3))) :-
		quick_check(integer(+byte), Result, [l(label3), n(1000)]).

	test(quick_check_3_12, subsumes(error(existence_error(procedure, label4/2), label4), Result)) :-
		quick_check(integer(+byte), Result, [l(label4), n(1000)]).

	test(quick_check_3_13, true) :-
		quick_check(integer(+byte), passed(Seed, Discarded, Pairs), [pc(condition1)]),
		assertion(seed, ground(Seed)),
		assertion(discarded, (integer(Discarded), Discarded > 0)),
		assertion(pairs, Pairs == []).

	test(quick_check_3_14, true) :-
		quick_check(integer(+byte), passed(Seed, Discarded, Pairs), [pc(condition2)]),
		assertion(seed, ground(Seed)),
		assertion(discarded, Discarded == 0),
		assertion(pairs, Pairs == []).

	test(quick_check_3_15, true(Result == error(pre_condition_always_fails, condition3))) :-
		quick_check(integer(+byte), Result, [pc(condition3)]).

	test(quick_check_3_16, subsumes(error(existence_error(procedure, condition4/1), condition4), Result)) :-
		quick_check(integer(+byte), Result, [pc(condition4)]).

	test(quick_check_3_17, true(Goal0-Seed0 == Goal-Seed)) :-
		% test that we are using a pseudo random generator
		quick_check(atom(+integer), failed(Goal0, Seed0), [ec(false)]),
		quick_check(atom(+integer), failed(Goal, Seed), [ec(false), rs(Seed0)]).

	% quick_check/2 tests

	test(quick_check_2_01, true(N == 100)) :-
		quick_check(atom(+atom), []),
		quick_check_passed(N).

	test(quick_check_2_02, true(N == 25)) :-
		quick_check(atom(+atom), [n(25)]),
		quick_check_passed(N).

	test(quick_check_2_03, false) :-
		quick_check(atom(+integer), []).

	test(quick_check_2_04, false) :-
		quick_check(atom(+integer), [n(25)]).

	test(quick_check_2_05, true) :-
		quick_check(type::valid({integer}, +integer), []).

	test(quick_check_2_06, true) :-
		quick_check(type::valid({integer}, +integer), [n(50)]).

	test(quick_check_2_07, false) :-
		quick_check(_, []).

	test(quick_check_2_08, false) :-
		quick_check(1, []).

	test(quick_check_2_09, false) :-
		quick_check(type::foo42(+integer), []).

	% quick_check/1 tests

	test(quick_check_1_01,  true(N == 100)) :-
		quick_check(atom(+atom)),
		quick_check_passed(N).

	test(quick_check_1_02, false) :-
		quick_check(atom(+integer)).

	test(quick_check_1_03, true) :-
		quick_check(type::valid({integer}, +integer)).

	test(quick_check_1_04, false) :-
		quick_check(_).

	test(quick_check_1_05, false) :-
		quick_check(1).

	test(quick_check_1_06, false) :-
		quick_check(type::foo42(+integer)).

	% auxiliary predicates

	foo(1).

	condition1(I) :-
		between(0, 127, I).

	condition2(_).

	condition3(_) :-
		fail.

	label1(I, Label) :-
		(	I mod 2 =:= 0 ->
			Label = even
		;	Label = odd
		).

	label2(I, [all, Label]) :-
		(	I mod 2 =:= 0 ->
			Label = even
		;	Label = odd
		).

	label3(_, _) :-
		fail.

	% suppress quick_check/1-3 messages and save option values for tests

	:- private(quick_check_passed/1).
	:- dynamic(quick_check_passed/1).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(quick_check_passed(NumberOfTests,_Seed,_Dicarded,_Labels), _, lgtunit, _) :-
		retractall(quick_check_passed(_)),
		assertz(quick_check_passed(NumberOfTests)).
	logtalk::message_hook(quick_check_failed(_,_,_,_), _, lgtunit, _).
	logtalk::message_hook(quick_check_error(_,_,_,_), _, lgtunit, _).
	logtalk::message_hook(quick_check_error(_,_), _, lgtunit, _).

:- end_object.
