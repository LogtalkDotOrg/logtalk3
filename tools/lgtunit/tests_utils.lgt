%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2020-04-26,
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

	:- discontiguous([
		succeeds/1, fails/1, throws/2
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

	% approximately_equal/3 tests

	succeeds(approximately_equal_3_01) :-
		approximately_equal(95.1, 100.0, 0.05).

	fails(approximately_equal_3_02) :-
		approximately_equal(94.7, 100.0, 0.05).

	throws(approximately_equal_3_03, error(instantiation_error,_)) :-
		approximately_equal(_, 100.0, 0.05).

	throws(approximately_equal_3_04, error(instantiation_error,_)) :-
		approximately_equal(95.1, _, 0.05).

	throws(approximately_equal_3_05, error(instantiation_error,_)) :-
		approximately_equal(95.1, 100.0, _).

	throws(approximately_equal_3_06, error(type_error(number,a),_)) :-
		approximately_equal(a, 100.0, 0.05).

	throws(approximately_equal_3_07, error(type_error(number,a),_)) :-
		approximately_equal(95.1, a, 0.05).

	throws(approximately_equal_3_08, error(type_error(number,a),_)) :-
		approximately_equal(95.1, 100.0, a).

	% essentially_equal/3 tests

	succeeds(essentially_equal_3_01) :-
		essentially_equal(98.7, 100.0, 0.05).

	fails(essentially_equal_3_02) :-
		essentially_equal(95.1, 100.0, 0.05).

	throws(essentially_equal_3_03, error(instantiation_error,_)) :-
		essentially_equal(_, 100.0, 0.05).

	throws(essentially_equal_3_04, error(instantiation_error,_)) :-
		essentially_equal(95.1, _, 0.05).

	throws(essentially_equal_3_05, error(instantiation_error,_)) :-
		essentially_equal(95.1, 100.0, _).

	throws(essentially_equal_3_06, error(type_error(number,a),_)) :-
		essentially_equal(a, 100.0, 0.05).

	throws(essentially_equal_3_07, error(type_error(number,a),_)) :-
		essentially_equal(95.1, a, 0.05).

	throws(essentially_equal_3_08, error(type_error(number,a),_)) :-
		essentially_equal(95.1, 100.0, a).

	% tolerance_equal/4 tests

	succeeds(tolerance_equal_4_01) :-
		tolerance_equal(0.0, 0.0, 1.0e-9, 1.0e-16).

	succeeds(tolerance_equal_4_02) :-
		tolerance_equal(100.0, 99.95, 1.0e-3, 1.0e-6).

	succeeds(tolerance_equal_4_03) :-
		tolerance_equal(100, 95, 0.05, 1).

	fails(tolerance_equal_4_04) :-
		tolerance_equal(100, 94, 0.05, 1).

	throws(tolerance_equal_4_05, error(instantiation_error,_)) :-
		tolerance_equal(_, 99.95, 1.0e-3, 1.0e-6).

	throws(tolerance_equal_4_06, error(instantiation_error,_)) :-
		tolerance_equal(100.0, _, 1.0e-3, 1.0e-6).

	throws(tolerance_equal_4_07, error(instantiation_error,_)) :-
		tolerance_equal(100.0, 99.95, _, 1.0e-6).

	throws(tolerance_equal_4_08, error(instantiation_error,_)) :-
		tolerance_equal(100.0, 99.95, 1.0e-3, _).

	throws(tolerance_equal_4_09, error(type_error(number,a),_)) :-
		tolerance_equal(a, 99.95, 1.0e-3, 1.0e-6).

	throws(tolerance_equal_4_10, error(type_error(number,a),_)) :-
		tolerance_equal(100.0, a, 1.0e-3, 1.0e-6).

	throws(tolerance_equal_4_11, error(type_error(number,a),_)) :-
		tolerance_equal(100.0, 99.95, a, 1.0e-6).

	throws(tolerance_equal_4_12, error(type_error(number,a),_)) :-
		tolerance_equal(100.0, 99.95, 1.0e-3, a).

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
		'=~='(1.0, _).

	throws('=~=_2_12', error(instantiation_error,_)) :-
		'=~='(_, 2.0).

	throws('=~=_2_13', error(instantiation_error,_)) :-
		'=~='([0.0,1.0,2.0,_], [0.0,1.0,2.0,3.0]).

	throws('=~=_2_14', error(instantiation_error,_)) :-
		'=~='([0.0,1.0,2.0,3.0], [0.0,1.0,2.0,_]).

	throws('=~=_2_15', error(instantiation_error,_)) :-
		'=~='([[0.0,1.0],[2.0,_]], [[0.0,1.0],[2.0,3.0]]).

	throws('=~=_2_16', error(instantiation_error,_)) :-
		'=~='([[0.0,1.0],[2.0,3.0]], [[0.0,1.0],[2.0,_]]).

	throws('=~=_2_17', error(type_error(number,a),_)) :-
		'=~='(1.0, a).

	throws('=~=_2_18', error(type_error(number,a),_)) :-
		'=~='(a, 2.0).

	throws('=~=_2_19', error(type_error(number,a),_)) :-
		'=~='([0.0,1.0,2.0,a], [0.0,1.0,2.0,3.0]).

	throws('=~=_2_20', error(type_error(number,a),_)) :-
		'=~='([0.0,1.0,2.0,3.0], [0.0,1.0,2.0,a]).

	throws('=~=_2_21', error(type_error(number,a),_)) :-
		'=~='([[0.0,1.0],[2.0,a]], [[0.0,1.0],[2.0,3.0]]).

	throws('=~=_2_22', error(type_error(number,a),_)) :-
		'=~='([[0.0,1.0],[2.0,3.0]], [[0.0,1.0],[2.0,a]]).

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

	% assertion/1 tests

	succeeds(assertion_1_01) :-
		% delay calling the assertion to runtime
		Assertion = integer(1),
		assertion(Assertion).

	succeeds(assertion_1_02) :-
		% delay calling the assertion to runtime
		Assertion = foo(1),
		assertion(Assertion).

	throws(assertion_1_03, assertion_failure(integer(1.1))) :-
		% delay calling the assertion to runtime
		Assertion = integer(1.1),
		assertion(Assertion).

	throws(assertion_1_04, assertion_failure(variant(1,2))) :-
		% delay calling the assertion to runtime
		Assertion = variant(1, 2),
		assertion(Assertion).

	throws(assertion_1_05, assertion_failure(foo(2))) :-
		% delay calling the assertion to runtime
		Assertion = foo(2),
		assertion(Assertion).

	throws(assertion_1_06, assertion_error(throw(e), e)) :-
		% delay calling the assertion to runtime
		Assertion = throw(e),
		assertion(Assertion).

	throws(assertion_1_07, assertion_error(foobar(1), error(existence_error(procedure,_),_))) :-
		% delay calling the assertion to runtime
		Assertion = foobar(1),
		assertion(Assertion).

	% assertion/2 tests

	succeeds(assertion_2_01) :-
		% delay calling the assertion to runtime
		Assertion = integer(1),
		assertion(1, Assertion).

	throws(assertion_2_02, assertion_failure(2)) :-
		% delay calling the assertion to runtime
		Assertion = integer(1.1),
		assertion(2, Assertion).

	throws(assertion_2_03, assertion_error(3, e)) :-
		% delay calling the assertion to runtime
		Assertion = throw(e),
		assertion(3, Assertion).

	throws(assertion_2_04, assertion_error(4, error(existence_error(procedure,_),_))) :-
		% delay calling the assertion to runtime
		Assertion = foobar(1),
		assertion(4, Assertion).

	% quick_check/3 tests

	succeeds(quick_check_3_01) :-
		quick_check(atom(+atom), Result, []),
		subsumes_term(passed(_,_,_), Result).

	succeeds(quick_check_3_02) :-
		quick_check(atom(+atom), Result, [n(25)]),
		subsumes_term(passed(_,_,_), Result).

	succeeds(quick_check_3_03) :-
		quick_check(atom(+integer), Result, []),
		Result = failed(atom(Integer), _Seed), integer(Integer).

	succeeds(quick_check_3_04) :-
		quick_check(atom(+integer), Result, [n(25)]),
		Result = failed(atom(Integer), _Seed), integer(Integer).

	succeeds(quick_check_3_05) :-
		quick_check(_, Result, []),
		subsumes_term(error(instantiation_error, _), Result).

	succeeds(quick_check_3_06) :-
		quick_check(1, Result, []),
		subsumes_term(error(type_error(callable, 1), _), Result).

	succeeds(quick_check_3_07) :-
		quick_check(type::foo42(+integer), Result, []),
		subsumes_term(error(existence_error(predicate_declaration, foo42/1), _, _), Result).

	succeeds(quick_check_3_08) :-
		quick_check(foo(@var), Result, []),
		subsumes_term(failed(foo(1), _), Result).

	succeeds(quick_check_3_09) :-
		quick_check(integer(+byte), passed(Seed, Discarded, Pairs), [l(label1), n(1000)]),
		ground(Seed),
		Discarded == 0,
		ground(Pairs), msort(Pairs, Sorted),
		Sorted = [even-Even, odd-Odd],
		integer(Even), integer(Odd), 1000 =:= Even + Odd.

	succeeds(quick_check_3_10) :-
		quick_check(integer(+byte), passed(Seed, Discarded, Pairs), [l(label2), n(1000)]),
		ground(Seed),
		Discarded == 0,
		ground(Pairs), msort(Pairs, Sorted),
		Sorted = [all-All, even-Even, odd-Odd],
		integer(All), integer(Even), integer(Odd), All =:= 1000, 1000 =:= Even + Odd.

	succeeds(quick_check_3_11) :-
		quick_check(integer(+byte), Result, [l(label3), n(1000)]),
		Result == error(label_goal_failure, label3).

	succeeds(quick_check_3_12) :-
		quick_check(integer(+byte), Result, [l(label4), n(1000)]),
		subsumes_term(error(existence_error(procedure, label4/2), label4), Result).

	succeeds(quick_check_3_13) :-
		quick_check(integer(+byte), passed(Seed, Discarded, Pairs), [pc(condition1)]),
		ground(Seed),
		integer(Discarded), Discarded > 0,
		Pairs == [].

	succeeds(quick_check_3_14) :-
		quick_check(integer(+byte), passed(Seed, Discarded, Pairs), [pc(condition2)]),
		ground(Seed),
		Discarded == 0,
		Pairs == [].

	succeeds(quick_check_3_15) :-
		quick_check(integer(+byte), Result, [pc(condition3)]),
		Result == error(pre_condition_always_fails, condition3).

	succeeds(quick_check_3_16) :-
		quick_check(integer(+byte), Result, [pc(condition4)]),
		subsumes_term(error(existence_error(procedure, condition4/1), condition4), Result).

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

	succeeds(quick_check_2_05) :-
		quick_check(type::valid({integer}, +integer), []).

	succeeds(quick_check_2_06) :-
		quick_check(type::valid({integer}, +integer), [n(50)]).

	fails(quick_check_2_07) :-
		quick_check(_, []).

	fails(quick_check_2_08) :-
		quick_check(1, []).

	fails(quick_check_2_09) :-
		quick_check(type::foo42(+integer), []).

	% quick_check/1 tests

	succeeds(quick_check_1_01) :-
		quick_check(atom(+atom)),
		quick_check_passed(N), N == 100.

	succeeds(quick_check_1_02) :-
		\+ quick_check(atom(+integer)).

	succeeds(quick_check_1_03) :-
		quick_check(type::valid({integer}, +integer)).

	fails(quick_check_1_04) :-
		quick_check(_).

	fails(quick_check_1_05) :-
		quick_check(1).

	fails(quick_check_1_06) :-
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
